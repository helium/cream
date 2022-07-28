use moka::sync::{Cache, ConcurrentCacheExt};
use rustler::{
    atoms, Atom, Binary, Encoder, Env, ListIterator, NewBinary, OwnedBinary, ResourceArc, Term,
};
use std::{
    borrow::Borrow,
    hash::{Hash, Hasher},
    sync::Arc,
    time::Duration,
};

////////////////////////////////////////////////////////////////////////////
// NIFs                                                                   //
////////////////////////////////////////////////////////////////////////////

#[rustler::nif]
fn new<'a>(env: Env<'a>, max_capacity: u64, advanced_opts: ListIterator<'a>) -> Term<'a> {
    let mut builder = Cache::builder().max_capacity(max_capacity);
    let mut memory_bound = false;
    for opt in advanced_opts {
        match opt.decode::<(Atom, u64)>() {
            Ok((atom, val)) if atom == atoms::initial_capacity() => {
                builder = builder.initial_capacity(val as usize)
            }
            Ok((atom, val)) if atom == atoms::seconds_to_live() => {
                builder = builder.time_to_live(Duration::from_secs(val))
            }
            Ok((atom, val)) if atom == atoms::seconds_to_idle() => {
                builder = builder.time_to_idle(Duration::from_secs(val))
            }
            _ => match opt.decode::<(Atom, Atom)>() {
                Ok((atom, val)) if atom == atoms::bounding() && val == atoms::items() => (),
                Ok((atom, val)) if atom == atoms::bounding() && val == atoms::memory() => {
                    memory_bound = true;
                    builder = builder.weigher(entry_weight_fn);
                }
                _ => return (atoms::error(), ("invalid option", opt)).encode(env),
            },
        }
    }
    (
        atoms::ok(),
        ResourceArc::new(Cream {
            cache: builder.build(),
            memory_bound,
        }),
    )
        .encode(env)
}

#[rustler::nif]
fn insert(cache: ResourceArc<Cream>, key: Binary, value: Binary) -> Atom {
    cache.insert(
        Bin(key.to_owned().expect("no mem")),
        Arc::new(Bin(value.to_owned().expect("no mem"))),
    );
    atoms::ok()
}

#[rustler::nif]
fn contains(cache: ResourceArc<Cream>, key: Binary) -> bool {
    cache.contains_key(key.as_slice())
}

#[rustler::nif]
fn get<'a>(env: Env<'a>, cache: ResourceArc<Cream>, key: Binary) -> Term<'a> {
    match cache.get(key.as_slice()) {
        Some(value) => (atoms::ok(), value.to_unowned(env)).encode(env),
        None => atoms::notfound().encode(env),
    }
}

#[rustler::nif]
fn evict(cache: ResourceArc<Cream>, key: Binary) -> Atom {
    cache.invalidate(key.as_slice());
    atoms::ok()
}

#[rustler::nif]
fn sync(cache: ResourceArc<Cream>) -> Atom {
    cache.sync();
    atoms::ok()
}

#[rustler::nif]
fn entry_count(cache: ResourceArc<Cream>) -> u64 {
    cache.entry_count()
}

#[rustler::nif]
fn mem_used(env: Env<'_>, cache: ResourceArc<Cream>) -> Term<'_> {
    if cache.memory_bound {
        (atoms::ok(), cache.weighted_size()).encode(env)
    } else {
        (
            atoms::error(),
            "cache was not created with `{bounding, memory}'",
        )
            .encode(env)
    }
}

#[rustler::nif]
fn drain(cache: ResourceArc<Cream>) -> Atom {
    cache.invalidate_all();
    atoms::ok()
}

////////////////////////////////////////////////////////////////////////////
// Helpers                                                                //
////////////////////////////////////////////////////////////////////////////

/// A newtype wrapper around OwnedBinary for implementing missing
/// traits.
struct Bin(OwnedBinary);

impl Bin {
    fn to_unowned<'a>(&self, env: Env<'a>) -> Binary<'a> {
        let mut newbin = NewBinary::new(env, self.0.as_slice().len());
        newbin.as_mut_slice().copy_from_slice(self.0.as_slice());
        newbin.into()
    }

    fn len(&self) -> usize {
        self.0.as_slice().len()
    }
}

impl Borrow<[u8]> for Bin {
    fn borrow(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl PartialEq for Bin {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_slice() == other.0.as_slice()
    }
}

impl Hash for Bin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_slice().hash(state)
    }
}

impl Eq for Bin {}

/// # Safety
///
/// See https://github.com/rusterlium/rustler/issues/469
unsafe impl Sync for Bin {}

////////////////////////////////////////////////////////////////////////////
// Resource                                                               //
////////////////////////////////////////////////////////////////////////////

// Note that values are wrapped in an Arc because Moka makes pretty
// heavy use of cloning:
//
// https://docs.rs/moka/latest/moka/sync/struct.Cache.html#avoiding-to-clone-the-value-at-get
//
struct Cream {
    //           Key
    //           |    Value
    //           |    |
    cache: Cache<Bin, Arc<Bin>>,
    memory_bound: bool,
}

// Weight function provided to Moka when user specifies `{bounding,
// memory}`.
fn entry_weight_fn(k: &Bin, v: &Arc<Bin>) -> u32 {
    (k.len() + v.len()) as u32
}

impl std::ops::Deref for Cream {
    type Target = Cache<Bin, Arc<Bin>>;
    fn deref(&self) -> &Self::Target {
        &self.cache
    }
}

////////////////////////////////////////////////////////////////////////////
// Setup                                                                  //
////////////////////////////////////////////////////////////////////////////

mod atoms {
    super::atoms! {
        bounding,
        error,
        initial_capacity,
        items,
        memory,
        notfound,
        ok,
        seconds_to_idle,
        seconds_to_live,
    }
}

pub fn load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(Cream, env);
    true
}

rustler::init!(
    "cream_nif",
    [
        new,
        insert,
        contains,
        get,
        evict,
        sync,
        entry_count,
        mem_used,
        drain,
    ],
    load = load
);
