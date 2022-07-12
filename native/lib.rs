use moka::sync::{Cache, ConcurrentCacheExt};
use rustler::{atoms, Atom, Binary, Encoder, Env, ListIterator, OwnedBinary, ResourceArc, Term};
use rustler_stored_term::term_box::TermBox;
use std::{
    borrow::Borrow,
    hash::{Hash, Hasher},
    time::Duration,
};

////////////////////////////////////////////////////////////////////////////
// NIFs                                                                   //
////////////////////////////////////////////////////////////////////////////

#[rustler::nif]
fn new<'a>(env: Env<'a>, max_capacity: u64, advanced_opts: ListIterator<'a>) -> Term<'a> {
    let mut builder = Cache::builder().max_capacity(max_capacity);

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
            _ => return (atoms::error(), ("invalid option", opt)).encode(env),
        }
    }
    (atoms::ok(), ResourceArc::new(Cream(builder.build()))).encode(env)
}

#[rustler::nif]
fn insert(cache: ResourceArc<Cream>, key: Binary, value: Term) -> Atom {
    cache.insert(Bin(key.to_owned().unwrap()), TermBox::new(&value));
    atoms::ok()
}

#[rustler::nif]
fn contains(cache: ResourceArc<Cream>, key: Binary) -> bool {
    cache.contains_key(&Bin(key.to_owned().unwrap()))
}

#[rustler::nif]
fn get<'a>(env: Env<'a>, cache: ResourceArc<Cream>, key: Binary) -> Term<'a> {
    match cache.get(&Bin(key.to_owned().unwrap())) {
        Some(term_box) => (atoms::ok(), term_box.get(env)).encode(env),
        None => atoms::notfound().encode(env),
    }
}

#[rustler::nif]
fn evict(cache: ResourceArc<Cream>, key: Binary) -> Atom {
    cache.invalidate(&Bin(key.to_owned().unwrap()));
    atoms::ok()
}

#[rustler::nif]
fn sync(cache: ResourceArc<Cream>) -> Atom {
    cache.sync();
    atoms::ok()
}

#[rustler::nif]
fn count(cache: ResourceArc<Cream>) -> u64 {
    cache.entry_count()
}

////////////////////////////////////////////////////////////////////////////
// Helpers                                                                //
////////////////////////////////////////////////////////////////////////////

/// A newtype wrapper around OwnedBinary for implementing missing
/// traits.
struct Bin(OwnedBinary);

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

struct Cream(Cache<Bin, TermBox>);

impl std::ops::Deref for Cream {
    type Target = Cache<Bin, TermBox>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

////////////////////////////////////////////////////////////////////////////
// Setup                                                                  //
////////////////////////////////////////////////////////////////////////////

mod atoms {
    super::atoms! {
        already_exists,
        error,
        initial_capacity,
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
    "cream",
    [new, insert, contains, get, evict, sync, count],
    load = load
);
