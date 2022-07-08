use moka::sync::Cache;
use rustler::{atoms, Atom, Binary, Encoder, Env, OwnedBinary, ResourceArc, Term};
use rustler_stored_term::term_box::TermBox;
use std::{borrow::Borrow, hash::Hash};

////////////////////////////////////////////////////////////////////////////
// NIFs                                                                   //
////////////////////////////////////////////////////////////////////////////

#[rustler::nif]
fn with_capacity(capacity: u64) -> ResourceArc<Cream> {
    ResourceArc::new(Cream(Cache::new(capacity)))
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

////////////////////////////////////////////////////////////////////////////
// Helpers                                                                //
////////////////////////////////////////////////////////////////////////////

/// A newtype wrapper around OwnedBinary for implementing missing
/// traits.
#[derive(Hash)]
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
        notfound,
        ok,
    }
}

pub fn load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(Cream, env);
    true
}

rustler::init!(
    "cream_nif",
    [with_capacity, insert, contains, get],
    load = load
);
