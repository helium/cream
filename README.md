Cache Rules Everything Around Me
================================

`cream` is a an Erlang caching module backed by the performant [Moka] library.

## Cache options

Creating a cache is as simple as calling `cream:new/1` with the max capacity of the cache. In addition to capacity, you can create a cache by calling `cream:new/2` with a property list of advanced cache options.

| Option             | Type             | Unit    | Default | Description                                                                |
|--------------------|------------------|---------|---------|----------------------------------------------------------------------------|
| `initial_capacity` | positive integer | items   |       0 | Size cache can grow to without reallocation.                               |
| `seconds_to_live`  | positive integer | seconds |       ∞ | How long until entries will be evicted after first caching.                |
| `seconds_to_idle`  | positive integer | seconds |       ∞ | How long until entries will be evicted after last access.                  |

## Examples

Create a cache that can hold up 10 items:

```erlang
MaxCapacity = 10,
{ok, Cache} = cream:new(MaxCapacity).
```

Create a cache that can hold up 62 items, where items are evicted 5 minutes after they are first cached:

```erlang
MaxCapacity = 62,
TTLSeconds = 5 * 60,
{ok, Cache} = cream:new(MaxCapacity, [{seconds_to_live, TTLSeconds}]).
```


Create a cache that can hold up 10,000 items (without reallocation), where items are evicted 5 seconds after they are last accessed (written or read):

```erlang
InitialCapacity = 10000,
MaxCapacity = 10000,
TTISeconds = 5,
{ok, Cache} = cream:new(MaxCapacity, [{initial_capacity, InitialCapacity}, {seconds_to_idle, TTISeconds}]).
```

## Dependencies

Because this module is based on Rust NIFs, you need to have a Rust toolchain [installed].

[Moka]: https://github.com/moka-rs/moka
[installed]: https://rustup.rs
