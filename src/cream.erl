-module(cream).

-export([
    new/1,
    new/2,
    cache/3,
    contains/2,
    insert/3,
    get/2,
    evict/2,
    entry_count/1,
    mem_used/1,
    sync/1,
    drain/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Advanced cache options.
-type advanced_cache_opts() :: [
    %% Specify whether to use item count or key+value byte-size cache
    %% bounds.
    {bounding, items | memory}
    %% Sets the initial capacity (entries or size) of the cache.
    | {initial_capacity, ItemsOrMemory :: non_neg_integer()}
    %% A cached entry will be expired after the specified duration
    %% past from insert.
    | {seconds_to_live, Seconds :: non_neg_integer()}
    %% A cached entry will be expired after the specified duration
    %% past from get or insert.
    | {seconds_to_idle, Seconds :: non_neg_integer()}
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic API                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(
    MaxCapacity :: non_neg_integer()
) -> {ok, reference()} | {error, term()}.
new(MaxCapacity) ->
    cream_nif:new(MaxCapacity, []).

-spec new(
    MaxCapacity :: non_neg_integer(),
    CacheOpts :: advanced_cache_opts()
) -> {ok, reference()} | {error, term()}.
new(MaxCapacity, CacheOpts) ->
    cream_nif:new(MaxCapacity, CacheOpts).

-spec cache(
    Cache :: reference(),
    Key :: term(),
    ExpensiveValFun :: fun(() -> term())
) -> term().
cache(Cache, Key, ExpensiveValFun) ->
    KeyBin = term_to_binary(Key),
    case cream_nif:get(Cache, KeyBin) of
        {ok, CachedValBin} ->
            binary_to_term(CachedValBin);
        notfound ->
            Val = ExpensiveValFun(),
            ValBin = term_to_binary(Val),
            ok = cream_nif:insert(Cache, KeyBin, ValBin),
            Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Low level API                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(
    Cache :: reference(),
    Key :: term(),
    Value :: term()
) -> ok.
insert(Cache, Key, Value) ->
    KeyBin = term_to_binary(Key),
    ValueBin = term_to_binary(Value),
    cream_nif:insert(Cache, KeyBin, ValueBin).

-spec contains(
    Cache :: reference(),
    Key :: term()
) -> boolean().
contains(Cache, Key) ->
    KeyBin = term_to_binary(Key),
    cream_nif:contains(Cache, KeyBin).

-spec get(
    Cache :: reference(),
    Key :: term()
) -> notfound | {ok, term()}.
get(Cache, Key) ->
    KeyBin = term_to_binary(Key),
    case cream_nif:get(Cache, KeyBin) of
        {ok, ValueBin} -> {ok, binary_to_term(ValueBin)};
        Other -> Other
    end.

-spec evict(
    Cache :: reference(),
    Key :: term()
) -> ok.
evict(Cache, Key) ->
    KeyBin = term_to_binary(Key),
    cream_nif:evict(Cache, KeyBin).

-spec sync(
    Cache :: reference()
) -> ok.
sync(Cache) ->
    cream_nif:sync(Cache).

-spec entry_count(
    Cache :: reference()
) -> non_neg_integer().
entry_count(Cache) ->
    cream_nif:entry_count(Cache).

-spec mem_used(
    Cache :: reference()
) -> non_neg_integer().
mem_used(Cache) ->
    cream_nif:mem_used(Cache).

-spec drain(
    Cache :: reference()
) -> ok.
drain(Cache) ->
    cream_nif:drain(Cache).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing                                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_all_features_test() ->
    {ok, Cache} = cream:new(3),

    1 = cream:cache(Cache, 1, fun() -> 1 end),
    ?assertEqual(true, cream:contains(Cache, 1)),
    ?assertEqual(false, cream:contains(Cache, <<2>>)),
    ?assertEqual(false, cream:contains(Cache, three)),
    ?assertEqual(false, cream:contains(Cache, "four")),

    two = cream:cache(Cache, <<2>>, fun() -> two end),
    ?assertEqual(true, cream:contains(Cache, 1)),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(false, cream:contains(Cache, three)),
    ?assertEqual(false, cream:contains(Cache, "four")),

    "three" = cream:cache(Cache, three, fun() -> "three" end),
    ?assertEqual(true, cream:contains(Cache, 1)),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(true, cream:contains(Cache, three)),
    ?assertEqual(false, cream:contains(Cache, "four")),

    <<"four">> = cream:cache(Cache, "four", fun() -> <<"four">> end),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(true, cream:contains(Cache, three)),
    ?assertEqual(true, cream:contains(Cache, "four")),

    ?assertEqual({ok, <<"four">>}, cream:get(Cache, "four")),
    ?assertEqual({ok, "three"}, cream:get(Cache, three)),
    ?assertEqual({ok, two}, cream:get(Cache, <<2>>)),
    ok = cream:evict(Cache, three),

    %% The cache is eventually consistent, so we need to force `sync'
    %% it to guarantee that `count' is accurate.
    ok = cream:sync(Cache),
    ?assertEqual(notfound, cream:get(Cache, three)),

    ok = cream:drain(Cache),
    ok = cream:sync(Cache),
    ?assertEqual(0, cream:entry_count(Cache)),

    ok.

-endif.
