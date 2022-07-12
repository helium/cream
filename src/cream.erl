-module(cream).

-export([
    new/1,
    new/2,
    cache/3,
    contains/2,
    insert/3,
    get/2,
    evict/2,
    count/1,
    sync/1
]).

-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Advanced cache options.
-type advanced_cache_opts() :: [
    %% Sets the initial capacity (number of entries) of the cache.
    {initial_capacity, Items :: non_neg_integer()} |
    %% A cached entry will be expired after the specified duration
    %% past from insert.
    {seconds_to_live, Seconds :: non_neg_integer()} |
    %% A cached entry will be expired after the specified duration
    %% past from get or insert.
    {seconds_to_idle, Seconds :: non_neg_integer()}
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(
    MaxCapacity :: non_neg_integer()
) -> {ok, reference()} | {error, term()}.
new(MaxCapacity) ->
    new(MaxCapacity, []).

-spec cache(
    Cache :: reference(),
    Key :: term(),
    ExpensiveValFun :: fun(() -> term())
) -> term().
cache(Cache, Key, ExpensiveValFun) ->
    KeyBin = term_to_binary(Key),
    case ?MODULE:get(Cache, KeyBin) of
        {ok, CachedVal} ->
            CachedVal;
        notfound ->
            Val = ExpensiveValFun(),
            ok = ?MODULE:insert(Cache, KeyBin, Val),
            Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public NIFs                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NOT_LOADED, not_loaded(?LINE)).

-spec new(
    MaxCapacity :: non_neg_integer(),
    CacheOpts :: advanced_cache_opts()
) -> {ok, reference()} | {error, term()}.
new(_MaxCapacity, _CacheOpts) ->
    ?NOT_LOADED.

-spec insert(
    Cache :: reference(),
    Key :: binary(),
    Value :: term()
) -> ok.
insert(_Cache, _Key, _Value) ->
    ?NOT_LOADED.

-spec contains(
    Cache :: reference(),
    Key :: binary()
) -> boolean().
contains(_Cache, _Key) ->
    ?NOT_LOADED.

-spec get(
    Cache :: reference(),
    Key :: binary()
) -> notfound | {ok, term()}.
get(_Cache, _Key) ->
    ?NOT_LOADED.

-spec evict(
    Cache :: reference(),
    Key :: binary()
) -> ok.
evict(_Cache, _Key) ->
    ?NOT_LOADED.

-spec sync(
    Cache :: reference()
) -> ok.
sync(_Cache) ->
    ?NOT_LOADED.

-spec count(
    Cache :: reference()
) -> non_neg_integer().
count(_Cache) ->
    ?NOT_LOADED.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing                                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_all_feature__test() ->
    {ok, Cache} = cream:new(3),

    ok = cream:insert(Cache, <<1>>, 1),
    ?assertEqual(true, cream:contains(Cache, <<1>>)),
    ?assertEqual(false, cream:contains(Cache, <<2>>)),
    ?assertEqual(false, cream:contains(Cache, <<3>>)),
    ?assertEqual(false, cream:contains(Cache, <<4>>)),

    ok = cream:insert(Cache, <<2>>, two),
    ?assertEqual(true, cream:contains(Cache, <<1>>)),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(false, cream:contains(Cache, <<3>>)),
    ?assertEqual(false, cream:contains(Cache, <<4>>)),

    ok = cream:insert(Cache, <<3>>, "three"),
    ?assertEqual(true, cream:contains(Cache, <<1>>)),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(true, cream:contains(Cache, <<3>>)),
    ?assertEqual(false, cream:contains(Cache, <<4>>)),

    ok = cream:insert(Cache, <<4>>, <<"four">>),
    ?assertEqual(true, cream:contains(Cache, <<2>>)),
    ?assertEqual(true, cream:contains(Cache, <<3>>)),
    ?assertEqual(true, cream:contains(Cache, <<4>>)),

    ?assertEqual({ok, <<"four">>}, cream:get(Cache, <<4>>)),
    ?assertEqual({ok, "three"}, cream:get(Cache, <<3>>)),
    ?assertEqual({ok, two}, cream:get(Cache, <<2>>)),
    ok = cream:evict(Cache, <<3>>),

    %% The cache is eventually consistent, so we need to force `sync'
    %% it to guarantee that `count' is accurate.
    ok = cream:sync(Cache),
    ?assertEqual(notfound, cream:get(Cache, <<3>>)),

    ok.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

init() ->
    SoName =
        case code:priv_dir(cream) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?MODULE]);
                    false ->
                        filename:join([priv, ?MODULE])
                end;
            Dir ->
                filename:join(Dir, ?MODULE)
        end,
    ok = erlang:load_nif(SoName, 0).
