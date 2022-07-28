-module(cream_nif).

-export([
    new/2,
    contains/2,
    insert/3,
    get/2,
    evict/2,
    entry_count/1,
    mem_used/1,
    sync/1,
    drain/1
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

-spec new(
    MaxCapacity :: non_neg_integer(),
    CacheOpts :: term()
) -> {ok, reference()} | {error, Reason :: term()}.
new(_MaxCapacity, _CacheOpts) ->
    ?NOT_LOADED.

-spec insert(
    Cache :: reference(),
    Key :: binary(),
    Value :: binary()
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
) -> notfound | {ok, binary()}.
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

-spec entry_count(
    Cache :: reference()
) -> non_neg_integer().
entry_count(_Cache) ->
    ?NOT_LOADED.

-spec mem_used(
    Cache :: reference()
) -> {ok, non_neg_integer()} | {error, Reason :: term()}.
mem_used(_Cache) ->
    ?NOT_LOADED.

-spec drain(
    Cache :: reference()
) -> ok.
drain(_Cache) ->
    ?NOT_LOADED.

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
