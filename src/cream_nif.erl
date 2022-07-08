-module(cream_nif).

-export([
    with_capacity/1,
    contains/2,
    insert/3,
    get/2
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec with_capacity(_Capacity :: non_neg_integer()) -> reference().
with_capacity(_Capacity) ->
    ?NOT_LOADED.

-spec insert(_Cache :: reference(), _Key :: binary(), _Value :: binary()) -> boolean().
insert(_Cache, _Key, _Value) ->
    ?NOT_LOADED.

-spec contains(_Cache :: reference(), _Key :: binary()) -> boolean().
contains(_Cache, _Key) ->
    ?NOT_LOADED.

-spec get(_Cache :: reference(), _Key :: binary()) -> notfound | {ok, term()}.
get(_Cache, _Key) ->
    ?NOT_LOADED.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

init() ->
    SoName =
        case code:priv_dir(e2qc) of
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
