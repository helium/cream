-module(cream).

-export([new/1]).

-spec new(Capacity :: non_neg_integer()) -> reference().
new(Capacity) ->
    cream_nif:with_capacity(Capacity).
