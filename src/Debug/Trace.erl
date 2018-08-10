-module(debug_trace@foreign).
-export([trace/3, spy/3]).

trace(_Dict,X,Res) ->
    io:format(user, "~p~n", [X]),
    Res(unit).
spy(_Dict,S,X) ->
    io:format(user, "~ts:~p~n", [S, X]),
    X.