-module(debug@foreign).
-export(['_trace'/2, '_spy'/2]).

'_trace'(X,Res) ->
    io:format(user, "~p~n", [X]),
    Res(unit).
'_spy'(S,X) ->
    io:format(user, "~ts:~p~n", [S, X]),
    X.
