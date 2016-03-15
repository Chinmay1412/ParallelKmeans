-module(serialkmeans).
-export([run/4,init/2]).
-compile(inline).

init(K, FileName) ->
    Data = read_points(FileName),
    InitCentroids = lists:sublist(Data, K),
    run(Data,K,5,InitCentroids).

read_points(FileName) ->
    {ok, Data} = file:read_file(FileName),
    [<<>>|Bins] = re:split(Data, "[][,]+", [trim]),
    points([binary_to_float(X) || X <- Bins]).

points([]) -> [];
points([H1, H2 | T]) ->
    [{H1, H2} | points(T)].


run(Xs, K, Iters, InitCentroids) ->
	Result = [ {Mean,{sum(Points),length(Points)}} || {Mean,Points} <- clusters(Xs,InitCentroids) ],

    Fun = fun(E) -> io:format(user,"~p ",[E]) end,
    Fun2 = fun(E) -> 
                lists:foreach(Fun,E),
                io:format(user," || ",[]) 
            end,

    io:format(user,"Current Node: ~p~n",[node()]),
    io:format(user,"Intial Centroids: ",[]),
    lists:foreach(Fun,InitCentroids),
    io:format(user,"~n",[]),
    io:format(user,"Final Centroids: ",[]),
    lists:foreach(Fun,Result),
    io:format(user,"~n",[]),
    
    
    
    receive
        {calc, From} ->
            From ! {centroids,Result,self()}
        %Message ->
        %    io:format(user,"Message: ~p~n",[Message])
    end.

divide({Px,Py}, K) ->
    {Px/K, Py/K}.

add({Px1, Py1}, {Px2, Py2}) ->
    {(Px1+Px2), (Py1+Py2)}.

sub({Px1, Py1}, {Px2, Py2}) ->
    {(Px1-Px2), (Py1-Py2)}.

sq(X) ->
    X*X.

modulus({Px, Py}) ->
    math:sqrt((sq(Px) + sq(Py))).

dist(P1, P2) ->
    modulus(sub(P1,P2)).

average(Q) ->
    divide(sum(Q),length(Q)).

sum(L) ->
    Add = fun(X, Acc) -> add(X, Acc) end,
    lists:foldl(Add, {0.0, 0.0}, L).

closest(P, Centroids) ->
    element(2, lists:min([{dist(P, C), C} || C <- Centroids])).

clusters(Xs, Centroids) ->
    groupBy(Xs, fun(X) -> closest(X, Centroids) end).

groupBy(L, Fn) ->
    Group = fun(X, Dict) ->
                    Add = fun(T) -> [X|T] end,
                    dict:update(Fn(X), Add, [X], Dict)
            end,
    Dict = lists:foldl(Group, dict:new(), L),
    [ {Mean,V} || {Mean,V} <- dict:to_list(Dict)].