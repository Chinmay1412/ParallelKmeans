-module(parallelkmeans).
-export([init/3]).

init(Nnodes, K, FileName) ->
	StartTime = now_ms(),
	Data = read_points(FileName),
	Reminder = length(Data) rem Nnodes,
	Division = length(Data) div Nnodes,
	io:format(user,"Total Nodes: ~p ~n", [Nnodes]),	
	io:format(user,"Parameter K: ~p ~n", [K]),
	io:format(user,"Total Points: ~p ~n", [length(Data)]),
	io:format(user,"Reminder: ~p ~n", [Reminder]),
	io:format(user,"Partition Length: ~p ~n", [Division]),
	Fun = fun(E) -> io:format(user,"~p ",[E]) end,
	Fun2 = fun(E) -> 
				lists:foreach(Fun,E),
				io:format(user," || ",[]) 
			end,
	
	%io:format(user,"Points: ",[]),
	%lists:foreach(Fun,Data),
	%io:format(user,"~n",[]),
	
	if
		Reminder == 0 ->
			Splitls = [ lists:sublist(Data, X, Division) || X <- lists:seq(1,length(Data),Division)];
		Reminder > 0 ->
			Limit = Division*(Nnodes-1),
			Partls = [ lists:sublist(Data, X, Division) || X <- lists:seq(1,Limit,Division)],
			Splitls = lists:append(Partls, [ lists:sublist(Data, Limit+1, length(Data)) ])
	end,
	
	%io:format(user,"Sublists: ",[]),
	%lists:foreach(Fun2,Splitls),
	%io:format(user,"~n",[]),

	io:format(user,"Initial Centroids: ",[]),
	InitCentroids = lists:sublist(Data, K),
	lists:foreach(Fun,InitCentroids),
	io:format(user,"~n",[]),	

	Iters = 15,
	calculate(Nnodes,K,Splitls,InitCentroids,Iters),
	EndTime = now_ms(),
	io:format(user,"Total Time in milliseconds: ~p~n", [(EndTime - StartTime)]).

calculate(Nnodes,K,Splitls,Centroids,Iters) ->
	
	Func1 = fun(X, Dict) ->
            	dict:store(X,{{0.0,0.0},0},Dict)        
            end,
    NewIter = Iters-1,
	Dict = lists:foldl(Func1,dict:new(),Centroids),

	
	NodeNames = nodes(),
	Fun = fun(E) -> io:format(user,"~p ",[E]) end,
	Fun2 = fun(E) -> 
				lists:foreach(Fun,E),
				io:format(user," || ",[]) 
			end,

	Pids = [ spawn(lists:nth(X,NodeNames), serialkmeans,run,[lists:nth(X,Splitls),K,Iters,Centroids])  || X <- lists:seq(1,Nnodes) ],
	io:format(user,"All worker Pids: ",[]),
	lists:foreach(Fun,Pids),
	io:format(user,"~n",[]),

	SendFun = fun(E) -> E ! {calc,self()} end,
	lists:map(SendFun, Pids),

	Clusters = loopreceive(Nnodes,1,[]),
	%io:format(user,"Clusters: ",[]),
	%lists:foreach(Fun,Clusters),
	%io:format(user,"~n",[]),

	Func2 = fun({{X,Y},_}) ->
				{X,Y}
			end,
	Func4 = fun({_,{{Sumx,Sumy},Npoints}}) ->
				{{Sumx,Sumy},Npoints}
			end,		


	Func3 = fun(X, PDict) ->
            	Add = fun(T) -> 
	            		sum(T,Func4(X))
	            	end,
            	dict:update(Func2(X),Add,{{0.0,0.0},0},PDict)       
            end,
 
	FDict = lists:foldl(Func3,Dict,Clusters),
	FinalList = [findmean(X) || X <- dict:to_list(FDict)],
	io:format(user,"New Means:~n",[]),
	lists:foreach(Fun,FinalList),
	io:format(user,"~n",[]),
	if
		NewIter /= 0 ->
			calculate(Nnodes,K,Splitls,FinalList,NewIter);
		true -> ok
	end.

sum({{Sumx1,Sumy1},Npoints1},{{Sumx,Sumy},Npoints}) ->
	{{Sumx1+Sumx,Sumy1+Sumy},Npoints1+Npoints}.

findmean({_,{{Sumx,Sumy},Npoints}}) ->
	{Sumx/Npoints,Sumy/Npoints}.


loopreceive(Nnodes,Workers,Clusters) ->
	Fun = fun(E) -> io:format(user,"~p ",[E]) end,
	receive
		{centroids, FinalCentroids, From} ->
			NewClusters = lists:append(FinalCentroids , Clusters),
			io:format(user,"Final Centroids from process ~p :",[From]),
			lists:foreach(Fun,FinalCentroids),
			io:format(user,"~n",[]),
			if
				Workers /= Nnodes ->
					loopreceive(Nnodes,Workers+1,NewClusters);
				true -> NewClusters
			end
	end.	

read_points(FileName) ->
	{ok, Data} = file:read_file(FileName),
	[<<>>|Bins] = re:split(Data, "[][,]+", [trim]),
	points([binary_to_float(X) || X <- Bins]).


points([]) -> [];
points([H1, H2 | T]) ->
	[{H1, H2} | points(T)].

now_ms() ->
    {MegaSecs,Secs,MicroSecs} = erlang:now(),
    ((MegaSecs*1000000 + Secs)*1000000 + MicroSecs) / 1000.  