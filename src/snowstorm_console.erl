-module(snowstorm_console).
-export([start/0]).

%% start application
start() ->
    % add path
    [code:add_path(P) || P <- filelib:wildcard("{apps,deps,lib}/*/ebin")],
    % start apps
    Apps = [A || A <- orderapps(), not(lists:member(A, [kernel, stdlib]))],
    [{A, application:start(A)} || A <- Apps].

%% clone from mad/'Maxim Sokhatsky'
%%
sort(Pairs) -> iterate(Pairs, [], lhs(Pairs) ++ rhs(Pairs)).
lhs(L) -> [X || {X, _} <- L].
rhs(L) -> [Y || {_, Y} <- L].
remove_pairs(L1, L2) -> [All || All={X, _Y} <- L2, not lists:member(X, L1)].
subtract(L1, L2) -> [X || X <- L1, not lists:member(X, L2)].
iterate([], L, All) -> {ok,remove_duplicates(L ++ subtract(All, L))};
iterate(Pairs, L, All) ->
    case subtract(lhs(Pairs), rhs(Pairs)) of
        []  -> io:format("Cycling Apps: ~p~n\r", [Pairs]);
        Lhs -> iterate(remove_pairs(Lhs, Pairs), L ++ Lhs, All) end.

remove_duplicates([]) -> [];
remove_duplicates([H|T]) ->
    case lists:member(H, T) of
          true  -> remove_duplicates(T);
          false -> [H|remove_duplicates(T)] end.

orderapps() ->
    Pairs = lists:flatten([ case 
       file:consult(F) of
         {ok,[{application,Name,Opt}]} -> 
              Apps = proplists:get_value(applications,Opt,[]),
              [ { A,Name} || A <- Apps ];
         {error,_} -> io:format("AppName: ~p~n",[F]), skip
    end || F <- filelib:wildcard("{apps,deps,lib}/*/ebin/*.app")  ++ 
                filelib:wildcard("ebin/*.app"), not filelib:is_dir(F) ]),
    {ok,Sorted} = sort(Pairs),
    Sorted.
%%
%% ----
