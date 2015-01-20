-module(snowstorm_n2o_route).
-author('Homeway Xue').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2, route/1]).
-export([bindings/0, id/0, resource/0, path/0]).

init(State, Ctx) -> handle(State, Ctx).
finish(State, Ctx) -> {ok, State, Ctx}.

%% @doc 支持外部路由转入
handle(State, Ctx) ->
    Path = pp:to_binary(wf:path(Ctx#cx.req)),
    {ok, State, req(Ctx, Path, route(Path))}.

req(Ctx, Path, {Module, Bindings})  -> Ctx#cx{path=Path, module=Module, state=[{snowstorm_n2o_route, Bindings}|Ctx#cx.state]};
req(Ctx, Path, Module)              -> Ctx#cx{path=Path, module=Module}.

route(<<"/ws/",P/binary>>) -> route2(P);
route(<<"/",P/binary>>) -> route2(P);
route(P) -> route2(P).

%% @doc 从sys.config中读取route_prefix
%%
%% 例如: {route_prefix, ["", "pp_cube_view"]}
route2(<<>>) -> index;
route2(P1) ->
    Pres = wf:config(n2o, route_prefix, [<<>>]),
    P = pp:to_list(P1),
    route2_acc(P, Pres).

route2_acc(P, [H|[]]) ->
    try_route(P, index, H);
route2_acc(P, [H|Rest]) ->
    case try_route(P, none, H) of
        none -> route2_acc(P, Rest);
        Result -> Result
    end.

%% @doc 构造模块名称
try_route(Path, Default, Prefix) ->
    Tokens = string:tokens(Path, "/"),
    case Prefix of
      "" -> PrefxTokens = [];
      <<>> -> PrefxTokens = [];
      _ -> PrefxTokens = [Prefix]
    end,
    case try_route_module([R|_]=Tokens, PrefxTokens) of
      {M, K} -> {M, #{resource=>R, id=>K}};
      _ -> Default
    end.

%% @doc 如果找到的模块导出main/0, 就认为是处理路由的函数
try_route_module([], _) -> <<>>;
try_route_module([H|Rest], Pre) ->
    AllTokens = Pre ++ [H],
    M = list_to_atom(string:join(AllTokens, "_")),
    %erlang:display(M),
    code:ensure_loaded(M),
    case erlang:function_exported(M, main, 0) of
        true -> {M, string:join(Rest, "_")};
        _ -> try_route_module(Rest, AllTokens)
    end.

%% @doc 工具函数 --------------------------------------

%% @doc 获取页面绑定变量
bindings() -> proplists:get_value(snowstorm_n2o_route, (wf:context())#cx.state).
id()      -> maps:get(id, bindings()).
resource() -> maps:get(resource, bindings()).
path()     -> binary_to_list(wf:path((wf:context())#cx.req)).
