
-module(dqsd_otel_tcp_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case gen_tcp:listen(8081, [binary, {packet, line}, {active, false}, {reuseaddr, true}]) of
    {ok, ListenSocket} ->
        spawn(fun() -> accept_loop(ListenSocket) end),
    
    {ok, #{socket => ListenSocket}};
        {error, Reason} ->
            {error, Reason}
    end.
    
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).


handle_client(Socket) ->
     case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Trimmed = binary:replace(Line, <<"\n">>, <<>>, [global]),
            io:format("~p~n", [Trimmed]),
            dqsd_otel:handle_c_message(Trimmed),
            handle_client(Socket);
        {error, closed} ->
             io:format("Closed"),
            ok
    end.
