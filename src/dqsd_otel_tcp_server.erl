-module(dqsd_otel_tcp_server).
-behaviour(gen_server).
-export([start_link/0, start_server/2, stop_server/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {socket, acceptor}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_server(IP, Port) ->
    gen_server:call(?MODULE, {start, IP, Port}).

stop_server() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.

handle_call({start, IP, Port}, _From, State) ->
    Options = [binary, {packet, line}, {active, false}, {reuseaddr, true}, {ip, parse_ip(IP)}],
    case gen_tcp:listen(Port, Options) of
        {ok, Socket} ->
            Acceptor = spawn(fun() -> accept_loop(Socket) end),
            io:format("dqsd_otel: Listening socket started on ~p:~p ~n", [IP, Port]),
            {reply, ok, State#state{socket = Socket, acceptor = Acceptor}};
        {error, Reason} ->
            io:format("dqsd_otel: Could not start listening socket on ~p:~p ~n", [IP, Port]),
            {reply, {error, Reason}, State}
    end;




handle_call(stop, _From, #state{socket = undefined, acceptor = undefined} = State) ->
    io:format("dqsd_otel: Server not running.~n"),
    {reply, {error, not_running}, State};

handle_call(stop, _From, #state{socket = Socket, acceptor = Acceptor}) ->
    catch exit(Acceptor, shutdown),
    gen_tcp:close(Socket),
    io:format("dqsd_otel: Stopped listening from oscilloscope"),
    {reply, ok, #state{socket = undefined, acceptor = undefined}}.

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Trimmed = binary:replace(Line, <<"\n">>, <<>>, [global]),
            dqsd_otel:handle_c_message(Trimmed),
            handle_client(Socket);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

parse_ip("127.0.0.1") -> {127,0,0,1};
parse_ip("0.0.0.0") -> {0,0,0,0};
parse_ip(IP) when is_list(IP) -> 
    {ok, Addr} = inet:parse_address(IP), Addr;
parse_ip(IP) -> IP.
