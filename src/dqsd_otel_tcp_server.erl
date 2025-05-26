%%%-------------------------------------------------------------------
%%% @doc
%%% TCP server for receiving and processing messages from an oscilloscope.
%%% Allows starting and stopping a TCP server on a given IP and port.
%%%
%%% When a line-delimited binary message is received, it is passed to
%%% `dqsd_otel:handle_c_message/1` for further processing.
%%%
%%%-------------------------------------------------------------------

-module(dqsd_otel_tcp_server).
-behaviour(gen_server).

%%% API
-export([start_link/0, start_server/2, stop_server/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    socket,    %% Listening socket
    acceptor   %% PID of acceptor process
}).

-spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts the TCP listener on the given IP and Port.
%% Spawns an accept loop to handle incoming connections.
%%
%% IP can be a string, binary, or tuple (e.g., "127.0.0.1" or {127,0,0,1}).
%% Port is an integer.
%%--------------------------------------------------------------------

-spec start_server(string() | binary() | tuple(), integer()) -> ok | {error, Reason}
start_server(IP, Port) ->
    gen_server:call(?MODULE, {start, IP, Port}).

%%--------------------------------------------------------------------
%% @doc Stops the TCP server and closes the listening socket.
%% Also shuts down the acceptor process.
%%
%% @spec stop_server() -> ok | {error, not_running}
%%--------------------------------------------------------------------
stop_server() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @private
%% gen_server init callback.
%% Initializes the state without an open socket or acceptor.
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% Handles the start request. Binds to the given IP and port.
%% On success, starts the accept loop.
%%--------------------------------------------------------------------
handle_call({start, IP, Port}, _From, State) ->
    Options = [binary, {packet, line}, {active, false}, {reuseaddr, true}, {ip, parse_ip(IP)}],
    case gen_tcp:listen(Port, Options) of
        {ok, Socket} ->
            Acceptor = spawn(fun() -> accept_loop(Socket) end),
            io:format("dqsd_otel: Listening socket started on ~p:~p~n", [IP, Port]),
            {reply, ok, State#state{socket = Socket, acceptor = Acceptor}};
        {error, Reason} ->
            io:format("dqsd_otel: Could not start listening socket on ~p:~p~n", [IP, Port]),
            {reply, {error, Reason}, State}
    end;

%%--------------------------------------------------------------------
%% @private
%% Handles stop request. Stops acceptor and closes socket.
%% Returns error if server is not running.
%%--------------------------------------------------------------------
handle_call(stop, _From, #state{socket = undefined, acceptor = undefined} = State) ->
    io:format("dqsd_otel: Server not running.~n"),
    {reply, {error, not_running}, State};


handle_call(stop, _From, #state{socket = Socket, acceptor = Acceptor}) ->
    catch exit(Acceptor, shutdown),
    gen_tcp:close(Socket),
    io:format("dqsd_otel: Stopped listening from oscilloscope~n"),
    {reply, ok, #state{socket = undefined, acceptor = undefined}}.

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% Accept loop that runs in a separate process.
%% Accepts TCP connections and spawns a handler for each one.
%%--------------------------------------------------------------------
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).

%%--------------------------------------------------------------------
%% @private
%% Handles a client socket connection. Reads line-by-line.
%% Forwards trimmed binary lines to `dqsd_otel:handle_c_message/1`.
%%--------------------------------------------------------------------
handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Trimmed = binary:replace(Line, <<"\n">>, <<>>, [global]),
            dqsd_otel:handle_c_message(Trimmed),
            handle_client(Socket);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

%%--------------------------------------------------------------------
%% @private
%% Parses an IP address from string, binary, or already-parsed tuple.
%%--------------------------------------------------------------------
parse_ip("127.0.0.1") -> {127,0,0,1};
parse_ip("0.0.0.0") -> {0,0,0,0};
parse_ip(IP) when is_list(IP) ->
    {ok, Addr} = inet:parse_address(IP), Addr;
parse_ip(IP) -> IP.
