
-module(dqsd_otel_tcp_client).
-behaviour(gen_server).

-export([start_link/0, send_span/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([disconnect/0, try_connect/2]).

-define(SERVER, ?MODULE).

-record(state, {
    socket = undefined,
    logged_disconnected = false
}).

%%% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_span(Data) ->
    gen_server:cast(?SERVER, {send, Data}).

%% @doc Connect to the oscilloscope at IP:Port.
%% Span data is sent on this connection; commands from C++ arrive on it too.
-spec try_connect(string() | binary(), integer()) -> ok.
try_connect(IP, Port) ->
    gen_server:cast(?SERVER, {try_connect, IP, Port}).

-spec disconnect() -> ok.
disconnect() ->
    gen_server:cast(?SERVER, disconnect).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_cast({send, _Data}, State = #state{socket = undefined, logged_disconnected = false}) ->
    io:format("dqsd_otel: No socket. Dropping subsequent spans.~n"),
    {noreply, State#state{logged_disconnected = true}};

handle_cast({send, _Data}, State = #state{socket = undefined}) ->
    {noreply, State};

handle_cast({send, Data}, State = #state{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            io:format("dqsd_otel: TCP send failed: ~p~n", [Reason]),
            {noreply, State#state{socket = undefined, logged_disconnected = true}}
    end;

handle_cast({try_connect, IP, Port}, State) ->
    IPStr = case IP of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        Str when is_list(Str)   -> Str
    end,
    %% {active, true}  — incoming data from C++ arrives as {tcp, Socket, Data} messages.
    %% {packet, line}  — messages are delivered one line at a time, matching the \n protocol.
    case gen_tcp:connect(IPStr, Port, [binary, {active, true}, {packet, line}, {keepalive, true}]) of
        {ok, Socket} ->
            io:format("dqsd_otel: Connected to oscilloscope at ~s:~p~n", [IPStr, Port]),
            case State#state.socket of
                undefined -> ok;
                Old       -> catch gen_tcp:close(Old)
            end,
            erlang:send_after(30000, self(), heartbeat),
            {noreply, State#state{socket = Socket, logged_disconnected = false}};
        {error, Reason} ->
            io:format("dqsd_otel: Connection to ~s:~p failed: ~p~n", [IPStr, Port, Reason]),
            {noreply, State}
    end;

handle_cast(disconnect, State = #state{socket = undefined}) ->
    io:format("dqsd_otel: Already disconnected.~n"),
    {noreply, State};

handle_cast(disconnect, State = #state{socket = Socket}) ->
    io:format("dqsd_otel: Disconnecting.~n"),
    gen_tcp:close(Socket),
    {noreply, State#state{socket = undefined, logged_disconnected = false}}.

%% Periodic heartbeat — keeps NAT/firewall entries alive when no spans are flowing.
handle_info(heartbeat, State = #state{socket = undefined}) ->
    {noreply, State};

handle_info(heartbeat, State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, <<"ping\n">>),
    erlang:send_after(30000, self(), heartbeat),
    {noreply, State};

%% Commands arriving from C++ on the shared socket.
handle_info({tcp, _Socket, Data}, State) ->
    Trimmed = binary:replace(Data, <<"\n">>, <<>>, [global]),
    dqsd_otel:handle_c_message(Trimmed),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    io:format("dqsd_otel: Oscilloscope disconnected.~n"),
    {noreply, State#state{socket = undefined, logged_disconnected = false}};

handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("dqsd_otel: TCP error: ~p~n", [Reason]),
    {noreply, State#state{socket = undefined, logged_disconnected = false}};

handle_info(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, #state{socket = Socket}) when Socket =/= undefined ->
    gen_tcp:close(Socket);
terminate(_Reason, _State) ->
    ok.
