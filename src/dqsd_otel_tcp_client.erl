
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

%% @doc Connect to a custom IP and Port. Replaces any existing connection.
-spec try_connect(string() | binary(), integer()) -> ok.
try_connect(IP, Port) ->
    gen_server:cast(?SERVER, {try_connect, IP, Port}).

-spec disconnect() -> ok.
disconnect() ->
    gen_server:cast(?SERVER, disconnect).

%%% gen_server callbacks

init([]) ->
    State = #state{socket = undefined, logged_disconnected = false},
    {ok, State}.

handle_cast({send, _Data}, State = #state{socket = undefined, logged_disconnected = false}) ->
    io:format("dqsd_otel: No socket. Dropping subsequent spans.~n"),
    {noreply, State#state{logged_disconnected = true}};

handle_cast({send, _Data}, State = #state{socket = undefined}) ->
    %% Already logged, suppress further logs
    {noreply, State};

handle_cast({send, Data}, State = #state{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            io:format("dqsd_otel: TCP send failed: ~p~n", [Reason]),
            NewState = State#state{socket = undefined, logged_disconnected = true},
            {noreply, NewState}
    end;

handle_cast({try_connect, IP, Port}, State) ->
    IPStr = case IP of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        Str when is_list(Str)   -> Str
    end,
    case gen_tcp:connect(IPStr, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("dqsd_otel: Adapter connected to ~s:~p~n", [IPStr, Port]),

            case State#state.socket of
                undefined -> ok;
                OldSocket -> catch gen_tcp:close(OldSocket)
            end,
            {noreply, State#state{socket = Socket, logged_disconnected = false}};
        {error, Reason} ->
            io:format("dqsd_otel: Connection to ~s:~p failed: ~p~n", [IPStr, Port, Reason]),
            {noreply, State}
    end;

handle_cast(disconnect, State = #state{socket = undefined}) ->
    io:format("dqsd_otel: Socket already disconnected.~n"),
    {noreply, State};

handle_cast(disconnect, State = #state{socket = Socket}) ->
    io:format("dqsd_otel: Disconnecting TCP socket.~n"),
    gen_tcp:close(Socket),
    {noreply, State#state{socket = undefined, logged_disconnected = false}}.

handle_info(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, State) when is_record(State, state) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end;
terminate(_Reason, _Other) ->
    ok.
