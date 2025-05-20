
-module(dqsd_otel_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{id => ets_init,
          start => {dqsd_otel, init_ets, []},
          restart => temporary,
          shutdown => brutal_kill,
          type => worker,
          modules => [dqsd_otel]},
         
        #{id => tcp_server,
          start => {dqsd_otel_tcp_server, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [dqsd_otel_tcp_server]},
          
        #{id => tcp_client,      
          start => {wrapper_tcp_client, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [dqsd_otel_tcp_client]}
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

