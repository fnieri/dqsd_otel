%%%-------------------------------------------------------------------
%% @doc dqsd_otel public API
%% @end
%%%-------------------------------------------------------------------

-module(dqsd_otel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dqsd_otel_sup:start_link().

stop(_State) ->
    ok.

