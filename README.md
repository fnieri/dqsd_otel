dqsd_otel
=====
The OpenTelemetry ΔQ Adapter for the [ΔQ oscilloscope](https://github.com/fnieri/DeltaQOscilloscope).

## Usage
A system instrumented with OpenTelemetry has spans and traces to observe the execution of an operation. The same level of observability must be assured in the oscilloscope, this is why we provide the concept of probes, which, like spans, follow an execution from start to end.
To observe a system, we must put probes in it. For each outcome of interest, a probe (observation point) is attached to measure the delay of the outcome, like one would in
a true oscilloscope.

A probe replaces the calls to OpenTelemetry start/end/with spans, to record informations about the execution of a task (outcome). These calls incorporate OpenTelemetry calls to start/with/end span, to augment the observability capabilities of OpenTelemetry.
Original functionalities of OpenTelemetry are retained with dqsd_otel. The result of the execution of a single task is called outcome instance.

### Starting custom start spans (outcome instances)

    $ {OtelCtx, Pid} = dqsd_otel:start_span(<<"name">>)
    $ {OtelCtx, Pid} = dqsd_otel:start_span(<<"probe">>, #{attributes =>[{attr, <<"attr">>}]})

### Ending start spans 
    $ dqsd_otel:end_span(Pid)
    
### Starting custom with_spans 
    $ dqsd_otel:with_span(<<"worker_2">>, fun() -> ok, #{attributes => [{attr, <<"my_attr">>}]})
    or without attributes
    $ dqsd_otel:with_span(<<"worker_2">>, fun() -> ok end)

With spans are ended automatically like OpenTelemetry with_spans.

### Failing spans
There may be error or exceptions that happen in the execution of a task, OpenTelemetry allows for these errors to be added in the information of a span. Recording these errors is necessary to observing a running system. To record these errors, the adapter needs to be called before or after adding the error to a span. 
Unlike previous functions, this one **does not** end OpenTelemetry spans. This is to allow the user to handle the errored span as they wish in their system.   

    $ dqsd_otel:fail_span(Pid).

Build
-----
    $ rebar3 compile
