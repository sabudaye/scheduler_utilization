scheduler_utilization
=====

Non blocking Erlang VM scheduler utilization calculator.

Because Erlang CPU usage as reported from `top` isn’t the most reliable value (due to schedulers doing idle spinning to avoid going to sleep and impacting latency), a metric exists that is based on scheduler wall time - [erlang.statistics(scheduler_wall_time)](https://erlang.org/doc/man/erlang.html#statistics_scheduler_wall_time).
For any time interval, Scheduler wall time can be used as a measure of how "busy" a scheduler is. A scheduler is busy when:
* Executing process code
* Executing linked-in driver or NIF code
* Executing BIFs, or any other runtime handling
* Garbage collecting
* Handling any other memory management

But the scheduler wall time isn't easy to read, it doesn't look like CPU usage in `top`:
```
iex(2)> :erlang.statistics(:scheduler_wall_time)
[
  {5, 87867, 27330418785},
  {7, 66353, 27330355600},
  {8, 44196, 27330389465},
  {2, 52264, 27330478740},
  {4, 61475, 27330400810},
  {1, 519905, 27330489650},
  {6, 63038, 27330349455},
  {3, 31529886, 27330400703},
  {9, 33784, 35213659141},
  {10, 23211, 35213659600},
  {11, 27003, 35213659867},
  {12, 24868, 35213660144},
  {13, 21020, 35213660403},
  {14, 23677, 35213660684},
  {15, 15166, 35213660949},
  {16, 20541, 35213661214}
]
```
So there is another metric based on difference between two samples of scheduler wall time - [scheduler.utilization/1](https://erlang.org/doc/man/scheduler.html#utilization-1):
```
iex(3)> :scheduler.utilization(1)
[
  {:total, 2.6368630920183277e-5, '0.0%'},
  {:weighted, 5.2737261840366555e-5, '0.0%'},
  {:normal, 1, 2.355525364207035e-5, '0.0%'},
  {:normal, 2, 2.323553121047311e-5, '0.0%'},
  {:normal, 3, 2.5641358543427174e-4, '0.0%'},
  {:normal, 4, 2.4300431925188596e-5, '0.0%'},
  {:normal, 5, 2.5207908203652448e-5, '0.0%'},
  {:normal, 6, 2.141886904716157e-5, '0.0%'},
  {:normal, 7, 2.502426597604747e-5, '0.0%'},
  {:normal, 8, 2.274278445407965e-5, '0.0%'},
  {:cpu, 9, 0.0, '0.0%'},
  {:cpu, 10, 0.0, '0.0%'},
  {:cpu, 11, 0.0, '0.0%'},
  {:cpu, 12, 0.0, '0.0%'},
  {:cpu, 13, 0.0, '0.0%'},
  {:cpu, 14, 0.0, '0.0%'},
  {:cpu, 15, 0.0, '0.0%'},
  {:cpu, 16, 0.0, '0.0%'}
]
```
Like `top` would report something closer to this for each core. The higher the ratio, the higher the workload.

Hovewer, this metrics is implemented using [sleep](https://github.com/erlang/otp/blob/master/lib/runtime_tools/src/scheduler.erl#L75), which blocks the calling process, so all other messages in the process mailbox will wait until sleep ends. That's not very useful for telemetry.
Unfortunately, `Recon`, a popular library for Erlang VM diagnostics, [does the same](https://github.com/ferd/recon/blob/master/src/recon.erl#L358)

How then the scheduler utilization can be calculated without blocking the calling process?
We would need another process to calculate scheduler utilization value and store it in the ets and `timer:send_after/1` to update that value periodically, so the calling process would just basically read the value from ets. That's exactly what this application does in the nutshell.

Build
-----

    $ rebar3 compile

Usage
-----
```
scheduler_utilization.total().
scheduler_utilization.weighted().
```
For the difference between `total` and `wighted` read the [Erlang/OTP reference manual](http://erlang.org/doc/man/erlang.html#statistics_scheduler_wall_time)
