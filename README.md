# Linn

Linn is a process pool manager for Erlang/OTP.

![CI](https://github.com/amoein/linn/actions/workflows/ci.yaml/badge.svg)

## Goals

Process pool like other pool like connection pool, but instead of connection, it keeps process.

Linn aims to provide an efficient process pool that distributes requests to all processes and handles back pressure.

One of the best places to use Linn is **IO** base operation.

## Installation

- Pre Requirement:
  Use `Erlang/OTP +18`.

- Step 1:
  Add `linn` to `rebar.config`.

```erlang
{deps, [
 ....
{linn,  {git,  "git@github.com:amoein/linn.git"}},
 ....
]}.
```

- Step 2:
  Make sure `linn` starts in `your_app.erl`:

```erlang
-module(your_app).

start(_StartType, _StartArgs)  ->
....
application:ensure_started(linn),
...
```

- Step 3:

```bash
rebar3 deps
```

## Usage

First of all you should include `linn/include/linn.hrl` in your module like this:

```erlang
-include_lib("linn/include/linn.hrl").
```

### Add new pool

For add new pool you should use `linn:add_pool`:

```erlang
{ok,  _}  =  linn:add_new_pool(#option{}),
```

- Spec

  - `#option{}`:
    It's record define by linn contains:

  ```erlang
  #option{ pool_id :: atom(),
          handler_module :: atom(),
          handler_func :: atom(),
          handler_func_arity :: tuple(),
          process_count :: integer()}
  ```

  - `pool_id`:

    It's your pool name and it's `atom()`

  - `handler_module` ,`handler_func` and `handler_func_arity` :

    There are simple `MFA`. for example:

    ```erlang
    -module(handler_module).

    handler_func(HandlerFuncArity)->
    .....
    end.
    ```

    It is obvious to have process pool you should have process.
    your `MFA` should be start a process and return `{ok, pid()}`.
    for example a `gen_serve` this:

    ```erlang
    - module(handler_module).

    -behaviour(gen_server).

    handler_func(_HandlerFuncArity) ->
        gen_server:start_link(?MODULE, [], []).
    ```

  - `process_count`:

    Number of item in pool: `non_neg_integer` from 0

### Get process from pool

For get process form pool you should use `linn:get_process`:

```erlang
{ok,  Pid}  =  linn:get_process(PoolId),
```

- Spec
  - PoolId: your pool id in option `atom()`

## Version History

- 0.2.0 (3 March 2023)
  - update ci config
  - Remove extra code
  - Improve performance on ets store
  - Improve test
- 0.1.0(8 JUl 2019)
  - Initial Release
