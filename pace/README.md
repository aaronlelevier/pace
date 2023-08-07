# Summary

## Pace

Project for fun hacking in Erlang.

# Local

## Start shell

Start application:

```bash
rebar3 shell
```

Add/remove items:

```erlang
pace_gs:add(3).
pace_gs:add(7).
pace_gs:remove(3).
```

## Build and run a Release

```bash
rebar3 shell
```

## Release

```bash
# build
rebar3 as prod release

# run
rebar3 as prod release
./_build/prod/rel/pace/bin/pace foreground
```

missing-functions error means that dependencies needed to be added to the `.app.src`. See: https://rebar3.org/docs/deployment/releases/#missing-functions

## Cowboy API

Can curl singl http endpoint

```bash
curl localhost:8080
```

# Notes

Started with example gen_server:
https://www.erlang.org/doc/design_principles/gen_server_concepts#example

Simplified gen_server to [pace_gs.erl](#src/pace_gs.erl)

gen_server docs:

- https://www.erlang.org/doc/design_principles/gen_server_concepts.html
- https://www.erlang.org/doc/man/gen_server.html

supervisor:

- https://www.erlang.org/doc/man/supervisor#start_child-2
- https://www.erlang.org/doc/man/supervisor#type-child_spec
- https://www.erlang.org/doc/design_principles/sup_princ#child-specification

lists:

- https://www.erlang.org/doc/man/lists.html

io:

- https://www.erlang.org/doc/man/io#format-2

