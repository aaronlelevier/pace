# Summary

## Pace

Project for fun hacking in Erlang.

# Usage

Start application:

```bash
rebar3 shell
```

Add remove items:

```erlang
pace_gs:add(3).
pace_gs:add(7).
pace_gs:remove(3).
```

# Local

## Release

Run:

```
rebar3 as prod release
```

missing-functions error means that dependencies needed to be added to the `.app.src`. See: https://rebar3.org/docs/deployment/releases/#missing-functions

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

