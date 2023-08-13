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

# Docker

Start container

```bash
docker build -t pace .
docker run -dp 8080:8080 --init --volume="$PWD:$PWD" pace
docker container ls
```

Stop container

```bash
docker stop <container-id>
```

# Minikube

## Quickstart

### Create

```bash
# set app version (must match deployment.yaml if present)
VERSION=v0.0.2

# Set docker to point to minikube
eval $(minikube docker-env)

# Push to minikube docker
docker build -t pace:$VERSION .

# Set your deployment to - imagePullPolicy=IfNotPresent
# Options:
# 1. edit deployment.yaml to set above config:
#       check local dir for deployment.yaml
#
# 2. run cmd to edit deployment config:
#       kubectl edit deployment pace

# create and run deployment
kubectl create deployment pace --image=pace:$VERSION
kubectl expose deployment pace --type=LoadBalancer --port=8080
minikube service pace
```

### Delete

```bash
kubectl delete service pace
kubectl get services

kubectl delete deployment pace
kubectl get deployments
```

## Deployments

```bash
# list
kubectl get deployments

# create
kubectl expose deployment pace --type=LoadBalancer --port=8080

# delete
kubectl delete deployment pace
```

## Services

```bash
# list
kubectl get services

# create
kubectl expose deployment pace --type=LoadBalancer --port=8080

# run
# https://kubernetes.io/docs/tutorials/hello-minikube/
minikube service pace

# edit
# https://stackoverflow.com/questions/53877516/how-do-i-set-the-imagepullpolicy-with-minikube
kubectl edit deployment pace

# delete
kubectl delete service pace
```

## Other Links

- kubectl cheatsheet: https://kubernetes.io/docs/reference/kubectl/cheatsheet/
- how to build local images SO answer: https://stackoverflow.com/a/49478889/1913888
- kubectl deployments: https://kubernetes.io/docs/concepts/workloads/controllers/deployment/


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

