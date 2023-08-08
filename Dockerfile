# Build stage 0
FROM erlang:26-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY pace pace

# And build the release
WORKDIR pace
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/pace/_build/prod/rel/pace /pace

# Expose relevant ports
EXPOSE 8080

CMD ["/pace/bin/pace", "foreground"]
