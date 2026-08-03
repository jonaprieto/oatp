FROM debian:bookworm-slim

ARG EPROVER_VERSION=2.6+ds-3

RUN apt-get update \
    && apt-get install --no-install-recommends --yes "eprover=${EPROVER_VERSION}" \
    && rm -rf /var/lib/apt/lists/*

COPY docker/eprover/entrypoint.sh /usr/local/bin/oatp-eprover
RUN chmod 0555 /usr/local/bin/oatp-eprover

WORKDIR /tmp
ENTRYPOINT ["/usr/local/bin/oatp-eprover"]

