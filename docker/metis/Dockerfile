FROM debian:bookworm-slim AS build

ARG METIS_VERSION=2.4.20260305
ARG METIS_SHA256=9f9567fd6f51e965e7385099e0c6c690a13eb185b1bb847330221086e8d2fe0a

RUN apt-get update \
    && apt-get install --no-install-recommends --yes ca-certificates curl make polyml perl-modules-5.36 \
    && curl --fail --location --silent --show-error \
      "https://github.com/gilith/metis/releases/download/v${METIS_VERSION}/metis.tar.gz" \
      --output /tmp/metis.tar.gz \
    && echo "${METIS_SHA256}  /tmp/metis.tar.gz" | sha256sum --check --strict \
    && tar -xzf /tmp/metis.tar.gz --directory /tmp \
    && make --directory /tmp/metis polyml \
    && /tmp/metis/bin/polyml/metis --version

FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install --no-install-recommends --yes libpolyml9 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /tmp/metis/bin/polyml/metis /usr/local/bin/metis
COPY docker/tptp/entrypoint.sh /usr/local/bin/oatp-tptp
RUN chmod 0555 /usr/local/bin/metis /usr/local/bin/oatp-tptp

WORKDIR /tmp
ENTRYPOINT ["/usr/local/bin/oatp-tptp", "metis"]
