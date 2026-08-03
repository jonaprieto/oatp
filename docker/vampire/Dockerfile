FROM debian:bookworm-slim

ARG VAMPIRE_VERSION=5.0.1
ARG VAMPIRE_SHA256=6ff2f42ea7fb9753ee104efc3e623d5e39443190f7c82a63e1e1517bf9d2cde3

RUN apt-get update \
    && apt-get install --no-install-recommends --yes ca-certificates curl unzip \
    && curl --fail --location --silent --show-error \
      "https://github.com/vprover/vampire/releases/download/v${VAMPIRE_VERSION}/vampire-Linux-X64.zip" \
      --output /tmp/vampire.zip \
    && echo "${VAMPIRE_SHA256}  /tmp/vampire.zip" | sha256sum --check --strict \
    && unzip -p /tmp/vampire.zip vampire > /usr/local/bin/vampire \
    && chmod 0555 /usr/local/bin/vampire \
    && rm -f /tmp/vampire.zip \
    && rm -rf /var/lib/apt/lists/*

COPY docker/tptp/entrypoint.sh /usr/local/bin/oatp-tptp
RUN chmod 0555 /usr/local/bin/oatp-tptp

WORKDIR /tmp
ENTRYPOINT ["/usr/local/bin/oatp-tptp", "vampire", "--input_syntax", "tptp", "--proof", "tptp"]
