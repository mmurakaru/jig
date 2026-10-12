# The jig release binary is x86-64 and needs glibc 2.38 or newer.
FROM --platform=linux/amd64 python:3.14.8-slim-trixie

RUN apt-get update \
    && apt-get install -y --no-install-recommends git ca-certificates curl \
    && rm -rf /var/lib/apt/lists/* \
    && pip install --no-cache-dir pytest==9.1.1 \
    && git config --global user.name "jig eval" \
    && git config --global user.email "eval@example.invalid" \
    && git config --global init.defaultBranch main

WORKDIR /app
COPY repo/ /app/
RUN git init -q && git add -A && git commit -q -m "Initial commit"
