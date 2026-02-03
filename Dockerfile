FROM python:3.14-alpine3.23 AS build

# Disable bytecode caching during build
ENV PYTHONDONTWRITEBYTECODE=1

# Install build dependencies
RUN apk upgrade --update-cache -a
RUN apk add --no-cache \
    curl \
    git \
    gcc \
    libffi-dev \
    musl-dev \
    tini \
    uv

# Install Rust toolchain
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --profile minimal
ENV PATH="/root/.cargo/bin:${PATH}"

# Copy files to prepare build
COPY scripts scripts

# Prepare build
RUN mkdir -p python/zensical
RUN python scripts/prepare.py

# Create a stub project, which will allow us to install dependencies and have
# them properly cached while changes to sources won't invalidate the cache
RUN mkdir crates
RUN cargo new --lib crates/zensical
RUN cargo add pyo3 \
    --manifest-path crates/zensical/Cargo.toml \
    --features extension-module

# Copy files to install dependencies - these will get installed into a virtual
# environment, which is fine, since uv can later reuse the cached versions
COPY pyproject.toml pyproject.toml
COPY README.md README.md
COPY uv.lock uv.lock

# Install dependencies
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --dev --no-install-project

# Install additional dependencies
RUN --mount=type=cache,target=/root/.cache/uv \
    uv pip install --system mkdocstrings-python

# Copy files to build project
COPY crates crates
COPY python python
COPY Cargo.lock Cargo.lock
COPY Cargo.toml Cargo.toml

# Build project
RUN . /.venv/bin/activate
RUN --mount=type=cache,target=/root/.cache/uv \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=target \
    uv pip install --system . -v

# -----------------------------------------------------------------------------

FROM scratch AS image

# Copy relevant files from build
COPY --from=build /bin/sh /bin/sh
COPY --from=build /sbin/tini /sbin/tini
COPY --from=build /lib /lib
COPY --from=build /usr/lib /usr/lib
COPY --from=build /usr/local /usr/local

# Set working directory and expose preview server port
WORKDIR /docs
EXPOSE 8000

# Start preview server by default
ENTRYPOINT ["/sbin/tini", "--", "zensical"]
CMD ["serve", "--dev-addr=0.0.0.0:8000"]
