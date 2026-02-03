FROM python:3.14-alpine3.23 AS build

COPY docs/ .
COPY pyproject.toml .
COPY uv.lock .
COPY zensical.toml .

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

RUN uv add zensical
RUN uv run zensical build 

# Set working directory and expose preview server port
WORKDIR /docs
EXPOSE 8080

# Start preview server by default
ENTRYPOINT ["uv", "run", "zensical"]
CMD ["serve", "--dev-addr=0.0.0.0:8080"]
