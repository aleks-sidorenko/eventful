#!/usr/bin/env bash
set -euo pipefail

# Ensures a local PostgreSQL instance is running on 127.0.0.1 for tests via Docker Compose.
# Safe to run multiple times; if Postgres is already up it exits quickly.

POSTGRES_HOST=${POSTGRES_HOST:-127.0.0.1}
POSTGRES_PORT=${POSTGRES_PORT:-5432}
POSTGRES_USER=${POSTGRES_USER:-postgres}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-password}
POSTGRES_DBNAME=${POSTGRES_DBNAME:-eventium_test}

# Fast path: already reachable
if PGPASSWORD="$POSTGRES_PASSWORD" pg_isready -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" >/dev/null 2>&1; then
  echo "Postgres already running at $POSTGRES_HOST:$POSTGRES_PORT"
  exit 0
fi

echo "Starting Postgres via Docker Compose at $POSTGRES_HOST:$POSTGRES_PORT..."

# Ensure Docker is available
if ! command -v docker >/dev/null 2>&1; then
  echo "Docker is required but not found in PATH" >&2
  exit 1
fi

# Bring up the postgres service. Compose will read env from current shell and optional .env file.
docker compose up -d postgres

# Wait for container health to be healthy (uses healthcheck from compose)
container_id=$(docker compose ps -q postgres)
if [ -z "$container_id" ]; then
  echo "Failed to get postgres container id" >&2
  docker compose ps || true
  exit 1
fi

echo "Waiting for container health..."
for i in $(seq 1 60); do
  status=$(docker inspect -f '{{.State.Health.Status}}' "$container_id" 2>/dev/null || echo "unknown")
  if [ "$status" = "healthy" ]; then
    echo "  health=healthy"
    break
  fi
  echo "  health=${status:-none} ($i)"
  sleep 1
done

# Resolve the published host:port from compose (robust to overrides)
published=$(docker compose port postgres 5432 | head -n1 | tr -d '\r')
if [ -z "$published" ]; then
  echo "Could not determine published port. Is the port already in use on host?" >&2
  echo "Hint: lsof -iTCP:$POSTGRES_PORT -sTCP:LISTEN || true" >&2
  docker compose ps || true
  exit 1
fi

published_host=${published%:*}
published_port=${published##*:}

echo "Published endpoint: ${published:-unknown}"

echo "Checking readiness inside container..."
for i in $(seq 1 60); do
  if docker exec "$container_id" pg_isready -h 127.0.0.1 -p 5432 -U "$POSTGRES_USER" >/dev/null 2>&1; then
    echo "  in-container pg_isready ok"
    break
  fi
  echo "  waiting in-container... ($i)"
  sleep 1
done

echo "Checking host port ${published_host}:${published_port}..."
for i in $(seq 1 60); do
  if nc -z "$published_host" "$published_port" >/dev/null 2>&1; then
    echo "  host port is open"
    break
  fi
  echo "  waiting for host port... ($i)"
  sleep 1
done

if command -v pg_isready >/dev/null 2>&1; then
  echo "Verifying readiness on $published_host:$published_port..."
  for i in $(seq 1 60); do
    if PGPASSWORD="$POSTGRES_PASSWORD" pg_isready -h "$published_host" -p "$published_port" -U "$POSTGRES_USER" >/dev/null 2>&1; then
      echo "Postgres is ready."
      exit 0
    fi
    echo "Waiting for Postgres... ($i)"
    sleep 1
  done
  echo "Postgres did not become ready in time" >&2
  docker compose logs --no-color postgres | tail -n 100 || true
  exit 1
else
  echo "pg_isready not found on host; relying on container health and open port."
  echo "Postgres is ready."
  exit 0
fi

echo "Postgres did not become ready in time" >&2
docker compose logs --no-color postgres | tail -n 100 || true
exit 1


