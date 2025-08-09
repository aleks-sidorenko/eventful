#!/usr/bin/env bash
set -euo pipefail

# Ensures a local PostgreSQL instance is running on 127.0.0.1 for tests.
# Safe to run multiple times; if Postgres is already up it exits quickly.

POSTGRES_HOST=${POSTGRES_HOST:-127.0.0.1}
POSTGRES_PORT=${POSTGRES_PORT:-5432}
POSTGRES_USER=${POSTGRES_USER:-postgres}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-password}
POSTGRES_DBNAME=${POSTGRES_DBNAME:-eventium_test}

# Probe existing instance
if pg_isready -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" >/dev/null 2>&1; then
  echo "Postgres already running at $POSTGRES_HOST:$POSTGRES_PORT"
  exit 0
fi

echo "Starting local Postgres at $POSTGRES_HOST:$POSTGRES_PORT..."

# Data directory (in-repo, ephemeral for CI/containers)
PGDATA_DIR=${PGDATA_DIR:-"$(pwd)/.pgdata"}
mkdir -p "$PGDATA_DIR"

if [ ! -s "$PGDATA_DIR/PG_VERSION" ]; then
  echo "Initializing database cluster in $PGDATA_DIR"
  initdb -D "$PGDATA_DIR" \
    --username="$POSTGRES_USER" \
    --no-password \
    --auth-local=trust \
    --auth-host=trust
fi

# Configure listen address and port
cat > "$PGDATA_DIR/postgresql.conf" <<EOF
listen_addresses = '127.0.0.1'
port = $POSTGRES_PORT
EOF

# Start server
pg_ctl -D "$PGDATA_DIR" -o "-h $POSTGRES_HOST -p $POSTGRES_PORT" -w start

# Create database if missing
createdb -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" "$POSTGRES_DBNAME" >/dev/null 2>&1 || true

echo "Postgres started. Verifying readiness..."
for i in {1..30}; do
  if pg_isready -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER"; then
    echo "Postgres is ready."
    exit 0
  fi
  echo "Waiting for Postgres... ($i)"
  sleep 2
done

echo "Postgres did not become ready in time" >&2
exit 1


