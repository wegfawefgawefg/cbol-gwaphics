#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
pushd "$ROOT_DIR" >/dev/null

PIPE_PATH="target/screenbuf.pipe"
COBOL_SRC="src/draw_gradient.cob"

cleanup() {
  if [[ -n "${SCREENBUF_PID:-}" ]]; then
    kill "$SCREENBUF_PID" 2>/dev/null || true
    wait "$SCREENBUF_PID" 2>/dev/null || true
  fi
  popd >/dev/null
}
trap cleanup EXIT

echo "[1/2] Launching screenbuf (background)..."
./launch_screenbuf.sh &
SCREENBUF_PID=$!

echo "Waiting for screenbuf pipe..."
for _ in {1..50}; do
  if [[ -p "$PIPE_PATH" ]]; then
    break
  fi
  sleep 0.1
done

if [[ ! -p "$PIPE_PATH" ]]; then
  echo "Failed to find $PIPE_PATH. Exiting." >&2
  exit 1
fi

echo "[2/2] Running COBOL gradient demo..."
./run_cobol.sh "$COBOL_SRC"
