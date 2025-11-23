#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
pushd "$ROOT_DIR" >/dev/null

TARGET_DIR="target"
PIPE_PATH="$TARGET_DIR/screenbuf.pipe"
SCREENBUF_DIR="screenbuf"
SCREENBUF_BIN="$SCREENBUF_DIR/target/release/screenbuf"

mkdir -p "$TARGET_DIR"

if [[ -e "$PIPE_PATH" && ! -p "$PIPE_PATH" ]]; then
  echo "Removing non-pipe entry at $PIPE_PATH"
  rm -f "$PIPE_PATH"
fi

if [[ ! -p "$PIPE_PATH" ]]; then
  mkfifo "$PIPE_PATH"
fi

echo "[1/2] Building Rust screenbuffer..."
(
  cd "$SCREENBUF_DIR"
  cargo build --release
)

echo "[2/2] Waiting for COBOL output on $PIPE_PATH"
echo "Press Ctrl-C to stop screenbuf."

cleanup() {
  popd >/dev/null
}
trap cleanup EXIT

stdbuf -oL tail -f "$PIPE_PATH" | "$SCREENBUF_BIN"
