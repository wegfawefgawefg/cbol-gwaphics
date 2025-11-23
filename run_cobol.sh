#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 path/to/program.cobol" >&2
  exit 1
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_PATH="$1"

# --- HELPER: Find the absolute path of the file ---
resolve_source_path() {
  local candidate="$1"
  if [[ -f "$candidate" ]]; then
    local abs_dir
    abs_dir="$(cd "$(dirname "$candidate")" && pwd)"
    printf "%s/%s\n" "$abs_dir" "$(basename "$candidate")"
    return 0
  fi

  local from_root="$ROOT_DIR/$candidate"
  if [[ -f "$from_root" ]]; then
    local abs_dir
    abs_dir="$(cd "$(dirname "$from_root")" && pwd)"
    printf "%s/%s\n" "$abs_dir" "$(basename "$from_root")"
    return 0
  fi

  local from_src="$ROOT_DIR/src/$candidate"
  if [[ -f "$from_src" ]]; then
    local abs_dir
    abs_dir="$(cd "$(dirname "$from_src")" && pwd)"
    printf "%s/%s\n" "$abs_dir" "$(basename "$from_src")"
    return 0
  fi

  return 1
}

# --- VALIDATION ---
if ! COBOL_SRC="$(resolve_source_path "$INPUT_PATH")"; then
  echo "COBOL source not found: $INPUT_PATH" >&2
  exit 1
fi

# Get the directory where the main program lives
SRC_DIR="$(dirname "$COBOL_SRC")"

# Find all subprograms (.cbl files) in the same directory, excluding the main program
readarray -t SUBPROGRAM_SRCS < <(find "$SRC_DIR" -maxdepth 1 -name "*.cbl" -not -path "$COBOL_SRC" | sort)

if [[ ${#SUBPROGRAM_SRCS[@]} -eq 0 ]]; then
  echo "Warning: No subprogram .cbl files found in $SRC_DIR" >&2
fi

pushd "$ROOT_DIR" >/dev/null
cleanup() {
  popd >/dev/null
}
trap cleanup EXIT

TARGET_DIR="target"
PIPE_PATH="$TARGET_DIR/screenbuf.pipe"

if [[ ! -p "$PIPE_PATH" ]]; then
  echo "Missing named pipe at $PIPE_PATH. Run ./launch_screenbuf.sh first." >&2
  exit 1
fi

mkdir -p "$TARGET_DIR"

COBOL_BASENAME="$(basename "$COBOL_SRC")"
COBOL_STEM="${COBOL_BASENAME%.*}"
if [[ -z "$COBOL_STEM" ]]; then
  COBOL_STEM="cobol_program"
fi
COBOL_BIN="$TARGET_DIR/$COBOL_STEM"

echo "[1/2] Building COBOL program..."

# -I "$SRC_DIR": Tells compiler to look in source dir for COPY files (.cpy)
# Compile main program with all discovered subprograms
cobc -x -o "$COBOL_BIN" -I "$SRC_DIR" "$COBOL_SRC" "${SUBPROGRAM_SRCS[@]}"

echo "[2/2] Streaming COBOL output into $PIPE_PATH"
stdbuf -oL "$COBOL_BIN" > "$PIPE_PATH"