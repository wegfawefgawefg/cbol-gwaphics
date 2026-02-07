# Screenbuf

Raylib-based pixel viewer that listens on `stdin` for `x y r g b` commands and blits them onto a window. Feed it from COBOL (or anything else) to treat Rust like a tiny GPU.

## Building

```bash
cargo build --release
# or run directly
cargo run --release < pixel_commands.txt
```

The window is resizable; the `64x64` buffer will scale to fit.

## Text Protocol

Each line on `stdin` must contain five decimal fields separated by whitespace:

```
x y r g b
```

- `x`, `y`: zero-based pixel coordinates (clamped to `0 <= x < 64`, `0 <= y < 64`)
- `r`, `g`, `b`: 0-255 color channels; alpha is forced to 255

Anything malformed is ignored so noisy producers do not crash the viewer.

## Demo Glue Script

From the repo root, `run_demo.sh` builds the Rust viewer, then compiles a COBOL producer and pipes it into the viewer via a named FIFO:

```bash
./run_demo.sh
```

You can also run a specific COBOL producer:

```bash
./run_demo.sh src/ANT.cob
```
