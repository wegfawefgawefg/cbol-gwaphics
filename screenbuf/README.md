# Screenbuf

Raylib-based pixel viewer that listens on `stdin` for `x y r g b` commands and blits them onto a window. Feed it from COBOL (or anything else) to treat Rust like a tiny GPU.

## Building

```bash
cargo build --release
# or run directly
cargo run --release < pixel_commands.txt
```

The window is resizable; the 640×480 buffer will scale to fit.

## Text Protocol

Each line on `stdin` must contain five decimal fields separated by whitespace:

```
x y r g b
```

- `x`, `y`: zero-based pixel coordinates (clamped to `0 <= x < 640`, `0 <= y < 480`)
- `r`, `g`, `b`: 0–255 color channels; alpha is forced to 255

Anything malformed is ignored so noisy producers do not crash the viewer.

## COBOL Producer Example

`refs/draw_gradient.cob` emits a simple gradient frame in the expected format. Build it with `cobc`:

```bash
cobc -x -o target/draw-gradient refs/draw_gradient.cob
```

You can swap in whatever drawing logic you want; just print lines in the protocol above.

## Demo Glue Script

`refs/run_demo.sh` compiles the COBOL program, builds the Rust screenbuffer, then pipes one into the other:

```bash
./refs/run_demo.sh
```

Feel free to replace `target/draw-gradient` with your own COBOL binary or point the pipe at a named FIFO for interactive multi-process drawing.
