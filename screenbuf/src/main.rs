use raylib::prelude::*;
use std::io::{self, BufRead};
use std::sync::mpsc::{self, TryRecvError};
use std::thread;

const WIDTH: usize = 64;
const HEIGHT: usize = 64;
const TITLE: &str = "COBOL Screenbuffer (stdin -> Raylib)";

#[derive(Debug)]
struct PixelCommand {
    x: usize,
    y: usize,
    color: Color,
}

#[derive(Debug)]
struct PixelBuffer {
    width: usize,
    height: usize,
    data: Vec<u8>,
}

impl PixelBuffer {
    fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            data: vec![0; width * height * 4],
        }
    }

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        &self.data
    }

    fn apply(&mut self, cmd: PixelCommand) -> bool {
        if cmd.x >= self.width || cmd.y >= self.height {
            return false;
        }

        let idx = (cmd.y * self.width + cmd.x) * 4;
        self.data[idx] = cmd.color.r;
        self.data[idx + 1] = cmd.color.g;
        self.data[idx + 2] = cmd.color.b;
        self.data[idx + 3] = cmd.color.a;
        true
    }
}

fn main() {
    let (mut rl, thread) = raylib::init()
        .size(WIDTH as i32, HEIGHT as i32)
        .title(TITLE)
        .resizable()
        .build();
    rl.set_target_fps(60);

    let mut buffer = PixelBuffer::new(WIDTH, HEIGHT);
    let image = Image::gen_image_color(WIDTH as i32, HEIGHT as i32, Color::BLACK);
    let mut texture = rl
        .load_texture_from_image(&thread, &image)
        .expect("Failed to create texture");

    let (tx, rx) = mpsc::channel::<PixelCommand>();
    spawn_stdin_reader(tx);

    let mut stdin_closed = false;
    while !rl.window_should_close() {
        let mut dirty = false;
        loop {
            match rx.try_recv() {
                Ok(cmd) => {
                    dirty |= buffer.apply(cmd);
                }
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    stdin_closed = true;
                    break;
                }
            }
        }

        if dirty {
            texture
                .update_texture(buffer.as_bytes())
                .expect("Failed to update texture");
        }

        {
            let mut d = rl.begin_drawing(&thread);
            d.clear_background(Color::BLACK);
            draw_buffer(&mut d, &texture);

            if stdin_closed {
                d.draw_text(
                    "stdin closed - showing last frame",
                    12,
                    12,
                    20,
                    Color::RAYWHITE,
                );
            } else {
                d.draw_text(
                    "Pipe pixels via stdin: `x y r g b` per line",
                    12,
                    12,
                    20,
                    Color::RAYWHITE,
                );
                d.draw_text("ESC or close window to exit", 12, 36, 20, Color::GRAY);
            }
        }
    }
}

fn spawn_stdin_reader(tx: mpsc::Sender<PixelCommand>) {
    thread::spawn(move || {
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = match line {
                Ok(l) => l,
                Err(_) => break,
            };

            if let Some(cmd) = parse_line(&line)
                && tx.send(cmd).is_err()
            {
                break;
            }
        }
    });
}

fn parse_line(line: &str) -> Option<PixelCommand> {
    let mut parts = line.split_whitespace();
    let x = parts.next()?.parse().ok()?;
    let y = parts.next()?.parse().ok()?;
    let r = parts.next()?.parse().ok()?;
    let g = parts.next()?.parse().ok()?;
    let b = parts.next()?.parse().ok()?;

    Some(PixelCommand {
        x,
        y,
        color: Color { r, g, b, a: 255 },
    })
}

fn draw_buffer(d: &mut RaylibDrawHandle, texture: &Texture2D) {
    let screen_w = d.get_screen_width() as f32;
    let screen_h = d.get_screen_height() as f32;

    let scale = (screen_w / WIDTH as f32).min(screen_h / HEIGHT as f32);

    let dest_w = WIDTH as f32 * scale;
    let dest_h = HEIGHT as f32 * scale;
    let dest_x = (screen_w - dest_w) / 2.0;
    let dest_y = (screen_h - dest_h) / 2.0;

    let src = Rectangle {
        x: 0.0,
        y: 0.0,
        width: WIDTH as f32,
        height: HEIGHT as f32,
    };
    let dest = Rectangle {
        x: dest_x,
        y: dest_y,
        width: dest_w,
        height: dest_h,
    };

    d.draw_texture_pro(
        texture,
        src,
        dest,
        Vector2::new(0.0, 0.0),
        0.0,
        Color::WHITE,
    );
}
