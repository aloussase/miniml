use std::io::{Read, Write};
use std::net::{SocketAddr, TcpStream};

mod line_editor;
mod syntax_highlight;

use line_editor::{LineEditor, LineResult};

const SERV_PORT: u16 = 3693;
const SERV_HOST: [u8; 4] = [127, 0, 0, 1];

fn main() {
    let mut editor = LineEditor::default();
    let mut conn = TcpStream::connect(SocketAddr::from((SERV_HOST, SERV_PORT))).ok();
    let mut buffer: [u8; 4096] = [0; 4096];

    println!("Welcome to the MiniML repl!\nEnter '.quit' to exit.");

    if let None = conn {
        eprintln!("Failed to connect to server, lines will just be echoed back.");
    }

    loop {
        let line = editor.read_line();
        if let LineResult::Line(line) = line {
            if line.is_empty() {
                continue;
            }

            if line == ".quit" {
                break;
            }

            if let Some(ref mut conn) = conn {
                conn.write(line.as_bytes())
                    .expect("failed to send line to server");
                conn.read(&mut buffer)
                    .expect("failed to read response from server");
                println!("{}", std::str::from_utf8(&buffer).unwrap());
            } else {
                println!("{}", line);
            }
        } else {
            break;
        }
    }
}
