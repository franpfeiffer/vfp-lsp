//! Example: Lex a VFP file and print tokens
//!
//! Usage: cargo run -p vfp-lexer --example lex_file <file.prg>

use std::env;
use std::fs;
use vfp_lexer::tokenize;

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = if args.len() > 1 {
        &args[1]
    } else {
        "test-fixtures/sample.prg"
    };

    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            std::process::exit(1);
        }
    };

    println!("Lexing: {}", file_path);
    println!("========================================");

    let mut pos = 0;
    let mut line = 1;
    let mut col = 1;

    for token in tokenize(&source) {
        let text = &source[pos..pos + token.len as usize];
        let display_text = if text.contains('\n') {
            text.replace('\n', "\\n").replace('\r', "\\r")
        } else {
            text.to_string()
        };

        // Only show non-trivia tokens for cleaner output
        if !token.kind.is_trivia() {
            println!(
                "{:4}:{:3} {:20?} {:?}",
                line,
                col,
                token.kind,
                if display_text.len() > 40 {
                    format!("{}...", &display_text[..40])
                } else {
                    display_text.clone()
                }
            );
        }

        // Update position tracking
        for c in text.chars() {
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        pos += token.len as usize;
    }

    println!("========================================");
    println!("Lexing complete. {} bytes processed.", source.len());
}
