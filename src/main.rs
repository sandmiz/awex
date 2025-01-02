use clap::Parser;
use std::fs;
mod astchecker;
mod lexer;
mod parser;
mod tree;

#[derive(Debug, Parser)]
#[command(name = "Awex")]
pub struct CompileTask {
    #[arg()]
    source: Vec<String>,

    #[arg(long, short, requires("source"))]
    output: Option<String>,
}

fn main() {
    let task = CompileTask::parse();

    for src_file in task.source {
        let src_content = fs::read_to_string(src_file).unwrap();
        let mut parse = parser::Parser::new(src_content.chars());

        parse.parse();
    }
}
