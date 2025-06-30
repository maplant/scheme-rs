use std::{
    env,
    error::Error,
    fs::{self, File},
    io::Write,
    path::Path,
};

#[path = "build/lexer.rs"]
mod lexer;

#[path = "build/parser.rs"]
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let manifest_dir = Path::new(&manifest_dir);

    let src = ["case_folding", "numeric_types"]
        .into_iter()
        .map(|p| format!("unicode/{p}.txt"))
        .map(|p| manifest_dir.join(p))
        .map(fs::read_to_string)
        .collect::<Result<String, _>>()?;

    let output = parser::File::try_from(lexer::lex(&src).map_err(|e| e.to_string())?.1)?;

    let output_dir = env::var("OUT_DIR")?;
    let output_dir = Path::new(&output_dir);
    let mut output_file = File::create(output_dir.join("unicode.rs"))?;
    writeln!(output_file, "{output}")?;

    Ok(())
}
