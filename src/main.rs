mod sca;

use clap::Parser;
use sca::{ProcessedLexicon, SoundChangeApplier, SoundChangeError};
use serde::Deserialize;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about = "Sound change applier CLI", long_about = None)]
struct Cli {
    /// Path to the TOML configuration (contains categories, rules, rewrites)
    #[arg(long, value_name = "FILE")]
    config: PathBuf,

    /// Path to the lexicon to transform
    #[arg(long, value_name = "FILE")]
    lexicon: PathBuf,

    /// Optional file to write the transformed lexicon to (stdout if omitted)
    #[arg(long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Print a short processing summary to stderr
    #[arg(long)]
    stats: bool,

    /// Output the transformed lexicon as CSV with input/output columns
    #[arg(long)]
    csv: bool,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let config_raw = fs::read_to_string(&cli.config)?;
    let config: ScaConfig = toml::from_str(&config_raw)?;
    let lexicon = fs::read_to_string(&cli.lexicon)?;

    let categories_src = join_lines(&config.categories);
    let rules_src = join_lines(&config.rules);
    let rewrites_src = join_lines(&config.rewrites);

    let applier = SoundChangeApplier::new(&categories_src, &rewrites_src, &rules_src)
        .map_err(|err| Box::new(SoundChangeCliError::from(err)) as Box<dyn std::error::Error>)?;
    let processed = applier.apply_lexicon(&lexicon);

    let output = if cli.csv {
        render_csv_output(&processed)
    } else {
        render_text_output(&processed)
    };

    if let Some(path) = &cli.output {
        fs::write(path, output)?;
    } else {
        print!("{}", output);
    }

    if cli.stats {
        report_stats(&applier, &processed);
    }

    Ok(())
}

#[derive(Debug, Deserialize)]
struct ScaConfig {
    categories: Vec<String>,
    rules: Vec<String>,
    #[serde(default)]
    rewrites: Vec<String>,
}

fn join_lines(lines: &[String]) -> String {
    if lines.is_empty() {
        String::new()
    } else {
        let mut joined = lines.join("\n");
        joined.push('\n');
        joined
    }
}

fn render_text_output(processed: &ProcessedLexicon) -> String {
    let mut lines = Vec::with_capacity(processed.entries.len());
    for entry in &processed.entries {
        match &entry.gloss {
            Some(gloss) => lines.push(format!("{} ‣ {}", entry.transformed, gloss)),
            None => lines.push(entry.transformed.clone()),
        }
    }
    if !lines.is_empty() {
        lines.join("\n") + "\n"
    } else {
        String::new()
    }
}

fn render_csv_output(processed: &ProcessedLexicon) -> String {
    let mut lines = Vec::with_capacity(processed.entries.len() + 1);
    lines.push("input,output,gloss".to_string());
    for entry in &processed.entries {
        let gloss = entry.gloss.as_deref().unwrap_or("");
        let mut row = String::new();
        row.push_str(&csv_escape(&entry.original));
        row.push(',');
        row.push_str(&csv_escape(&entry.transformed));
        row.push(',');
        row.push_str(&csv_escape(gloss));
        lines.push(row);
    }
    lines.join("\n") + "\n"
}

fn csv_escape(value: &str) -> String {
    if value.is_empty() {
        return String::new();
    }

    let needs_quotes =
        value.contains(',') || value.contains('"') || value.contains('\n') || value.contains('\r');

    if !needs_quotes {
        return value.to_string();
    }

    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');
    for ch in value.chars() {
        if ch == '"' {
            escaped.push('"');
        }
        escaped.push(ch);
    }
    escaped.push('"');
    escaped
}

fn report_stats(applier: &SoundChangeApplier, processed: &ProcessedLexicon) {
    let categories: String = applier.category_symbols().iter().collect();
    eprintln!("Categories: {categories}");
    eprintln!("Valid rules: {}", applier.rule_count());
    eprintln!("Words processed: {}", processed.words_processed);
    eprintln!("Words changed: {}", processed.words_changed);
}

#[derive(Debug)]
struct SoundChangeCliError(SoundChangeError);

impl From<SoundChangeError> for SoundChangeCliError {
    fn from(err: SoundChangeError) -> Self {
        Self(err)
    }
}

impl std::fmt::Display for SoundChangeCliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{0}", self.0)
    }
}

impl std::error::Error for SoundChangeCliError {}
