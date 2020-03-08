use mod_language::{
  source::{ Source, SourceRegion, SourceLocation },
  lexer::{ Lexer, InvalidLexicalSymbol },
  token::print_tokens,
  ansi,
};



fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  
  let source = Source::load("./test_scripts/min.ms".to_owned())?;

  let mut lexer = Lexer::new(&source).unwrap();

  let mut tokens = Vec::new();
  
  loop {
    match lexer.lex_token() {
      Ok(tok_or_eof) => if let Some(token) = tok_or_eof { tokens.push(token) } else { break },
      Err(InvalidLexicalSymbol { symbol, origin }) => lexer.error_at(origin, format!("Unexpected lexical symbol {:?}", symbol))
    }
  }

  print_tokens(&tokens, &source);

  source.notice(None, format!("Test {}", 123));
  source.warning(None, format!("Test {}", 456));

  let region = SourceRegion {
    start: SourceLocation {
      index: source.line_and_column_to_index(17, 0).unwrap(),
      line: 17,
      column: 11,
    },
    end: SourceLocation {
      index: source.line_and_column_to_index(19, 2).unwrap(),
      line: 19,
      column: 11,
    },
  };

  source.warning(Some(region), "Theres a problem or whatever".to_string());

  source.print_notices();
  source.print_warnings();
  source.print_errors();

  Ok(())
}