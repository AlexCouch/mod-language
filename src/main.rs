use mod_language::{
  source::Source,
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
      Err(InvalidLexicalSymbol { symbol, origin }) => lexer.error_at(origin, Some(format!("Unexpected lexical symbol {:?}", symbol)))
    }
  }

  print_tokens(&tokens, &source);

  source.notice(None, Some(format!("Test {}", 123)));
  source.warning(None, Some(format!("Test {}", 456)));

  source.print_notices();
  source.print_warnings();
  source.print_errors();

  Ok(())
}