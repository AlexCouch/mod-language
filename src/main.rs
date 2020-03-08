use mod_language::{
  source::Source,
  lexer::{ Lexer, InvalidLexicalSymbol },
  token::TokenVecDebugger,
};


fn main () -> std::io::Result<()> {
  let source = Source::load("./test_scripts/min.ms".to_owned())?;

  let mut lexer = Lexer::new(&source).unwrap();

  let mut tokens = Vec::new();
  
  loop {
    match lexer.lex_token() {
      Ok(tok_or_eof) => if let Some(token) = tok_or_eof { tokens.push(token) } else { break },
      Err(InvalidLexicalSymbol { symbol, origin }) => lexer.error_at(origin, Some(format!("Unexpected lexical symbol {:?}", symbol)))
    }
  }

  println!("Got tokens: {:?}", TokenVecDebugger::new(&tokens, &source));

  source.print_notices();
  source.print_warnings();
  source.print_errors();

  Ok(())
}