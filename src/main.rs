use mod_language::{
  source::Source,
  lexer::Lexer,
  token::TokenVecDebugger,
};


fn main () -> std::io::Result<()> {
  let source = Source::load("./test_scripts/min.ms".to_owned())?;

  let mut lexer = Lexer::new(&source).unwrap();

  let mut tokens = Vec::new();
  
  loop {
    match lexer.lex_token() {
      Ok(tok_or_eof) => if let Some(token) = tok_or_eof { tokens.push(token) } else { break },
      Err(ch) => lexer.error_at(lexer.curr_region(), Some(format!("Unexpected lexical symbol {:?}", ch)))
    }
  }

  println!("Got tokens: {:?}", TokenVecDebugger::new(&tokens, &source));

  source.print_notices();
  source.print_warnings();
  source.print_errors();

  Ok(())
}