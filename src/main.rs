use mod_language::{
  source::{ load_source },
  lexer::Lexer
};


fn main () -> std::io::Result<()> {
  let source_id = load_source("./test.ms".to_owned())?;

  let mut lexer = Lexer::new(source_id).unwrap();

  let mut tokens = Vec::new();
  
  loop {
    match lexer.lex_token() {
      Ok(tok_or_eof) => if let Some(token) = tok_or_eof { tokens.push(token) } else { break },
      Err(ch) => println!("Unexpected lexical symbol [{:?}]", ch)
    }
  }

  println!("Got tokens: {:#?}", tokens);

  Ok(())
}