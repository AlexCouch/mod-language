#![feature(box_syntax)]

extern crate mod_language;

use mod_language::{
  source::Source,
  lexer::Lexer,
  parser::Parser,
  ansi,
};



fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
 
  
  let source = Source::load("./test_scripts/item.ms".to_owned())?;

  let mut lexer = Lexer::new(&source);

  let stream = lexer.lex_stream();

  println!("Lexing complete:\n{}", &stream);

  let mut parser = Parser::new(&stream);

  let ast = parser.parse_ast();

  println!("Got ast: {}", ast);

  if source.messages.borrow().len() != 0 {
    source.print_messages();
    panic!("Error parsing items");
  }


  Ok(())
}