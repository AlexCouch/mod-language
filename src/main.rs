#![feature(box_syntax)]

extern crate mod_language;

use mod_language::{
  session::SESSION,
  source::SOURCE_MANAGER,
  lexer::Lexer,
  parser::Parser,
  ansi,
};


fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  
  SESSION.init();
  SOURCE_MANAGER.init();

  let source = SOURCE_MANAGER.load("./test_scripts/module.ms")?;

  let mut lexer = Lexer::new(source);

  let stream = lexer.lex_stream();

  println!("Lexing complete:\n{:#?}", &stream);

  let mut parser = Parser::new(&stream);

  let ast = parser.parse_ast();

  println!("Got ast: {:#?}", &ast);

  if !SESSION.messages().is_empty() {
    SESSION.print_messages();
    panic!("Error parsing items");
  }


  Ok(())
}