#![feature(box_syntax)]

extern crate mod_language;

use mod_language::{
  session::SESSION,
  source::SOURCE_MANAGER,
  lexer::Lexer,
  parser::Parser,
  analyzer::Analyzer,
  ansi,
};


fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  

  SESSION.init();
  SOURCE_MANAGER.init();


  let source = SOURCE_MANAGER.load("./test_scripts/item_analysis.ms")?;


  let mut lexer = Lexer::new(source);

  let stream = lexer.lex_stream();

  println!("Got token stream, dumping to ./stream.log");
  std::fs::write("./stream.log", format!("{:#?}", stream)).expect("Failed to dump token stream to ./stream.log");


  let mut parser = Parser::new(&stream);

  let ast = parser.parse_ast();

  println!("Got ast, dumping to ./ast.log");
  std::fs::write("./ast.log", format!("{:#?}", ast)).expect("Failed to dump token ast to ./ast.log");


  let analyzer = Analyzer::new(&ast);

  let context = analyzer.analyze();

  println!("Got context, dumping to ./context.log");
  std::fs::write("./context.log", format!("{:#?}", context)).expect("Failed to dump context to ./context.log");

  if !SESSION.messages().is_empty() {
    SESSION.print_messages();
    panic!("Error parsing items");
  }


  Ok(())
}