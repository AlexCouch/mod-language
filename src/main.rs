#![feature(box_syntax)]

extern crate mod_language;

use mod_language::{
  ansi,
  session::SESSION,
  source::SOURCE_MANAGER,
  lexer::Lexer,
  parser::Parser,
  analyzer::Analyzer,
  decl_builder::generate_declarations,
  ast,
};


fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  

  SESSION.init();
  SOURCE_MANAGER.init();


  let source = SOURCE_MANAGER.load("./test_scripts/item_analysis.ms")?;


  let mut lexer = Lexer::new(source);

  let stream = lexer.lex_stream();

  println!("Got token stream, dumping to ./log/stream");
  if !std::path::Path::new("./log").exists() { std::fs::create_dir("./log").expect("Failed to create ./log dir"); }
  std::fs::write("./log/stream", format!("{:#?}", stream)).expect("Failed to dump token stream to ./log/stream");


  let mut parser = Parser::new(&stream);

  let ast_vec = parser.parse_ast();

  println!("Got ast, dumping to ./log/ast");
  std::fs::write("./log/ast", format!("{:#?}", ast_vec)).expect("Failed to dump token ast to ./log/ast");


  let analyzer = Analyzer::new();

  let (context, transformed_ast) = analyzer.analyze(ast_vec);

  println!("Got context, dumping to ./log/context");
  std::fs::write("./log/context", format!("{:#?}", context)).expect("Failed to dump context to ./log/context");

  println!("Got transformed ast, dumping to ./log/transformed_ast");
  std::fs::write("./log/transformed_ast", format!("{}", ast::Displayer(&transformed_ast))).expect("Failed to dump transformed ast to ./log/transformed_ast");


  let decls = generate_declarations(&context);

  println!("Got declaration AST, dumping to ./log/decl");
  std::fs::write("./log/decl", format!("{}", ast::Displayer(&decls))).expect("Failed to dump declaration ast to ./log/decl");


  if !SESSION.messages().is_empty() {
    SESSION.print_messages();
  }


  Ok(())
}