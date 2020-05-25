//! The main driver application for mod_language

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::useless_let_if_seq)]

use mod_ansi as ansi;
use mod_utils::UnwrapPretty;
use mod_declaration_builder::generate_declarations;
use mod_codegen::Codegen;

use mod_frontend::{
  session::SESSION,
  source::SOURCE_MANAGER,
  lexer::Lexer,
  parser::Parser,
  analyzer::Analyzer,
  ast,
};

use mod_rc::Context;


fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  

  SESSION.init();
  SOURCE_MANAGER.init("./test_scripts/modules/".into());


  let source = SOURCE_MANAGER.load_source("./test_scripts/fibonacci.ms").expect("Could not find entry source file");


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


  if !SESSION.messages().is_empty() {
    SESSION.print_messages();
    if SESSION.count_errors() > 0 {
      println!("Cannot procede to codegen due to errors");
      return Err(std::io::Error::from(std::io::ErrorKind::InvalidInput))
    }
  }


  let decls = generate_declarations(&context);

  println!("Got declaration ast, dumping to ./log/decl");
  std::fs::write("./log/decl", format!("{}", ast::Displayer(&decls))).expect("Failed to dump declaration ast to ./log/decl");


  let codegen = Codegen::new(&context, "test_module".to_owned(), (0, 0, 0).into());

  let bc = codegen.generate();

  println!("Got bytecode, dumping to ./log/bc");
  std::fs::write("./log/bc", format!("{}", bc)).expect("Failed to dump bytecode to ./log/bc");


  let mut context = Context::default();

  let load_result = context.load_module(bc);

  load_result.expect_pretty("Failed to compile bytecode module");

  fn native_fibonacci (n: i32) -> i32 {
    if n < 2 {
      n
    } else {
      native_fibonacci(n - 1) + native_fibonacci(n - 2)
    }
  }
  let fibonacci_addr = context.get_address(&["test_module", "fibonacci"]).expect("Failed to get function address");

  let fibonacci = unsafe { std::mem::transmute::<_, extern "C" fn (i32) -> i32>(fibonacci_addr) };

  let n = 32;

  let res = fibonacci(n);

  let native_res = native_fibonacci(n);

  assert_eq!(res, native_res);

  println!("fibonacci({}) = {}", n, res);

  Ok(())
}