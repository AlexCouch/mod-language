#![feature(box_syntax)]

use mod_language::{
  source::{ Source, SourceRegion, SourceLocation, },
  lexer::Lexer,
  ansi,
};



fn main () -> std::io::Result<()> {
  if !ansi::enable() { println!("Failed to enable ansi coloring for terminal") }
  else { println!("\n{}Ansi coloring enabled for terminal{}\n", ansi::Foreground::Green, ansi::Foreground::Reset) }
  
  { // test lexer
    let source = Source::load("./test_scripts/min.ms".to_owned())?;

    let mut lexer = Lexer::new(&source);

    let stream = lexer.lex_stream();

    println!("Lexing complete:\n{}", stream);

    source.notice(None, format!("Test {}", 123));
    source.warning(None, format!("Test {}", 456));

    let region = SourceRegion {
      start: SourceLocation {
        index: source.line_and_column_to_index(17, 11).unwrap(),
        line: 17,
        column: 11,
      },
      end: SourceLocation {
        index: source.line_and_column_to_index(19, 11).unwrap(),
        line: 19,
        column: 11,
      },
    };

    source.warning(Some(region), "Theres a problem or whatever".to_string());

    source.print_notices();
    source.print_warnings();
    source.print_errors();
  }

  println!("\n-------------------------\n");


  { // test type_expression
    use mod_language::{ ast::{ TypeExpression, TypeExpressionData, }, parser::{ Parser, type_expression, }, };


    let source = Source::load("./test_scripts/type_expression.ms".to_owned())?;

    let mut lexer = Lexer::new(&source);

    let stream = lexer.lex_stream();

    let mut parser = Parser::new(&stream);

    let ident = type_expression(&mut parser);

    if let Some(TypeExpression { data: TypeExpressionData::Identifier(ident), .. }) = ident {
      println!("Got type identifier {}", ident);
    } else {
      panic!("Expected type identifier, got {:#?}", ident);
    }

    let none = type_expression(&mut parser);

    if none.is_none() && source.messages.borrow().len() == 1 {
      println!("Got expected error instead of type expression");
      source.print_errors();
    } else {
      source.print_messages();
      panic!("Expected error, got {:#?}", none);
    }
  }

  println!("\n-------------------------\n");


  { // test expression
    use mod_language::{ ast::{ ExpressionData, }, parser::{ Parser, expression, }, token::{ Operator, }, };


    let source = Source::load("./test_scripts/expression.ms".to_owned())?;

    let mut lexer = Lexer::new(&source);

    let stream = lexer.lex_stream();

    println!("Lexing complete:\n{}", &stream);

    let mut parser = Parser::new(&stream);

    let expr = expression(&mut parser);

    println!("Got expression: {:#?}", expr);

    assert_eq!(expr, Some(ExpressionData::Call {
      callee: box "func".into(),
      arguments: vec! [
        ExpressionData::Binary {
          left: box 1.into(),
          right: box ExpressionData::Binary {
            left: box 2.into(),
            right: box 3.into(),
            operator: Operator::Add
          }.into(),
          operator: Operator::Mul
        }.into()
      ]
    }.into()));

    if source.messages.borrow().len() != 0 {
      source.print_messages();
      panic!("Parsing expression failed");
    }
  }


  println!("\n-------------------------\n");


  { // test statements
    use mod_language::{ parser::{ Parser, block, }, ast::{ StatementData, ExpressionData, Block, Conditional, ConditionalBranch, }, token::{ Operator, } };


    let source = Source::load("./test_scripts/block.ms".to_owned())?;

    let mut lexer = Lexer::new(&source);

    let stream = lexer.lex_stream();

    println!("Lexing complete:\n{}", &stream);

    let mut parser = Parser::new(&stream);

    let block = block(&mut parser);

    println!("Got block: {:#?}", block);

    assert_eq!(block, Some(Block::no_src(
      vec! [
        StatementData::Declaration {
          identifier: "variable".into(),
          explicit_type: Some("u32".into()),
          initializer: Some(64.into())
        }.into(),
        StatementData::ModAssignment {
          target: "variable".into(),
          value: 99.into(),
          operator: Operator::AssignAdd
        }.into(),
      ],
      Some(ExpressionData::Conditional(box Conditional::no_src(
        ConditionalBranch::no_src(
          "condition".into(),
          Block::no_src(
            vec![],
            Some("variable".into())
          )
        ),
        vec![],
        Some(Block::no_src(
          vec![],
          Some(200.into())
        ))
      )).into())
    )));

    if source.messages.borrow().len() != 0 {
      source.print_messages();
      panic!("Parsing block failed");
    }
  }


  Ok(())
}