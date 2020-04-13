use super::*;
use crate::ast;
use crate::code::{self, concat_instructions};
use crate::lexer::Lexer;
use crate::parser::Parser;

#[test]
fn test_compiler_scopes() {
    let mut compiler = Compiler::default();
    assert_eq!(compiler.scope_index, 0);
    assert!(compiler.symbol_table.outer.is_none());

    compiler.emit(Opcode::Mul, &[]);

    compiler.enter_scope();
    assert_eq!(compiler.scope_index, 1);
    compiler.emit(Opcode::Sub, &[]);

    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1);
    assert_eq!(
        compiler.scopes[compiler.scope_index].last.opcode,
        Opcode::Sub
    );
    assert!(compiler.symbol_table.outer.is_some());

    compiler.leave_scope();
    assert_eq!(compiler.scope_index, 0);
    assert!(compiler.symbol_table.outer.is_none());
    compiler.emit(Opcode::Add, &[]);

    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2);
    assert_eq!(
        compiler.scopes[compiler.scope_index].last.opcode,
        Opcode::Add
    );
    assert_eq!(
        compiler.scopes[compiler.scope_index].previous.opcode,
        Opcode::Mul
    );
}

#[test]
fn test_integer_arithmetic() {
    let cases = vec![
        (
            "1 + 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Add, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 - 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Sub, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 * 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Mul, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "2 / 1",
            vec![2.into(), 1.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Div, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1; 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "-1",
            vec![1.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Minus, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_boolean_expressions() {
    run_compiler_tests(vec![
        (
            "true",
            vec![],
            vec![
                code::make(Opcode::True, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "false",
            vec![],
            vec![
                code::make(Opcode::False, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 > 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::GreaterThan, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 < 2",
            vec![2.into(), 1.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::GreaterThan, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 == 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Equal, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "1 != 2",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::NotEqual, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "true == false",
            vec![],
            vec![
                code::make(Opcode::True, &[]).unwrap(),
                code::make(Opcode::False, &[]).unwrap(),
                code::make(Opcode::Equal, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "true != false",
            vec![],
            vec![
                code::make(Opcode::True, &[]).unwrap(),
                code::make(Opcode::False, &[]).unwrap(),
                code::make(Opcode::NotEqual, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "!true",
            vec![],
            vec![
                code::make(Opcode::True, &[]).unwrap(),
                code::make(Opcode::Bang, &[]).unwrap(),
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
    ]);
}

#[test]
fn test_conditionals() {
    let cases = vec![
        (
            "if (true) { 10 }; 3333;",
            vec![10.into(), 3333.into()],
            vec![
                // 0000
                make_single(Opcode::True),
                // 0001
                code::make(Opcode::JumpFalsy, &[10]).unwrap(),
                // 0004
                code::make(Opcode::Constant, &[0]).unwrap(),
                // 0007
                code::make(Opcode::Jump, &[11]).unwrap(),
                // 0010
                make_single(Opcode::Null),
                // 0011
                code::make(Opcode::Pop, &[]).unwrap(),
                // 0012
                code::make(Opcode::Constant, &[1]).unwrap(),
                // 0015
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
        (
            "if (true) { 10 } else { 20 }; 3333;",
            vec![10.into(), 20.into(), 3333.into()],
            vec![
                // 0000
                make_single(Opcode::True),
                // 0001
                code::make(Opcode::JumpFalsy, &[10]).unwrap(),
                // 0004
                code::make(Opcode::Constant, &[0]).unwrap(),
                // 0007
                code::make(Opcode::Jump, &[13]).unwrap(),
                // 0010
                code::make(Opcode::Constant, &[1]).unwrap(),
                // 0013
                code::make(Opcode::Pop, &[]).unwrap(),
                // 0008
                code::make(Opcode::Constant, &[2]).unwrap(),
                // 0011
                code::make(Opcode::Pop, &[]).unwrap(),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_global_let_statements() {
    let cases = vec![
        (
            "let one = 1;
let two = 2;",
            vec![1.into(), 2.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::SetGlobal, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::SetGlobal, &[1]).unwrap(),
            ],
        ),
        (
            "let one = 1;
one;",
            vec![1.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::SetGlobal, &[0]).unwrap(),
                code::make(Opcode::GetGlobal, &[0]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "let one = 1;
let two = one;
two;",
            vec![1.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::SetGlobal, &[0]).unwrap(),
                code::make(Opcode::GetGlobal, &[0]).unwrap(),
                code::make(Opcode::SetGlobal, &[1]).unwrap(),
                code::make(Opcode::GetGlobal, &[1]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_string_expressions() {
    let cases = vec![
        (
            "\"monkey\"",
            vec!["monkey".into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "\"mon\" + \"key\"",
            vec!["mon".into(), "key".into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                make_single(Opcode::Add),
                make_single(Opcode::Pop),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_array_literals() {
    let cases = vec![
        (
            "[]",
            vec![],
            vec![
                code::make(Opcode::Array, &[0]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "[1, 2, 3]",
            vec![1.into(), 2.into(), 3.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                code::make(Opcode::Constant, &[2]).unwrap(),
                code::make(Opcode::Array, &[3]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "[1 + 2, 3 - 4, 5 * 6]",
            vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
                make_single(Opcode::Add),
                code::make(Opcode::Constant, &[2]).unwrap(),
                code::make(Opcode::Constant, &[3]).unwrap(),
                make_single(Opcode::Sub),
                code::make(Opcode::Constant, &[4]).unwrap(),
                code::make(Opcode::Constant, &[5]).unwrap(),
                make_single(Opcode::Mul),
                code::make(Opcode::Array, &[3]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_hash_literals() {
    let cases = vec![
        (
            "{}",
            vec![],
            vec![
                code::make(Opcode::Hash, &[0]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "{1: 2, 3: 4, 5: 6}",
            vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
            vec![
                make_constant(0),
                make_constant(1),
                make_constant(2),
                make_constant(3),
                make_constant(4),
                make_constant(5),
                code::make(Opcode::Hash, &[6]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "{1: 2 + 3, 4: 5 * 6}",
            vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
            vec![
                make_constant(0),
                make_constant(1),
                make_constant(2),
                make_single(Opcode::Add),
                make_constant(3),
                make_constant(4),
                make_constant(5),
                make_single(Opcode::Mul),
                code::make(Opcode::Hash, &[4]).unwrap(),
                make_single(Opcode::Pop),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_index_expressions() {
    let cases = vec![
        (
            "[1, 2, 3][1 + 1]",
            vec![1.into(), 2.into(), 3.into(), 1.into(), 1.into()],
            vec![
                make_constant(0),
                make_constant(1),
                make_constant(2),
                code::make(Opcode::Array, &[3]).unwrap(),
                make_constant(3),
                make_constant(4),
                make_single(Opcode::Add),
                make_single(Opcode::Index),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "{1: 2}[2 - 1]",
            vec![1.into(), 2.into(), 2.into(), 1.into()],
            vec![
                make_constant(0),
                make_constant(1),
                code::make(Opcode::Hash, &[2]).unwrap(),
                make_constant(2),
                make_constant(3),
                make_single(Opcode::Sub),
                make_single(Opcode::Index),
                make_single(Opcode::Pop),
            ],
        ),
    ];

    run_compiler_tests(cases);
}

#[test]
fn test_functions() {
    let cases = vec![
        (
            "fn() { return 5 + 10; }",
            vec![
                5.into(),
                10.into(),
                concat_instructions(vec![
                    make_constant(0),
                    make_constant(1),
                    make_single(Opcode::Add),
                    make_single(Opcode::ReturnValue),
                ])
                .into(),
            ],
            vec![make_constant(2), make_single(Opcode::Pop)],
        ),
        (
            "fn() { 5 + 10; }",
            vec![
                5.into(),
                10.into(),
                concat_instructions(vec![
                    make_constant(0),
                    make_constant(1),
                    make_single(Opcode::Add),
                    make_single(Opcode::ReturnValue),
                ])
                .into(),
            ],
            vec![make_constant(2), make_single(Opcode::Pop)],
        ),
        (
            "fn() { 1; 2 }",
            vec![
                1.into(),
                2.into(),
                concat_instructions(vec![
                    make_constant(0),
                    make_single(Opcode::Pop),
                    make_constant(1),
                    make_single(Opcode::ReturnValue),
                ])
                .into(),
            ],
            vec![make_constant(2), make_single(Opcode::Pop)],
        ),
    ];

    run_compiler_tests(cases)
}

#[test]
fn test_functions_without_return_value() {
    let cases = vec![(
        "fn() { }",
        vec![make_single(Opcode::Return).into()],
        vec![make_constant(0), make_single(Opcode::Pop)],
    )];

    run_compiler_tests(cases);
}

#[test]
fn test_function_calls() {
    let cases = vec![(
        "fn () { 24 }();",
        vec![
            24.into(),
            concat_instructions(vec![make_constant(0), make_single(Opcode::ReturnValue)]).into(),
        ],
        vec![
            make_constant(1),
            make_single(Opcode::Call),
            make_single(Opcode::Pop),
        ],
    )];

    run_compiler_tests(cases)
}

#[test]
fn test_let_statement_scopes() {
    let cases = vec![
        (
            "let num = 55;
fn() { num }",
            vec![
                55.into(),
                concat_instructions(vec![
                    code::make(Opcode::GetGlobal, &[0]).unwrap(),
                    make_single(Opcode::ReturnValue),
                ])
                .into(),
            ],
            vec![
                make_constant(0),
                code::make(Opcode::SetGlobal, &[0]).unwrap(),
                make_constant(1),
                make_single(Opcode::Pop),
            ],
        ),
        (
            "fn() {
  let num = 55;
  num
}",
            vec![
                55.into(),
                CompiledFunction::new(
                    concat_instructions(vec![
                        make_constant(0),
                        code::make(Opcode::SetLocal, &[0]).unwrap(),
                        code::make(Opcode::GetLocal, &[0]).unwrap(),
                        make_single(Opcode::ReturnValue),
                    ]),
                    1,
                )
                .into(),
            ],
            vec![make_constant(1), make_single(Opcode::Pop)],
        ),
        (
            "fn() {
  let a = 55;
  let b = 77;
  a + b
}",
            vec![
                55.into(),
                77.into(),
                CompiledFunction::new(
                    concat_instructions(vec![
                        make_constant(0),
                        code::make(Opcode::SetLocal, &[0]).unwrap(),
                        make_constant(1),
                        code::make(Opcode::SetLocal, &[1]).unwrap(),
                        code::make(Opcode::GetLocal, &[0]).unwrap(),
                        code::make(Opcode::GetLocal, &[1]).unwrap(),
                        make_single(Opcode::Add),
                        make_single(Opcode::ReturnValue),
                    ]),
                    2,
                )
                .into(),
            ],
            vec![make_constant(2), make_single(Opcode::Pop)],
        ),
    ];

    run_compiler_tests(cases)
}

fn run_compiler_tests(cases: Vec<(&str, Vec<Object>, Vec<Instructions>)>) {
    for (input, constants, instructions) in cases.into_iter() {
        let program = parse(input);

        let mut compiler = Compiler::default();
        compiler.compile(program.into()).unwrap();

        let bytecode = compiler.bytecode();
        test_instructions(instructions, bytecode.instructions);
        assert_eq!(constants, bytecode.constants);
    }
}

fn make_single(opcode: Opcode) -> Instructions {
    code::make(opcode, &[]).unwrap()
}

fn make_constant(index: isize) -> Instructions {
    code::make(Opcode::Constant, &[index]).unwrap()
}

fn parse(input: &str) -> ast::Program {
    Parser::new(Lexer::new(input.to_owned()))
        .parse_program()
        .expect("Parse errors found")
}

fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
    let concatted = concat_instructions(expected);

    if concatted != actual {
        panic!(format!("Expected {}, received {}", concatted, actual));
    }
}
