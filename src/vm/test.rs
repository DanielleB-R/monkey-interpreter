
use super::*;
use crate::ast;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

#[test]
fn test_integer_arithmetic() {
    let cases = vec![
        ("1", Object::Integer(1)),
        ("2", Object::Integer(2)),
        ("1 + 2", Object::Integer(3)),
        ("1 - 2", Object::Integer(-1)),
        ("1 * 2", Object::Integer(2)),
        ("4 / 2", Object::Integer(2)),
        ("50 / 2 * 2 + 10 - 5", Object::Integer(55)),
        ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
        ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
        ("5 * 2 + 10", 20.into()),
        ("5 + 2 * 10", 25.into()),
        ("5 * (2 + 10)", 60.into()),
        ("-5", (-5).into()),
        ("-10", (-10).into()),
        ("-50 + 100 + -50", 0.into()),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50.into()),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_boolean_expressions() {
    let cases = vec![
        ("true", true.into()),
        ("false", false.into()),
        ("1 < 2", true.into()),
        ("1 > 2", false.into()),
        ("1 < 1", false.into()),
        ("1 > 1", false.into()),
        ("1 == 1", true.into()),
        ("1 != 1", false.into()),
        ("1 == 2", false.into()),
        ("1 != 2", true.into()),
        ("true == true", true.into()),
        ("false == false", true.into()),
        ("true == false", false.into()),
        ("true != false", true.into()),
        ("false != true", true.into()),
        ("(1 < 2) == true", true.into()),
        ("(1 < 2) == false", false.into()),
        ("(1 > 2) == true", false.into()),
        ("(1 > 2) == false", true.into()),
        ("!true", false.into()),
        ("!false", true.into()),
        ("!5", false.into()),
        ("!!true", true.into()),
        ("!!false", false.into()),
        ("!!5", true.into()),
        ("!(if (false) { 5; })", true.into()),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_conditionals() {
    let cases = vec![
        ("if (true) { 10 }", 10.into()),
        ("if (true) { 10 } else { 20 }", 10.into()),
        ("if (false) { 10 } else { 20 }", 20.into()),
        ("if (1) { 10 }", 10.into()),
        ("if (1 < 2) { 10 }", 10.into()),
        ("if (1 < 2) { 10 } else { 20 }", 10.into()),
        ("if (1 > 2) { 10 } else { 20 }", 20.into()),
        ("if (1 > 2) { 10 }", Object::Null),
        ("if (false) { 10 }", Object::Null),
        ("if ((if (false) { 10 })) { 10 } else { 20 }", 20.into()),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_global_let_statments() {
    let cases = vec![
        ("let one = 1; one", 1.into()),
        ("let one = 1; let two = 2; one + two", 3.into()),
        ("let one = 1; let two = one + one; one + two", 3.into()),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_string_expressions() {
    let cases = vec![
        ("\"monkey\"", "monkey".into()),
        ("\"mon\" + \"key\"", "monkey".into()),
        ("\"mon\" + \"key\" + \"banana\"", "monkeybanana".into()),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_array_expressions() {
    let cases = vec![
        ("[]", vec![].into()),
        ("[1, 2, 3]", vec![1.into(), 2.into(), 3.into()].into()),
        (
            "[1 + 2, 3 * 4, 5 + 6]",
            vec![3.into(), 12.into(), 11.into()].into(),
        ),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_hash_literals() {
    let cases = vec![
        ("{}", HashValue::default().into()),
        (
            "{1: 2, 2: 3}",
            vec![(1.into(), 2.into()), (2.into(), 3.into())]
                .into_iter()
                .collect::<HashValue>()
                .into(),
        ),
        (
            "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
            vec![(2.into(), 4.into()), (6.into(), 16.into())]
                .into_iter()
                .collect::<HashValue>()
                .into(),
        ),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_index_expressions() {
    let cases = vec![
        ("[1, 2, 3][1]", 2.into()),
        ("[1, 2, 3][0 + 2]", 3.into()),
        ("[[1, 1, 1]][0][0]", 1.into()),
        ("[][0]", Object::Null),
        ("[1, 2, 3][99]", Object::Null),
        ("[1][-1]", Object::Null),
        ("{1: 1, 2: 2}[2]", 2.into()),
        ("{1: 1}[0]", Object::Null),
        ("{}[0]", Object::Null),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_calling_functions_without_arguments() {
    let cases = vec![
        (
            "
let fivePlusTen = fn() { 5 + 10; };
fivePlusTen();
",
            15.into(),
        ),
        (
            "
let one = fn() { 1; };
let two = fn() { 2; };
one() + two()",
            3.into(),
        ),
        (
            "
let a = fn() { 1 };
let b = fn() { a() + 1 };
let c = fn() { b() + 1 };
c();",
            3.into(),
        ),
        (
            "
let earlyExit = fn() { return 99; 100; };
earlyExit();",
            99.into(),
        ),
        (
            "
let earlyExit = fn() { return 99; return 100; };
earlyExit();",
            99.into(),
        ),
        (
            "
let noReturn = fn() { };
noReturn()",
            Object::Null,
        ),
        (
            "
let noReturn = fn() { };
let noReturnTwo = fn() { noReturn(); };
noReturn();
noReturnTwo();",
            Object::Null,
        ),
    ];

    run_vm_tests(cases);
}

#[test]
fn test_first_class_functions() {
    let cases = vec![
        (
            "let returnsOne = fn() { 1; };
let returnsOneReturner = fn() { returnsOne; };
returnsOneReturner()();",
            1.into(),
        ),
        (
            "let returnsOneReturner = fn() {
               let returnsOne = fn() { 1; };
               returnsOne;
           };
           returnsOneReturner()();",
            1.into(),
        ),
    ];

    run_vm_tests(cases)
}

#[test]
fn test_calling_functions_with_bindings() {
    let cases = vec![
        (
            "let one = fn() { let one = 1; one };
one();",
            1.into(),
        ),
        (
            "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
           oneAndTwo();",
            3.into(),
        ),
        (
            "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
           let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
           oneAndTwo() + threeAndFour();",
            10.into(),
        ),
        (
            "let firstFoobar = fn() { let foobar = 50; foobar; };
           let secondFoobar = fn() { let foobar = 100; foobar; };
           firstFoobar() + secondFoobar();",
            150.into(),
        ),
        (
            "let globalSeed = 50;
           let minusOne = fn() {
let num = 1;
               globalSeed - num;
           }
           let minusTwo = fn() {
               let num = 2;
               globalSeed - num;
           }
           minusOne() + minusTwo();",
            97.into(),
        ),
    ];

    run_vm_tests(cases)
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let cases = vec![
        (
            "let identity = fn(a) { a; };
           identity(4);",
            4.into(),
        ),
        (
            "let sum = fn(a, b) { a + b; };
           sum(1, 2);",
            3.into(),
        ),
        (
            "let sum = fn(a, b) {
               let c = a + b;
  c;
};
sum(1, 2);",
            3.into(),
        ),
        (
            "let sum = fn(a, b) {
               let c = a + b;
c; };
           sum(1, 2) + sum(3, 4);",
            10.into(),
        ),
        (
            "let sum = fn(a, b) {
let c = a + b;
c; };
           let outer = fn() {
               sum(1, 2) + sum(3, 4);
           };
           outer();",
            10.into(),
        ),
        (
            "let globalNum = 10;
           let sum = fn(a, b) {
               let c = a + b;
               c + globalNum;
};
           let outer = fn() {
               sum(1, 2) + sum(3, 4) + globalNum;
};
           outer() + globalNum;",
            50.into(),
        ),
    ];

    run_vm_tests(cases)
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let cases = vec![
        (
            "fn() { 1; }(1);",
            VMError::WrongArgumentCount {
                expected: 0,
                found: 1,
            },
        ),
        (
            "fn(a) { a; }();",
            VMError::WrongArgumentCount {
                expected: 1,
                found: 0,
            },
        ),
        (
            "fn(a, b) { a + b; }(1);",
            VMError::WrongArgumentCount {
                expected: 2,
                found: 1,
            },
        ),
    ];

    for (input, err) in cases {
        let program = parse(input);

        let mut comp = Compiler::default();
        comp.compile(program.into()).unwrap();

        let mut vm = VM::new(comp.bytecode());
        assert_eq!(vm.run().unwrap_err(), err);
    }
}

fn parse(input: &str) -> ast::Program {
    Parser::new(Lexer::new(input.to_owned()))
        .parse_program()
        .expect("Parse errors found")
}

fn run_vm_tests(tests: Vec<(&str, Object)>) {
    for (input, output) in tests {
        let program = parse(input);

        let mut comp = Compiler::default();
        comp.compile(program.into()).unwrap();

        let mut vm = VM::new(comp.bytecode());
        vm.run().unwrap();

        assert_eq!(vm.last_popped_stack_element(), &output);
    }
}
