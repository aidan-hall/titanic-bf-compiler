/// The instruction set of BF, tokenised.
#[derive(Debug, PartialEq, Eq)]
enum Token {
    Inc,
    Dec,
    ShiftLeft,
    ShiftRight,
    BracketLeft,
    BracketRight,
    Input,
    Output,
}

/// Lex the source into a token stream, ignoring non-BF characters.
/// Having a separate lexing stage makes it easy to use BF "skins" such as Ook!
fn lexed(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    for c in source.chars() {
        use Token::*;
        if let Some(t) = match c {
            '+' => Some(Inc),
            '-' => Some(Dec),
            '<' => Some(ShiftLeft),
            '>' => Some(ShiftRight),
            '[' => Some(BracketLeft),
            ']' => Some(BracketRight),
            ',' => Some(Input),
            '.' => Some(Output),
            // Simply ignore non-BF characters.
            _ => None,
        } {
            tokens.push(t);
        }
    }

    tokens
}

#[derive(Debug, PartialEq, Eq)]
enum Symbol {
    // Subtraction is negative addition.
    Add(i32),
    // A positive value means a right shift. Left shift is a negative right shift.
    Shift(i32),
    Input,
    Output,
    // This is what makes it an abstract syntax *tree*!
    Loop(Ast),
}

#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    ImbalancedBrackets,
}

/// Abstract Syntax Tree
type Ast = Vec<Symbol>;

/// Parse the token stream into an AST. Perform no optimisations.
/// Since this is the only function that generates an Ast from something else,
/// we can assume all Asts are valid (hence the value of the type alias).
fn parsed(tokens: &Vec<Token>) -> Result<Ast, ParseError> {
    use std::collections::LinkedList;

    let mut asts: LinkedList<Ast> = LinkedList::new();
    asts.push_front(Vec::new());

    for token in tokens {
        use Symbol::*;
        use Token::*;
        if let Some(symbol) = match token {
            Inc => Some(Add(1)),
            Dec => Some(Add(-1)),
            ShiftLeft => Some(Shift(-1)),
            ShiftRight => Some(Shift(1)),
            BracketLeft => {
                // Begin parsing tokens for the contents of the loop.
                asts.push_front(Vec::new());
                None
            }
            BracketRight => Some(Loop(
                asts.pop_front()
                    .expect("Height ≥ 1 should be guaranteed here."),
            )),
            Token::Input => Some(Symbol::Input),
            Token::Output => Some(Symbol::Output),
        } {
            if let Some(ast) = asts.front_mut() {
                ast.push(symbol);
            } else {
                return Err(ParseError::ImbalancedBrackets);
            }
        }
    }

    match asts.len() {
        1 => {
            // Take ownership: Ha ha, I understand Rust!
            let ast = asts
                .pop_front()
                .expect("len = 1, so there should be a front element.");
            Ok(ast)
        }
        _ => Err(ParseError::ImbalancedBrackets),
    }
}

/// Discard sequences which do nothing.
fn is_useful(symbol: &Symbol) -> bool {
    symbol != &Symbol::Add(0) && symbol != &Symbol::Shift(0)
}

/// "Flatten" multiple sequential Add & Shift instructions, recursively.
/// This function's name is a verb since it takes ownership of/consumes the input.
fn optimise(ast: Ast) -> Ast {
    println!("Optimising AST: {ast:?}");

    // Just saves reallocating a new vector.
    if ast.is_empty() {
        return ast;
    }

    use Symbol::*;

    // The initial value will automatically be discarded if the next symbol isn't an Add(),
    // since it can have no effect.
    const ACC_DEFAULT_VALUE: Symbol = Add(0);

    // This variable accumulates the effect of multiple of the same Symbol.
    let mut acc = ACC_DEFAULT_VALUE;

    let mut optimised_ast: Ast = Vec::new();

    for symbol in ast {
        if let Some(optimised_symbol) = match symbol {
            Add(n) => {
                if let Add(m) = acc {
                    acc = Add(n + m);
                    None
                } else if is_useful(&acc) {
                    let res = acc;
                    acc = Add(n);
                    Some(res)
                } else {
		    acc = Add(n);
                    None
                }
            }
            Shift(n) => {
                if let Shift(m) = acc {
                    acc = Shift(n + m);
                    None
                } else if is_useful(&acc) {
                    let res = acc;
                    acc = Shift(n);
                    Some(res)
                } else {
		    acc = Shift(n);
                    None
                }
            }
            Loop(loop_contents) => {
                if is_useful(&acc) {
                    optimised_ast.push(acc);
                    acc = ACC_DEFAULT_VALUE;
                }
                let res = Loop(optimise(loop_contents));
		acc = ACC_DEFAULT_VALUE;
		Some(res)
            },
            // Wild card for all other symbol types.
            a => {
                if is_useful(&acc) {
                    optimised_ast.push(acc);
                    acc = ACC_DEFAULT_VALUE;
                }
		Some(a)
	    },
        } {
            optimised_ast.push(optimised_symbol);
        }
    }

    if is_useful(&acc) {
        optimised_ast.push(acc);
    }

    println!("Optimised AST: {optimised_ast:?}");
    optimised_ast
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_and_parsing_and_optimisation() {
        let source = "++-  ><>[++Hi there!],.";

        let expected_tokens = vec![
            Token::Inc,
            Token::Inc,
            Token::Dec,
            Token::ShiftRight,
            Token::ShiftLeft,
            Token::ShiftRight,
            Token::BracketLeft,
            Token::Inc,
            Token::Inc,
            Token::BracketRight,
            Token::Input,
            Token::Output,
        ];

        let tokens = lexed(source);

        assert_eq!(tokens.len(), expected_tokens.len());
        for (exp, act) in std::iter::zip(&expected_tokens, &tokens) {
            assert_eq!(exp, act);
        }

        let expected_symbols = vec![
            Symbol::Add(1),
            Symbol::Add(1),
            Symbol::Add(-1),
            Symbol::Shift(1),
            Symbol::Shift(-1),
            Symbol::Shift(1),
            Symbol::Loop(vec![Symbol::Add(1), Symbol::Add(1)]),
            Symbol::Input,
            Symbol::Output,
        ];

        let symbols = parsed(&tokens).expect("Parses correctly.");

        assert_eq!(symbols.len(), expected_symbols.len());
        for (exp, act) in std::iter::zip(&expected_symbols, &symbols) {
            assert_eq!(exp, act);
        }

        let expected_optimised_symbols = vec![
            Symbol::Add(1),
            Symbol::Shift(1),
            Symbol::Loop(vec![Symbol::Add(2)]),
            Symbol::Input,
            Symbol::Output,
        ];
        let optimised_symbols = optimise(symbols);

        for (exp, act) in std::iter::zip(&expected_optimised_symbols, &optimised_symbols) {
            assert_eq!(exp, act);
        }
        assert_eq!(optimised_symbols.len(), expected_optimised_symbols.len());
    }

    #[test]
    fn imbalanced_opening_bracket() {
        let source = "[";
        let symbols = parsed(&lexed(source));
        assert_eq!(symbols, Err(ParseError::ImbalancedBrackets));
    }

    #[test]
    fn imbalanced_closing_bracket() {
        let source = "]";
        let symbols = parsed(&lexed(source));
        assert_eq!(symbols, Err(ParseError::ImbalancedBrackets));
    }

    #[test]
    fn nested_brackets() {
        let source = "[[++]]";
        let symbols = parsed(&lexed(source)).expect("Valid source.");

        use Symbol::*;
        let expected_symbols = vec![Loop(vec![Loop(vec![Add(1), Add(1)])])];

        assert_eq!(symbols.len(), expected_symbols.len());
        for (exp, act) in std::iter::zip(&expected_symbols, &symbols) {
            assert_eq!(exp, act);
        }
    }

    #[test]
    fn optimisation() {
        let source = "+++->>><<[><<]>";
        let ast = optimise(parsed(&lexed(source)).expect("Valid source."));
        use Symbol::*;
        let expected_ast = vec![Add(2), Shift(1), Loop(vec![Shift(-1)]), Shift(1)];

        assert_eq!(ast.len(), expected_ast.len());
        for (exp, act) in std::iter::zip(&expected_ast, &ast) {
            assert_eq!(exp, act);
        }
    }
}

/// Compile the AST to NASM x86 assembly.
fn compiled_x86(ast: &Ast) -> String {
    let mut asm = String::from(
        "SYS_EXIT:	equ	1
SYS_READ:	equ	3
SYS_WRITE:	equ	4
STDIN:		equ	0
STDOUT: 	equ	1

	; Parameters: left bracket identifier, right bracket identifier
%macro left_bracket 2
%1:
	cmp ecx, 0
	jz %2
%endmacro

	; Parameters: left bracket identifier, right bracket identifier
%macro right_bracket 2
	jmp %1
%2:
%endmacro

%macro macro_shift 1
	mov ebx, %1
	call memory_shift
%endmacro

section .text
	global _start

_start:
	;; Zero ecx & eax
	xor ecx, ecx
	xor eax, eax
",
    );

    let (mapped, _) = mapped_x86(ast, 0);

    asm.push_str(mapped.as_str());

    asm.push_str(
        "
	; Exit with status 0 (success).
	mov eax, SYS_EXIT
	mov ebx, 0
	int 80h

write_value:
	mov [cells+eax], ecx
	push eax
	push ecx

	mov ecx, cells
	add ecx, eax
	mov eax, SYS_WRITE
	mov ebx, STDOUT
	mov edx, 1
	int 80h

	pop ecx
	pop eax
	ret


memory_shift:			; Uses ebx as value to shift by.
	mov [cells+eax], ecx
	add eax, ebx
	mov ecx, [cells+eax]
	ret

section .bss
cells:	times 30000 db 0
",
    );

    asm
}

/// Maps the AST to a String, tracking number of labelled loops to avoid conflict.
fn mapped_x86(ast: &Ast, loop_counter: usize) -> (String, usize) {
    // Actually a local copy.
    let mut loop_counter = loop_counter;

    let mut asm = String::new();

    for symbol in ast {
        use Symbol::*;
        let instruction = match symbol {
            Add(n) => format!("\tadd ecx, {n}\n"),
            Shift(n) => format!("\tmacro_shift {n}\n"),
            Input => todo!(),
            Output => format!("\tcall write_value\n"),
            Loop(loop_contents) => {
                let loop_num = loop_counter;
                let (res, updated_loop_counter) = mapped_x86(loop_contents, loop_counter + 1);
                loop_counter = updated_loop_counter;
                format!(
                    "\tleft_bracket lb{loop_num}, rb{loop_num}
{res}
\tright_bracket lb{loop_num}, rb{loop_num}
"
                )
            }
        };
        asm.push_str(instruction.as_str());
    }

    (asm, loop_counter)
}

fn main() {
    use std::env;
    use std::fs;

    let args: Vec<String> = env::args().collect();

    for arg in &args.as_slice()[1..] {
        let name = format!("{arg}-compiled.asm");
	let src = fs::read_to_string(arg)
	    .expect("Couldn't read {arg}.");

	match parsed(&lexed(src.as_str())) {
	    Err(e) => eprintln!("Couldn't compile {arg}: {e:?}"),
	    Ok(ast) => {
		let asm = compiled_x86(&optimise(ast));
		fs::write(&name, asm).expect("Couldn't write to {name}.");
		eprintln!("Assembly written to {name}");
	    }
	}
    }

//     let src = compiled_x86(
//         &parsed(&lexed(
//             "
// +++++++++++++++++++++++++++++++++++++++++++++++++++++>+++[-<+>]<.
// Output new line character to flush stdout
// >>++++++++++.",
//         ))
//         .expect("Valid"),
//     );
//     println!("{src}");
}
