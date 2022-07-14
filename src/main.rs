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
                    None
                }
            }
            Loop(loop_contents) => {
                if is_useful(&acc) {
                    optimised_ast.push(acc);
                    acc = ACC_DEFAULT_VALUE;
                }
                Some(Loop(optimise(loop_contents)))
            }
            // Wild card for all other symbol types.
            a => Some(a),
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
    fn optimisation() {
        let source = "+++->>><<";
        let ast = optimise(parsed(&lexed(source)).expect("Valid source."));
        use Symbol::*;
        let expected_ast = vec![Add(2), Shift(1)];

        assert_eq!(ast.len(), expected_ast.len());
        for (exp, act) in std::iter::zip(&expected_ast, &ast) {
            assert_eq!(exp, act);
        }
    }
}

/// Compile the AST to NASM x86 assembly.
fn compiled_x86(ast: &Ast) -> String {
    let mut asm = String::new();

    // TODO: Symbols... ASSEMBLE!!!

    asm
}

fn main() {
    println!("Hello, world!");
}
