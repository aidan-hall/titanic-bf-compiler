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
fn lex(source: &str) -> Vec<Token> {
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
    Loop(Vec<Symbol>),
}

#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    ImbalancedBrackets,
}

/// Abstract Syntax Tree
type Ast = Vec<Symbol>;

/// Parse the token stream into an AST. Perform no optimisations.
fn parse(tokens: &Vec<Token>) -> Result<Ast, ParseError> {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lexing_and_parsing() {
        let source = "+-  <>[++Hi there!],.";

        let expected_tokens = vec![
            Token::Inc,
            Token::Dec,
            Token::ShiftLeft,
            Token::ShiftRight,
            Token::BracketLeft,
            Token::Inc,
            Token::Inc,
            Token::BracketRight,
            Token::Input,
            Token::Output,
        ];

        let tokens = lex(source);

        assert_eq!(tokens.len(), expected_tokens.len());
        for (exp, act) in std::iter::zip(&expected_tokens, &tokens) {
            assert_eq!(exp, act);
        }

        let expected_symbols = vec![
            Symbol::Add(1),
            Symbol::Add(-1),
            Symbol::Shift(-1),
            Symbol::Shift(1),
            Symbol::Loop(vec![Symbol::Add(1), Symbol::Add(1)]),
            Symbol::Input,
            Symbol::Output,
        ];

        let symbols = parse(&tokens).expect("Parses correctly.");

        assert_eq!(symbols.len(), expected_symbols.len());
        for (exp, act) in std::iter::zip(expected_symbols, symbols) {
            assert_eq!(exp, act);
        }
    }

    #[test]
    fn imbalanced_opening_bracket() {
        let source = "[";
        let symbols = parse(&lex(source));
        assert_eq!(symbols, Err(ParseError::ImbalancedBrackets));
    }

    #[test]
    fn imbalanced_closing_bracket() {
        let source = "]";
        let symbols = parse(&lex(source));
        assert_eq!(symbols, Err(ParseError::ImbalancedBrackets));
    }
}

fn main() {
    println!("Hello, world!");
}
