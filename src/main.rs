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

enum ParseError {
    ImbalancedBrackets,
}

type Ast = Vec<Symbol>;

/// Parse the token stream into an AST. Perform no optimisations.
fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    use std::collections::LinkedList;

    let mut asts: LinkedList<Ast> = LinkedList::new();
    asts.push_front(Vec::new());

    // TODO: Consoom tokens.

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
    fn lexer_symbols() {
        let source = "+-  <>[Hi there!],.";

        use Token::*;
        let expected = vec![
            Inc,
            Dec,
            ShiftLeft,
            ShiftRight,
            BracketLeft,
            BracketRight,
            Input,
            Output,
        ];

        let tokens = lex(source);

        assert_eq!(tokens.len(), expected.len());
        for (exp, act) in std::iter::zip(expected, tokens) {
            assert_eq!(exp, act);
        }
    }
}

fn main() {
    println!("Hello, world!");
}
