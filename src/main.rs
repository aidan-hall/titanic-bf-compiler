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

/// Parse the token stream into an AST, with (trivial) optimisations.
fn parse(tokens: Vec<Token>) -> Vec<Symbol> {
    Vec::new()
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
