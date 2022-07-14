# Titanic BF Compiler

I had made preparations to modify my BF interpreter for this, but
having recognised that I was going to have to make major changes to
the architecture anyway, I decided to do it as a separate project.
It's possible the Rust experience I've gained will make it even better
than that was!

This is more for me to learn about writing a compiler than for
flexing, so it's going to be titanic!

## Architecture Overview

1. Lexer: Source → `Token` “stream”.
2. Parser: `Token` stream → Abstract Syntax Tree. Makes sure brackets
   are balanced.
3. Optimisation: Consecutive operator reduction, buffered I/O. This
   may occur within several of the other stages.
4. Code Generation: (NASM) x86 assembly, possibly ARM in the future.
