# Lambda Calculus Interpreter

This project implements a lambda calculus interpreter in Haskell. Lambda calculus is a formal system in mathematical logic and computer science for expressing computation based on function abstraction and application using variable binding and substitution. The interpreter provides a command-line interface for evaluating lambda calculus expressions.

## Files

- *main.hs*: The main.hs file contains the main program, which sets up a default context and interacts with the user through the command line. It uses modules from Lambda, Parser, Expr, and Examples.

- *Expr.hs*: The Expr.hs file defines the data structures for lambda calculus expressions (Expr) and code sequences (Code). It also provides functions for equality checking, reduction, and manipulation of expressions.

- *Lambda.hs*: The Lambda.hs file contains functions for evaluating lambda calculus expressions using both normal and applicative evaluation strategies. It also includes functions for finding free variables and reducing redexes.

- *Parser.hs*: The Parser.hs file defines a simple parser using the Parser monad. It includes parsers for lambda calculus expressions (exprParser) and code sequences (codeParser). The module also provides functions to parse expressions and code from strings.

- *Examples.hs*: The Examples.hs file contains predefined examples of lambda calculus expressions, combinators, boolean values, and numbers. Additionally, it includes code examples (p0, p1, p2, p3) and their expected results (a0, a1, a2, a3).

## Usage

To run the lambda calculus interpreter, use the following command:

```bash
runhaskell main.hs
```

The interpreter will prompt you with a lambda calculus REPL (λ>), where you can enter expressions for evaluation or define new macros. There is a default context already defined for easier usage. Type exit to quit the interpreter.

Here is an example of using the interpreter:

```
λ> true = \x.\y.x
λ> false = \x.\y.y
λ> and = \x.\y.(x y x)
λ> $and $true $false
λx.λy.y
λ> or = \x.\y.(x x y)
λ> $or $true $false
λx.λy.x
λ> exit
```

Feel free to explore and experiment!