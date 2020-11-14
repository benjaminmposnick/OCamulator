# Installing OCamulator
- To install our software, simply run `make build`.
- To delete any build files, run `make clean`.
- To run the test suite, run `make test`. All 263 tests should pass (as of the
end of the Beta sprint).
- Most importantly, to use the command line application that runs our calculator,
first install `ledit` using `brew install ledit` or `apt-get install ledit` and
then run `make start`.
  - If you do not have `brew` or `apt-get`, try whichever package manager you
  normally use for your distribution (e.g. `apt`, `yum`, `pip`).
  - Note that we have found that we are unable to install `ledit` on UGCLinux
  without first acquiring administrative privilege (i.e. even with `sudo`, the
  installation is blocked).
  - If none of these methods work, the system can still be run without `ledit` 
  by using `make start_no_keys`. In this mode, the calculator's mathematical
  functionality is no different, it is just that the user cannot use the arrow
  keys to fix a mistake earlier in an expression or "arrow up" to a recently
  used command.

# Using the Command Line Application
Try entering mathematical expressions into our calculator!
- Type `scope` to see the current variables in scope during the calculator
  session.
- A variable can be stored with the following abstract syntax:
  `<name> := <expr>`, where `name` is an arbitrary length alphabetical identifier
  with no spaces, no symbols, and no numbers, and `expr` is a mathematical
  expression (e.g. arithmetic expression, vector, matrix, etc).
- If you want to store the result of the last command executed, input
`<name> := ans`.
- You should see both the parsed input and the final result (if
that functionality is not yet fully functional, you may see an error).
- When you are done, type `quit` to exit the application.

This is the list of functionality that is currently integrated into the command
line application as of the end of the Beta sprint. Note that there is additional
functionality that is written or almost completed that has not yet been integrated
with the command line application.
- Addition, subtraction, multiplication, division, exponentiation, modulo for
arbitrarily complex arithmetic expressions, using parentheses to force evaluation
order
  - E.g. Try inputing `(3^2 + 4^2)^(1/2) % 2`, which should yield 1
- Efficient row reduction of an arbitrarily-sized matrix (no smaller than 2x2)
to reduced row echelon form
  - E.g. Try inputting `rref: [1,2,3;4,5,6;7,8,9]`
- Transposing a matrix, row vector (delimited by commas), column vector (delimited
by semicolons)
  - E.g. Try inputing `transpose: [1;2;3;4;5]`
- Probability density functions (PDFs) and cumulative density functions (CDFs)
  - E.g. Try inputing `Uniform pdf 0.0 1.0 1.0` which should yield 1.0
  - E.g. Try inputting `Uniform cdf 0.0 1.0 0.5` which should yield 0.5

More functionality is coming in the next sprint -- stay tuned! For now: peace,
love, and 3110.