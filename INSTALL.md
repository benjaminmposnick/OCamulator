# Installing OCamulator
- To install our software, simply run `make build`.
- To delete any build files, run `make clean`.
- To run the test suite, run `make test`. All 591 tests should pass.
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
  keys to fix a mistake earlier in an expression or "arrow up/down" to a
  recently used command.

# Using the Command Line Application
Try entering mathematical expressions into our calculator!
- Type `#state` to see the current variables in scope during the calculator
  session. `ans` is a variable that is always in scope and contains the result
  of the most recent computation. `ans` can be used directly in expressions,
  just like in Matlab.
- A variable can be stored with the following abstract syntax:
  `<name> := <expr>`, where `name` is an arbitrary length alphabetical identifier
  with no spaces, no symbols, and no numbers, and `expr` is a mathematical
  expression (e.g. arithmetic expression, vector, matrix, etc).
- If you want to store the result of the last command executed, input
  `<name> := ans`.
- To call a function on a given input, use the following abstract syntax:
  `$<func> <expr>`, where `func` is a keyword for some supported function (see
  the list of supported functions below) and `expr` is a mathematical
  expression. For two argument functions, the syntax is almost the same, except
  there is a pair of expressions separated by a tilde: `$<func> (<expr> ~ <expr>)`
- If a value list is returned by a function, the elements of that list can be
  accessed using the abstract syntax `#<nat> <value_list>`, where `nat` is a
  natural number (not including zero).
- When you are done, type `#quit` to exit the application.

# Supported Functionality
## Arithmetic
- Addition, subtraction, multiplication, division, exponentiation, modulo for
  arbitrarily complex arithmetic expressions, using parentheses to force evaluation
  order
  - E.g. `((1 - 2) / (3 % 4) + 5 * 6)^(1/2)`, which should yield 1

## Linear algebra
- Row reduction of an arbitrarily-sized matrix (no smaller than 2x2) to reduced
  row echelon form
  - E.g. `$rref [1,2,3;4,5,6;7,8,9]`
- Transposing a matrix, row vector (delimited by commas), column vector (delimited
  by semicolons)
  - E.g. `$transpose [1;2;3]`
  - E.g. `$transpose [1,2,3]`
  - E.g. `$transpose [1,2,3;4,5,6;7,8,9]`
- PLU decomposition of a square matrix
  - E.g. `$plu [1,2,3;4,5,6;7,8,9]`
  - This always returns a value list. One can use the `#` syntax to get an
    element from this list.
  - E.g. If `tmp := $plu [1,2,3;4,5,6;7,8,9]` then `p := #1 tmp`, `l := #2 tmp`,
    `u := #3 tmp`, which could then be used to verify the factorization by 
    running `[1,2,3;4,5,6;7,8,9] = ($transpose p) * l * u`.
- Determinant of a square matrix
  - E.g. `$det [1,2,3;4,5,6;7,8,9]`
- Inverse of a square matrix
  - E.g. `$inv [1,2;3,4]`
  - If the matrix is not singular, then an error is raised.
- Solve a linear system of equations of the form Ax=b, where A is square
  - E.g. `[1,2;3,4] \ [5;6]`
  - If the matrix A is not singular, then an error is raised.
- Pivot columns of a matrix
  - E.g. `$pivots [1,2,3;4,5,6;7,8,9]`

## Probability
The probability module of the calculator supports evaluting for the probability
mass or density, evalutating the cumulative density, and sampling from various 
standard probability distributions, as well as some standard probabilistic
functions.

### Standard Probability Functions
  $[command_name] [args]

[command_name]
 - "choose" or "comb": Number of combinations (order does not matter)
 - "perm": Number of Permutations (order does matter)
 - "fac" : factorial

[args]
 - "choose" or "comb": n (int); k (int)
 - "perm": n (int); k (int)
 - "fac": n (int)

Examples:
  - 8 factorial 
    $fac 8

  - 10 choose 2
    $choose 10 2
 
### Probability Distribution Functions:
  [distribution_tag] [op_tag] [distribution_params] [value_param]

distribution_tag - tag of one of the below distributions
op_tag - tag of one of the below operations
distribution_params - arguments that define the distribution
value_param - value to evaluate the operation on

[distribution_tag] 
Tags:
- Bernoulli: "bernoulli" or "bern"
- Geometric: "geometric" or "geo"
- Binomial: "binomial" or "binom"
- Exponential: "exponential" or "exp"
- Poisson: "poisson" or "pois"
- Uniform: "uniform" or "unif"
- Normal: "normal" or "norm"

[distribution_params]
The supported distributions also with their identifing parameters in order:
- Bernoulli: p - the probability of a success [0,1]
- Geometric: p - the probability of a success [0,1]
- Binomial: n - the number of draws [0,+inf] (int); p - the probability of a success [0,1]
- Exponential: l - the rate parameter [0,+inf]
- Poisson: l - the rate parameter [0,+inf]
- Uniform: a - smallest value [-inf,+inf]; b - largest value [-inf,+inf]
- Normal: mu - the mean [-inf,+inf]; s - the standard deviation [0,+inf]

[op_tag]
Tags:
- Probability Mass/Denisty Function : "pdf"
- Cumulative Density Function : "cdf"
- Sampling : "smpl"

[value_param]
Each also takes a parameter for evaluating the pdf or cdf:
- Bernoulli: Outcome of 1 draw; 0 or 1 (int)
- Geometric: Number of draws till a success; [0,+inf] (int)
- Binomial: Number of successful draws; [0,n] (int)
- Exponential: Amount of time till a success; [0,+inf]
- Poisson: Number of successes in 1 time unit; [0,+ing] (int)
- Uniform:  Value at point on in Uniform area; [-inf,+inf]
- Normal: Value at point on Normal Curve; [-inf,+inf]

For sampling the final argument must alway be a non-zero int. A value n greater 
than zero will return n random varaibles drawn from the distribution as a 
row vector. A value of 0 or omitting the last argument will return one 
random variable as a float

Examples pdf and cdf:
- Bernoulli (p = 0.5) pdf at 1

  bern pdf 0.5 1
  or 
  bernoulli pdf 0.5 1

  - You can use either the full name of the distribution or the shortend one

- Binomial (n = 5, p = 0.95, k = 2) cdf

  binomial cdf 5 0.95 2

  - Because the Binomial distribution takes requires 2 values to be defined 
  both 5 and 0.95 make up the distribution params

Examples sampling:
- Sample 1 rv from Uniform (a = 5, b = 10) as a float

  unif smpl 5 10
  or 
  unif smpl 5 10 0

  - To get a float random variable pass 0 or omit the last argument

  unif smpl 5 10 1

  - This will give a vector of length 1 instead

- Sample 10 rv from Exp (l = 5)
  exp smpl 5 10

## Statistics
The statistics module includes various statistical functions on vectors.

### Basic Statistics
Basic statistics such as mean and median require no other arguments and have the form:

$[statistic] [data]

[statistic]
supported basic statistics:
- "mean" : average of the data
- "median" : middle value in the data if length of data is odd. average of the 2
middle values if length of data is even
- "mode" : most common value in data. if tie then which ever has the first occurence
- "max" : largest value in the data
- "min" : smallest value in the data
- "range" : largest - smallest
- "sort_asc" : sorts data in ascending order
- "sort_desc" : sorts data in descending order
- "sum" : sum of all values in data
- "product" : product of all values in data
- "rms" : root mean squared of the data
- "variance" : the sample variance of the data
- "std" : the sample standard deviation of the data
- "unique" : the set representation of the data

All functions that return a vector return a vector of length [] when passed one.
and statistical functions will return 0. when given an empty vector

Example:
- Mean
  $mean [1;2;3]
  $mean x

### More Functions
Some functions require added arguments as well as the data. The arguments must be pass as a tuple.

$[statistic] ([arg] ~ [data])

[statistic] [arg]
supported functions and their arguments:
- "count" : the number of occurences of the [arg] in data
- "quantile" : the value of [arg]th quantile of data rounding up. 
at least [arg]% of data will lie below the resulting value

Example:
- The number of occurences of "1." in the following vector 

$count (1. ~ [1.;1.;2.])

Linear regression:
The line of best fit for a set of points can be computed with and gives a tuple
pair with the first value being the slope and the second being the y-intercept

$[linreg_tag] ([data_x] ~ [data_y])

[linreg_tag]
- "linreg" or "bestfit"

[data_x] [data_y]
- the ordered values of the coordinates
- data_x is a vector of the x coords
- data_y is a vector of the y coords

Example:
- Linear regression on (1,2) and (2,4)

  $linreg ([1;2] ~ [2,4])


## Solving Equations
Basic linear equations containing one variable can be solved
`$solve [equation]`
The user is then prompted to enter the variable to solve for.

Example: `$solve x + 5 = 6`
will result in the user being prompted with:
```What variable would you like to solve for?```
If the user enters `x`, then the output will be `1`.

The user can also enter an equation with two distinct variables (*NOTE* only
once instance of the variable being solved for is permitted)
`$solve x + 5 = 6 + y`
The user can enter either `x` or `y`. In the case that the answer contains a
variable, the output is the AST representation of the answer. For example,
if the user chose to solve this equation for x, the answer would be
`Binop(Sub, Binop(Add, Int 6, Var "y"), Int 5)`

Equations must:
- Contain only one instance of the variable being solved for
- Include one, and only one, equal sign
- Be linear (no powers)
- Include only the operators `+`, `-`, `*`, and `/`

### Trigonometry Commands
The user can enter trigonometry commands preceded by the `$` symbol.
The output is the result of the OCaml built in trigonometry functions.

The input is entered in the form:
`$[function] [argument]`

Supported trigonometry commands are:
- `$sin`, the sine function
- `$tan`, the tangent function
- `$cos`, the cosine function
- `$arcsin`, the inverse sine function
- `$arccos`, the inverse cosine function
- `$arctan`, the inverse tangent function

The user must enter a numeric value (`pi` is allowed as well) as the argument.

*The arguments for the trig functions are in radians.*

The user can simply write `$sin 0` or `$cos pi`. 
If a more complex input is needed, parantheses are necessary.
Example: `$sin (2 * pi)`

### Least Common Multiple
The `$lcm` command solves for the Least Common Multiple of two numeric inputs. 
The two required arguments are entered with the syntax `(x ~ y)`.

Example: `$lcm (2 ~ 5)` is `10`. 

Though input can be floats or ints, the function solves the input as integers.
Floats with numbers after the decimal are effectively truncated.

### Greatest Common Divisor
The `$gcd` command solves for the Greatest Common Divisor of two numeric inputs. 
The two required arguments are entered with the syntax `(x ~ y)`.

Example: `$gcd (6 ~ 8)` is `2`. 

Though input can be floats or ints, the function solves the input as integers.
Floats with numbers after the decimal are effectively truncated.