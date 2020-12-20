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

-------- Maybe split this into sections
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

# Probability:

The probability module of the calculator supports evaluting for the probability
mass or density, evalutating the cumulative density, and sampling from various 
standard probability distributions, as well as some standard probabilistic
functions.

# Standard Probability Functions
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
 
# Probability Distribution Functions:
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

# Statistics:
The statistics module includes various statistical functions on vectors.

# Basic Statistics:
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

# More Functions:
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




