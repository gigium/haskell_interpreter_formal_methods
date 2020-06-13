# haskell_interpreter_formal_methods
Implementation of an interpreter for a simple imperative language. (University project)
```
Università degli Studi di Bari “Aldo Moro”
CdL in Informatica Magistrale
Formal Methods for Computer Science
a.a 2018/
```
# CSKN 2.
**Introduction**
In this document we will first have a small theory discussion about the main concept behind the
implementation of our system defining what is a parser and the AST structure, followed by a
description about the eager evaluate strategy and a quick description about functor, applicative and
monad in haskell.
Later we will go much on detail about the grammar of the CSKN language described by BNF
notation and we will describe the architecture of the system explaining the single parts.
Finally we will propose some example to try the system and we will explain more in detail some
non trivial implementation of the system.


**Theory background
Parser**
A parser is a program that takes a string of characters as input, and produces some form of tree that
makes the syntactic structure of the string explicit. For example, given the string 2*3+4, a parser for
arithmetic expressions might produce a tree of the following form, in which the numbers appear at
the leaves of the tree, and the operators appear at the nodes:
The structure of this tree makes explicit that + and * are operators with two arguments, and that *
has higher priority than +. Parsers are an important topic in computing, because most real-life
programs use a parser to preprocess their input.
Making the structure of the input explicit considerably simplifies its further processing. For
example, once a numeric expression has been parsed into a tree structure as in the example above,
evaluating the expression is then straightforward.
**Abstract Syntax Tree (AST)**
An abstract syntax tree (AST), or just syntax tree, is a tree representation of the abstract syntactic
structure of source code written in a programming language. Each node of the tree denotes a
construct occurring in the source code. The syntax is "abstract" in the sense that it does not
represent every detail appearing in the real syntax, but rather just the structural, content-related
details. For instance, grouping parentheses are implicit in the tree structure, and a syntactic
construct like an if-condition-then expression may be denoted by means of a single node with three
branches.
From now on each we refer to AST we intend the Abstract Syntax Tree produced by the parsing
process.
**Eager interpretation**
When we have to evaluate arguments to a given function we have two different and opposite
strategy that we can follow:

- Call-by-value (​ _Eager_ ​).
- Call-by-name (​ _lazy_ ​).


Give a function, e.g ​ _f(t) ,_ ​ the eager strategy suggest to evaluate the argument t (that can be a simple
variable or the result of another function) first and once a result is obtained, for example an integer
n, to then proceed with the evaluation of ​ _f_ ​(n); in the sense that as soon as the argument is read it is
immediately evaluated.
The lazy strategy (as the name suggest) pass directly to the definition of the function, and if the
value of a certain argument t is needed just then the evaluation of the t argument is performed.
The two choices have vastly different effects.
If a certain argument t is frequently use in the definition of the function, a call-by-name strategy
will ri-evaluate each time the argument is needed in the definition, a call-by-value strategy instead
will calculate first all the arguments and then will proceed with the evaluation of the function.
On the other hand, if the evaluation of an argument (which may never be used in the function),
leads to a divergence, its divergence can needlessly cause the divergence of the all function.
In our implementation we have chosen to use an eager approach.
**Haskell: Functor, Applicative and Monads**
Functors
In Haskell, functions of type a -> b transform values of type a into values of type b. For example,
the (+1) :: Int -> Int function increments a number by 1, and the show :: Int -> String turns a number
into its string representation. But what if your value was enclosed in some sort of a container?
That is the idea behind the functor algebra.
The class of types that support such a mapping function are called functors. In Haskell, this concept
is captured by the following class declaration in the standard prelude:
**class** ​ ​ **Functor** ​ f ​ **where
fmap** ​ :: (a -> b) -> f a -> f b
That is, for a parameterised type f to be an instance of the class Functor, it must support a function
fmap of the specified type. The intuition is that fmap takes a function of type a -> b and a structure
of type f a whose elements have type a, and applies the function to each such element to give a
structure of type f b whose elements now have type b. The fact that f must be a parameterised type,
that is, a type that takes another type as a parameter, is determined automatically during type
inference by virtue of the application of f to the types a and b in the specified type for fmap in the
class declaration.
As we would expect, the type of lists can be made into a functor by simply defining fmap to be the
function map. Another example can be the built-in type Maybe a that represents values of type a
that may either fail or succeed:
**data** ​ ​ **Maybe** ​ a = ​ **Nothing** ​ | ​ **Just** ​ a
It is straightforward to make the Maybe type into a functor by defining a function fmap of the


appropriate type, as follows:
**instance** ​ ​ **Functor** ​ ​ **Maybe** ​ ​ **where**
_-- fmap :: (a -> b) -> Maybe a -> Maybe b_
**fmap** ​ _ ​ **Nothing** ​ = ​ **Nothing
fmap** ​ g (​ **Just** ​ x) = ​ **Just** ​ (g x)
That is, mapping a function over a failed value results in the failure being propagated, while for
success we apply the function to the underlying value and retag the result. For example:
> fmap (+1) Nothing
Nothing
> fmap (*2) (Just 3)
Just 6
> fmap not (Just False)
Just True
Note that in addition to providing a function fmap of the specified type, functors are also required to
satisfy two equational laws:

- fmap id = id
- fmap (g. h) = fmap g. fmap h
Where id is the identity and. is the composition of the functions g and h.
The first equation states that fmap preserves the identity function, in the sense that applying fmap to
this function returns the same function as the result. The second equation above states that fmap
also preserves function composition, in the sense that applying fmap to the composition of two
functions gives the same result as applying fmap to the two functions separately and then
composing.
Applicatives
Functors abstract the idea of mapping a function over each element of a structure. Suppose now that
we wish to generalise this idea to allow functions with any number of arguments to be mapped,
rather than being restricted to functions with a single argument.
In fact the Applicative functor does just that:
**class** ​ ​ **Functor** ​ f => ​ **Applicative** ​ f ​ **where
pure** ​ :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
That is, pure converts a value of type a into a structure of type f a, while <*> is a generalised form
of function application for which the argument function, the argument value, and the result value
are all contained in f structures. As with normal function application, the <*> operator is written
between its two arguments and is assumed to associate to the left.


Continuing the example from the Functor section, using the fact that Maybe is a functor and hence
supports fmap, it is straightforward to make this type into an applicative functor:
**instance** ​ ​ **Applicative** ​ ​ **Maybe** ​ ​ **where**
_-- pure :: a -> Maybe a_
**pure** ​ = ​ **Just**
_-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b_
**Nothing** ​ <*> _ = ​ **Nothing**
(​ **Just** ​ g) <*> mx = fmap g mx
That is, the function pure transforms a value into a successful result, while the operator <*> applies
a function that may fail to an argument that may fail to produce a result that may fail. For example:
> pure (+1) <*> Just 1
Just 2
> pure (+) <*> Just 1 <*> Just 2
Just 3
> pure (+) <*> Nothing <*> Just 2
Nothing
In summary, the applicative style for lists supports a form of non-deterministic programming in
which we can apply pure functions to multi-valued arguments without the need to manage the
selection of values or the propagation of failure, as this is taken care of by the applicative
machinery.
In addition to providing the functions pure and <*>, applicative functors are also required to satisfy
four equational laws:

- pure id <*> x = x
- pure (g x) = pure g <*> pure x
- x <*> pure y = pure (\g -> g y) <*> x
- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
The first equation states that pure preserves the identity function, in the sense that applying pure to
this function gives an applicative version of the identity function. The second equation states that
pure also preserves function application, in the sense that it distributes over normal function
application to give applicative application. The third equation states that when an effectful function
is applied to a pure argument, the order in which we evaluate the two components doesn’t matter.
And finally, the fourth equation states that, modulo the types that are involved, the operator <*> is
associative. It is a useful exercise to work out the types for the variables in each of these laws.


Monads
A Monad wraps a value or a computation with a particular context. A monad must define both a
means of wrapping normal values in the context, and a way of combining computations within the
context.
Just like with functors and applicative functors, Haskell represents monads with a type class. It has
two functions:
**class** ​ ​ **Applicative** ​ m => ​ **Monad** ​ m ​ **where
return** ​ :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
**return** ​ = pure
That is, a monad is an applicative type m that supports return and >>= functions of the specified
types. The default definition return = pure means that return is normally just another name for the
applicative function pure, but can be overridden in instances declarations if desired.
Following the Maybe example, the Maybe Monad is defined in the following way:
**instance** ​ ​ **Monad** ​ ​ **Maybe** ​ ​ **where**
_-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b_
**Nothing** ​ >>= _ = ​ **Nothing**
(​ **Just** ​ x) >>= f = f x
As defined in the standard prelude, the bind operator for the Maybe type is defined using pattern
matching rather than case analysis for simplicity.
As an example we define the function half in the following manner:
**half** ​ x = ​ **if** ​ even x
​ **then** ​ ​ **Just** ​ (x `div` ​ 2 ​)
​ **else** ​ ​ **Nothing**
Since Maybe is a Monad we can do the following with the half function:
> Just 20 >>= half >>= half >>= half
Nothing
Haskell also provides us with some syntactical sugar for monads, called do notation. For example
using the IO Monad we can chain functions with the >>= or do notation:
**getLine** ​ >>= readFile >>= putStrLn
**foo** ​ = ​ **do**


filename <- getLine
contents <- readFile filename
putStrLn contents
In a similar manner to functors and applicatives, the two monadic primitives are required to satisfy
some equational laws:

- return x >>= f = f x
- mx >>= return = mx
- (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
The first equation states that if we return a value and then feed this into a monadic function, this
should give the same result as simply applying the function to the value. Dually, the second
equation states that if we feed the result of a monadic computation into the function return, this
should give the same result as simply performing the computation. Together, these two equations
state, modulo the fact that the second argument to >>= involves a binding operation, that return is
the identity for the >>= operator. The third equation concerns the link between >>= and itself, and
expresses (again modulo binding) that >>= is associative.


**CSKN Language
Language description**
The CSKN language is an eager imperative programming language composed by few commonly
used commands.
Specifically :
● **Sequencing** ​: executes statements in sequence;
● **Assignment** ​: instruction that assign a value to a variable that is calculated if it is written by
an expression;
● **Conditional** ​: control structure if-then like;
● **While-loop** ​: iterative control structure while-do like;
CSKN is a language that accept Boolean and Integer values and can evaluate the following type of
arithmetic expressions:
● **Number** ​: Any n ∊ ℤ;
● **Location** ​: content of a variable;
● **Sum, Subtraction, Product, Division** ​: canonical arithmetical operations with division that
returns the quotient of the operation;
and boolean expressions:
● **Boolean** ​: b ∊ {True, False};
● **Location** ​: content of a variable;
● **And, Or, Not** ​: canonical logic operations;
● **<, <=, >, >=, ==, /=** ​: canonical interpretation of these operators, compares two arithmetic
expression and returns a boolean value.


**BNF Grammar**
Our interpreter will accept string belonging to the language define by the following grammar.
We define our grammar using BNF notation​.
**<program>** ​ ::= {<sqnc>}
**<sqnc>** ​ ::= <stm>; <sqnc> ​|​ <stm>;
**<stm>** ​ ::= <command> ​|​ <expr>
**<comm>** ​::= <identifier> = <expr> ​|​ IF [<bexpr>] <program>
| ​WHILE [<bexpr>] <program>
**<expr>** ​::= <bexpr> ​|​ <aexpr>
**<aexpr>** ​::= <aterm> ​|​ <aterm> + <aexpr> ​| ​<aterm> - <aexpr>
<​ **bexpr** ​> ::= <bterm> ​| ​<bterm> AND <bexpr> ​| ​<bterm> OR <bexpr>
|​ <bterm> GREATER <aexpr> | <bterm> GREATER_EQUAL <aexpr>
|​ <bterm> LESS <aexpr> ​| ​<bterm> LESS_EQUAL <aexpr>
|​ <bterm> EQUAL <aexpr> ​| ​<bterm> NOT_EQUAL <aexpr>
**<aterm>** ​ ::= <afactor> ​|​ <afactor> * <aterm> ​| ​<afactor> / <aterm>
**<afactor>** ​ ::= ( <aexpr> ) ​| ​+ <afactor> ​|​ - <afactor> ​|​ <identifier> ​|​ <natural>
<​ **bterm** ​> ::= ( <bexpr> ) ​|​ NOT <bterm> ​|​ <identifier> ​|​ <bool_val> ​|​ <aexpr>


Terminals
**<natural> ::=** ​ 0 ​| ​ 1 ​| ​ 2 ​...
**<bool_val> ::=** ​ TRUE ​|​ FALSE
**<identifier>** ​ ​ **::=** ​ A lower case alphabetic character followed by any alphanumeric string
(with no space in between).
Program, sequence and statement
● The program node <​ **program>** ​is the root of the tree, is objective is simply bound the
program between the two braces.
● The sequence node <​ **sqnc** ​> split each input in statements ( <stm> ) which ends with a
semicolon.
● The statement node <​ **stm** ​> is used to produce a command or an expression.
Commands
The objective of command is to produce the two control structure if and while and give the
possibility to assign a specific expression to an identifier.
More precisely the if control structure is composed by the keyword IF, an expression <expr>
enclose between square brackets and a program <program>; the while structure is similar to the IF
structure but it’s repeat the statements while the condition is true as the canonical definition.
Expressions: Arithmetic and Boolean
Expression <expr> can be an arithmetic expression <aexpr> or a boolean expression <bexpr>:
● arithmetic expressions: aexpr
An additive expression that starts with an <aterm> followed by either plus or minus, followed
by another <aexpr>.
This is a typical recursive definition for parsing expressions of the type <aterm> + <aterm> -
<aterm> ... etc.. Example: x - 5 + y.
A lonely <aterm> is also considered an <aexpr>, e.g 44.
<aterms> are more tightly bound than <aexpr>, corresponding to higher precedence of
multiplicative vs. additive operators.


We'll consider two forms or terms: <afactor> followed by a multiplication or division sign,
followed by another <aterm>. This production corresponds to terms of the form <afactor> *
<afactor> / <afactor> .... Example: 2 * x /2.
An <aterm> could also be a lonely <afactor>. Example: 44.
Finally, the most tightly bound production is that of the <afactor>, which can be one of:
A <natural>, like 147; an <identifier> (variable name), like x11; a unary plus or minus in front
of an <afactor>, like -x or +12; a parenthesized <aexpr>, like (a + b/2).
● boolean expressions: bexpr
A boolean expression that starts with a <bterm> followed by either AND or OR logical
operations, followed by another <bexpr>.
So it is possible the recursive definition for parsing expressions of the type <bterm> AND
<bterm> - <bterm> ... etc.. Example: x AND TRUE OR y.
A lonely <bterm> is also considered an <bexpr>. Example: FALSE.
A <bexpr> can be also composed by a <bterm> follower by the operators GREATER,
GREATER_EQUAL, LESS, LESS_EQUAL, EQUAL, NOT_EQUAL and an <aexpr>.
This makes it possible to define comparisons between arithmetic expressions like 4+
GREATER 6-5.
<bterms> are of the type: <aexpr> for confronting an <aexpr> with another; a <bool_val>, like
TRUE or FALSE; an <identifier> (variable name), like b1; a unary NOT operator in front of a
<bterm>, like NOT x or NOT TRUE; a parenthesized <bexpr>, like (a AND NOT b2).


**Design
Architecture**
The interpreter for the CSKN language has two main modules:
● The Parser, creates the AST structure from the program input string;
● The Evaluator, evaluates expressions, execute commands modifying the memory.
**Parsing**
The parser is the component in charge to compute the input program string and recursively builds
the AST structure that makes the syntactic structure of the string explicit and permits to finally
evaluate the program. We note that for the implementation of the system we choose to use a parser
type monadic strategy. That is we make the parser type into an instance of the functor, applicative
and monad classes, in order that the do notation can then be used to combine parsers in sequence.
In addition we make the parser type into an instance of Alternative to apply one parser to the input
string, and if this fails to then apply another to the same input instead.
After parsing a string for a program written in CSKN, we’ll have as a result an AST that makes
explicit how to evaluate the program.


For example a program {a=3+4;} will result in:
parse program ​"{a=3+4;}"
>[(SeqNode [StatementNode (AssignNode ​"a"​ (SumNode Plus (NumNode 3) (NumNode 4)))],​""​)]
The result can be easily viewed as the tree:
**Evaluation**
For simplicity and for the purposes of this activity it was preferred to use an Eager evaluation
strategy, often used for imperative languages. In other words, the interpreter realized will evaluate
the expressions to be associated with the variables as soon as the relative assignment statement is
scanned.
For the implementation of this part we use a composition of monads, functor and applicative,
similarly to the parser implementation. Further details are discussed after.
Another notable design choice is the implementation of a custom type recognized by the language
called Value. The motivation is to simplify the memory implementation and the evaluation process;
more details are discussed in the implementation part.


**Implementation
Parser**
Basic definitions: Functor, Applicative, Monad and Alternative
In Haskell, a parser can naturally be viewed directly as a function that takes a string and produces a
tree. Hence, given a suitable type Tree of trees, the notion of a parser can be represented as a
function of type String -> Tree.
The Tree data type is a recursive structure defined as follows:
**data** ​ ​ **Tree** ​ = ​ **SeqNode** ​ [​ **Tree** ​]
| ​ **StatementNode** ​ ​ **Tree**
| ​ **AssignNode** ​ ​ **String** ​ ​ **Tree**
​ _--arithmetic_
| ​ **SumNode** ​ ​ **Operator** ​ ​ **Tree** ​ ​ **Tree**
| ​ **ProdNode** ​ ​ **Operator** ​ ​ **Tree** ​ ​ **Tree**
| ​ **UnaryNode** ​ ​ **Operator** ​ ​ **Tree**
​ _--boolean logic_
| ​ **LogicNode** ​ ​ **Operator** ​ ​ **Tree** ​ ​ **Tree**
| ​ **ArithmeticLogicNode** ​ ​ **Operator** ​ ​ **Tree** ​ ​ **Tree**
| ​ **UnaryBoolNode** ​ ​ **Operator** ​ ​ **Tree**
​ _-- While, If_
| ​ **CommandNode** ​ ​ **Operator** ​ ​ **Tree** ​ ​ **Tree**
​ _--terminals_
| ​ **NumNode** ​ ​ **Int** ​ | ​ **BoolNode** ​ ​ **Bool** ​ | ​ **VarNode** ​ ​ **String**
​ **deriving** ​ ​ **Show
data** ​ ​ **Operator** ​ = ​ **Plus** ​ | ​ **Minus** ​ | ​ **Times** ​ | ​ **Div**
| ​ **And** ​ | ​ **Or** ​ | ​ **Not**
| ​ **Greater** ​ | ​ **GreaterEqual** ​ | ​ **Less** ​ | ​ **LessEqual** ​ | ​ **Equal** ​ | ​ **NotEqual**
| ​ **While** ​ | ​ **If**
​ **deriving** ​ (​ **Show** ​, ​ **Eq** ​)
The data structure will be returned by the parsers and so construct the AST in a recursive manner.
It is useful to abstract from the specific type Tree of result values, and make this into a parameter of
the Parser type:
**newtype** ​ ​ **Parser** ​ a = ​ **String** ​ -> [(​ **a** ​,​ **String** ​)]


Parser of this type can then be applied to an input string using a function that simply removes the
dummy constructor.
**parse** ​ :: ​ **Parser** ​ a -> ​ **String** ​ -> [(a, ​ **String** ​)]
**parse** ​ (​ **P** ​ p) inp = p inp
We now make the parser type into an instance of the functor, applicative and monad classes, in
order that the do notation can then be used to combine parsers in sequence.
The first step is to make the Parser type into a functor:
**instance** ​ ​ **Functor** ​ ​ **Parser** ​ ​ **where**
_-- fmap :: (a -> b) -> Parser a -> Parser b_
fmap f p = ​ **P** ​ $ \inp ->
​ **case** ​ parse p inp ​ **of**
[] -> []
[(v, out)] -> [(f v, out)]
That is, fmap applies a function to the result value of a parser if the parser succeeds, and propagates
the failure otherwise (failure is the [] empty list).
The Parser type can then be made into an applicative functor as follows:
**instance** ​ ​ **Applicative** ​ ​ **Parser** ​ ​ **where**
_-- pure :: a -> Parser a_
pure v = ​ **P** ​ $ \inp -> [(v, inp)]
_-- <*> :: Parser (a -> b) -> Parser a -> Parser b_
pf <*> px = ​ **P** ​ $ \inp ->
​ **case** ​ parse pf inp ​ **of**
[] -> []
[(f, out)] -> parse (fmap f px) out
Finally, we make the Parser type into a monad:
**instance** ​ ​ **Monad** ​ ​ **Parser** ​ ​ **where**
_-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b_
p >>= f = ​ **P** ​ $ \inp ->
​ **case** ​ parse p inp ​ **of**
[] -> []
[(v, out)] -> parse (f v) out
That is, the parser p >>= f fails if the application of the parser p to the input string inp fails, and
otherwise applies the function f to the result value v to give another parser f v, which is then applied
to the output string out that was produced by the first parser to give the final result.


Because we made Parser as a monadic type, the do notation can be used to sequence parsers and
process their result values.
Another natural way of combining parsers is to apply one parser to the input string, and if this fails
to then apply another to the same input instead.
We now consider how such a choice operator can be defined for parsers. That is, for an applicative
functor to be an instance of the Alternative class, it must support empty and <|> primitives of the
specified types: empty represents an alternative that has failed, and <|> is an appropriate choice
operator for the type.
**instance** ​ ​ **Alternative** ​ ​ **Parser** ​ ​ **where**
_-- empty :: Parser a_
empty = ​ **P** ​ $ \inp -> []
_-- (<|>) :: Parser a -> Parser a -> Parser a_
p <|> q = ​ **P** ​ $ \inp ->
​ **case** ​ parse p inp ​ **of**
[] -> parse q inp
[(v, out)] -> [(v, out)]
Basic Parsers
Note: primitive parser from this section are based on the ones explained in the book ”Programming
in haskell”
● Item
first parsing primitive is called item, which fails if the input string is empty, and succeeds
with the first character as the result value otherwise:
**item** ​ :: ​ **Parser** ​ ​ **Char
item** ​ = ​ **P** ​ (\inp -> ​ **case** ​ inp ​ **of**
[] -> []
(x:xs) -> [(x,xs)])
The item parser is the basic building block from which all other parsers that consume
characters from the input will ultimately be constructed.


● Sat
The parser sat p for single characters that satisfy the predicate p:
**sat** ​ :: (​ **Char** ​ -> ​ **Bool** ​) -> ​ **Parser** ​ ​ **Char**
sat p = ​ **do** ​ x <- item
​ **if** ​ p x ​ **then** ​ return x ​ **else** ​ empty
Using sat and appropriate predicates from the library Data.Char, we can now define parsers
for single digits (digit), lower-case letters (lower), upper-case letters (upper), arbitrary letters
(letter), alphanumeric characters (alphanum), and specific characters (char).
● String
Using char we can define a parser string xs for the string of characters xs, with the string
itself returned as the result value:
**string** ​ :: ​ **String** ​ -> ​ **Parser** ​ ​ **String
string** ​ [] = return []
**string** ​ (x:xs) = ​ **do** ​ char x
**string** ​ xs
**return** ​ (x:xs)
That is, the empty string can always be parsed, while for a non-empty string we parse the
first character, recursively parse the remaining characters, and return the string as the result
value. Note that string only succeeds if the entire target string is consumed from the input.
● Many and Some
The next two parsers, many p and some p, apply a parser p as many times as possible until it
fails, with the result values from each successful application of p being returned in a list.
The difference between these two repetition primitives is that many permits zero or more
applications of p, whereas some requires at least one successful application.
Suitable default definitions of many and some are already provided in the Alternative class.
● Ident, Nat and Space
Using many and some, we can now define parsers for identifiers (variable names)
comprising a lowercase letter followed by zero or more alphanumeric characters, natural
numbers comprising one or more digits, and spacing comprising zero or more space, tab,
and newline characters, respectively:
**ident** ​ :: ​ **Parser** ​ ​ **String
ident** ​ = ​ **do** ​ x <- lower
​ **xs** ​ <- many alphanum
return (x:xs)


**nat** ​ :: ​ **Parser** ​ ​ **Int
nat** ​ = ​ **do** ​ xs <- some digit
​ **return** ​ (read xs)
**space** ​ :: ​ **Parser** ​ ()
**space** ​ = ​ **do** ​ many (sat isSpace)
​ **return** ​ ()
● Token and Symbol
To handle such spacing, we define a new primitive that ignores any space before and after
applying a parser for a token:
**token** ​ :: ​ **Parser** ​ a -> ​ **Parser** ​ a
**token** ​ p = ​ **do** ​ space v <- p
​ **space**
​ **return** ​ v
Using token, we can now define parsers that ignore spacing around strings:
**symbol** ​ :: ​ **String** ​ -> ​ **Parser** ​ ​ **String
symbol** ​ xs = token (string xs)
● True and False keywords
With symbol is now easy to recognize and accept keywords in the language like TRUE and
FALSE:
**true_keyword** ​ :: ​ **Parser** ​ ​ **String
true_keyword** ​ = symbol ​"TRUE"
**false_keyword** ​ :: ​ **Parser** ​ ​ **String
false_keyword** ​ = symbol ​"FALSE"


Terminals Parsers
● Natural
The parser for natural numbers utilises the already defined nat and token parsers to
recognize natural numbers ignoring space around.
The parser will return the integer value recognized:
**natural** ​ :: ​ **Parser** ​ ​ **Int
natural** ​ = token nat
● Bool Value
The parser for boolean values utilises true_keyword and false_keyword.
This will try to recognize the string TRUE or the string FALSE returning the boolean value
recognized.
**bool_val** ​ :: ​ **Parser** ​ ​ **Bool
bool_val** ​ = ​ **do**
true_keyword
return (​ **True** ​)
<|>
​ **do**
false_keyword
return(​ **False** ​)
● Identifier
The parser for identifiers utilises the already defined ident and token parsers to recognize
variable names ignoring space around:
The parser will return the string recognized:
**identifier** ​ :: ​ **Parser** ​ ​ **String
identifier** ​ = token ident


Expressions:Arithmetic Parsers
It is straightforward to translate this grammar directly into a parser for arithmetic expressions, by
simply rewriting the rules using the parsing primitives we have introduced.
Sequencing in the grammar is translated into the do notation, choice | is translated into the <|>
operator, special symbols such as + and * are handled using the symbol function, and natural
numbers are parsed using the natural primitive.
Like in the grammar, the parsing of arithmetic expressions is divided in three distinct sub-parsers
that help maintain order and precedence between operations: aexpr, aterm and afactor.
All these parsers will return a Tree type node wrapped in the Parser type.
● aexpr
**aexpr** ​ :: ​ **Parser** ​ ​ **Tree
aexpr** ​ = ​ **do**
t <- aterm
​ **do** ​ symbol ​"+"
e <- aexpr
return (​ **SumNode** ​ ​ **Plus** ​ t e)
<|> ​ **do** ​ symbol ​"-"
e <- aexpr
return (​ **SumNode** ​ ​ **Minus** ​ t e)
<|> return t
● aterm
**aterm** ​ :: ​ **Parser** ​ ​ **Tree
aterm** ​ = ​ **do**
f <- afactor
​ **do** ​ symbol ​"*"
t <- aterm
return (​ **ProdNode** ​ ​ **Times** ​ t f)
<|> ​ **do** ​ symbol ​"/"
t <- aterm
return (​ **ProdNode** ​ ​ **Div** ​ t f)
<|> return f
● afactor
**afactor** ​ :: ​ **Parser** ​ ​ **Tree
afactor** ​ =
​ **do** ​ symbol ​"("
e <- aexpr
symbol ​")"
return e
<|>


**do** ​ symbol ​"+"
a <- afactor
return (​ **UnaryNode** ​ ​ **Plus** ​ a)
<|>
​ **do** ​ symbol ​"-"
a <- afactor
return (​ **UnaryNode** ​ ​ **Minus** ​ a)
<|>
​ **do**
id <-identifier
return (​ **VarNode** ​ id)
<|>
​ **do**
n <- natural
return (​ **NumNode** ​ n)
Expressions: Boolean Parsers
For boolean expressions it is also straightforward to translate the grammar into the parser.
Operators like AND, OR, ..., are recognized through the symbol parser and TRUE and FALSE
keywords are recognized with the bool_val parser.
Following the grammar the parsing is divided into two different parsers: bexpr and bterm:
● bexpr
**bexpr** ​ :: ​ **Parser** ​ ​ **Tree
bexpr** ​ = ​ **do**
t <- bterm
​ **do** ​ symbol ​"AND"
e <- bexpr
return (​ **LogicNode** ​ ​ **And** ​ t e)
<|> ​ **do** ​ symbol ​"OR"
e <- bexpr
return (​ **LogicNode** ​ ​ **And** ​ t e)
<|> ​ **do** ​ symbol ​"GREATER"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **Greater** ​ t e)
<|> ​ **do** ​ symbol ​"GREATER_EQUAL"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **GreaterEqual** ​ t e)
<|> ​ **do** ​ symbol ​"LESS"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **Less** ​ t e)
<|> ​ **do** ​ symbol ​"LESS_EQUAL"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **LessEqual** ​ t e)
<|> ​ **do** ​ symbol ​"EQUAL"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **Equal** ​ t e)


<|> ​ **do** ​ symbol ​"NOT_EQUAL"
e <- aexpr
return (​ **ArithmeticLogicNode** ​ ​ **NotEqual** ​ t e)
<|> return t
● bterm
**bterm** ​ :: ​ **Parser** ​ ​ **Tree
bterm** ​ =
​ **do**
b <- aexpr
return b
<|>
​ **do** ​ symbol ​"("
e <- bexpr
symbol ​")"
return e
<|>
​ **do** ​ symbol ​"NOT"
b <- bterm
return (​ **UnaryBoolNode** ​ ​ **Not** ​ b)
<|>
​ **do**
id <-identifier
return (​ **VarNode** ​ id)
<|>
​ **do**
b <- bool_val
return (​ **BoolNode** ​ b)
Expressions
A general expression can be composed by either a boolean or an arithmetic one. This is captured in
the following parser:
● expr
**expr** ​ :: ​ **Parser** ​ ​ **Tree
expr** ​ = ​ **do**
bexpr <|> aexpr


Commands Parsers
For commands it is also easy to translate the grammar into the parser.
Operators like IF and WHILE are recognized through the symbol parser and the same is valid for
the special symbols like square brackets and braces.
The parsing of commands is encapsulated in only one parser: comm
● comm
**comm** ​ :: ​ **Parser** ​ ​ **Tree
comm** ​ = ​ **do**
var <- identifier
​ **do** ​ symbol ​"="
e <- expr
return (​ **AssignNode** ​ var e)
<|>
​ **do** ​ symbol ​"IF"
symbol ​"["
b <- expr
symbol ​"]"
s <- program
return (​ **CommandNode** ​ ​ **If** ​ b s)
<|>
​ **do** ​ symbol ​"WHILE"
symbol ​"["
b <- expr
symbol ​"]"
s <- program
return (​ **CommandNode** ​ ​ **While** ​ b s)


Statements, Sequence and Programs
The high level parsers for statements, sequences and program will respectively parse:
● Statements: ​stm
stm will recognize a statement composed by either a command or an expression:
**stm** ​ :: ​ **Parser** ​ ​ **Tree
stm** ​ = ​ **do**
c <- comm
return (​ **StatementNode** ​ c)
<|>
​ **do**
e <- expr
return (​ **StatementNode** ​ e)
The stm parser will return a StatementNode of type Tree with the command or expression
identified.
● Sequences: ​sqnc
A sequence is composed by a series of statements divided by a semicolon (;).
Upon recognition of a sequence this parser will return a list of nodes of type Tree.
**sqnc** ​ :: ​ **Parser** ​ [​ **Tree** ​]
**sqnc** ​ = ​ **do**
s <- stm
symbol ​";"
sq <- sqnc
return ([s]++sq)
<|>
​ **do**
s <- stm
symbol ​";"
return ([s])
● Program: ​program
A program it’s simply a sequence wrapped in two braces. The parser will return a SeqNode
with the list of statements recognized in the sequence.
**program** ​ :: ​ **Parser** ​ ​ **Tree
program** ​ = ​ **do**
symbol ​"{"
s <- sqnc
symbol ​"}"
return (​ **SeqNode** ​ s)


**Evaluation**
The evaluation process start when the input code has been parsed.
As we have discussed before we choose to adopt a eager evaluation strategy for our system.
Value structure
First we define the Value data type which represents values that can be stored in the store
(memory).
The need for such structure is to store in the same memory structure different value types
recognized by our language namely an Int type for arithmetic operations or Bool type for logic
operations.
**data** ​ **Value** ​ **=**
​ **IntVal** ​ ​ **Int
|** ​ **BoolVal** ​ ​ **Bool
|** ​ **NullVal
deriving(** ​ **Show** ​ **)**
Value is a kind of container for boolean or integer values, for example: IntVal 3 or BoolVal False.
In this case IntVal and BoolVal are a wrapper for Int and Bool values respectively.
NullVal is a special value used to make possible printing expression which are “orphan”, i.e they
doesn't has a variable associated to them, (this aspect is better explained in the usage part of this
document).
Since we have wrapped the int value inside our “container” type Value, it is not possible to perform
simple operations between two values.
We choose to instantiate it as the numeric type classes Num which makes it possible to
add/multiply/subtract values of type IntVal.
**instance** ​ ​ **Num** ​ ​ **Value** ​ ​ **where**
​ **IntVal** ​ a + ​ **IntVal** ​ b = ​ **IntVal** ​(a+b)
​ **IntVal** ​ a * ​ **IntVal** ​ b = ​ **IntVal** ​(a*b)
​ **IntVal** ​ a - ​ **IntVal** ​ b = ​ **IntVal** ​(a-b)
abs (​ **IntVal** ​ a) = ​ **IntVal** ​(abs(a))
signum(​ **IntVal** ​ a) = ​ **IntVal** ​(signum(a))
This makes possible operation between IntVal simple and straightforward as if they were num, e.g.
IntVal 3 + IntVal 4 = IntVal 7.
For the same reason above we instantiate Value as an Eq sub-class, this will allow us to use a series
of functions to compare operands of type Value:
**instance** ​ ​ **Eq** ​ ​ **Value** ​ ​ **where**
​ **IntVal** ​ a == ​ **IntVal** ​ b = a==b


​ **BoolVal** ​ a == ​ **BoolVal** ​ b = a==b
We also map boolean operations to Value types creating custom logic functions:
**m_and** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
m_and** ​ (​ **BoolVal** ​ a) (​ **BoolVal** ​ b) = ​ **BoolVal** ​(a && b)
**m_or** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
m_or** ​ (​ **BoolVal** ​ a) (​ **BoolVal** ​ b) = ​ **BoolVal** ​(a || b)
**m_not** ​ :: ​ **Value** ​ -> ​ **Value
m_not** ​ (​ **BoolVal** ​ b) = ​ **BoolVal** ​(not b)
**greater** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
greater** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a>b)
**greater_eq** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
greater_eq** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a>=b)
**less** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
less** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a<b)
**less_eq** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
less_eq** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a<=b)
**eq** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
eq** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a==b)
**not_eq** ​ :: ​ **Value** ​ -> ​ **Value** ​ -> ​ **Value
not_eq** ​ (​ **IntVal** ​ a) (​ **IntVal** ​ b) = ​ **BoolVal** ​(a/=b)
Store: read and write variables in memory
We define our store type which is a list of tuples: string which represents the name of a variable and
Value that is the the value stored in the corresponding variable name.
**type** ​ ​ **Store** ​ = [(​ **String** ​, ​ **Value** ​)]
For reading from and writing to the store, we introduce effectful functions rd and wr, note that rd
produces a Left-wrapped error message if a variable lookup fails.
**rd** ​ :: ​ **String** ​ -> ​ **Interp** ​ ​ **Value
rd** ​ x = ​ **Interp** ​ $ \r -> ​ **case** ​ lookup x r ​ **of**
​ **Nothing** ​ -> ​ **Left** ​ (​"unbound variable `"​ ++ x ++ ​"'"​)
​ **Just** ​ v -> ​ **Right** ​ (v, r)
**wr** ​ :: ​ **String** ​ -> ​ **Value** ​ -> ​ **Interp** ​ ()
**wr** ​ x v = ​ **Interp** ​ $ \r -> ​ **Right** ​ ((), (x, v) : r)


Interpreter type: Functor, Applicative and Monand
After all the previous definition we have defined the new type Interp and the function associated
with it runInterp. Much like the Parser type and the parse function previously defined, these
definitions permit to apply an interpreter to a given input: a Tree type (that is the parsed program)
and a Store type (defined above, to keep in memory variable-value associations).
It is useful also to make the new Interp type into a Functor, Applicative and Monad instance.
There are two "effects" that are candidates for being captured by a monadic structure: the passing
around and updating of the store, aborting running the program when a run-time error is
encountered.
The first effect is typically captured by a state monad, the second by an error monad.
We used monad transformers to construct a composite monad for our two effects by combining a
basic state monad and a basic error monad.
**newtype** ​ ​ **Interp** ​ a = ​ **Interp** ​ { ​ **runInterp** ​ :: ​ **Store** ​ -> ​ **Either** ​ ​ **String** ​ (​ **a** ​, ​ **Store** ​) }
**instance** ​ ​ **Monad** ​ ​ **Interp** ​ ​ **where**
return x = ​ **Interp** ​ $ \r -> ​ **Right** ​ (x, r)
i >>= k = ​ **Interp** ​ $ \r -> ​ **case** ​ runInterp i r ​ **of**
​ **Left** ​ msg -> ​ **Left** ​ msg
​ **Right** ​ (x, r') -> runInterp (k x) r'
fail msg = ​ **Interp** ​ $ \_ -> ​ **Left** ​ msg


Execution of commands, statements and sequences
The exec​ ​function is the main function of the evaluation process.
Given an AST it executes the commands which modify the working memory that stores the results
of evaluations.
Execution of commands is done in this function, while the evaluation of expressions happens in the
eval​ ​function, called by exec​ ​each time an evaluation of an expression both arithmetic and boolean
is encountered.
The following is the implementation of the discussed exec function.
**exec** ​ :: ​ **Tree** ​ -> ​ **Interp** ​ ()
**exec** ​ (​ **SeqNode** ​ []) =
​ **do** ​ return ()
**exec** ​ (​ **SeqNode** ​ (s : ss)) =
​ **do**
exec s
exec (​ **SeqNode** ​ ss)
**exec** ​(​ **StatementNode** ​ e) = ​ **do** ​ exec e
**exec** ​ (​ **AssignNode** ​ x e) =
​ **do**
v <- eval e
wr x v
**exec** ​ (​ **CommandNode** ​ ​ **While** ​ e s) =
​ **do**
v <- eval e
when (v /= (​ **BoolVal** ​ ​ **False** ​)) (exec (​ **SeqNode** ​ [s,​ **CommandNode** ​ ​ **While** ​ e s]))
**exec** ​ (​ **CommandNode** ​ ​ **If** ​ e s) =
​ **do**
v <- eval e
when (v /= (​ **BoolVal** ​ ​ **False** ​)) (exec s)
**exec** ​(n) =
​ **do**
v <- eval n
wr ​"stdOut"​ v


Evaluation of expressions
The eval​ ​function gives a Val value to arithmetic or boolean expression, it takes an AST and
produce a Value type as result of the evaluation.
An important note is that we choose to wrap int and boolean value inside our custom type Value in
order also to make possible the restitution of just a type in one function and makes this process
easier.
We can divide the code in two main part that has in charge the objective to evaluate arithmetic or
boolean expression.
The implementation of this function is quite straightforward looking of the grammar of the
language.
Before present the snippet of code just few note about the implementation:
When we have to evaluate a division between two number we also check if the denominator of the
operation is different to 0.
It’s important to note that in order to do the basic operation between arithmetic and boolean
expression we use our custom functions which ,as we have discussed before, redefine the operation
between the Value type operants.
**eval** ​ :: ​ **Tree** ​ -> ​ **Interp** ​ ​ **Value
eval** ​ (​ **NumNode** ​ n) = ​ **do** ​ return (​ **IntVal** ​ n)
**eval** ​ (​ **VarNode** ​ x) = ​ **do** ​ rd x
**eval** ​ (​ **SumNode** ​ ​ **Plus** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (v1 + v2)
**eval** ​ (​ **SumNode** ​ ​ **Minus** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (v1 - v2)
**eval** ​ (​ **ProdNode** ​ ​ **Times** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (v1 * v2)
**eval** ​ (​ **ProdNode** ​ ​ **Div** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
​ **if** ​ v2 == (​ **IntVal** ​ ​ 0 ​)
​ **then** ​ fail ​"division by zero"
​ **else** ​ return (m_div v1 v2)
**eval** ​ (​ **UnaryNode** ​ ​ **Plus** ​ e1) =
​ **do**
v1 <- eval e1
return (v1)


**eval** ​ (​ **UnaryNode** ​ ​ **Minus** ​ e1)=
​ **do**
v1 <- eval e1
return (​ **IntVal** ​ ​ 0 ​ - v1)
**eval** ​ (​ **BoolNode** ​ n) = ​ **do** ​ return (​ **BoolVal** ​ n)
**eval** ​ (​ **LogicNode** ​ ​ **And** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (m_and (v1) (v2))
**eval** ​ (​ **LogicNode** ​ ​ **Or** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (m_or (v1) (v2))
**eval** ​ (​ **UnaryBoolNode** ​ ​ **Not** ​ e1) =
​ **do**
v1 <- eval e1
return (m_not (v1))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **Greater** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (greater (v1) (v2))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **GreaterEqual** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (greater_eq (v1) (v2))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **Less** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (less (v1) (v2))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **LessEqual** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (less_eq (v1) (v2))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **Equal** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2
return (eq (v1) (v2))
**eval** ​ (​ **ArithmeticLogicNode** ​ ​ **NotEqual** ​ e1 e2) =
​ **do**
v1 <- eval e1
v2 <- eval e2


return (not_eq (v1) (v2))


**Usage and examples
Command Line Interpreter**
To execute the command line interpreter it is required to run the ​ **command_line_interpreter** ​batch
file in the given folder.
This will load the modules in the haskell script and run the ​ **main** ​ function.
After opening the ​ **command_line_interpreter** ​batch script a cmd window will appear and it will be
possible use the interpreter.
Now it is possible to submit arithmetic and boolean expressions and commands following the
defined BNF grammar.
Some examples can be:

- 3+5
- TRUE AND FALSE
- 4 LESS 5
- x=6
- x
- count=0
- WHILE[x GREATER 0]{x=x-1; count=count+1;}
- x
- count


The following image reports the example above:
Note that the interpreter will print ​ **NullVal** ​whenever a command is executed, while when an
expression is evaluated it just prints the value.
That is because the command line interpreter prints the values stored in the ​ **stdOut** ​variable and
only expressions not associated with a variable are stored in this special variable while commands
assign expression values to a variable name.


**Run programs**
To execute programs written by the user it is possible to call the ​ **run** ​function after opening the
**launcher** ​batch file.
To correctly execute the ​ **run** ​ function, it should be called in the following way:
**run “<program>” <store>** ​ ​where:

- **“<program>”** ​ ​is a string containing the program we want to execute.
    To execute correctly must follow the grammar of the CSKN language.
- **<store>** ​ is a list of tuples of the type (identifier, value) that we want to pass as parameters
    to the program.
-
An example call of the ​ **run** ​function, defining for reference the factorial program, can be:
run ​"{a=3; c=a+b;}"​ [(​"b"​, IntVal 5)]
Note that executing just the run function will yield an error because the run function doesn’t return
an IO() value but the modified store.
To read a value from the store after executing a program it is possible to chain the function
**read_variable_value “<variable_name>”** ​to the ​ **run** ​function with the ​ **$** ​operator.
The function ​ **read_variable_value** ​will find the variable name in the store and return the value
associated with that name or yield an error for an unbound variable. For example:
read_variable_value ​"c"​ $ run ​"{a=3; c=a+b;}"​ [(​"b"​, IntVal 5)]
> IntVal 8
It is also possible to print the store stack after the execution of a program calling the function
**debug “<program>” <store>** ​ which similar to ​ **run** ​executes the program that will chance the store,
but with the difference that the function will output the store after the execution. For example:
debug ​"{a=3; c=a+b;}"​ [(​"b"​, IntVal 5)]
> [(​"c"​,IntVal 8),(​"a"​,IntVal 3),(​"b"​,IntVal 5)]


**Sample programs**
To execute sample programs it is required to run the ​ **launch** ​batch file in the given folder.
This will load the modules in the haskell script and so makes it possible to recall functions (sample
programs) defineded in it.
After opening the launcher script, a cmd window will appear and it will be possible to recall the
sample programs.
For example in the following image the ​ **fibonacci** ​function is called on the input ​ **10** ​which outputs
**IntVal 55** ​.


● Factorial
Code for factorial in CSKN language:
{
exit=1;
n=num;
result=num;
WHILE[n EQUAL_NOT exit]
{
n=n-1;
result=result*n;
};
}
Callable function in haskell code which executes the factorial function written in CSKN:
_-- factorial "5"_
**factorial** ​ :: ​ **Int** ​ -> ​ **IO** ​()
**factorial** ​ number =
​ **let** ​ ​ **Just** ​ o = lookup ​"result"
$ run ​"{exit=1; n=num; result=num; WHILE[n NOT_EQUAL exit]{n=n-1;
result=result*n;};}"​ [(​"num"​, ​ **IntVal** ​ number)]
**in** ​ print o
To execute this function, after launching the interpreter, the user needs to write in the
command line: factorial <number>;
For example factorial 5 will yield the value 120.
● Fibonacci
Code for fibonacci in CSKN language:
{
result=0;
n=num;
w=0;
y=1;
WHILE[n EQUAL_NOT w]
{
z=result+y;
result=y;
y=z;
n=n-1;
};
}
Callable function in haskell code which executes the fibonacci function written in CSKN:


_-- fibonacci "5"_
**fibonacci** ​ :: ​ **Int** ​ -> ​ **IO** ​()
**fibonacci** ​ number =
​ **let** ​ ​ **Just** ​ o = lookup ​"result"
$ run ​"{result=0; n=num; w=0; y=1; WHILE[n NOT_EQUAL w]{z=result+y;
result=y; y=z; n=n-1;};}"​ [(​"num"​, ​ **IntVal** ​ number)]
**in** ​ print o
To execute this function, after launching the interpreter, the user needs to write in the
command line: fibonacci <number>;
For example fibonacci 4 will yield the value 3.
● Power
Code for power in CSKN language:
{
result=1;
count=0;
n=num;
ex=exp;
WHILE[count LESS ex]
{
count=count+1;
result=result*n;
};
}
Callable function in haskell code which executes the power function written in CSKN:
_-- power "2" "4"_
**power** ​ :: ​ **Int** ​ -> ​ **Int** ​ -> ​ **IO** ​()
**power** ​ number exp =
​ **let** ​ ​ **Just** ​ o = lookup ​"result"​ $
run ​"{result=1; count=0; n=num; ex=exp; WHILE[count LESS
ex]{count=count+1;result=result*n;};}"
[(​"num"​, ​ **IntVal** ​ number), (​"exp"​, ​ **IntVal** ​ exp)]
**in** ​ print o
To execute this function, after launching the interpreter, the user needs to write in the
command line: power <base> <exp>;
For example power 2 3 will yield the value 8.


