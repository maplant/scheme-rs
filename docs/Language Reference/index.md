# Language Reference

There are three core parts to the scheme language: 

- The syntax for describing syntax datums, more commonly known as 
  [s-expressions](https://en.wikipedia.org/wiki/S-expression). This describes 
  how code and data are written as text.
- The syntax for defining libraries. In their essence, libraries define a set
  of symbols to "import" (bring into their scope from other libraries) and 
  "export" (provide upon being imported by other libraries). 
- A set of core libraries that provide syntax and functions allowing one to 
  write programs.
  
This system is incredibly malleable; importing symbols, including the ones most
commonly associated with scheme's semantics, such as `define`, `set!` or even
`lambda` is entirely optional. New languages can be defined as libraries, 
allowing for an unprecedented amount of flexibility. 

We will go over each of these core parts briefly. For a more in depth overview 
of the precise semantics of the language, please refer to the 
[r6rs specification](https://www.r6rs.org/), of which this document is a highly
abridged and paraphrased facsimile. 

## Syntax

### Lexical syntax

The lexical syntax of Scheme determines how characters are split into sequence
of tokens, also known as "lexemes". Lexemes are separated by delimiters, which 
include any amount of whitespace along with the characters `(`, `)`, `[`, `]`, 
`"`, `;`, and `#`. The following lists all of the possible classifications of
lexemes:

#### Booleans

The values of True and False are represented by the lexemes `#t` (or `#T`) and 
`#f` (or `#F`) respectively.

#### Identifiers

Identifiers are strings of characters that must start with any character that 
is not a delimiter and not a number. Practically, that means that an identifier
starts with a letter or special character like `$` or `<`. 

The following are all examples of identifiers:

```
<=
a->b
hello-world
goodbye_world
foo/bar
$$$12343$$$
```

Scheme is much more permissive with what characters can be included in 
identifiers than other languages. Thus underscores are typically discouraged
for naming multi-word variable names, instead using hyphens in what is 
typically called 
[kebab case](https://en.wikipedia.org/wiki/Letter_case#Kebab_case).

```
dont_do_this
do-this-instead
```

#### Numbers

Scheme provides the ability to specify a much wider collection of numerical
literals than other programming languages:

```scheme
1234       ; Typical base-ten digit numeral
-53        ; Negative number
+54        ; Positive number
3.1415926  ; Floating point numbers
#b0101010  ; Binary number
#o0237777  ; Octal number
#xdeadbeef ; Hexadecimal number
7/22       ; Rational number
+5.2+7.3i  ; Complex number
+inf.0     ; Positive infinity
-inf.0     ; Negative infinity
+nan.0     ; Not A Number (NaN)
```

#### Characters

Unicode code points (also known as characters) are represented by the prefix
`#\` followed by the character literal, a special character name, or `x` 
followed by a hexadecimal scalar value. Valid special character names include
`nul`, `alarm`, `backspace`, `tab`, `linefeed`, `newline`, `vtab`, `page`,
`return`, `esc`, `space`, and `delete`.

```scheme
#\a         ; Lower case letter a
#\A         ; Upper case letter A
#\(         ; Left parenthesis
#\linefeed  ; U+000A
#\newline   ; Same as #\linefeed but considered depricated
#\λ         ; U+03BB
```

#### Strings

Strings are formatted pretty similarly to other programming languages; they are
a string of characters surrounded by two double quotes (`"`). `\` is used to 
escape `"` and various other escape sequences. A `\` at the end of line can be 
used to escape whitespace between the current line and the next:

```scheme
"hello, world!"
"hello?\nworld!"
"A
bc"               ; This is U+0041, U+000A, U+0062 and U+0063
"A\
bc"               ; This is U+0041, U+0062 and U+0063
```

#### Comments

Comments in Scheme come in three different flavors:

##### Line comments

Single line comments are indicated with the semicolon (`;`) character. The 
comment extends to the end of the line:

```scheme
(+ 1 2 3 ; ) 4 5 6
   4) ; => returns 10
```

##### Block comments

Block comments are delimited by pairs of `#|` and `|#` characters. They can be
nested:

```scheme
#|
    This is a block comment
    #|
        This is an inner block comment
    |#
|#
```

##### Datum comments

The `#;` prefix can be used to comment out whole [datums](#datum-syntax). Here is an 
example that shows every type of comment in action:

```scheme
#|
    The FACT procedure computes the factorial
    of a non-negative integer.
|#
(define fact
    (lambda (n)
        ;; base case
        (if (= n 0)
            #;(= n 1)
            1       ; identity of *
            (* n (fact (- n 1))))))
```
    
### Datum syntax

The datum syntax is a description of how Scheme 
[s-expressions](https://en.wikipedia.org/wiki/S-expression) are represented in 
terms of sequence of lexemes. There are three components to the datum syntax:

- Pairs and lists, enclosed by `(` `)` or `[` `]`
- Vectors, enclosed by `#(` `)`
- Bytevectors, enclosed by `#vu8(` `)`
- Non-standard datums, such as hashtable literals

We will go through each of these one-by-one:

#### Pairs and lists 

The most fundamental datums are pairs and lists, the most basic of which is 
`()` which represents the empty list. `()` only has one value and it is itself.

Pairs can be represented via dot notation, i.e. `(⟨datum1⟩ . ⟨datum2⟩)`. The 
first field is called the "car" (more commonly known as the "head") and the 
second is called the "cdr" (more commonly known as the "tail). 

Lists are constructed from multiple pairs recursively in their cdr fields. For
example, 

```
(a b c d e)
```
is equivalent to 
```
(a . (b . (c . (d . (e . ())))))
```

A list is considered "proper" if the final cdr is the empty list. For example,
```
(a b c d . e)
```
is equivalent to
```
(a . (b . (c . (d . e))))
```
and is not considered proper since the final cdr is the symbol `e` rather than
the empty list `()`.

#### Vectors 

Vectors, also known as arrays, are represented with the notation 
`#(⟨datum⟩ ...)`. For example, a vector of length four that contains the number
zero at index zero, a pair of two numbers and index one, and a string at index
three could be represented as follows:

```scheme
#(0 (1 . 2) "Alice")
``` 

#### Bytevectors 

Similar to vectors, bytevectors are arrays, but they can only contain values 
that can fit in a single 
[unsigned 8-bit byte](https://en.wikipedia.org/wiki/Byte). For example, a 
bytevector of length three containing the values 1, 2, and 255 could be 
represented as follows:

```scheme
#vu8(1 2 255)
```

#### Non-standard datums

scheme-rs supports some non-standard but common datum literals:

##### Hashtables 

The prefix `#hash` can be used to construct hashtable literals that use the
`equal?` equivalence predicate and `hash-equal` hash function. Hashtable 
literals have the form
```
#hash((key . value) ...)
``` 

For example, the following
is a map of countries in North America to their capitals:

```scheme
#hash(
    ("United States" . "Washington D.C.")
    ("Mexico" . "Ciudad de México")
    ("Canada" . "Ottawa")
)
```
 
## Library Syntax

Libraries provide a syntax for importing and exporting symbols. 
