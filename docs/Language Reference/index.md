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
[r6rs specification](https://www.r6rs.org/).

## Syntax
