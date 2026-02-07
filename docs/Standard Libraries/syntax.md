# Syntax

The syntax library `(import (syntax))` provides syntax and functions for 
defining Scheme keywords, more commonly known as 
[_macros_](https://en.wikipedia.org/wiki/Macro_(computer_science)). In Scheme, 
macros are [_hygienic_](https://en.wikipedia.org/wiki/Hygienic_macro), although
there are ways to break hygiene with procedural macros.

## `define-syntax` _syntax_

```scheme
(define-syntax ⟨keyword⟩ ⟨expression⟩)
```

The `define-syntax` form binds a _keyword_ to the result of the evaluated 
_expression_ which must be a _transformer_. A _transformer_ is a procedure
that takes one argument, a syntax object, and returns another syntax object.

Whenever the _keyword_ is encountered, the expander will pass the syntax into 
the transformer and replace it with the output.

## `syntax` _syntax_

```scheme
(syntax ⟨expr⟩)
#'⟨expr⟩
```

## `let-syntax` and `letrec-syntax` _syntax_

### `let-syntax`

```scheme
(let-syntax ((⟨keyword⟩ ⟨expr⟩) ...) ⟨body⟩)
```

### `letrec-syntax`

```scheme
(letrec-syntax ((⟨keyword⟩ ⟨expr⟩) ...) ⟨body⟩)
```

## `syntax-rules` _syntax_

Hygeinic syntax transformers

## `syntax-case` _syntax_

Low-level syntax transformers

## `free-identifier=?` _procedure_

```scheme
(free-identifier=? ⟨identifier1⟩ ⟨identifier2⟩)
```

