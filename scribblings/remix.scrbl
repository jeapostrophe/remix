#lang scribble/manual
@(require (for-label (prefix-in rkt: racket/base)
                     remix))

@(define rmx @racketmodname[remix])

@title{remix - a revised version of Racket}
@author{Jay McCarthy}

This document describes @(hash-lang) @rmx, a version
of Racket with a revised syntax and standard library.

@table-of-contents[]

@section{Philosophy}

remix intends to only change the standard library of Racket, rather
than providing a radically new model of programs, semantics, or data
values. It should be possible and easy for remix programs to use
Racket libraries and Racket programs to use remix libraries, although
it is acceptable for some light, local adaptation to be required.

remix attempts to make the following changes to the standard library:
@itemize[

@item{Remove outdated, deprecated, or unadvised functions.}

@item{Expand keyword argument adoption, removing extraneous functions
in the process, such as @racket[rkt:assoc] and @racket[rkt:assq]. }

@item{Systemize required function argument order and names to a
subject-verb-object style.}

@item{Update syntactic forms with more extensible positions and
form-specific expanders.}

@item{Add richer surface notations, specifically different parenthesis
shapes, a C-style dot notation, and enabling the @secref["reader"
#:doc '(lib "scribblings/scribble/scribble.scrbl")] by default.}

@item{Change library mores to better support backwards and forwards
compatibility.}

]

@section{Basic Remix}

@defmodule[remix #:lang]

The main @rmx language has a few differences relative to the Racket reader notation.

@itemize[

@item{@racket[rkt:read-square-bracket-as-paren] and
@racket[rkt:read-curly-brace-as-paren] are @racket[#f], while
@racket[rkt:read-square-bracket-with-tag] and
@racket[rkt:read-curly-brace-with-tag] are @racket[#t]. This means that
literal syntax like @litchar{[a b]} is read as @racket[(#%brackets a
b)] and @litchar{{a b}} is read as @racket[(#%braces a b)].}

@item{@racket[rkt:read-accept-dot] and
@racket[rkt:read-accept-infix-dot] are @racket[#f], so it is not
possible to use dotted pairs in the input at all. Most significantly,
this means that rest arguments are defined differently in
@racket[lambda].}

@item{@racket[rkt:read-cdot] is @racket[#t]. This means that literal
syntax like @litchar{a.b} is read as @racket[(#%dot a b)].}

@item{@secref["reader" #:doc '(lib
"scribblings/scribble/scribble.scrbl")] is enabled, with the default
context being a @litchar{<datum>} piece.}

]

Furthermore, the @rmx language provides few bindings. Specifically, it
provides only three bindings.

@defform[(#%module-begin f ...)]{Expands to
@racket[rkt:#%module-begin], but also cooperates with
@racket[require*].}

@defform[(require m ...)]{Like @racket[rkt:require], except that if an
imported module @racket[m] provides @racket[#%required], then expands
to @racket[(rkt:begin (rkt:require m) (#%required m))].

This allows, for example, required modules to inject code into modules
that require them.}

@defform[(require* m)]{Legal only inside
@racket[#%module-begin]. @racket[(#%module-begin before ... (require*
m) after ...)] expands to @racket[(#%module-begin before ... (rkt:require
m) (#%require*d after ...))] where @racket[#%require*d] is provided by
@racket[m].

This allows gives the required module control over the expansion of
the remainder of the requiring module.}
