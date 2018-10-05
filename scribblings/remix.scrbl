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

@section{Core Remix}

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

@section[#:tag "stdlibmores"]{Standard Library Mores}

A @rmx module @racket[_m] should contain submodules for each version
of its interface. For example, if @racket[remix/list] had two
versions, then it would have a @racket[(submod remix/list 0)] and
@racket[(submod remix/list 1)] submodule, in addition to providing
the most recent interface as @racket[remix/list]. In most cases,
these submodules will simply provide slightly different symbols, but
occasionally may implement different behavior. This allows clients to
bind to specific past versions to maintain compatibility (forwards or
backwards.)

In most cases, @rmx modules implement this pattern with
@racket[provide/versions]@margin-note{TODO Actually implement this
macro.}. The @rmx documentation notates which version identifiers
appear in, but does not explicitly document the
submodules.@margin-note{TODO Is that a good idea? Maybe just make
@racket[provide/versions] generate a documentation blob to import.}

@section{Basic Remix}

@defmodule[remix/base]

Exports the bindings defined elsewhere in this manual, unless
otherwise noted. In accordance with the @secref["stdlibmores"], also
provides submodules such that @racket[(submod remix/base _n)] provides
all modules at a version closest to but not greater than
@racket[_n]. (For example, @racket[(submod remix/base 3)] would
provide @racket[(submod remix/list 2)] if @racket[remix/list] did not
have a version @racket[3].) In some cases, other modules are listed as
starting at a version other than @racket[1] to signify that they
should not appear in earlier versions of
@racket[remix/base].@margin-note{TODO Make a macro to make this easy
to do.}

@racketmodname[remix/base]

@section{Syntactic Forms}

@defmodule[remix/stx]

The core syntax of @rmx is provided in this module.

XXX stx gen:def-transformer
XXX stx def-transformer?
XXX stx def-transform
XXX def

XXX stx gen:def*-transformer
XXX stx def*-transformer?
XXX stx def*-transform
XXX def*
XXX remix-block
XXX #%brackets

XXX gen:binary-operator
XXX binary-operator?
XXX binary-operator-precedence
XXX #%braces

XXX gen:dot-transformer
XXX dot-transformer?
XXX dot-transform?
XXX dot-ref
XXX #%dot

XXX gen:app-dot-transformer
XXX app-dot-transformer?
XXX app-dot-transform
XXX #%app

XXX #%rest
XXX lambda

XXX $
XXX remix-cut

XXX impossible!
XXX cond

XXX lang
XXX lang*

XXX val
XXX stx
XXX mac
