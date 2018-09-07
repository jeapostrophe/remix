#lang scribble/manual

@title{remix - a revised version of Racket}
@author{Jay McCarthy}

This document describes @(hash-lang) @racketmodname[remix], a version
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
in the process, such as @racket[assoc] and @racket[assq]. }

@item{Systemize required function argument order and names to a
subject-verb-object style.}

@item{Update syntactic forms with more extensible positions and
form-specific expanders.}

@item{Add richer surface notations, specifically different parenthesis
shapes, a C-style dot notation, and enabling the @"@"-reader by
default.}

@item{Change library mores to better support backwards and forwards
compatibility.}

]
