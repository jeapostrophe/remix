#lang scribble/manual

@title{Racket 2}
@author{Jay McCarthy}

This document attempts to describe Racket 2.

@table-of-contents[]

@section{Introduction}

My dream for Racket is based on two things:

@itemlist[

@item{Racket will be a full spectrum programming language, meaning
that it supports programming at all levels. We traditionally
understand this under the rubric of "from scripts to programs" that
inspired Typed Racket, but we could go much further in our ability to
verify programs, such as by producing Pure Racket, Total Racket, and
VeriRacket. Along another dimension of the spectrum, we could offer
more and more control of the low-level details of programming, such as
with safe manual memory management and data representation control.}

@item{Racket will be a language with an extensible compiler.}

]

I believe that the first goal is a philosophical goal while the second
is a technical goal. I think the two goals work together because we
believe that the extensible compiler is the tool with which we will
spread across the full spectrum.

I do not believe we can achive these goals if we are not flexible in
our understanding of what is Racket and what isn't, nor if we are so
attached to historical coincidence that we cannot accept changes.

Nevertheless, Racket is more than just an academic pursuit of
perfection, but we desire it to be usable for practical programming
today and each day along the way. We often understand this constraint
as our commitment to backwards compatibility.

We are at a moment in Racket's history where many of us feel a desire
for a major break in compatibility to increase our flexibility in the
future. This document attempts to structure this moment so we can make
the most of it.

@subsection{Structure}

I believe that for the process of creating Racket 2 to be productive,
we need to decide when we will stop changing it and settle in to
a "new normal". Similarly, it would be desirable to have a criteria to
decide whether feature X or Y is more in line with the vision of
Racket 2.

At this moment, I do not have an idea of the principle that will guide
Racket 2. I hope as I write this document, I will discover it. But
lacking a certain principle, I propose that we allocate ourselves a
fixed amount of changes to the Racket 2 design document. For instance,
we could say that we will revise the design six times, meaning that
there will be six "beta" releases of Racket 2 before we solidify it
and live with it for a while. I don't think it matters so much how
many we give ourselves or what constitutes a release, but I think a
process like this could work well.

One principle may be, "Make it easy to do the right thing." Another
is "Transformers everywhere."

@subsection{The Scope of Racket 2}

I think a useful way to think about the scope of Racket 2 is to
imagine that we are writing a new Reference manual. We must decide
what things are described by the Reference (i.e. which things are
core) and how they are described. This includes the syntactic forms,
core data-structures, modularity mechanisms, etc. It leaves out the
way Racket 2 influences outside libraries, etc.

In particular, I think that the Reference is not an appropriate place
to write about implementation details and how the virtual machine
works. In other words, in our discussion of Racket 2, I hope that we
can focus on the end-programmer experience and assume that the virtual
machine is what it is.

Based on this scope, the rest of this document is written as if it is
the new Reference, although with only the changed and interesting
parts left in.

@section{Language Model}
@subsection{The Reader}

R2's syntax is a combination of Honu and the @"@"-reader.

@emph{Why Honu?} Racket is defined as having an extensible
compiler. In R1, this was only possible by using S-expressions to
build on existing macro technology. Now that we have discovered Honu,
we must push it to develop a new kind of extensible compiler
front-end.

@emph{Why @"@"?} R1 has taught us the importance of embedding prose in
programs. By including the @"@"-reader at all times, we make it easier
to embed prose.

@section{Syntactic Forms}
@subsection{Modules}

@verbatim|{
#lang r2

module duck : racket/base { }

module* duck { } // language implicitly #f if left out

module+ duck { }
}|

Build-in convenient macros for defining test and main sub-modules.

@verbatim|{
#lang r2

test { }
main { }

}|

XXX labeled effects

@subsection{Importing and Exporting}

R2's importing and exporting will decrease the "nominalness" of Racket
modules by requiring interfaces to be imported and exported rather
than names alone. The existing set of @racket[require] forms will
become interface transformers and combinators to change the interface
before using it.

@verbatim|{
#lang r2/base
open racket/list, racket/dict; // Open gets the interfaces, but not
                               // the values or macros, thus it is like
                               // the current require
require list1^ from racket/list, // Require actually gets the bindings
        dict1^ from racket/dict,
        contract1^ from racket/contract; // Perhaps we implicitly open
                                         // any module on the RHS of from
                                         // and just use open for 'strange' 
                                         // interface sources

interface database1^ {
 db : any -> boolean?, // : is an interface transformer like contract-out
 open : list? -> db?
};

provide database1^
}|

In the same way that today the @racketmodname[racket/base] language
is "more strict" (because it requires you to name all normal imports),
@racketmodname[r2/base] will be "more strict" as well. In particular
however, @racketmodname[r2] will allow you to use the @racket[*]
interface which is otherwise disallowed.

@verbatim|{
#lang r2
require * from racket/list,
        * from racket/dict;

}|

@emph{Why?} This serves two purposes. First, it builds a package-like
notion of versioning into every library, because you can extend the
module but not add the new features to every old interface. A single
module like @racketmodname[racket/list] could provide interfaces
@racket[list1^] and @racket[list2^]. Second, it provides more
information about what a module needs from its requires so that
modules are more analyzable. In particular, I hope that we can use
this information to make a "module consuming meta-language" to go with
our current "module producing meta-language" that will override the
sources of interfaces with different choices (i.e. a program needs
@racket[list1^] and specifies the normal list functions as the
default, but the context of the application changes it to be provided
by a version of the list modules that optimizes @racket[snoc] and
@racket[append].)

@subsection{Procedure Expressions, Local Binding, Local Definition, Definitions, and Assignments}

One of the earliest wishes for R2 has been "match everywhere". I think
this is a noble goal, but a more noble goal in my mind
is "transformers everywhere". In particular, transformers, like match
expanders, in binding positions. For instance, if in R1 the formals
part of a @racket[λ] had expanders, then @racket[(λ (cons x y) y)]
would be valid not because we have "match everywhere", but because
@racket[cons] would be a "binding expander" in addition to a function.

@verbatim|{
#lang r2

x = 5;     // = is define

var y = 5; // var is a binding transformer that boxes the RHS and allows mutation
y := 6;    // the var binding transformer on y set up y as a := transformer to allow this to be set-box!
z = y + 4; // this is not an error because the var transformer made y the same as (unbox y)
yb = &y;   // the var binding transformer on y set up y as a & transformer to allow this to get the box itself

// This means that R2 is more like ML in that only explicitly labeled
// references are mutable and nothing else is.

a , b = 10 `quo/rem` 3; // , is a binding transformer for values
c , d = a + 4, b - 3;   // but also a normal macro for returning values

best :: rest = range(1,10) // :: is a binding transformer for cons

a = 5;
a = 4; // since = is define, this is an error

a = 10;
a *= a + 1; // *= is like define* from racket/package so a is 11 from this point on

a = { b = 4; b + 5; } // {} sets up a new scope so it is easy to have "internal definition everywhere"

// All these transformers could be nested

var x, y::z = 1, [3 4 5]; // x = box 1, y = 3, z = [4 5]

// All these transformers make sense inside of function formals too

fun f ( a::[b], var c ) { a + b }
f = λ ( a::[b], var c ) { a * c + b }
f( [ 1 2 ], 3 )

// But some transformers only make sense inside of function formals

fun f ( a, [b = 60]) { a + b } // %brackets is a function binding transformer for optional arguments

f(1) should be 61
f(1,2) should be 3

fun f ( a , b = b ) { a + b } // keywords forms [left of = is the kw]
f(1, b = 3)
f(b = 3, 1)

}|

We drop @racket[case-lambda].

@subsection{Conditionals, Dispatch, Guarded Evaluation}

@verbatim|{
#lang r2

if ( true ) { .... } else { .... }
when ( true ) { .... }
unless ( false ) { .... }
cond { question: expr; question: {expr; expr}; else: expr }
switch expr { pat: expr; pat: {expr; expr}; else: expr }

}|

@racket[else] is a binding. If you don't have it, the default behavior
is an error message with source location information.

@subsection{Iteration and Comprehensions}

XXX This is a good place to do new things.

@section{Datatypes}

A key principle for R2's data structures is that we define common
interfaces and generic operations rather than many operations for each
kind of structures. The structures would be at least Sequences, Maps,
Sets. We prefer immutable versions of everything with the mutable
operations cordonned into a different interface.

XXX More discussion needed here

@section{Structures}

We extend the idea of transformers everywhere to be inside of
structure definitions and has something that are produced by structure
definitions.

@verbatim|{
#lang r2

struct posn {
 x;
 y;
};

fun f ( posn p ) { return p.x + p.y; }
// This works because 'posn' is a binding transformer that defines p.x and p.y

struct bullet {
 posn src;
 posn dest;
};
fun f ( bullet b ) { return b.src.x; }
// This works because 'posn' is a struct field transformer that works
// with the binding transfomer.

fun f ( x ) { return posn( x = x, y = 7 ); }
// Constructors are always by kw

posn f ( x ) { return posn( x = x, y = 7 ); }
f(5).x;;
// This works because 'posn' is a function definition transformer that
// works with applications to communicate to . that .x is in the result
// of f()

struct locator extends posn {
 z;
};
// Because constructors are always by kw, the parents and child
// structs cannot conflict.

}|

XXX use var for a field is mutable, otherwise immutable
XXX mutable fields define := transformers
XXX every struct has built-in updater that preserves sub-struct-ness
XXX structs define a single binding
XXX optional fields
XXX struct in interface is used for restricting access to pieces of structures

@section{Classes and Objects}

I'm tempted to say that Generics in R1 is a failure of the class
system to provide what we want. If this is the case, then I think a
simplified class system that builds on top of structures is the right
thing to do in R2. I think Go actually does something sensible here.

@verbatim|{
#lang r2

struct posn {
 x;
 y;
};

// XXX Do we mandate interfaces? Are they the same kind of interfaces
// as for modules?
posn implements {
 public:
  meth distanceTo ( x ) {
   return this.x - x;
  }

  meth updateX ( x ) {
   return this.{x = x};
  }
}

}|
