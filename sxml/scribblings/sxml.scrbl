#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label racket/base
                     sxml))

@title{SXML: S-Expression Representation of XML}
@;{@author[(author+email "John Clements" "clements@racket-lang.org")]}

@defmodule[sxml]{

 This library contains Oleg Kiselyov's SXML 
 libraries in a Racket-friendly format. It is a direct descendant of 
 Dmitry Lizorkin's PLaneT package.  It's different from that package in that
 
 @itemize[#:style 'ordered
 @item{It contains some documentation (here it is!),}
 @item{it contains some tests that run in Racket,}
 @item{it raises racket exceptions rather than printing to stderr and raising "-1",}
 @item{it has only one require point (ssax & sxml are both included), and}
 @item{it doesn't depend on schemeunit:3, so it compiles quickly.}]
 
 This documentation is scraped together from various sources; the bulk of it
 (currently) is pulled from in-source comments.
 
 I'm hoping that this will become a Racket community project, with various
 people contributing documentation and test cases and maybe even bug fixes.
 
 To that end, this code currently lives in a github repository which should
 be fairly easy to find.  Patches gratefully accepted.

 It's perhaps also worth noting that this project currently has less than 23%
 test case coverage. Bleh.
 
 For Heaven's sake, report lots of bugs!

 --John Clements, 2011-02-17
}

@include-section["sxml-rep.scrbl"]
@include-section["sax-parsing.scrbl"]
@include-section["serialization.scrbl"]
@include-section["sxpath.scrbl"]
@include-section["sxslt.scrbl"]

@include-section["extracted-sperber.scrbl"]
@include-section["all-exported.scrbl"]

@(close-eval the-eval)
