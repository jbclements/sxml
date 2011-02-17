#lang scribble/doc

@(require scribble/manual)

@title{@bold{SXML}: The S-Expression representation of XML terms}

@;{@author[(author+email "John Clements" "clements@racket-lang.org")]}

@(require (planet cce/scheme:7:2/require-provide))

@(require (for-label racket
                     (this-package-in main)))

@defmodule[(planet clements/sxml2)]{This planet library contains Oleg Kiselyov's SXML 
 libraries in a Racket-friendly format. It is a direct descendant of 
 Dmitry Lizorkin's PLaneT package.  It's different from that package in that
 
 @itemize[#:style 'ordered
 @item{It contains some documentation (here it is!),}
 @item{it contains some tests,}
 @item{it has only one require point (ssax & sxml are both included), and}
 @item{it doesn't depend on schemeunit:3, so it compiles quickly.}]
 
 This documentation is scraped together from various sources; the bulk of it
 (currently) is pulled from in-source comments.
 
 I'm hoping that this will become a Racket community project, with various
 people contributing documentation and test cases and maybe even bug fixes.
 
 To that end, this code currently lives in a github repository which should
 be fairly easy to find.  Patches gratefully accepted.
 
 --John Clements, 2011-02-17}

@section{SAX Parsing (input)}

@defproc[(ssax:xml->sxml [port port?] [namespace-prefix-assig (listof (cons/c symbol? string?))]) sxml?]{
 Reads a single xml element from the given @racket[port], and returns the 
 corresponding sxml representation.  The @racket[namespace-prefix-assig]
 association list provides shortened forms to be used in place of 
 namespaces.
 
 So, for instance,
 
 @racketblock[
(ssax:xml->sxml 
 (open-input-string 
  "<zippy><pippy pigtails=\"2\">ab</pippy>cd</zippy>")
 '())]
 
 Evaluates to:
 
 @racketblock['(*TOP* (zippy (pippy (|@| (pigtails "2")) "ab") "cd"))]
 }

@section{Serialization (output)}


@defproc[(srl:sxml->xml [sxml-obj sxml?] [dest port-or-filename? null]) (or/c string? unspecified)]{
  Serializes the @racket[sxml-obj] into XML, with indentation to facilitate
 readability by a human.

@itemize[
 @item{@racket[sxml-obj] - an SXML object (a node or a nodeset) to be serialized}
 
 @item{@racket[dest] - an output port or an output file name, an optional
  argument}]

 If @racket[dest] is not supplied, the functions return a string that
 contains the serialized representation of the @racket[sxml-obj].
 If @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port and return an
 unspecified result.

 If @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of @racket[sxml-obj] is written to
 that filename and an unspecified result is returned. If a file with the given
 name already exists, the effect is unspecified.

}


@defproc[(srl:sxml->xml-noindent [sxml-obj sxml?] 
                                 [dest port-or-filename? null])
         (or/c string? unspecified)]{
 Serializes the @racket[sxml-obj] into XML, without indentation.}

                                    
@defproc[(srl:sxml->html [sxml-obj sxml?] [dest port-or-filename? null])
         (or/c string? unspecified)]{
Serializes the @racket[sxml-obj] into HTML, with indentation to facilitate
readability by a human.

@itemize[
 @item{@racket[sxml-obj] - an SXML object (a node or a nodeset) to be serialized}
 @item{@racket[dest] - an output port or an output file name, an optional
  argument}]

  If @racket[dest] is not supplied, the functions return a string that
 contains the serialized representation of the @racket[sxml-obj].
 If @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port and return an
 unspecified result.
 
 If @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of @racket[sxml-obj] is written to
 that filename and an unspecified result is returned. If a file with the given
 name already exists, the effect is unspecified.
}

@defproc[(srl:sxml->html-noindent  [sxml-obj sxml?] [dest port-or-filename? null]) 
         (or/c string? unspecified)]{

 Serializes the @racket[sxml-obj] into HTML, without indentation.

}
                                    
@include-section["extracted-sperber.scrbl"]

@include-section["all-exported.scrbl"]


@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!