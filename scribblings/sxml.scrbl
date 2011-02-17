#lang scribble/doc

@(require scribble/manual)

@title{@bold{SXML}: The S-Expression representation of XML terms}

@;{@author[(author+email "John Clements" "clements@racket-lang.org")]}

@(require (planet cce/scheme:7:2/require-provide))

@(require (for-label racket
            #;(this-package-in main)))

@defmodule[(planet clements/sxml2)]{This planet library contains Oleg Kiselyov's SXML 
 libraries in a Racket-friendly format.}

@section{The Two Most Important Functions}

@defproc[(ssax:xml->sxml [port port?] [namespace-prefix-assig (listof (cons symbol? string?))]) sxml?]{
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


 High-level functions for popular serialization use-cases.
 These functions use only a subset of serializer functionality. However, this
 subset seems sufficient for most practical purposes.


@defproc[(srl:sxml->xml [sxml sxml?] [dest port-or-filename? null]) (or/c string? unspecified)]{
  Serializes the `sxml-obj' into XML, with indentation to facilitate
 readability by a human.

 sxml-obj - an SXML object (a node or a nodeset) to be serialized
 port-or-filename - an output port or an output file name, an optional
  argument
 If `port-or-filename' is not supplied, the functions return a string that
 contains the serialized representation of the `sxml-obj'.
 If `port-or-filename' is supplied and is a port, the functions write the
 serialized representation of `sxml-obj' to this port and return an
 unspecified result.
 If `port-or-filename' is supplied and is a string, this string is treated as
 an output filename, the serialized representation of `sxml-obj' is written to
 that filename and an unspecified result is returned. If a file with the given
 name already exists, the effect is unspecified.

}



; procedure srl:sxml->xml :: SXML-OBJ [PORT-OR-FILENAME] -> STRING|unspecified
;
;
; procedure srl:sxml->xml-noindent :: SXML-OBJ [PORT-OR-FILENAME] ->
;                                      -> STRING|unspecified
;
; Serializes the `sxml-obj' into XML, without indentation.
(define (srl:sxml->xml-noindent sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #f 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")
      (srl:display-sxml sxml-obj (car port-or-filename) '() #f 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")))

; procedure srl:sxml->html :: SXML-OBJ [PORT-OR-FILENAME] -> STRING|unspecified
;
; Serializes the `sxml-obj' into HTML, with indentation to facilitate
; readability by a human.
;
; sxml-obj - an SXML object (a node or a nodeset) to be serialized
; port-or-filename - an output port or an output file name, an optional
;  argument
; If `port-or-filename' is not supplied, the functions return a string that
; contains the serialized representation of the `sxml-obj'.
; If `port-or-filename' is supplied and is a port, the functions write the
; serialized representation of `sxml-obj' to this port and return an
; unspecified result.
; If `port-or-filename' is supplied and is a string, this string is treated as
; an output filename, the serialized representation of `sxml-obj' is written to
; that filename and an unspecified result is returned. If a file with the given
; name already exists, the effect is unspecified.
(define (srl:sxml->html sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #t 'html '() #t 'omit "4.0")
      (srl:display-sxml sxml-obj (car port-or-filename)
                        '() #t 'html '() #t 'omit "4.0")))

; procedure srl:sxml->html-noindent :: SXML-OBJ [PORT-OR-FILENAME] ->
;                                       -> STRING|unspecified
;
; Serializes the `sxml-obj' into HTML, without indentation.
(define (srl:sxml->html-noindent sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #f 'html '() #t 'omit "4.0")
      (srl:display-sxml sxml-obj (car port-or-filename)
                        '() #f 'html '() #t 'omit "4.0")))




@include["all-exported.scrbl"]


@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!