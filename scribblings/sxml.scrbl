#lang scribble/doc

@(require scribble/manual
          planet/scribble)

@title{@bold{SXML}: The S-Expression representation of XML terms}

@;{@author[(author+email "John Clements" "clements@racket-lang.org")]}

@;@(require (planet cce/scheme:7:2/require-provide))

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

@section{SXML}

SXML is a representation of XML elements using s-expressions. Specifically, an XML
element containing some content is represented as an s-expression whose first
element is a symbol containing the tag name, and whose remaining elements are
the SXML representation of the original content.

To be more specific, the XML element

@commandline{<abc>def<ghi />jkl</abc>}

corresponds to the SXML value

@racketblock['(abc "def" (ghi) "jkl")]

XML attributes are represented using an optional second element of the s-expression
whose "tag" is "@"@"".

So, the xml element

@commandline{<customer specialness="gazonga">Barry White</customer>}

corresponds to the XML element

@racketblock['(customer (|@| (specialness "gazonga")) "Barry White")]

That's the easy part.  Things get more icky when you start talking about documents 
and namespaces.



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


@defproc[(srl:sxml->xml-noindent [sxml-obj sxml?] [dest port-or-filename? null])
         (or/c string? unspecified) ]{
 Serializes the @racket[sxml-obj] into XML, without indentation.
}

                                    
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
      
@section{Search (SXPATH)}

@defproc[(sxpath [path abbr-sxpath?] [ns-binding ns-binding? '()]) procedure?]{
 Given a path, produces a procedure that accepts an sxml document and returns 
 a list of matches. Note that the @racket[*TOP*] node of the document is required.
                            
@verbatim{
AbbrPath is a list. It is translated to the full SXPath according
to the following rewriting rules
(sxpath '()) -> (node-join)
(sxpath '(path-component ...)) ->
	(node-join (sxpath1 path-component) (sxpath '(...)))
(sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
(sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
(sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
(sxpath1 '(*or* ...))  -> (select-kids (ntype-names??
                                         (cdr '(*or* ...))))
(sxpath1 '(*not* ...)) -> (select-kids (sxml:complement 
                                        (ntype-names??
                                         (cdr '(*not* ...)))))
(sxpath1 '(ns-id:* x)) -> (select-kids 
                                     (ntype-namespace-id?? x))
(sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
(sxpath1 ?string)     -> (txpath ?string)
(sxpath1 procedure)   -> procedure
(sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
(sxpath1 '(path reducer ...)) ->
	(node-reduce (sxpath path) (sxpathr reducer) ...)
(sxpathr number)      -> (node-pos number)
(sxpathr path-filter) -> (filter (sxpath path-filter))
}

Examples:

All cells of an html table:

@racketblock[
(define table
  `(*TOP*
    (table 
     (tr (td "a") (td "b"))
     (tr (td "c") (td "d")))))

((sxpath '(table tr td)) table)]

... produces:

@racketblock['((td "a") (td "b") (td "c") (td "d"))]

All cells anywhere in a document:

@racketblock[
(define table
  `(*TOP*
    (div
     (p (table 
         (tr (td "a") (td "b"))
         (tr (td "c") (td "d"))))
     (table
      (tr (td "e"))))))

((sxpath '(// td)) table)]

... produces:

@racketblock['((td "a") (td "b") (td "c") (td "d") (td "e"))]

One result may be nested in another one:

@racketblock[
(define doc
  `(*TOP*
    (div
     (p (div "3")
        (div (div "4"))))))

((sxpath '(// div)) doc)
]

... produces:

@racketblock[
'((div (p (div "3") (div (div "4")))) (div "3") (div (div "4")) (div "4"))]


There's also a string-based syntax, @racket[txpath]. As shown in the grammar above,
@racket[sxpath] assumes that any strings in the path are expressed using the 
@racket[txpath] syntax.  

So, for instance, the prior example could be rewritten using a string:

@racketblock[
(define doc
  `(*TOP*
    (div
     (p (div "3")
        (div (div "4"))))))

((sxpath "//div") doc)
]

... produces:

@racketblock[
'((div (p (div "3") (div (div "4")))) (div "3") (div (div "4")) (div "4"))]

More generally, lists in the s-expression syntax correspond to string concatenation
in the txpath syntax.

So, to find all italics that appear at top level within a paragraph:

@racketblock[(define doc
  `(*TOP*
    (div
     (p (i "3")
        (froogy (i "4"))))))
                            
((sxpath "//p/i") doc)
]

... produces: 

@racketblock['((i "3"))]



Handling of namespaces in @racket[sxpath] is a bit surprising.  In particular,
it appears to me that sxpath's model is that namespaces must appear fully expanded
in the matched source.

For instance, this call:

@racketblock[((sxpath "//ns:p" `((ns . "http://example.com")))
  '(*TOP* (html (|http://example.com:body| 
                 (|http://example.com:p| "first para") 
                 (|http://example.com:p| 
                  "second para containing" 
                  (|http://example.com:p| "third para") "inside it")))))]

... produces:

@racketblock[
 '((|http://example.com:p| "first para") 
                (|http://example.com:p| "second para containing" 
                                        (|http://example.com:p| "third para") "inside it")
                (|http://example.com:p| "third para"))]

But the corresponding example where the source document contains a namespace shortcut does 
not match in the same way.  That is, 

@racketblock[((sxpath "//ns:p" `((ns . "http://example.com")))
  '(*TOP* (|@| (*NAMESPACES* (ns "http://example.com")))
          (html (ns:body (ns:p "first para") 
                         (ns:p "second para containing" 
                               (ns:p "third para") "inside it")))))]

...produces the empty list. Instead, you must pretend that the shortcut is actually the namespace, so
that

@racketblock[((sxpath "//ns:p" `((ns . "ns")))
  '(*TOP* (|@| (*NAMESPACES* (ns "http://example.com")))
          (html (ns:body (ns:p "first para") 
                         (ns:p "second para containing" 
                               (ns:p "third para") "inside it")))))]

... produces:

@racketblock[
 '((ns:p "first para") 
   (ns:p "second para containing" 
         (ns:p "third para") "inside it")
   (ns:p "third para"))]

Ah well.
}
                                    
@section{Transformation (SXSLT)}


@defproc[(pre-post-order [tree sxml?] [bindings (listof binding?)]) sxml?]{

Pre-Post-order traversal of a tree and creation of a new tree.


@verbatim{
<binding> ::= (<trigger-symbol> *preorder* . <handler>) |
              (<trigger-symbol> *macro* . <handler>) |
	      (<trigger-symbol> <new-bindings> . <handler>) |
	      (<trigger-symbol> . <handler>)
<trigger-symbol> ::= XMLname | *text* | *default*
<handler> :: <trigger-symbol> x [<tree>] -> <new-tree>}

The pre-post-order function visits the nodes and nodelists
pre-post-order (depth-first).  For each <Node> of the form (name
<Node> ...) it looks up an association with the given 'name' among
its @racket[bindings]. If it fails, @racket[pre-post-order] tries to locate a
*default* binding. It's an error if the latter attempt fails as
well.  Having found a binding, the pre-post-order function first
checks to see if the binding is of the form

@racketblock[(<trigger-symbol> *preorder* . <handler>)]

If it is, the handler is 'applied' to the current node. Otherwise,
the pre-post-order function first calls itself recursively for each
child of the current node, with <new-bindings> prepended to the
<bindings> in effect. The result of these calls is passed to the
<handler> (along with the head of the current <Node>). To be more
precise, the handler is _applied_ to the head of the current node
and its processed children. The result of the handler, which should
also be a <tree>, replaces the current <Node>. If the current <Node>
is a text string or other atom, a special binding with a symbol
*text* is looked up.

A binding can also be of a form

@racketblock[(<trigger-symbol> *macro* . <handler>)]

This is equivalent to *preorder* described above. However, the result
is re-processed again, with the current stylesheet.


A tiny example:

@racketblock[

(require (planet clements/sxml2))

(define sample-doc
  `(*TOP* 
    (html (title "the title")
          (body (p "paragraph 1")
                (p "paragraph 2")))))

(define italicizer
  `((p . ,(lambda (tag . content)
            (cons tag (cons "PARAGRAPH BEGINS: " content))))
    (*text* . ,(lambda (tag content)
                `(i ,content)))
    (*default* . ,(lambda args args))))

(pre-post-order sample-doc italicizer)]

produces:

@racketblock[
'(*TOP*
  (html
   (title (i "the title"))
   (body
    (p "PARAGRAPH BEGINS: " (i "paragraph 1"))
    (p "PARAGRAPH BEGINS: " (i "paragraph 2")))))]


}

@include-section["extracted-sperber.scrbl"]

@include-section["all-exported.scrbl"]


@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!
