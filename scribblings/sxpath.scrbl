#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "sxpath"]{Search (SXPath)}

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
