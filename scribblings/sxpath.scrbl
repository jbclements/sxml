#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          scribble/racket
          (for-syntax racket/base)
          (for-label (this-package-in main)))

@title[#:tag "sxpath"]{Search (SXPath)}

@(define-syntax-rule (rewrite-table [lhs -> rhs] ...)
   (tabular (list (list @racketblock[lhs] @elem{⇒} @racketblock[rhs]) ...)))


@defproc[(sxpath [path abbr-sxpath?]
                 [ns-binding ns-binding? '()])
         (-> sxml? (listof sxml?))]{

 Given a path, produces a procedure that accepts an sxml document and returns 
 a list of matches. 

@;{
docs previously said: Note that the @racket[*TOP*] node of the document is required.
But it isn't!
}

AbbrPath is a list. It is translated to the full SXPath according
to the following rewriting rules

@(let (;; shadow the names so we don't get annoying undefined tag warnings
       [sxml:descendant-or-self #f]
       [select-kids #f]
       [sxml:node? #f]
       [node-equal? #f]
       [ntype?? #f]
       [ntype-names?? #f]
       [ntype-namespace-id?? #f]
       [node-join #f]
       [node-reduce #f]
       [node-pos #f]
       [node-eq? #f]
       [sxml:complement #f]
       [filter #f])
  (define-syntax-rule (defmetas id ...)
          (begin (define-syntax id
                   (make-element-id-transformer
                    (lambda _
                     #'(racketvarfont (symbol->string 'id)))))
                 ...))
  (defmetas pc0 pc x p path symbol reducer)
(rewrite-table
[(sxpath '()) -> (node-join)]
[(sxpath '(pc0 pc ...))
 ->
 (node-join (sxpath1 pc0) (sxpath '(pc ...)))]
[(sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)]
[(sxpath1 '(equal? x)) -> (select-kids (node-equal? _x))]
[(sxpath1 '(eq? x))    -> (select-kids (node-eq? _x))]
[(sxpath1 '(*or* p ...)) -> (select-kids (ntype-names??
                                           '(p ...)))]
[(sxpath1 '(*not* p ...)) -> (select-kids (sxml:complement 
                                         (ntype-names??
                                          '(p ...))))]
[(sxpath1 '(ns-id:* x)) -> (select-kids 
                            (ntype-namespace-id?? x))]
[(sxpath1 _symbol)     -> (select-kids (ntype?? _symbol))]
[(sxpath1 _string)     -> (txpath _string)]
[(sxpath1 _procedure)   -> _procedure]
[(sxpath1 '(@#,racket[symbol] ...)) -> (sxpath1 '((@#,racket[symbol]) ...))]
[(sxpath1 '(@#,racket[path] reducer ...))
 ->
 (node-reduce (sxpath path) 
              (sxpathr reducer) ...)]
[(sxpathr _number)      -> (node-pos _number)]
[(sxpathr _path-filter) -> (filter (sxpath _path-filter))]))

To extract all cells from an html table:

@interaction[#:eval the-eval
(define table
  `(*TOP*
    (table 
     (tr (td "a") (td "b"))
     (tr (td "c") (td "d")))))
((sxpath '(table tr td)) table)
#| should produce '((td "a") (td "b") (td "c") (td "d")) |#
]

To extract all cells anywhere in a document:

@interaction[#:eval the-eval
(define table
  `(*TOP*
    (div
     (p (table 
         (tr (td "a") (td "b"))
         (tr (td "c") (td "d"))))
     (table
      (tr (td "e"))))))
((sxpath '(// td)) table)
#| should produce '((td "a") (td "b") (td "c") (td "d") (td "e")) |#
]

One result may be nested in another one:

@interaction[#:eval the-eval
(define doc
  `(*TOP*
    (div
     (p (div "3")
        (div (div "4"))))))
((sxpath '(// div)) doc)
#| should produce
   '((div (p (div "3") (div (div "4")))) (div "3") (div (div "4")) (div "4"))
|#
]

There's also a string-based syntax, @racket[txpath]. As shown in the grammar above,
@racket[sxpath] assumes that any strings in the path are expressed using the 
@racket[txpath] syntax.  

So, for instance, the prior example could be rewritten using a string:

@interaction[#:eval the-eval
(define doc
  `(*TOP*
    (div
     (p (div "3")
        (div (div "4"))))))
((sxpath "//div") doc)
#| should produce 
   '((div (p (div "3") (div (div "4")))) (div "3") (div (div "4")) (div "4"))
|#
]

More generally, lists in the s-expression syntax correspond to string
concatenation in the txpath syntax.

So, to find all italics that appear at top level within a paragraph:

@interaction[#:eval the-eval
(define doc
  `(*TOP*
    (div
     (p (i "3")
        (froogy (i "4"))))))
((sxpath "//p/i") doc)
#| should produce '((i "3")) |#
]

Handling of namespaces in @racket[sxpath] is a bit surprising.  In particular,
it appears to me that sxpath's model is that namespaces must appear fully expanded
in the matched source. For instance:

@interaction[#:eval the-eval
((sxpath "//ns:p" `((ns . "http://example.com")))
 '(*TOP* (html (|http://example.com:body| 
                (|http://example.com:p| "first para") 
                (|http://example.com:p| 
                 "second para containing" 
                 (|http://example.com:p| "third para") "inside it")))))
#| should produce
   '((|http://example.com:p| "first para") 
     (|http://example.com:p| "second para containing" 
                             (|http://example.com:p| "third para") "inside it")
     (|http://example.com:p| "third para"))
|#
]

But the corresponding example where the source document contains a namespace shortcut does 
not match in the same way. That is:

@interaction[#:eval the-eval
((sxpath "//ns:p" `((ns . "http://example.com")))
 '(*TOP* (|@| (*NAMESPACES* (ns "http://example.com")))
         (html (ns:body (ns:p "first para") 
                        (ns:p "second para containing" 
                              (ns:p "third para") "inside it")))))
]

It produces the empty list. Instead, you must pretend that the
shortcut is actually the namespace. Thus:

@interaction[#:eval the-eval
((sxpath "//ns:p" `((ns . "ns")))
 '(*TOP* (|@| (*NAMESPACES* (ns "http://example.com")))
         (html (ns:body (ns:p "first para") 
                        (ns:p "second para containing" 
                              (ns:p "third para") "inside it")))))
#| should produce
 '((ns:p "first para") 
   (ns:p "second para containing" 
         (ns:p "third para") "inside it")
   (ns:p "third para"))
|#
]

Ah well.
}
