#lang scribble/manual
@(require scribble/core
          "util.rkt"
          scribble/racket
          (for-syntax racket/base)
          (for-label sxml))

@title[#:tag "sxpath"]{Search (SXPath)}

The @hyperlink["https://www.w3.org/TR/xpath/"]{W3C "XPath" standard} describes a standardized way to perform
searches in XML documents. For instance, the XPath string 
@racket["/A/B/C"] (thank you, Wikipedia) describes a search for
a @tt{C} element whose parent is a @tt{B} element whose parent
is an @tt{A} element that is the root of the document.

The @racket[sxpath] function performs a similar search over SXML data,
 using either the standard XPath strings or a list of Racket values.

@interaction[#:eval (make-base-eval)
  (require sxml/sxpath)
  ((sxpath "/A/B/C")
   '(*TOP* (A (B (C)))))
  ((sxpath '(A B C))
   '(*TOP* (A (B (C)))))
  ((sxpath "//p[contains(@class, 'blue')]/text()")
   '(*TOP*
     (body
      (p (|@| (class "blue")) "P1")
      (p (|@| (class "blue green")) "P2")
      (p (|@| (class "red")) "P3"))))]

(This documentation desperately needs more examples.)

Let's consider the following XML document:
@verbatim{
"<AAA>
  <BBB>
     <CCC/>
     <www> www content <xxx/><www>
     <zzz/>
  </BBB>
  <XXX>
     <DDD> content in ccc 
     </DDD>
  </XXX>
</AAA>"}

If we use Neil Van Dyke's html parser, we might parse this into
the following sxml document:

@interaction[#:eval the-eval
(define example-doc
  '(*TOP*
    (aaa "\n" "  "
      (bbb "\n" "     "
        (ccc) "\n" "     "
        (www " www content " (xxx) (www "\n" "     " (zzz) "\n" "  ")))
      "\n" "  "
      (xxx "\n" "     " (ddd " content in ccc \n" "     ") "\n" "  ")
      "\n")))]

@defproc[(sxpath [path (or/c list? string?)]
                 [ns-bindings (listof (cons/c symbol? string?)) '()])
         (-> (or/c _node nodeset?) nodeset?)]{

  Given a representation of a @racket[path], produces a procedure that
  accepts an SXML document and returns a list of matches. Path
  representations are interpreted according to the following rewrite
  rules.

  @;{
  docs previously said: 
    Note that the @racket[*TOP*] node of the document is required.
  But it isn't!
  }

@(let ()
  (define-syntax-rule (rewrite-table line ...)
    (tabular #:style (let ([cs (style #f (list 'top))])
                       (style #f (list (table-columns (list cs cs cs)))))
             (append line ...)))
  (define-syntax-rule (line lhs rhs)
    (list (list @racketblock[lhs] @elem{⇒} @racketblock[rhs])))
  (define-syntax-rule (LINE lhs rhs)
    (list (list @racketblock[lhs] 'cont 'cont)
          (list @elem{} @elem{⇒} @racketblock[rhs])))
  (define-syntax-rule (BREAK)
      (list (list @elem{} @elem{@~} @elem{})))
  (define-syntax-rule (defmetas id ...)
          (begin (define-syntax id
                   (make-element-id-transformer
                    (lambda _
                     #'(racketvarfont (symbol->string 'id)))))
                 ...))
  (defmetas path-component0 path-components x p path symbol reducer)
(rewrite-table
(line (sxpath '()) (node-join))
(LINE (sxpath (cons path-component0 path-components))
      (node-join (sxpath1 path-component0)
                 (sxpath path-components)))
(BREAK)
(line (sxpath1 '//)
      (sxml:descendant-or-self sxml:node?))
(line (sxpath1 `(equal? ,x))
      (select-kids (node-equal? x)))
(line (sxpath1 `(eq? ,x))
      (select-kids (node-eq? x)))
(line (sxpath1 `(*or* ,p ...))
      (select-kids (ntype-names?? `(,p ...))))
(line (sxpath1 `(*not* ,p ...))
      (select-kids
       (sxml:complement
        (ntype-names?? `(,p ...)))))
(line (sxpath1 `(ns-id:* ,x))
      (select-kids (ntype-namespace-id?? x)))
(line (sxpath1 _symbol)
      (select-kids (ntype?? _symbol)))
(line (sxpath1 _string)
      (txpath _string))
(line (sxpath1 _procedure)
      _procedure)
;; ryanc: symbol has to be followed by reducer, right?
;;  original comments just have ellipses, unclear
(LINE (sxpath1 `(,symbol ,reducer ...))
      (sxpath1 `((,symbol) ,reducer ...)))
(LINE (sxpath1 `(,path ,reducer ...))
      (node-reduce (sxpath path) 
                   (sxpathr reducer) ...))
(BREAK)
(line (sxpathr _number)
      (node-pos _number))
(line (sxpathr _path)
      (sxml:filter (sxpath _path)))))

To extract the @tt{xxx}'s inside the @tt{aaa} from the example document:

@interaction[#:eval the-eval
((sxpath '(aaa xxx)) example-doc)]

To extract all cells from an HTML table:

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

@section{Trailing Matches}

There's probably a better name for this feature than "trailing matches",
please let me know what it is (or better yet, make a pull request).

It may sometimes be the case that you're looking for @racket[abc]
nodes that contain @racket[def] nodes. You could try

@racketblock[(sxpath '(// abc def))]

... but that would give you the inner @racket[def] nodes, not the
@racket[abc] parent nodes that contain the @racket[def] nodes.
If you pore over the expansion above, you will discover that you can
do this, using ... well, essentially, using a pair of nested lists:

@racketblock[
((sxpath '(// (abc (def))))
 '(x (x (abc (x)
             (def "1")))
     (abc (x) "2")
     (abc (def) "3")))]

Note that this is also the right way to go if for instance you're looking
for a @racket[div] with a particular id:

@codeblock|{
((sxpath '(// (div (@ id (equal? "wanted")))))
 '(body (foo (div (@ (id "a"))
                  (div (@ (id "b")) "abc")
                  "def")
             (div (@ (id "wanted"))
                  (div (@ (id "c")) "qq")
                  "ghi"))))
}|


@section{Custom Predicates}

If you take a look at the expansion above, you'll see that you can
directly embed procedures (essentially a "3D syntax" approach) into
sxpath terms, to provide more customizable matching. However, you'll
be hard-pressed to find out exactly what the interface to those procedures
looks like. I've done some experimentation and some light code-reading,
and it @emph{appears} to me that such procedures are called with two
arguments; a list of nodes, and another list ... of ... variable bindings?
I realize that it's uncommon for documentation to be so wishy-washy,
but I'm erring on the side of getting the information out there.

Anyhow, suppose (just for instance) that you wanted to be able to use
sxpath to find divs with a particular classname (e.g. @racket[zzz]),
 but that—like CSS—you wanted
to treat the value of the "class" property as being a space-separated
list of class names, so what you're really looking for is a node whose
class property contains @racket["zzz"] as part of a space-separated
list, for instance @racket["yyy zzz aaa"].

Can you do this? Yes you can! Here's some example code:

@codeblock|{
;; an sxpath matcher for "class" nodes with the given classname
;; as part of its space-separated list
(define ((class-str-includes? classname) nodes vars-thingy)
  (ormap (node-with-class-str classname) nodes))

;; is this a node named "class" with the given classname as
;; part of its space-separated list?
(define ((node-with-class-str classname) node)
  (match node
    [(list 'class (? string? classnames-str))
     (member classname
             (map string-trim (regexp-split #px" " classnames-str)))]))

((sxpath `(// abc (z (@ class ,(class-str-includes? "bar")))))
 '(*TOP* (x (x (x (abc (y (x))))
               (abc (z (@ (class "bar tar")) (x) "blorg"))
               (abc (z (@ (class "car bar")) (x) "blorg3"))
               (abc (z (@ (class "baz car")) (x) "blorg2"))))))
}|

There are several clunky things about this solution; it's clunky that I
repeat "class" in two different places, but abstracting over this would
probably naturally soak up the @"@" as well, and I wasn't sure I wanted
to do that. Also, I was surprised to see that the class node was actually
passed to the matcher, rather than its content. But sure, okay. Also,
the name @racket[vars-thingy] should strongly suggest that I'm not entirely
sure what's going on here.

As before: pull requests welcome!

}

@defproc[(txpath [xpath-location-path string?]
                 [ns-bindings (listof (cons/c symbol? string?)) '()])
         (-> (or/c _node nodeset?) nodeset?)]{

  Like @racket[sxpath], but only accepts an XPath query in string
  form, using the standard XPath syntax.

  Deprecated; use @racket[sxpath] instead.
}

@;{ ============================================================ }

A @deftech{sxml-converter} is a function
@racketblock[(-> (or/c _node nodeset?) 
                 nodeset?)]
that is, it takes nodes or nodesets to nodesets. A
@deftech{sxml-converter-as-predicate} is an @tech{sxml-converter} used
as a predicate; a return value of @racket['()] indicates false.

@defproc[(nodeset? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a list of nodes (that is, a
  list that does not start with a symbol).

@examples[#:eval the-eval
(nodeset? '(p "blah"))
(nodeset? '((p "blah") (br) "more"))
]
}

@defproc[(as-nodeset [v any/c])
         nodeset?]{

  If @racket[v] is a nodeset, returns @racket[v], otherwise returns
  @racket[(list v)].

@examples[#:eval the-eval
(as-nodeset '(p "blah"))
(as-nodeset '((p "blah") (br) "more"))
]
}

@; ----

@defproc[(node-eq? [v any/c])
         (-> any/c boolean?)]{
  Curried @racket[eq?].
}
@defproc[(node-equal? [v any/c])
         (-> any/c boolean?)]{
  Curried @racket[equal?].
}

@defproc[(node-pos [n (or/c exact-positive-integer? exact-negative-integer?)])
         @#,tech{sxml-converter}]{

  Returns a converter that selects the @racket[n]th element (counting
  from 1, not 0) of a nodelist and returns it as a singleton nodelist. If
  @racket[n] is negative, it selects from the right: @racket[-1]
  selects the last node, and so forth.

@examples[#:eval the-eval
((node-pos 2) '((a) (b) (c) (d) (e)))
((node-pos -1) '((a) (b) (c)))
]
}

@; ----

@defproc[(sxml:filter [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]

@defproc[(sxml:complement [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter-as-predicate}]

@;{
take-until
take-after
map-union
node-reverse
node-trace
}

@defproc[(select-kids [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]{

  Returns a converter that selects an (ordered) subset of the children
  of the given node (or the children of the members of the given
  nodelist) satisfying @racket[pred].

@examples[#:eval the-eval
((select-kids (ntype?? 'p)) '(p "blah"))
((select-kids (ntype?? '*text*)) '(p "blah"))
((select-kids (ntype?? 'p)) (list '(p "blah") '(br) '(p "blahblah")))
]
}

@defproc[(select-first-kid [pred @#,tech{sxml-converter-as-predicate}])
         (-> (or/c _node nodeset?) (or/c _node #f))]{

  Like @racket[select-kids] but returns only the first one, or
  @racket[#f] if none.
}

@defproc[(node-self [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]{

  Returns a function that when applied to @racket[_node], returns
  @racket[(list _node)] if @racket[(pred _node)] is neither @racket[#f]
  nor @racket['()], otherwise returns @racket['()].

@examples[#:eval the-eval
((node-self (ntype?? 'p)) '(p "blah"))
((node-self (ntype?? 'p)) '(br))
]
}

@defproc[(node-join [selector @#,tech{sxml-converter}])
         @#,tech{sxml-converter}]

@defproc[(node-reduce [converter @#,tech{sxml-converter}])
         @#,tech{sxml-converter}]

@defproc[(node-or [converter @#,tech{sxml-converter}])
         @#,tech{sxml-converter}]

@defproc[(node-closure [converter @#,tech{sxml-converter}])
         @#,tech{sxml-converter}]

@deftogether[[
@defproc[(sxml:attribute [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defproc[(sxml:child [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defthing[sxml:child-nodes @#,tech{sxml-converter}]
@defthing[sxml:child-elements @#,tech{sxml-converter}]
@defproc[(sxml:descendant [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defproc[(sxml:descendant-or-self [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
]]{

  XPath axes and accessors.
}

The following procedures depend explicitly on the root node.

@deftogether[[
@defproc[((sxml:parent [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]
@defproc[(node-parent [root _node])
         @#,tech{sxml-converter}]
@defproc[((sxml:ancestor [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]
@defproc[((sxml:ancestor-or-self [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]
@defproc[((sxml:following [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]
@defproc[((sxml:following-sibling [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]
]]{
   Gosh, I wish these functions were documented.
}

@defproc[((sxml:preceding [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]{
 given a predicate and a root node, returns a procedure that accepts a 
 nodeset and returns all nodes that appear before the given nodes in
 document order, filtered using the predicate.
 
 Here's an example:
 
 @interaction[#:eval the-eval
(((sxml:preceding (ntype?? 'www)) example-doc) ((sxpath `(aaa xxx)) example-doc))]
 
}

@defproc[((sxml:preceding-sibling [pred @#,tech{sxml-converter-as-predicate}])
          [root _node])
         @#,tech{sxml-converter}]{
 given a predicate and a root node, returns a procedure that accepts a
 nodeset and returns all dones that are preceding siblings (in document
 order) of the given nodes.
 
@interaction[#:eval the-eval
(define doc '(*TOP* (div (p "foo") (p "bar")
                         (img "baz") (p "quux"))))
(((sxml:preceding-sibling (ntype?? 'p)) doc) ((sxpath '(// img)) doc))]
                                  }

