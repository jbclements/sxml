#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title{SXML}

@deftech{SXML} is a representation of XML elements using
s-expressions. The following grammar describes the structure of SXML:

@(define ATSIGN (racketidfont "@"))

@racketgrammar*[
#:literals (*TOP* *PI* *COMMENT* *ENTITY* URI *NAMESPACES* |@|)
[top (*TOP* maybe-annotations 
            PI ... 
            comment ... 
            element)]
[element (name maybe-annot-attributes child ...)]
[annot-attributes (|@| attribute ... maybe-annotations)]
[attribute (name maybe-value maybe-annotations)]
[child element
       character-data-string
       PI
       comment
       entity]
[PI (*PI* pi-target 
          maybe-annotations
          processing-instruction-content-string)]
[comment (*COMMENT* comment-string)]
[entity (*ENTITY* public-id-string system-id-string)]
[name local-name
      exp-name]
[local-name @#,elem{symbol conforming to XML Namespace recommendation}]
[exp-name @#,elem{symbol of the form @racket[_namespace-id]@litchar{:}@racket[_local-name]}]
[namespace-id URI-symbol
              user-ns-shortcut-symbol]
[namespaces (*NAMESPACES* namespace-assoc ...)]
[namespace-assoc (namespace-id uri-string maybe-original-prefix)]
[annotations (|@| maybe-namespaces annotation ...)]
[annotation @#,elem{not yet specified}]
]

Some tools, such as SXPath, use the following coarse approximation of
SXML structure for simplicity:

@racketgrammar*[
#:literals (*TOP* *PI* *COMMENT* *ENTITY* URI *NAMESPACES* |@|)
[node (name . node-list)
      string]
[node-list (node ...)]
[name local-name exp-name |@| *TOP* *PI* *COMMENT* *ENTITY* *NAMESPACES*]
]

Refer to @hyperlink["http://okmij.org/ftp/Scheme/SXML.html"]{the
original SXML specification} for a more detailed explanation of the
representation, including examples.

In short, an XML element is represented as a list consisting of its
tag name as a symbol followed by its children nodes. If the XML
element has attributes, they come immediately after the tag symbol, in
a list tagged by an @racket[|@|] symbol.

For example, the XML element

@tt{<abc>def<ghi />jkl</abc>}

is represented by the SXML datum

@racket['(abc "def" (ghi) "jkl")]

and the XML element

@tt{<customer specialness="gazonga">Barry White</customer>}

is represented by the SXML datum 

@racket['(customer (|@| (specialness "gazonga")) "Barry White")]

That's the easy part.  Things get more tricky when you start talking
about documents and namespaces.

@;{ ============================================================ }

@section{SXML Functions}

@;{ ============================================================ }
@;{ -- From ssax/sxpathlib -- }

A @deftech{sxml-converter} is a function
@racketblock[(-> (or/c sxml:node? (listof sxml:node?))
                 (listof sxml:node?))]
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

  If @racket[v] is not a nodeset, returns @racket[(list v)], otherwise
  returns @racket[v].

@examples[#:eval the-eval
(as-nodeset '(p "blah"))
(as-nodeset '((p "blah") (br) "more"))
]
}

@defproc[(sxml:element? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] (shallowly) fits the grammar for
  @racket[_element], @racket[#f] otherwise.
}

@defproc[(ntype-names?? [tags (listof symbol?)])
         (-> any/c boolean?)]{

  Given a list of allowable tag names, returns a predicate that
  recognizes @racket[_element]s with those tags.

@examples[#:eval the-eval
((ntype-names?? '(a p)) '(p "blah"))
((ntype-names?? '(a p)) '(br))
]
}

@defproc[(ntype?? [crit symbol?])
         (-> any/c boolean?)]{

  If @racket[_crit] is a special symbol, a predicate is returned that
  accepts the following classes of @racket[_node]:

  @itemlist[
  @item{@racket['|@|]: an @racket[_annot-attributes] node}
  @item{@racket['*]: any @racket[_element] (@racket[sxml:element?])}
  @item{@racket['*any*]: any @racket[_node]}
  @item{@racket['*text*]: any string}
  @item{@racket['*data*]: anything except a pair (@racket[_element])}
  @item{@racket['*COMMENT*]: a @racket[_comment] node}
  @item{@racket['*PI*]: a @racket[_PI] (processing instruction) node}
  @item{@racket['*ENTITY*]: an @racket[_entity] node}
  ]

  Otherwise, it is an ordinary tag name, and a predicate is returned
  that recognizes @racket[_element]s with that tag.

@examples[#:eval the-eval
((ntype?? '*) "blah")
((ntype?? '*) '(p "blah"))
((ntype?? '*text*) "blah")
((ntype?? '*text*) '(p "blah"))
]
}

@defproc[(ntype-namespace-id?? [ns-id (or/c string? #f)])
         (-> any/c boolean?)]{

  Returns a predicate that recognizes @racket[_element]s with tags
  belonging to the namespace @racket[ns-id]. If @racket[ns-id] is
  @racket[#f], the predicate recognizes elements whose tags have no
  namespace.

@examples[#:eval the-eval
((ntype-namespace-id?? "atom") '(atom:id "blah"))
((ntype-namespace-id?? "atom") '(atomic "section"))
((ntype-namespace-id?? #f) '(atomic "section"))
]
}

@;{
sxml:complement
}

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

@;{
sxml:filter
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

@defproc[(node-self [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]{

  Returns a function that when applied to @racket[_node], returns
  @racket[(list node)] if @racket[(pred node)] is neither @racket[#f]
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

@defproc[(sxml:node? [v any/c]) boolean?]{

  Returns @racket[#t] for anything except an attribute list.

@examples[#:eval the-eval
(sxml:node? '(a (|@| (href "link.html")) "blah"))
(sxml:node? '(|@| (href "link.html")))
]
}

@defproc[(sxml:attr-list [node sxml:node?])
         (listof @racket[_attribute])]{

  If @racket[node] is an @racket[_element], returns its list of
  attributes (or @racket['()]) if it has no attributes; for all other
  types of @racket[_node], returns @racket['()].

@examples[#:eval the-eval
(sxml:attr-list '(a (|@| (href "link.html")) "blah"))
(sxml:attr-list '(p "blah"))
(sxml:attr-list "blah")
]
}

@deftogether[[
@defproc[(sxml:attribute [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defproc[(sxml:child [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defthing[sxml:child-nodes @#,tech{sxml-converter}]
@defthing[sxml:child-elements @#,tech{sxml-converter}]
]]{

XPath axes.
}

@deftogether[[
@defproc[((sxml:parent [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[(node-parent [root sxml:node?])
         @#,tech{sxml-converter}]
]]{

XPath axes.
}

@;{ ============================================================ }
@;{ -- From sxpath-ext.rkt -- }

@deftogether[[
@defproc[((sxml:ancestor [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[((sxml:ancestor-or-self [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[(sxml:descendent [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defproc[(sxml:descendent-or-self [pred @#,tech{sxml-converter-as-predicate}])
         @#,tech{sxml-converter}]
@defproc[((sxml:following [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[((sxml:following-sibling [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[((sxml:preceding [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
@defproc[((sxml:preceding-sibling [pred @#,tech{sxml-converter-as-predicate}])
          [root sxml:node?])
         @#,tech{sxml-converter}]
]]{

XPath axes.
}

@;{ ============================================================ }
@;{ -- From sxml-tools.rkt -- }

@defproc[(sxml:attr-list-node [elem sxml:element?])
         (or/c #f (cons '|@| (listof @racket[_attribute])))]{

  Returns an element's attribute list node, or @racket[#f] it is has
  none. Compare @racket[sxml:attr-list].

@examples[#:eval the-eval
(sxml:attr-list-node '(a (|@| (href "link.html")) "blah"))
(sxml:attr-list-node '(p "blah"))
]
}

@;{
sxml:attr-as-list
sxml:aux-list-node
sxml:aux-as-list
}

@defproc[(sxml:empty-element? [elem sxml:element?])
         boolean?]{

  Returns @racket[#t] if @racket[elem] has no nested elements, text
  nodes, etc. The element may have attributes.

@examples[#:eval the-eval
(sxml:empty-element? '(br))
(sxml:empty-element? '(p "blah"))
(sxml:empty-element? '(link (|@| (rel "self") (href "here.html"))))
]
}

@;{
sxml:shallow-normalized?
sxml:normalized
sxml:shallow-minimized?
sxml:minimized?
sxml:name ;; what is domain???
}

@defproc[(sxml:element-name [elem sxml:element?])
         symbol?]{

  Returns an element's tag.
}

@defproc[(sxml:ncname [qualified-name symbol?])
         string?]{

  Returns the local part of a qualified name.
}

@defproc[(sxml:name->ns-id [qualified-name symbol?])
         (or/c string? #f)]{

  Returns the namespace part of a qualified name.
}

@defproc[(sxml:content [node-or-nodeset (or/c sxml:node? nodeset?)])
         (listof sxml:node?)]{
  Returns the contents (elements and text nodes) of an element or
  nodeset.
}

@defproc[(sxml:text [node-or-nodeset (or/c sxml:node? nodeset?)])
         string?]{

  Returns a string consisting of all of the character data immediately
  within @racket[node-or-nodeset].

@examples[#:eval the-eval
(sxml:text '(p (em "red") " fish; " (em "blue") " fish"))
]
}
