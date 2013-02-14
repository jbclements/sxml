#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label sxml))

@title{SXML}

@deftech{SXML} is a representation of XML elements using
unique s-expressions. The following grammar describes the structure of SXML:

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

NOTE! Some of the sxml libraries, particularly sxml:modify, depend
on the fact that sxml elements in a legal document are all "unique";
as I understand it, the requirement is that no two subtrees of a given
SXML document can be 'eq?' to each other. This can easily
occur when rewriting a tree, for instance a pass that inserts `(delete-me)
in multiple places.

That's the easy part.  Things get more tricky when you start talking
about documents and namespaces.

Refer to @hyperlink["http://okmij.org/ftp/Scheme/SXML.html"]{the
original SXML specification} for a more detailed explanation of the
representation, including examples.


@;{ ============================================================ }

@section{SXML Functions}

@defproc[(sxml:element? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a list starting with a symbol
  that is not a special symbol, @racket[#f] otherwise.

@examples[#:eval the-eval
(sxml:element? '(p "blah"))
(sxml:element? '(*COMMENT* "ignore me"))
(sxml:element? '(|@| (href "link.html")))
]
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

@defproc[(sxml:node? [v any/c]) boolean?]{

  Returns @racket[#t] for anything except an attribute list (that is,
  a list whose first element is @racket['|@|]).

  Note that the set of values accepted by @racket[sxml:node?] is
  different from the non-terminal @racket[_node].

@examples[#:eval the-eval
(sxml:node? '(a (|@| (href "link.html")) "blah"))
(sxml:node? '(|@| (href "link.html")))
]
}

@defproc[(sxml:attr-list [node _node])
         (listof _attribute)]{

  If @racket[node] is an @racket[_element], returns its list of
  attributes (or @racket['()]) if it has no attributes; for all other
  types of @racket[_node], returns @racket['()].

@examples[#:eval the-eval
(sxml:attr-list '(a (|@| (href "link.html")) "blah"))
(sxml:attr-list '(p "blah"))
(sxml:attr-list "blah")
]
}

@;{ ============================================================ }
@;{ -- From sxml-tools.rkt -- }

@defproc[(sxml:attr-list-node [elem sxml:element?])
         (or/c #f (cons/c '|@| (listof @#,racket[_attribute])))]{

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

@defproc[(sxml:content [node-or-nodeset (or/c _node nodeset?)])
         (listof _node)]{
  Returns the contents (elements and text nodes) of an element or
  nodeset.
}

@defproc[(sxml:text [node-or-nodeset (or/c _node nodeset?)])
         string?]{

  Returns a string consisting of all of the character data immediately
  within @racket[node-or-nodeset].

@examples[#:eval the-eval
(sxml:text '(p (em "red") " fish; " (em "blue") " fish"))
]
}

@defproc[(sxml:attr [elem sxml:element?]
                    [attr-name symbol?])
         (or/c string? #f)]{

  Gets the value of the @racket[attr-name] attribute of @racket[elem].
}

@;{
sxml:content-raw
sxml:attr-list-u
sxml:aux-list
sxml:aux-list-u
sxml:aux-node
sxml:aux-nodes
sxml:attr-from-list
sxml:num-attr
sxml:attr-u
sxml:ns-list
sxml:ns-id->nodes
sxml:ns-id->uri
sxml:ns-uri->nodes
sxml:ns-id
sxml:ns-uri
sxml:ns-prefix
}

@defproc[(sxml:change-content [elem sxml:element?]
                              [new-content (listof _child)])
         sxml:element?]{

  Replaces the content of @racket[elem] with @racket[new-content],
  preserving its attributes and auxiliary information.
}

@defproc[(sxml:change-attrlist [elem sxml:element?]
                               [new-attrlist (listof _attribute)])
         sxml:element?]{

  Replaces the attributes of @racket[elem] with @racket[new-attrlist],
  preserving its contents and auxiliary information.
}

@defproc[(sxml:change-name [elem sxml:element?]
                           [tag symbol?])
         sxml:element?]{

  Changes the tag name of @racket[elem], preserving its attributes,
  auxiliary information, and contents.
}

@defproc[(sxml:set-attr [elem sxml:element?]
                        [attr (list/c symbol? any/c)])
         sxml:element?]{

  Returns an element like @racket[elem] but with the attribute
  @racket[attr], which replaces any existing attribute with
  @racket[attr]'s key.
}

@defproc[(sxml:add-attr [elem sxml:element?]
                        [attr (list/c symbol? any/c)])
         (or/c sxml:element? #f)]{

  Like @racket[sxml:set-attr], but returns @racket[#f] if
  @racket[elem] already contains an attribute with @racket[attr]'s
  key.
}

@defproc[(sxml:change-attr [elem sxml:element?]
                           [attr (list/c symbol? any/c)])
         (or/c sxml:element? #f)]{

  Like @racket[sxml:set-attr], but returns @racket[#f] unless
  @racket[elem] already contains an attribute with @racket[attr]'s
  key.
}

@defproc[(sxml:squeeze [elem sxml:element?])
         sxml:element?]{

  Eliminates empty attribute lists and auxiliary lists.
}

@defproc[(sxml:clean [elem sxml:element?])
         sxml:element?]{

  Eliminates empty attribute lists and removes all auxilary lists.
}

@;{
sxml:add-aux
}

@;{ -- }

@;{
sxml:node-parent
sxml:lookup
sxml:attr->xml
sxml:string->xml
sxml:sxml->xml
sxml:attr->html
sxml:string->html
sxml:sxml->html
}
