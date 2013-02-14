#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label sxml))

@title[#:tag "sxslt"]{SXML Transformation}

@defproc[(sxml:modify [updater _update-spec] ...)
         (-> _node _node)]{

Returns a procedure that applies the given @racket[updater]s to an
SXML document, producing a new SXML document. Each @racket[updater]
has the following form:

@racketgrammar*[
#:literals (list quote)
[update-spec (list xpath-location-path action action-param ...)]
[action 'delete
         'delete-undeep
         'insert-into
         'insert-following
         'insert-preceding
         'replace
         'move-into
         'move-following
         'move-preceding
         handler-proc]]

The @racket[xpath-location-path] describes the nodes to be transformed
(see also @racket[sxpath]). The @racket[xpath-location-path] is
interpreted with respect to some base node. If the location path is
absolute, the base node is the root of the document being
transformed. If the location path is relative, the base node is the
node selected by the previous @racket[updater].

Note! @racket[sxml:modify] depends on the uniqueness of all subtrees; 
if an @racket[eq?] subtree occurs more than once, @racket['delete]
may fail to delete all instances, for example.

The following combinations of @racket[action]s and
@racket[action-param]s are supported:
                       
@specsubform[#:literals (quote) 'delete]{
  Deletes the selected nodes.
}
@specsubform[#:literals (quote) 'delete-undeep]{
  Deletes the selected nodes, but keeps all of their contents, which
  thus move one level upwards in the document tree.
}
@specsubform[#:literals (quote) (code:line 'insert-into new-node ...)]{
  Inserts the @racket[new-node]s as the last children of the selected
  nodes.
}
@specsubform[#:literals (quote) (code:line 'insert-following new-node ...)]{
  Inserts the @racket[new-node]s after the selected nodes.
}
@specsubform[#:literals (quote) (code:line 'insert-preceding new-node ...)]{
  Inserts the @racket[new-node]s before the selected nodes.
}
@specsubform[#:literals (quote) (code:line 'replace new-node ...)]{
  Replaces the selected nodes with the @racket[new-node]s.
}
@specsubform[#:literals (quote) (code:line 'rename new-tag)]{
  Renames the selected nodes, replacing its element tag with
  @racket[new-tag].
}
@specsubform[#:literals (quote) (code:line 'move-into new-location-path)]{
  Moves the selected nodes to a new location. The selected nodes become
  the last children of the nodes selected by @racket[new-location-path].
}
@specsubform[#:literals (quote) (code:line 'move-following new-location-path)]{
  Moves the selected nodes to a new location. The selected nodes are
  placed immediately after the nodes selected by
  @racket[new-location-path].
}
@specsubform[#:literals (quote) (code:line 'move-preceding new-location-path)]{
  Moves the selected nodes to a new location. The selected nodes are
  placed immediately before the nodes selected by
  @racket[new-location-path].
}
@specsubform[handler-proc]{
  Applies @racket[handler-proc] to three arguments: the selected node,
  a context (?), and the base node. The procedure must return a node
  or nodeset, which replaces the selected node.
}

@examples[#:eval the-eval
(define sample-doc
  `(*TOP*
    (html (title "the title")
          (body (p "paragraph 1")
                (p "paragraph 2")))))
((sxml:modify (list "//title" 'delete)) sample-doc)
((sxml:modify (list "//body" 'delete-undeep)) sample-doc)
((sxml:modify (list "//body" 'rename 'table)
              (list "p" (lambda (node ctx root)
                          `(tr (td ,node)))))
 sample-doc)
]
}


@defproc[(pre-post-order [tree _node]
                         [bindings (listof _binding)])
         _node]{

Traverses @racket[tree], applying the transformation rules specified
by @racket[bindings] to each node.

@racketgrammar*[
#:literals (list* quote *preorder* *macro* *text* *default*)
[binding (list* trigger-symbol '*preorder* . handler-proc)
         (list* trigger-symbol '*macro* . handler-proc)
         (list* trigger-symbol new-bindings . handler-proc)
         (list* trigger-symbol . handler-proc)]
[trigger-symbol XMLname
                '*text*
                '*default*]
]

The @racket[pre-post-order] function visits the nodes and nodelists
pre-post-order (depth-first).  For each node of the form
@racket[(_name _node ...)] it looks up an association with the given
@racket[_name] among its @racket[bindings]. If it fails,
@racket[pre-post-order] tries to locate a default binding. It's an
error if the latter attempt fails as well.  

The following types of @racket[_binding] are supported:

@specsubform[#:literals (list* quote)
(list* trigger-symbol '*preorder* . handler-proc)]{

The @racket[handler-proc] is applied to each node matching
@racket[trigger-symbol] without first processing the node's
contents. The result is not traversed by @racket[pre-post-order].
}

@specsubform[#:literals (list* quote)
(list* trigger-symbol '*macro* . handler-proc)]{

This is equivalent to @racket['*preorder*] described above. However,
the result is re-processed again, with the current stylesheet.
}

@specsubform[#:literals (list* quote)
(list* trigger-symbol new-bindings . handler-proc)]
@specsubform[#:literals (list* quote)
(list* trigger-symbol . handler-proc)]{

The @racket[handler-proc] is applied to each node matching
@racket[trigger-symbol], but only after the node's contents have been
recursively processed, using the additional @racket[_new-bindings], if
present.

To be more precise, the handler is applied to the head of the current
node and its processed children. The result of the handler, which
should also be a tree, replaces the current node. If the current node
is a text string or other atom, a special binding with a symbol
@racket[*text*] is looked up.
}

@examples[#:eval the-eval
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
(pre-post-order sample-doc italicizer)
#| should produce
  '(*TOP*
    (html
     (title (i "the title"))
     (body
      (p "PARAGRAPH BEGINS: " (i "paragraph 1"))
      (p "PARAGRAPH BEGINS: " (i "paragraph 2")))))
|#
]
}
