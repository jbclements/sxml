#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "sxslt"]{SXML Transformation}

@defproc[(sxml:modify [updater _update-spec] ...)
         (-> _node _node)]{

Returns a procedure that applies the given @racket[updater]s to an
SXML document. Each @racket[updater] has the following form:

@specsubform/subs[
#:literals (list quote)
(list xpath-location-path action action-param ...)
([action 'delete
         'delete-undeep
         'insert-into
         'insert-following
         'insert-preceding
         'replace
         'move-into
         'move-following
         'move-preceding
         handler-proc])]{

The @racket[xpath-location-path] describes the nodes to be transformed
(see also @racket[sxpath]). The @racket[xpath-location-path] is
interpreted with respect to some base node. If the location path is
absolute, the base node is the root of the document being
transformed. If the location path is relative, the base node is the
node selected by the previous @racket[updater].

The following combinations of @racket[action]s and
@racket[action-param]s are supported:
                       
@specsubform[#:literals (quote) 'delete]{
  Deletes the node.
}
@specsubform[#:literals (quote) 'delete-undeep]{
  Deletes the node, but keeps all its content, which thus moves one
  level upwards in the document tree.
}
@specsubform[#:literals (quote) (code:line 'insert-into new-node ...)]{
  Inserts the @racket[new-node]s as the last children of the selected
  nodes.
}
@specsubform[#:literals (quote) (code:line 'insert-following new-node ...)]{
  Inserts the @racket[new-node]s after the selected node.
}
@specsubform[#:literals (quote) (code:line 'insert-preceding new-node ...)]{
  Inserts the @racket[new-node]s before the selected node.
}
@specsubform[#:literals (quote) (code:line 'replace new-node ...)]{
  Replaces the selected node with the @racket[new-node]s.
}
@specsubform[#:literals (quote) (code:line 'rename new-tag)]{
  Renames the selected node, replacing its element tag with
  @racket[new-tag].
}
@specsubform[#:literals (quote) (code:line 'move-into new-location-path)]{
  Moves the selected node to a new location. The selected node becomes
  the last child of the node selected by @racket[new-location-path].
}
@specsubform[#:literals (quote) (code:line 'move-following new-location-path)]{
  Moves the selected node to a new location. The selected node is
  placed immediately after the node selected by
  @racket[new-location-path].
}
@specsubform[#:literals (quote) (code:line 'move-preceding new-location-path)]{
  Moves the selected node to a new location. The selected node is
  placed immediately before the node selected by
  @racket[new-location-path].
}
@specsubform[handler-proc]{
  Applies @racket[handler-proc] to three arguments: the selected node,
  a context (?), and the base node. The procedure must return a node
  or nodeset, which replaces the selected node.
}
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


@defproc[(pre-post-order [tree sxml?]
                         [bindings (listof _binding)])
         sxml?]{

Pre-Post-order traversal of a tree and creation of a new tree.

@racketgrammar*[
#:literals (*preorder* *macro* *text* *default*)
[binding (trigger-symbol *preorder* . _handler)
         (trigger-symbol *macro* . _handler)
         (trigger-symbol _new-bindings . _handler)
         (trigger-symbol . _handler)]
[trigger-symbol XMLname
                *text*
                *default*]
[handler @#,elem{procedure: @racket[(symbol? sxml? ... -> sxml?)]}]
]

The @racket[pre-post-order] function visits the nodes and nodelists
pre-post-order (depth-first).  For each node of the form
@racket[(_name _node ...)] it looks up an association with the given
@racket[_name] among its @racket[bindings]. If it fails,
@racket[pre-post-order] tries to locate a default binding. It's an
error if the latter attempt fails as well.  Having found a binding,
the @racket[pre-post-order] function first checks to see if the
binding is of the form

@racketblock[(_trigger-symbol *preorder* . _handler)]

If it is, the @racket[_handler] is applied to the current
node. Otherwise, the @racket[pre-post-order] function first calls
itself recursively for each child of the current node, with
@racket[_new-bindings] prepended to the @racket[bindings] in
effect. The result of these calls is passed to the @racket[_handler]
(along with the head of the current node). To be more precise, the
handler is applied to the head of the current node and its processed
children. The result of the handler, which should also be a tree,
replaces the current node. If the current node is a text string or
other atom, a special binding with a symbol @racket[*text*] is looked
up.

A binding can also be of a form

@racketblock[(_trigger-symbol *macro* . _handler)]

This is equivalent to @racket[*preorder*] described above. However,
the result is re-processed again, with the current stylesheet.

A tiny example:

@interaction[#:eval the-eval
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
