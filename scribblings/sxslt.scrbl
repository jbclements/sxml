#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "sxslt"]{Transformation (SXSLT)}

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
