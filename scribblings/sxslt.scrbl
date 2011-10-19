#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "sxslt"]{Transformation (SXSLT)}

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
