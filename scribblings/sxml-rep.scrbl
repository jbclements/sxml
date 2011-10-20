#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title{SXML}

SXML is a representation of XML elements using s-expressions. The
following grammar describes the structure of SXML:

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
[local-name #, @elem{symbol conforming to XML Namespace recommendation}]
[exp-name #, @elem{symbol of the form @racket[_namespace-id]@litchar{:}@racket[_local-name]}]
[namespace-id URI-symbol
              user-ns-shortcut-symbol]
[namespaces (*NAMESPACES* namespace-assoc ...)]
[namespace-assoc (namespace-id uri-string maybe-original-prefix)]
[annotations (|@| maybe-namespaces annotation ...)]
[annotation @#,elem{not yet specified}]
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

That's the easy part.  Things get more icky when you start talking about documents 
and namespaces.
