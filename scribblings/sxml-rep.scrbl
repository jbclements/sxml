#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title{SXML}

SXML is a representation of XML elements using s-expressions. Specifically, an XML
element containing some content is represented as an s-expression whose first
element is a symbol containing the tag name, and whose remaining elements are
the SXML representation of the original content.

To be more specific, the XML element

@commandline{<abc>def<ghi />jkl</abc>}

corresponds to the SXML value

@racketblock['(abc "def" (ghi) "jkl")]

XML attributes are represented using an optional second element of the s-expression
whose "tag" is "@"@"".

So, the xml element

@commandline{<customer specialness="gazonga">Barry White</customer>}

corresponds to the XML element

@racketblock['(customer (|@| (specialness "gazonga")) "Barry White")]

That's the easy part.  Things get more icky when you start talking about documents 
and namespaces.
