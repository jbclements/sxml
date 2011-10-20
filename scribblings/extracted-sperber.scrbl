#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title{Automatically Extracted Comments}

The following "documentation" was generated automatically, using 
a script that I believe is due to Mike Sperber. This documentation
has not been read or formatted for scribble, 
and should be considered only as raw material
for use in creating actual documentation.

@section{ssax.ss}


@defproc[(make-xml-token [KIND KIND] [HEAD HEAD]) ???]{
This creates an XML token.
}

@defproc[(xml-token? [THING any/c]) ???]{
}

@defproc[(xml-token-kind [XML-TOKEN symbol?]) ???]{
}

@defproc[(xml-token-head [XML-TOKEN symbol?]) ???]{
}

@defproc[(ssax:read-markup-token [PORT port?]) ???]{
This procedure starts parsing of a markup token. The current position
in the stream must be #\<. This procedure scans enough of the input stream
to figure out what kind of a markup token it is seeing. The procedure returns
an xml-token structure describing the token. Note, generally reading
of the current markup is not finished! In particular, no attributes of
the start-tag token are scanned.

Here's a detailed break out of the return values and the position in the PORT
when that particular value is returned:
PI-token:	only PI-target is read.
		To finish the Processing Instruction and disregard it,
		call ssax:skip-pi. ssax:read-attributes may be useful
		as well (for PIs whose content is attribute-value
		pairs)
END-token:	The end tag is read completely; the current position
		is right after the terminating #\> character.	
COMMENT		is read and skipped completely. The current position
		is right after "-->" that terminates the comment.
CDSECT		The current position is right after "<!CDATA["
		Use ssax:read-cdata-body to read the rest.
DECL		We have read the keyword (the one that follows "<!")
		identifying this declaration markup. The current
		position is after the keyword (usually a
		whitespace character)

START-token	We have read the keyword (GI) of this start tag.
		No attributes are scanned yet. We don't know if this
		tag has an empty content either.
		Use ssax:complete-start-tag to finish parsing of
		the token.

}

@defproc[(ssax:read-pi-body-as-string [PORT port?]) ???]{
The current position is right after reading the PITarget. We read the
body of PI and return is as a string. The port will point to the
character right after '?>' combination that terminates PI.
[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'

}

@defproc[(ssax:skip-internal-dtd [PORT port?]) ???]{
The current pos in the port is inside an internal DTD subset
(e.g., after reading #\[ that begins an internal DTD subset)
Skip until the "]>" combination that terminates this DTD
}

@defproc[(ssax:read-cdata-body [PORT port?] [STR-HANDLER procedure?] [SEED SEED]) ???]{

This procedure must be called after we have read a string "<![CDATA["
that begins a CDATA section. The current position must be the first
position of the CDATA body. This function reads _lines_ of the CDATA
body and passes them to a STR-HANDLER, a character data consumer.

The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
The first STRING1 argument to STR-HANDLER never contains a newline.
The second STRING2 argument often will. On the first invocation of
the STR-HANDLER, the seed is the one passed to ssax:read-cdata-body
as the third argument. The result of this first invocation will be
passed as the seed argument to the second invocation of the line
consumer, and so on. The result of the last invocation of the
STR-HANDLER is returned by the ssax:read-cdata-body.  Note a
similarity to the fundamental 'fold' iterator.

Within a CDATA section all characters are taken at their face value,
with only three exceptions:
CR, LF, and CRLF are treated as line delimiters, and passed
as a single #\newline to the STR-HANDLER
"]]>" combination is the end of the CDATA section.
&gt; is treated as an embedded #\> character
Note, &lt; and &amp; are not specially recognized (and are not expanded)!

}

@defproc[(ssax:read-char-ref [PORT port?]) ???]{

[66]  CharRef ::=  '&#' [0-9]+ ';' 
                 | '&#x' [0-9a-fA-F]+ ';'

This procedure must be called after we we have read "&#" 
that introduces a char reference.
The procedure reads this reference and returns the corresponding char
The current position in PORT will be after ";" that terminates
the char reference
Faults detected:
WFC: XML-Spec.html#wf-Legalchar

According to Section "4.1 Character and Entity References"
of the XML Recommendation:
 "[Definition: A character reference refers to a specific character
  in the ISO/IEC 10646 character set, for example one not directly
  accessible from available input devices.]"
Therefore, we use a ucscode->char function to convert a character
code into the character -- *regardless* of the current character
encoding of the input stream.

}

@defproc[(ssax:handle-parsed-entity [PORT port?] [NAME ???] [ENTITIES ???] ) ???]{
	CONTENT-HANDLER [STR-HANDLER procedure?] SEED

Expand and handle a parsed-entity reference
port - a PORT
name - the name of the parsed entity to expand, a symbol
entities - see ENTITIES
content-handler -- procedure PORT ENTITIES SEED
that is supposed to return a SEED
str-handler - a STR-HANDLER. It is called if the entity in question
turns out to be a pre-declared entity

The result is the one returned by CONTENT-HANDLER or STR-HANDLER
Faults detected:
WFC: XML-Spec.html#wf-entdeclared
WFC: XML-Spec.html#norecursion

}

@defproc[(make-empty-attlist) ???]{
The ATTLIST Abstract Data Type
Currently is implemented as an assoc list sorted in the ascending
order of NAMES.

}

@defproc[(attlist-add [ATTLIST ???] [NAME-VALUE-PAIR ???]) ???]{
Add a name-value pair to the existing attlist preserving the order
Return the new list, in the sorted ascending order.
Return #f if a pair with the same name already exists in the attlist

}

@defproc[(attlist-null? [ATTLIST ???]) ???]{
}

@defproc[(attlist-remove-top [ATTLIST ???]) ???]{
Given an non-null attlist, return a pair of values: the top and the rest
}

@defproc[(attliast->alist) ???]{
}

@defproc[(attlist-fold) ???]{
}

@defproc[(ssax:read-attributes [PORT port?] [ENTITIES ???]) ???]{

This procedure reads and parses a production Attribute*
[41] Attribute ::= Name Eq AttValue
[10] AttValue ::=  '"' ([^<&"] | Reference)* '"' 
                | "'" ([^<&'] | Reference)* "'"
[25] Eq ::= S? '=' S?


The procedure returns an ATTLIST, of Name (as UNRES-NAME), Value (as string)
pairs. The current character on the PORT is a non-whitespace character
that is not an ncname-starting character.

Note the following rules to keep in mind when reading an 'AttValue'
"Before the value of an attribute is passed to the application
or checked for validity, the XML processor must normalize it as follows: 
- a character reference is processed by appending the referenced
  character to the attribute value 
- an entity reference is processed by recursively processing the
  replacement text of the entity [see ENTITIES]
  [named entities amp lt gt quot apos are assumed pre-declared]
- a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20
  to the normalized value, except that only a single #x20 is appended for a
  "#xD#xA" sequence that is part of an external parsed entity or the
  literal entity value of an internal parsed entity 
- other characters are processed by appending them to the normalized value "


Faults detected:
WFC: XML-Spec.html#CleanAttrVals
WFC: XML-Spec.html#uniqattspec

}

@defproc[(ssax:uri-string->symbol [URI-STR string?]) ???]{
Convert a URI-STR to an appropriate symbol
}

@defproc[(ssax:complete-start-tag [TAG symbol?] [PORT port?] [ELEMS ???] [ENTITIES ???] [NAMESPACES ???]) ???]{

This procedure is to complete parsing of a start-tag markup. The
procedure must be called after the start tag token has been
read. TAG is an UNRES-NAME. ELEMS is an instance of xml-decl::elems;
it can be #f to tell the function to do _no_ validation of elements
and their attributes.

This procedure returns several values:
 ELEM-GI: a RES-NAME.
 ATTRIBUTES: element's attributes, an ATTLIST of (RES-NAME . STRING)
pairs. The list does NOT include xmlns attributes.
 NAMESPACES: the input list of namespaces amended with namespace
(re-)declarations contained within the start-tag under parsing
 ELEM-CONTENT-MODEL

On exit, the current position in PORT will be the first character after
#\> that terminates the start-tag markup.

Faults detected:
VC: XML-Spec.html#enum 
VC: XML-Spec.html#RequiredAttr
VC: XML-Spec.html#FixedAttr
VC: XML-Spec.html#ValueType
WFC: XML-Spec.html#uniqattspec (after namespaces prefixes are resolved)
VC: XML-Spec.html#elementvalid 
WFC: REC-xml-names/#dt-NSName

Note, although XML Recommendation does not explicitly say it,
xmlns and xmlns: attributes don't have to be declared (although they
can be declared, to specify their default value)

Procedure:  ssax:complete-start-tag tag-head port elems entities namespaces
}

@defproc[(ssax:read-external-id [PORT port?]) ???]{

This procedure parses an ExternalID production:
@verbatim{[75] ExternalID ::= 'SYSTEM' S SystemLiteral
	| 'PUBLIC' S PubidLiteral S SystemLiteral
[11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'") 
[12] PubidLiteral ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
[13] PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9]
                        | [-'()+,./:=?;!*#@"@"$_%]
}
This procedure is supposed to be called when an ExternalID is expected;
that is, the current character must be either #\S or #\P that start
correspondingly a SYSTEM or PUBLIC token. This procedure returns the
SystemLiteral as a string. A PubidLiteral is disregarded if present.
}

@defproc[(ssax:read-char-data [PORT port?] 
                              [EXPECT-EOF? boolean?] 
                              [STR-HANDLER procedure?] [SEED ???]) ???]{

This procedure is to read the character content of an XML document
or an XML element.
[43] content ::= 
(element | CharData | Reference | CDSect | PI
	| Comment)*
To be more precise, the procedure reads CharData, expands CDSect
and character entities, and skips comments. The procedure stops
at a named reference, EOF, at the beginning of a PI or a start/end tag.

port
a PORT to read
expect-eof?
a boolean indicating if EOF is normal, i.e., the character
data may be terminated by the EOF. EOF is normal
while processing a parsed entity.
str-handler
a STR-HANDLER
seed
an argument passed to the first invocation of STR-HANDLER.

The procedure returns two results: SEED and TOKEN.
The SEED is the result of the last invocation of STR-HANDLER, or the
original seed if STR-HANDLER was never called.

TOKEN can be either an eof-object (this can happen only if
expect-eof? was #t), or:
    - an xml-token describing a START tag or an END-tag;
For a start token, the caller has to finish reading it.
    - an xml-token describing the beginning of a PI. It's up to an
application to read or skip through the rest of this PI;
    - an xml-token describing a named entity reference.

CDATA sections and character references are expanded inline and
never returned. Comments are silently disregarded.

As the XML Recommendation requires, all whitespace in character data
must be preserved. However, a CR character (#xD) must be disregarded
if it appears before a LF character (#xA), or replaced by a #xA character
otherwise. See Secs. 2.10 and 2.11 of the XML Recommendation. See also
the canonical XML Recommendation.

}

@defproc[(ssax:assert-token [TOKEN ???] [KIND ???] [GI ???]) ???]{
Make sure that TOKEN is of anticipated KIND and has anticipated GI
Note GI argument may actually be a pair of two symbols, Namespace
URI or the prefix, and of the localname.
If the assertion fails, error-cont is evaluated by passing it
three arguments: token kind gi. The result of error-cont is returned.
}

@defproc[(ssax:make-pi-parser [my-pi-handlers ???]) ???]{
Create a parser to parse and process one Processing Element (PI).

my-pi-handlers
An assoc list of pairs (PI-TAG . PI-HANDLER)
where PI-TAG is an NCName symbol, the PI target, and
PI-HANDLER is a procedure PORT PI-TAG SEED
where PORT points to the first symbol after the PI target.
The handler should read the rest of the PI up to and including
the combination '?>' that terminates the PI. The handler should
return a new seed.
One of the PI-TAGs may be the symbol *DEFAULT*. The corresponding
handler will handle PIs that no other handler will. If the
*DEFAULT* PI-TAG is not specified, ssax:make-pi-parser will assume
the default handler that skips the body of the PI

The output of the ssax:make-pi-parser is a procedure
PORT PI-TAG SEED
that will parse the current PI according to the user-specified handlers.

The previous version of ssax:make-pi-parser was a low-level macro:
@racketblock[
(define-macro ssax:make-pi-parser
  (lambda (my-pi-handlers)
  `(lambda (port target seed)
    (case target
	; Generate the body of the case statement
      ,@(let loop ((pi-handlers my-pi-handlers) (default #f))
	 (cond
	  ((null? pi-handlers)
	   (if default `((else (,default port target seed)))
	       '((else
		  (ssax:warn port "Skipping PI: " target nl)
		  (ssax:skip-pi port)
		  seed))))
	  ((eq? '*DEFAULT* (caar pi-handlers))
	   (loop (cdr pi-handlers) (cdar pi-handlers)))
	  (else
	   (cons
	    `((,(caar pi-handlers)) (,(cdar pi-handlers) port target seed))
	    (loop (cdr pi-handlers) default)))))))))
]
}

@defproc[(ssax:make-elem-parser [my-new-level-seed ???]
                                [my-finish-element ???]) ???]{
			my-char-data-handler my-pi-handlers

Create a parser to parse and process one element, including its
character content or children elements. The parser is typically
applied to the root element of a document.

my-new-level-seed
procedure ELEM-GI ATTRIBUTES NAMESPACES EXPECTED-CONTENT SEED
	where ELEM-GI is a RES-NAME of the element
	about to be processed.
This procedure is to generate the seed to be passed
to handlers that process the content of the element.
This is the function identified as 'fdown' in the denotational
semantics of the XML parser given in the title comments to this
file.

my-finish-element
procedure ELEM-GI ATTRIBUTES NAMESPACES PARENT-SEED SEED
This procedure is called when parsing of ELEM-GI is finished.
The SEED is the result from the last content parser (or
from my-new-level-seed if the element has the empty content).
PARENT-SEED is the same seed as was passed to my-new-level-seed.
The procedure is to generate a seed that will be the result
of the element parser.
This is the function identified as 'fup' in the denotational
semantics of the XML parser given in the title comments to this
file.

my-char-data-handler
A STR-HANDLER

my-pi-handlers
See ssax:make-pi-handler above


The generated parser is a
procedure START-TAG-HEAD PORT ELEMS ENTITIES
NAMESPACES PRESERVE-WS? SEED
The procedure must be called after the start tag token has been
read. START-TAG-HEAD is an UNRES-NAME from the start-element tag.
ELEMS is an instance of xml-decl::elems.
See ssax:complete-start-tag::preserve-ws?

Faults detected:
VC: XML-Spec.html#elementvalid 
WFC: XML-Spec.html#GIMatch


}

@defproc[(ssax:make-parser [user-handler-tag ???]
                           [user-handler-proc ???] ...) ???]{

Create an XML parser, an instance of the XML parsing framework.
This will be a SAX, a DOM, or a specialized parser depending
on the supplied user-handlers.

user-handler-tag is a symbol that identifies a procedural expression
that follows the tag. Given below are tags and signatures of the
corresponding procedures. Not all tags have to be specified. If some
are omitted, reasonable defaults will apply.


tag: DOCTYPE
handler-procedure: PORT DOCNAME SYSTEMID INTERNAL-SUBSET? SEED
If internal-subset? is #t, the current position in the port
is right after we have read #\[ that begins the internal DTD subset.
We must finish reading of this subset before we return
(or must call skip-internal-subset if we aren't interested in reading it).
The port at exit must be at the first symbol after the whole
DOCTYPE declaration.
The handler-procedure must generate four values:
ELEMS ENTITIES NAMESPACES SEED
See xml-decl::elems for ELEMS. It may be #f to switch off the validation.
NAMESPACES will typically contain USER-PREFIXes for selected URI-SYMBs.
The default handler-procedure skips the internal subset,
if any, and returns (values #f '() '() seed)

tag: UNDECL-ROOT
handler-procedure: ELEM-GI SEED
where ELEM-GI is an UNRES-NAME of the root element. This procedure
is called when an XML document under parsing contains _no_ DOCTYPE
declaration.
The handler-procedure, as a DOCTYPE handler procedure above,
must generate four values:
ELEMS ENTITIES NAMESPACES SEED
The default handler-procedure returns (values #f '() '() seed)

tag: DECL-ROOT
handler-procedure: ELEM-GI SEED
where ELEM-GI is an UNRES-NAME of the root element. This procedure
is called when an XML document under parsing does contains the DOCTYPE
declaration.
The handler-procedure must generate a new SEED (and verify
that the name of the root element matches the doctype, if the handler
so wishes). 
The default handler-procedure is the identity function.

tag: NEW-LEVEL-SEED
handler-procedure: see ssax:make-elem-parser, my-new-level-seed

tag: FINISH-ELEMENT
handler-procedure: see ssax:make-elem-parser, my-finish-element

tag: CHAR-DATA-HANDLER
handler-procedure: see ssax:make-elem-parser, my-char-data-handler

tag: PI
handler-procedure: see ssax:make-pi-parser
The default value is '()
}

@defproc[(ssax:reverse-collect-str [LIST-OF-FRAGS ???]) ???]{
given the list of fragments (some of which are text strings)
reverse the list and concatenate adjacent text strings.
We can prove from the general case below that if LIST-OF-FRAGS
has zero or one element, the result of the procedure is equal?
to its argument. This fact justifies the shortcut evaluation below.
}




@section{input-parse.ss}


@defproc[(parser-error [PORT port?] [MESSAGE ???] [SPECIALISING-MSG* ???]) ???]{
Many procedures of this package call parser-error to report a parsing
error.  The first argument is a port, which typically points to the
offending character or its neighborhood. Most of the Scheme systems
let the user query a PORT for the current position. MESSAGE is the
description of the error. Other arguments supply more details about
the problem.

}



@section{sxml-tree-trans.ss}


@defproc[(SRV:send-reply [FRAGMENT ???] ...) ???]{

Output the 'fragments'
The fragments are a list of strings, characters,
numbers, thunks, #f, #t -- and other fragments.
The function traverses the tree depth-first, writes out
strings and characters, executes thunks, and ignores
#f and '().
The function returns #t if anything was written at all;
otherwise the result is #f
If #t occurs among the fragments, it is not written out
but causes the result of SRV:send-reply to be #t

}


@defproc[(post-order [TREE ???] [BINDINGS ???]) ???]{
post-order is a strict subset of pre-post-order without *preorder*
(let alone *macro*) traversals. 
Now pre-post-order is actually faster than the old post-order.
The function post-order is deprecated and is aliased below for
backward compatibility.
}

@defproc[(replace-range:: [BEG-PRED ???] [END-PRED ???] 
                          [FOREST ???] ) ???]{
Traverse a forest depth-first and cut/replace ranges of nodes.

The nodes that define a range don't have to have the same immediate
parent, don't have to be on the same level, and the end node of a
range doesn't even have to exist. A replace-range procedure removes
nodes from the beginning node of the range up to (but not including)
the end node of the range.  In addition, the beginning node of the
range can be replaced by a node or a list of nodes. The range of
nodes is cut while depth-first traversing the forest. If all
branches of the node are cut a node is cut as well.  The procedure
can cut several non-overlapping ranges from a forest.

replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST
where
type FOREST = (NODE ...)
type NODE = Atom | (Name . FOREST) | FOREST

The range of nodes is specified by two predicates, beg-pred and end-pred.
beg-pred:: NODE -> #f | FOREST
end-pred:: NODE -> #f | FOREST
The beg-pred predicate decides on the beginning of the range. The node
for which the predicate yields non-#f marks the beginning of the range
The non-#f value of the predicate replaces the node. The value can be a
list of nodes. The replace-range procedure then traverses the tree and skips
all the nodes, until the end-pred yields non-#f. The value of the end-pred
replaces the end-range node. The new end node and its brothers will be
re-scanned.
The predicates are evaluated pre-order. We do not descend into a node that
is marked as the beginning of the range.

}



@section{sxml-to-html.ss}


@defproc[(SXML->HTML [TREE ???]) ???]{

The following procedure is the most generic transformation of SXML
into the corresponding HTML document. The SXML tree is traversed
post-oder (depth-first) and transformed into another tree, which,
written in a depth-first fashion, results in an HTML document.
}

@defproc[(entag [TAG symbol?] [ELEMS (listof sxml?)]) ???]{
Create the HTML markup for tags.
This is used in the node handlers for the post-order function, see
above.

}

@defproc[(enattr [ATTR-KEY ???] [VALUE ???]) ???]{
Create the HTML markup for attributes.
This and entag are being used in the node handlers for the post-order function, see
above.
}

@defproc[(string->goodHTML [STRING string?]) ???]{
Given a string, check to make sure it does not contain characters
such as '<' or '&' that require encoding. Return either the original
string, or a list of string fragments with special characters
replaced by appropriate character entities.

}



@section{sxml-to-html-ext.ss}


@defproc[(make-header [HEAD-PARMS (listof (list/c symbol? any/c))]) ???]{
Create the 'head' SXML/HTML tag. HEAD-PARMS is an assoc list of
(h-key h-value), where h-value is a typically string;
h-key is a symbol:
title, description, AuthorAddress, keywords,
Date-Revision-yyyymmdd, Date-Creation-yyyymmdd,
long-title
One of the h-key can be Links.
In that case, h-value is a list of
(l-key l-href (attr value) ...)
where l-key is one of the following:
start, contents, prev, next, top, home

}

@defproc[(make-navbar: [HEAD-PARMS ???]) ???]{
Create a navigational bar. The argument head-parms is the same
as the one passed to make-header. We're only concerned with the
h-value Links
}

@defproc[(make-footer [HEAD-PARMS ???]) ???]{
Create a footer. The argument head-parms is the same
as passed to make-header.
}

@defproc[(universal-conversion-rules) ???]{
Bindings for the post-order function, which traverses the SXML tree
and converts it to a tree of fragments

The universal transformation from SXML to HTML. The following rules
work for every HTML, present and future
}

@defproc[(universal-protected-rules) ???]{
A variation of universal-conversion-rules which keeps '<', '>', '&'
and similar characters intact. The universal-protected-rules are
useful when the tree of fragments has to be traversed one more time.
}

@defproc[(alist-conv-rules) ???]{
The following rules define the identity transformation
}





