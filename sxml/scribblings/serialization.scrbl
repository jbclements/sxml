#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label sxml))

@title[#:tag "srl"]{Serialization}

@defproc*[([(srl:sxml->xml [sxml-obj sxml?])
            string?]
           [(srl:sxml->xml [sxml-obj sxml?]
                           [dest (or/c output-port? path-string?)])
            void?])]{

 Serializes the SXML node or nodeset @racket[sxml-obj] into XML, with
 indentation to facilitate readability by a human.

 If @racket[dest] is not supplied, the function returns a string that
 contains the serialized representation of the @racket[sxml-obj].  If
 @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port.  If
 @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of
 @racket[sxml-obj] is written to that filename. If a file with the
 given name already exists, the effect is unspecified.

@examples[#:eval the-eval
(srl:sxml->xml '(zippy (pippy (|@| (pigtails "2")) "ab") "bc"))
(srl:sxml->xml '(zippy (pippy (|@| (pigtails "2")) "ab") "bc")
               (current-output-port))
(srl:sxml->xml (for/fold ([body '(nothing)]) ([i (in-range 5)])
                 `(doll (|@| (level ,(number->string i))) ,body))
               (current-output-port))
]
}

@defproc*[([(srl:sxml->xml-noindent [sxml-obj sxml?])
            string?]
           [(srl:sxml->xml-noindent [sxml-obj sxml?]
                                    [dest (or/c output-port? path-string?)])
            void?])]{

 Like @racket[srl:sxml->xml] but without indentation.

@examples[#:eval the-eval
(srl:sxml->xml-noindent
 '(zippy (pippy (|@| (pigtails "2")) "ab") "bc"))
(srl:sxml->xml-noindent
 '(zippy (pippy (|@| (pigtails "2")) "ab") "bc")
 (current-output-port))
(srl:sxml->xml-noindent
 (for/fold ([body '(nothing)]) ([i (in-range 5)])
   `(doll (|@| (level ,(number->string i))) ,body))
 (current-output-port))
]
}
                                    
@defproc*[([(srl:sxml->html [sxml-obj sxml?])
            string?]
           [(srl:sxml->html [sxml-obj sxml?]
                            [dest (or/c output-port? path-string?)])
            void?])]{

 Serializes the SXML node or nodeset @racket[sxml-obj] into HTML, with
 indentation to facilitate readability by a human.

 If @racket[dest] is not supplied, the functions return a string that
 contains the serialized representation of the @racket[sxml-obj].  If
 @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port.  If
 @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of
 @racket[sxml-obj] is written to that filename. If a file with the
 given name already exists, the effect is unspecified.
 
 NOTE: As far as I can tell, the result of this transformation
 is more accurately described as XHTML than as HTML.  The most noticeable
 difference is that certain tags may be rendered surprisingly by
 many browsers when they are expressed in the @tt{<tag />} form
 rather than in the @tt{<tag></tag>} form. Giving these tags an 
 empty string as a body will force them to render correctly. The
 list of tags for which the W3 consortium recommends using the 
 expanded form is this:
 @itemlist[@item{param}
  @item{meta}
  @item{link}
  @item{isindex}
  @item{input}
  @item{img}
  @item{hr}
  @item{frame}
  @item{col}
  @item{br}
  @item{basefont}
  @item{base}
  @item{area}]
 
 Should this function automatically treat these differently? Yes, probably
 so. 
}

@defproc*[([(srl:sxml->html-noindent [sxml-obj sxml?])
            string?]
           [(srl:sxml->html-noindent [sxml-obj sxml?]
                                     [dest (or/c output-port? path-string?)])
            void?])]{

 Like @racket[srl:sxml->html] but without indentation.
}
