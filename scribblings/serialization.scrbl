#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "srl"]{Serialization}

@defproc[(srl:sxml->xml [sxml-obj sxml?] [dest port-or-filename? null]) (or/c string? unspecified)]{
  Serializes the @racket[sxml-obj] into XML, with indentation to facilitate
 readability by a human.

@itemize[
 @item{@racket[sxml-obj] - an SXML object (a node or a nodeset) to be serialized}
 
 @item{@racket[dest] - an output port or an output file name, an optional
  argument}]

 If @racket[dest] is not supplied, the functions return a string that
 contains the serialized representation of the @racket[sxml-obj].
 If @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port and return an
 unspecified result.

 If @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of @racket[sxml-obj] is written to
 that filename and an unspecified result is returned. If a file with the given
 name already exists, the effect is unspecified.

}


@defproc[(srl:sxml->xml-noindent [sxml-obj sxml?] [dest port-or-filename? null])
         (or/c string? unspecified) ]{
 Serializes the @racket[sxml-obj] into XML, without indentation.
}

                                    
@defproc[(srl:sxml->html [sxml-obj sxml?] [dest port-or-filename? null])
         (or/c string? unspecified)]{
Serializes the @racket[sxml-obj] into HTML, with indentation to facilitate
readability by a human.

@itemize[
 @item{@racket[sxml-obj] - an SXML object (a node or a nodeset) to be serialized}
 @item{@racket[dest] - an output port or an output file name, an optional
  argument}]

  If @racket[dest] is not supplied, the functions return a string that
 contains the serialized representation of the @racket[sxml-obj].
 If @racket[dest] is supplied and is a port, the functions write the
 serialized representation of @racket[sxml-obj] to this port and return an
 unspecified result.
 
 If @racket[dest] is supplied and is a string, this string is treated as
 an output filename, the serialized representation of @racket[sxml-obj] is written to
 that filename and an unspecified result is returned. If a file with the given
 name already exists, the effect is unspecified.
}

@defproc[(srl:sxml->html-noindent  [sxml-obj sxml?] [dest port-or-filename? null]) 
         (or/c string? unspecified)]{

 Serializes the @racket[sxml-obj] into HTML, without indentation.

}
