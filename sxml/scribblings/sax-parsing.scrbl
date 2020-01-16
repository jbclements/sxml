#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label sxml))

@title[#:tag "ssax"]{SAX Parsing}

@defproc[(ssax:xml->sxml [port input-port?]
                         [namespace-prefix-assig (listof (cons/c symbol? string?))])
         sxml?]{

 Reads an XML document (which can be a single XML element) from
 @racket[port], and returns the corresponding SXML (@racket[_top])
 representation. The @racket[namespace-prefix-assig] association list
 provides shortened forms to be used in place of namespaces.

@examples[#:eval the-eval
(ssax:xml->sxml 
 (open-input-string
  "<zippy><pippy pigtails=\"2\">ab</pippy>cd</zippy>")
 '())
#| should be '(*TOP* (zippy (pippy (|@| (pigtails "2")) "ab") "cd")) |#

(ssax:xml->sxml
  (open-input-string
   "<car xmlns=\"vehicles\"><wheels>4</wheels></car>")
  '())

(ssax:xml->sxml
  (open-input-string
   "<car xmlns=\"vehicles\"><wheels>4</wheels></car>")
  '((v . "vehicles")))
]
}

               
@defproc[(sxml:document [url-string string?] [namespace-prefix-assig any/c]) sxml?]{
 Given a local file URI, return the corresponding SXML representation.

 NOTE: currently, this appears to work only for local documents.

  NAMESPACE-PREFIX-ASSIG - is passed as-is to the SSAX parser: there it is
  used for assigning certain user prefixes to certain namespaces.

 NAMESPACE-PREFIX-ASSIG is an optional argument and has an effect for an
  XML resource only. For an HTML resource requested, NAMESPACE-PREFIX-ASSIG
  is silently ignored.

  So, for instance, if the file @filepath{/tmp/foo.xml} contains an XML file,
  you should be able to call

@racketblock[
 (sxml:document "file:///tmp/foo")]

(Note the plethora of slashes required by the URI format.)


}


@;{has to be a defform because of Oleg's approach to argument naming.
can't see a way in defform to describe result.}
@defform[#:kind "procedure"
         (ssax:make-parser new-level-seed-spec
                           finish-element-spec
                           char-data-handler-spec
                           tag-spec ...)
         #:grammar
         [(new-level-seed-spec NEW-LEVEL-SEED new-level-seed-proc)
          (finish-element-spec FINISH-ELEMENT finish-element-proc)
          (char-data-handler-spec CHAR-DATA-HANDLER char-data-handler-proc)
          (tag-spec tag tag-proc)]]{

 Returns a procedure of two arguments, an input port @racket[xml-port], and an
 object @racket[init-seed].  That procedure will parse the XML document
 produced by @racket[xml-port], and the object @racket[init-seed], according to the
 specifications @racket[new-level-seed-spec], @racket[finish-element-spec],
 @racket[char-data-handler-spec], and @racket[tag-spec]s, and will return an object
 of the same type as @racket[init-seed].


 @racket[new-level-seed-spec] consists of the tag @racket[NEW-LEVEL-SEED] in upper
 case, followed by a procedure @racket[new-level-seed-proc].  This procedure
 must take the arguments @racket[element-name], @racket[attributes], @racket[namespaces],
 @racket[expected-content], and @racket[seed].  It must return an object of the same
 type as @racket[init-seed].


 @racket[finish-element-spec] consists of the tag @racket[FINISH-ELEMENT] in upper
 case, followed by a procedure @racket[finish-element-proc].  This procedure
 must take the arguments @racket[element-name], @racket[attributes], @racket[namespaces],
 @racket[parent-seed], and @racket[seed].  It must return an object of the same type
 as @racket[init-seed].

 @racket[char-data-handler-spec] consists of the tag @racket[CHAR-DATA-HANDLER] in
 upper case, followed by a procedure @racket[char-data-handler-proc].  This
 procedure must take the arguments @racket[string-1], @racket[string-2], and @racket[seed].
 It must return an object of the same type as @racket[init-seed].


 `tag-spec': TODO.


 Here's an example that returns a string containing the text, after removing markup, from the
 XML document produced by the input port `in'.

@codeblock|{
#lang racket

(require racket/string sxml)

(define (remove-markup xml-port)
  (let* ((parser
          (ssax:make-parser NEW-LEVEL-SEED remove-markup-nls
                            FINISH-ELEMENT remove-markup-fe
                            CHAR-DATA-HANDLER remove-markup-cdh))
         (strings (parser xml-port null)))
    (string-join (reverse strings) "")))

(define (remove-markup-nls gi attributes namespaces expected-content
                           seed)
  seed)

(define (remove-markup-fe gi attributes namespaces parent-seed seed)
  seed)

(define (remove-markup-cdh string-1 string-2 seed)
  (let ((seed (cons string-1 seed)))
    (if (non-empty-string? string-2)
        (cons string-2 seed)
        seed)))

(remove-markup
 (open-input-string
  "<foo>Hell<bar>o, world!</bar></foo>"))
  }|
}