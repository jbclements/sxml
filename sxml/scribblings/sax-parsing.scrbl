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

