#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

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
