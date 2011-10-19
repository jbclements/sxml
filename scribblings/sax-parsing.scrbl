#lang scribble/doc
@(require scribble/manual
          "util.rkt"
          (for-label (this-package-in main)))

@title[#:tag "ssax"]{SAX Parsing}

@defproc[(ssax:xml->sxml [port port?]
                         [namespace-prefix-assig (listof (cons/c symbol? string?))])
         sxml?]{
 Reads a single xml element from the given @racket[port], and returns the 
 corresponding sxml representation.  The @racket[namespace-prefix-assig]
 association list provides shortened forms to be used in place of 
 namespaces.
 
 So, for instance,
 
 @racketblock[
(ssax:xml->sxml 
 (open-input-string 
  "<zippy><pippy pigtails=\"2\">ab</pippy>cd</zippy>")
 '())]
 
 Evaluates to:
 
 @racketblock['(*TOP* (zippy (pippy (|@| (pigtails "2")) "ab") "cd"))]
}
