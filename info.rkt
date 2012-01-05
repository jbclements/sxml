#lang setup/infotab

(define name "sxml")
(define blurb
  (list "Collection of tools for processing markup documents "
        "in the form of S-expressions"))
(define primary-file "main.rkt")
(define homepage "http://modis.ispras.ru/Lizorkin/sxml-tutorial.html")
(define categories '(xml))
(define compile-omit-paths '("tests/" "ssax/tests/"))
(define repositories '("4.x"))
(define release-notes (list "releasing changes from Ryan Culpepper"))
(define version "2012-01-03 20:25")

(define scribblings '(("scribblings/sxml.scrbl" (multi-page))))
