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
(define release-notes (list "bug fixes, serializers can now accept paths"))
(define version "2012-02-16 10:11")

(define scribblings '(("scribblings/sxml.scrbl" (multi-page))))
