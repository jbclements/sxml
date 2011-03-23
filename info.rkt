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
(define release-notes (list "doc updates, changed error behavior"))
(define version "201103230729")

(define scribblings '(("scribblings/sxml.scrbl")))
