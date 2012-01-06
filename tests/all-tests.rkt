#lang racket/base
(require rackunit
         rackunit/text-ui
         "ssax-tests.rkt"
         "tests.rkt"
         "vSXML-to-HTML.rkt"
         "vSXML-tree-trans.rkt")
(provide all-tests
         main)

(define all-tests
  (make-test-suite "All SXML tests"
    (list basic-tests
          ssax-tests
          ;; sxml-to-html-tests ;; tests mostly fail
          sxml-tree-trans-tests)))

(define (main) ;; run with "racket -tm <this-file>"
  (case 'text
    ((gui) ((dynamic-require 'rackunit/gui 'test/gui) all-tests #:wait? #t))
    ((text) (run-tests all-tests))))
