#lang scheme
(require "../main.rkt"
         rackunit)
(provide basic-tests)

;; this set of tests is wildly inadequate.

;; call xml->sxml with a port created from a string
(define (read-from-string str [ns '()])
  (ssax:xml->sxml (open-input-string str) ns))

(define basic-tests
  (test-suite "basic ssax tests"
    (test-equal? "a plain tag"
                 (read-from-string "<a />")
                 `(*TOP* (a)))
    (test-equal? "a default namespace"
                 (read-from-string "<a xmlns=\"gooble\" />")
                 `(*TOP* (gooble:a)))
    (test-equal? "a default namespace applies to sub-elements"
                 (read-from-string "<a xmlns=\"gooble\" ><b /></a>")
                 `(*TOP* (gooble:a (gooble:b))))
    (test-equal? "a non-default namespace has to be triggered explicitly"
                 (read-from-string "<a xmlns:ns=\"gooble\" ><b /></a>")
                 `(*TOP* (a (b))))
    (test-equal? "here's how you use a non-default namespace"
                 (read-from-string "<ns:a xmlns:ns=\"gooble\" ><ns:b /></ns:a>")
                 `(*TOP* (gooble:a (gooble:b))))
    (test-equal? "non-default namespace is non-default"
                 (read-from-string "<ns:a xmlns:ns=\"gooble\" ><b /></ns:a>")
                 `(*TOP* (gooble:a (b))))
    (test-equal? "you can use the namespace argument to shorten the prefix tag"
                 (read-from-string "<ns:a xmlns:ns=\"gooble\" ><b /></ns:a>"
                                   `((g . "gooble")))
                 `(*TOP* (@ (*NAMESPACES* (g "gooble")))
                         (g:a (b))))
    (test-equal? "empty tags are indistinguishable from tags with empty strings"
                 (read-from-string "<a></a>")
                 `(*TOP* (a)))
    (test-equal? "empty tag 2"
                 (read-from-string "<a />")
                 `(*TOP* (a)))
    (test-equal? "by default, the SSAX reader discards ... whitespace-only strings?"
                 (read-from-string "<a>\n</a>")
                 `(*TOP* (a)))
    (test-equal? "discard whitespace 2"
                 (read-from-string "<a>\nt\n</a>")
                 `(*TOP* (a "\nt\n")))
    (test-exn "parsing empty string fails"
              #rx"unexpected EOF"
              (lambda () (ssax:xml->sxml (open-input-string "") '())))
    (test-equal? "serialization 1"
                 (srl:sxml->xml `(*TOP* (p)))
                 "<p />")
    (test-exn "serialization bad"
              (lambda (e) #t)
              (lambda () (srl:sxml->xml '(foo (@ (bar (13)))))))
    (let ([temp (make-temporary-file)])
    (test-not-exn "serialization accepts paths"
                  (lambda () (and (path? temp)
                                  (not (string? temp))
                                  (delete-file temp)
                                  (srl:sxml->xml `(*TOP* (p)) temp)))))))
