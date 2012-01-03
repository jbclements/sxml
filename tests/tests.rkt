#lang scheme

(require "../main.rkt"
         rackunit)


;; this set of tests is wildly inadequate.

;; parsing tests

;; call xml->sxml with a port created from a string
(define (read-from-string str)
  (ssax:xml->sxml (open-input-string str) `()))

(define (read-from-string/ns str ns)
  (ssax:xml->sxml (open-input-string str) ns))

;; a plain tag:
(check-equal? (read-from-string "<a />")
              `(*TOP* (a)))

;; a default namespace:
(check-equal? (read-from-string "<a xmlns=\"gooble\" />")
              `(*TOP* (gooble:a)))

;; a default namespace applies to sub-elements:
(check-equal? (read-from-string "<a xmlns=\"gooble\" ><b /></a>")
              `(*TOP* (gooble:a (gooble:b))))

;; a a non-default namespace has to be triggered explicitly:
(check-equal? (read-from-string "<a xmlns:ns=\"gooble\" ><b /></a>")
              `(*TOP* (a (b))))

;; here's how you use it:
(check-equal? (read-from-string "<ns:a xmlns:ns=\"gooble\" ><ns:b /></ns:a>")
              `(*TOP* (gooble:a (gooble:b))))

(check-equal? (read-from-string "<ns:a xmlns:ns=\"gooble\" ><b /></ns:a>")
              `(*TOP* (gooble:a (b))))

;; you can use the namespace argument to shorten the prefix tag:
(check-equal? (read-from-string/ns "<ns:a xmlns:ns=\"gooble\" ><b /></ns:a>"
                                   `((g . "gooble")))
              `(*TOP* (@ (*NAMESPACES* (g "gooble")))
                      (g:a (b))))


;; empty tags are indistinguishable from tags with empty strings:
(check-equal? (read-from-string "<a></a>")
             `(*TOP* (a)))

(check-equal? (read-from-string "<a />")
             `(*TOP* (a)))

;; by default, the SSAX reader discards ... whitespace-only strings?
(check-equal? (read-from-string "<a>
</a>")
             `(*TOP* (a)))

(check-equal? (read-from-string "<a>
t
</a>")
             `(*TOP* (a "\nt\n")))





#;(check-exn (lambda (exn) #t)
           (lambda () (ssax:xml->sxml (open-input-string "") `())))



(check-equal? (srl:sxml->xml `(*TOP* (p))) "<p />")

;; this error message should be better...
(check-exn (lambda (x) #t) (lambda () (srl:sxml->xml '(foo (@ (bar (13)))))))


