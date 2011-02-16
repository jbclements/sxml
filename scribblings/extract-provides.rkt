#lang racket

(require racket/runtime-path)

(define-runtime-path parent-dir "..")

(define-namespace-anchor ns-anchor)

(define (interleave requires-and-provides)
  (define reqs (map 
                (lambda (x)
                  (list 'section (second x)))
                (filter (lambda (x) (equal? (first x) '#%require)) 
                        requires-and-provides)))
  (define pros (filter (lambda (x) (equal? (first x) '#%provide))
                       requires-and-provides))
  (apply append
         (map list reqs pros)))

(call-with-input-file (build-path parent-dir "ssax" "ssax.rkt")
  (lambda (port)
    (parameterize ([current-directory (build-path parent-dir "ssax")]
                   [read-accept-reader #t]
                   [current-namespace (namespace-anchor->namespace ns-anchor)])
      (interleave
       (rest (fourth (syntax->datum (expand (read-syntax "main.rkt" port)))))))))

