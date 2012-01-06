#lang racket/base
(require racket/contract/base)
(provide/contract
 [current-sxml-warning-handler
  (parameter/c
   (->* (symbol? string?) () #:rest list? any))]
 [make-warning-handler
  (-> (or/c output-port? (-> output-port?))
      (->* (symbol? string?) () #:rest list? any))]
 [sxml:warn
  (->* (symbol? string?) () #:rest list? void?)]
 [sxml:warn/concat
  (->* (symbol?) () #:rest list? void?)])

;; Goal: replace all uses of 'cerr' etc with 'sxml:warn'

;; warning-handler adds newline; fmt doesn't need to
(define (make-warning-handler out)
  (lambda (who fmt . args)
    (let ([out (if (procedure? out) (out) out)])
      (apply fprintf out (string-append "~a: " fmt "\n") who args))))

(define current-sxml-warning-handler
  (make-parameter (make-warning-handler current-error-port)))

(define (sxml:warn who fmt . args)
  (apply (current-sxml-warning-handler) who fmt args)
  (void))

(define (sxml:warn/concat who . args)
  ((current-sxml-warning-handler)
   who
   (apply string-append (map (lambda (x) (format "~a" x)) args))))
