#lang racket/base
(require racket/contract/base
         syntax/readerr
         "errors-and-warnings.rkt")
(provide/contract
 [parser-error
  (->* (port?) () #:rest list? any)]
 [ssax:warn
  (->* (port?) () #:rest list? any)])

; This code provides informative error messages
;   for SSAX (S)XML parser.

;==============================================================================
; Error handler

; According to the SSAX convention this function
; accepts the port as its first argument which is used for
; location of the error in input file.
; Other parameters are considered as error messages,
;  they are printed to stderr as is.

;; NB : updated to signal a racket error rather than printing to stdout.
(define (parser-error p . args)
  (let-values ([(line col pos) (port-next-location p)])
    (raise-read-error (format "SXML parser error: ~a" (args->display-string args))
                      (object-name p)
                      line col pos #f)))

;; map args to their display representations, glue them together:
(define (args->display-string args)
  (apply string-append (map (lambda (x) (format "~a" x)) args)))

(define (ssax:warn p . args)
  (sxml:warn 'ssax:warn
             "warning at position ~a: ~a"
             (file-position p)
             (args->display-string args)))
