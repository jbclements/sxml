#lang racket/base
(require "myenv.ss")
(provide parser-error
         ssax:warn)

; This code provides informative error messages
;   for SSAX (S)XML parser.
;
;
; NOTE: PLT-specific ! 
; It was tested with SSAX version 4.6 / PLT 103
;

;==============================================================================
; Error handler

; According to the SSAX convention this function
; accepts the port as its first argument which is used for
; location of the error in input file.
; Other parameters are considered as error messages,
;  they are printed to stderr as is.

;; NB : updated to signal a racket error rather than printing to stdout.
(define (parser-error arg0 . args)
  (if (port? arg0)
      (error 'parser-error
             "Error at position ~s: ~a" 
             (file-position arg0)
             (args->display-string args))
      (error 'parser-error
             "Error in error handler: its first parameter is not a port: ~a"
             (args->display-string (cons arg0 args)))))

;; map args to their display representations, glue them together:
(define (args->display-string args)
  (apply string-append (map (lambda (x) (format "~a" x)) args)))


(define ssax:warn
  (lambda  args
    (if
      (port? (car args))
      (cerr nl "Warning at position " 
	    (file-position (car args)) nl
	    (cdr args) nl)
      #f)
    ))
