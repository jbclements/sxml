#lang racket

(require "myenv.ss")

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
(define parser-error
  (lambda args
    (if
      (port? (car args))
      (error 'parser-error (format "Error at position ~s: ~a" 
                                          (file-position (car args))
                                          (args->display-string (cdr args))))
      (error 'parser-error (format 
                            "Error in error handler: its first parameter is not a port: "
                            (args->display-string args))))))

;; map args to their display representations, glue them together:
(define (args->display-string args)
  (apply string-append (map (lambda (x) (format "~a" x)) args)))


(define SSAX:warn
  (lambda  args
    (if
      (port? (car args))
      (cerr nl "Warning at position " 
	    (file-position (car args)) nl
	    (cdr args) nl)
      #f)
    ))

; Alias
(define ssax:warn SSAX:warn)

(provide (all-defined-out))
