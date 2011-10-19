#lang racket/base
(require scribble/manual
         scribble/eval
         planet/scribble
         planet/version
         (for-label racket/base
                    racket/contract))
(provide (all-from-out scribble/manual)
         (all-from-out scribble/eval)
         (all-from-out planet/scribble)
         (for-label
          (all-from-out racket/base)
          (all-from-out racket/contract))
         the-eval)

(define the-eval (make-base-eval))
(the-eval `(begin (require (planet ,(this-package-version-symbol main))
                           racket/pretty)
                  (current-print pretty-print-handler)))
