#lang racket/base
(require scribble/manual
         scribble/eval
         (for-label racket/base
                    racket/contract))
(provide (all-from-out scribble/manual)
         (all-from-out scribble/eval)
         (for-label
          (all-from-out racket/base)
          (all-from-out racket/contract))
         the-eval)

(define the-eval (make-base-eval))
(the-eval `(begin (require sxml
                           racket/pretty)
                  (current-print pretty-print-handler)))
