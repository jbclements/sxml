#lang racket/base
(require (for-syntax racket/base syntax/stx))
(provide myenv:error
         assert
         cout
         cerr
         nl
         ++
         ++!
         --
         --!
         whennot
         push!
         cond-expand
         inc
         dec
         cons*)

;; $Id: myenv.ss,v 1.14 2002/03/28 22:23:06 nwv Exp $
;; $Source: /home/nwv/cvsroot/projects/ssax-plt/myenv.ss,v $
;; [ssax-plt] This is a modified version of "official/lib/myenv.scm".
 ;(module myenv mzscheme
 ;  (require (lib "defmacro.ss"))
 ;  (require (rename (lib "pretty.ss") pp pretty-print))

; 		   My Standard Scheme "Prelude"
;
; This version of the prelude contains several forms and procedures
; that are specific to a Gambit-C 3.0 system.
; See myenv-scm.scm, myenv-bigloo.scm, etc. for versions
; of this prelude that are tuned to other Scheme systems.
;
; Id: myenv.scm,v 1.2 2001/09/21 19:53:30 oleg Exp

(define myenv:error error)

; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ...
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).

;; ryanc: The original assert macro here was broken. It sometimes
;; calls cerr on procedures expecting to print them out, but (cerr
;; proc) applies proc to stderr port! Also, report: was only used by
;; assure macro, which was unused, so both assure and report: deleted.

(define-syntax-rule (assert e ...)
  (let ([v (and e ...)])
    (unless v
      (fprintf (current-error-port)
               "assertion failure: ~s\n" '
               '(assert e ...)))
    v))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

;; [ssax-plt] In `cerr', `##stderr' replaced with `(current-error-port)'.

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x (current-error-port))
                  (display x (current-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))

;; [ssax-plt] `##fixnum.' prefix removed.

; Some useful increment/decrement operators
; Note, ##fixnum prefix is Gambit-specific, it means that the
; operands assumed FIXNUM (as they ought to be anyway).
; This perfix could be safely removed: it'll leave the code just as
; correct, but more portable (and less efficient)

(define-syntax-rule (++! x) (set! x (add1 x)))
(define-syntax-rule (++ x) (add1 x))
(define-syntax-rule (--! x) (set! x (sub1 x)))
(define-syntax-rule (-- x) (sub1 x))

; Some useful control operators

; if condition is false execute stmts in turn
; and return the result of the last statement
; otherwise, return #t
; This primitive is often called 'unless'
(define-syntax-rule (whennot condition . stmts)
  (or condition (begin . stmts)))

; Prepend an ITEM to a LIST, like a Lisp macro PUSH
; an ITEM can be an expression, but ls must be a VAR
(define-syntax-rule (push! item ls)
  (set! ls (cons item ls)))

; Implementation of SRFI-0
;; ryanc: rewrote, not very robust
(define-syntax (cond-expand stx)
  (define (feature-req-satisfied? freq)
    (cond [(memq freq '(plt srfi-0 else)) #t]
          [(not (pair? freq)) #f]
          [(eq? 'and (car freq))
           (andmap feature-req-satisfied? (cdr freq))]
          [(eq? 'or (car freq))
           (ormap feature-req-satisfied? (cdr freq))]
          [(eq? 'not (car freq))
           (not (feature-req-satisfied? (cadr freq)))]
          [else #f]))
  (syntax-case stx ()
    [(ce [freq . body] ...)
     (or (for/or ([clause (in-list (syntax->list #'([freq . body] ...)))]
                  #:when (feature-req-satisfied? (syntax->datum (stx-car clause))))
           #`(begin . #,(stx-cdr clause)))
         (raise-syntax-error #f "unsatisfied" stx))]))

;==============================================================================
; DL: this piece of code is taken from the previous version of "myenv.scm"
; Stubs

(define-syntax-rule (inc x) (add1 x))
(define-syntax-rule (dec x) (sub1 x))

(define cons* list*)
