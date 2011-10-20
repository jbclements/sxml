#lang racket/base
(provide close-output-string
         and-let*)

(define close-output-string get-output-string)

(define-syntax and-let*                                                            
  (syntax-rules ()                   
    ((and-let* () body ...)
     (begin body ...))
    ((and-let* ((var expr) clauses ...) body ...) 
     (let ((var expr))
       (if var (and-let* (clauses ...) body ...) #f)))
    ((and-let* ((expr) clauses ...) body ...)
     (if expr (and-let* (clauses ...) body ...) #f))
    ((and-let* (var clauses ...) body ...)
     (if var (and-let* (clauses ...) body ...) #f))))
