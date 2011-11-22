#lang racket/base
(provide and-let*)

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
