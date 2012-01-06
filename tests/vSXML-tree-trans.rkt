; Validation code for SXML-tree-trans.scm
;
; IMPORT
; SXML-tree-trans.scm and all of its imports
; Pretty-printer of trees (named pp)
; ,open sxml-tree-trans ppretty-prints coutputs assertions
;
; $Id: vSXML-tree-trans.scm,v 1.2 2004/07/07 16:02:31 sperber Exp $

#lang racket/base
(require racket/port
         rackunit
         "../ssax/SXML-tree-trans.rkt")
(provide sxml-tree-trans-tests)

;; STATUS: all tests pass

(define sxml-tree-trans-tests
  (test-suite "SXML-tree-trans"
    (let* ((tree
            '(root (n1 (n11) "s12" (n13))
               "s2"
               (n2 (n21) "s22")
               (n3 
                (n31 (n311))
                "s32"
                (n33 (n331) "s332" (n333))
                "s34")))
           (test
            (lambda (pred-begin pred-end expected)
              (let ((computed
                     (car (replace-range pred-begin pred-end (list tree)))))
                (check-equal? computed expected)))))
      (test-case "Remove one node, s2"
        (test
         (lambda (node)
           (and (equal? node "s2") '()))
         (lambda (node) (list node))
         '(root (n1 (n11) "s12" (n13))
            (n2 (n21) "s22")
            (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34"))))
        
      (test-case "Replace one node, s2 with s2-new"
        (test 
         (lambda (node)
           (and (equal? node "s2") '("s2-new")))
         (lambda (node) (list node))
         '(root (n1 (n11) "s12" (n13))
            "s2-new"
            (n2 (n21) "s22")
            (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34"))))

      (test-case "Replace one node, s2 with s2-new and its brother (n-new s)"
        (test 
         (lambda (node)
           (and (equal? node "s2") '("s2-new" (n-new "s"))))
         (lambda (node) (list node))
         '(root (n1 (n11) "s12" (n13))
            "s2-new" (n-new "s")
            (n2 (n21) "s22")
            (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34"))))
        
      (test-case "Remove everything from s2 onward"
        (test 
         (lambda (node)
           (and (equal? node "s2") '()))
         (lambda (node) #f)
         '(root (n1 (n11) "s12" (n13)))))
        
      (test-case "Remove everything from n1 onward"
        (test 
         (lambda (node)
           (and (pair? node) (eq? 'n1 (car node)) '()))
         (lambda (node) #f)
         '(root)))

      (test-case "Replace from n1 through n33"
        (test 
         (lambda (node)
           (and (pair? node)
                (eq? 'n1 (car node))
                (list node '(n1* "s12*"))))
         (lambda (node)
           (and (pair? node)
                (eq? 'n33 (car node))
                (list node)))
         '(root
              (n1 (n11) "s12" (n13))
            (n1* "s12*")
            (n3 
             (n33 (n331) "s332" (n333))
             "s34"))))
      )))
