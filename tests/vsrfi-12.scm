; Validation of SRFI-12

; Examples are taken from SRFI-12, with a few modifications as needed
; We also added a few more tests and extensions
;
; IMPORT
; (include "myenv.scm")     or a similar prelude
; (include "srfi-12.scm")
; See the Makefile for more details
;
; $Id: vsrfi-12.scm,v 1.2 2002/11/15 00:18:43 oleg Exp $

(cerr nl nl "Validating SRFI-12..." nl)

(assert '()
   (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (x) (k '()))
			      (lambda () (car '()))))))

; displays "Went wrong"
(assert (equal? "Went wrong\n"
  (with-output-to-string
    (lambda ()
      (handle-exceptions
       exn
       (begin
	 (display "Went wrong")
	 (newline))
       (car '()))))))

(cerr "step 1 done" nl)

; = 'zero, 1, or (ABORT "Something else")                                    
(let
    ((test 
      (lambda (selection)
	(handle-exceptions
	 exn
	 (cond
	  ((eq? exn 'one) 1)
	  (else (abort exn)))
	 (case selection
	   ((0) 'zero)
	   ((1) (abort 'one))
	   (else (abort "Something else")))))))

  (assert (equal? 'zero (test 0)))
  (assert (equal? 1 (test 1)))
  (assert (handle-exceptions exn (equal? exn "Something else") (test 'x)))
)

(cerr "Verifying the continuation after a continuable exception (signal) ..."
      nl)
(let ((result
       (with-output-to-string
	 (lambda ()
	   (with-exception-handler 
	    (lambda (exn) (display "Got exception...")
		    ) ; returning from the handler
	    (lambda ()
	      (display "Before exception...")
	      (exc:signal 'a) ;(/ 1 0)
	      (display "After exception...")))
	   (display "After with-exception-handler...")))))
    (assert (equal? result
		    "Before exception...Got exception...After exception...After with-exception-handler...")
))

(cerr nl "Verifying exception conditions..." nl)
(let* ((cs-key (list 'color-scheme))
       (bg-key (list 'background))
       (color-scheme? (condition-predicate cs-key))
       (color-scheme-background
	(condition-property-accessor cs-key bg-key))
       (condition1 (make-property-condition cs-key bg-key 'green))
       (condition2 (make-property-condition cs-key bg-key 'blue))
       (condition3 (make-composite-condition condition1 condition2))
       (result
	(and (color-scheme? condition1)
	     (color-scheme? condition2)
	     (color-scheme? condition3)
	     (color-scheme-background condition3))))
  (assert (memq result '(green blue))))


(cerr "Catching system exceptions and extracting their messages..." nl)
(cerr "Attempting to take (car '())..." nl)
(handle-exceptions
 exn
 (begin
   (cerr "Went wrong: "
	 ((condition-property-accessor 'exn 'message) exn) nl))
 (car '()))

(cerr nl "More examples, from the end of SRFI-12..." nl)
(cerr "Expect the output: Not a pair: 0" nl)
(let
    ()
  (define (try-car v)
    (let ((orig (current-exception-handler)))
      (with-exception-handler
       (lambda (exn)
	 (orig (make-composite-condition
		(make-property-condition
		 'not-a-pair
		 'value
		 v)
		exn)))
       (lambda () (car v)))))
  (assert (eqv? 1 (try-car '(1))))

  (handle-exceptions
   exn
   (begin (cerr "got exception: " (lambda (port) (write exn port)) nl)
   (if ((condition-predicate 'not-a-pair) exn)
       (cerr "Not a pair: "
	     ((condition-property-accessor 'not-a-pair 'value) exn) nl)
       (abort exn)))
   (try-car 0))
)

(cerr nl "Platform-specific tests..." nl)

(cond-expand
 (gambit
  (cerr "Gambit-interpreter-specific tests..." nl)
  (cerr "Testing if a variable is bound via exceptions ..." nl)
  (define-macro (bound? var) `(bound-encap-var? (lambda () ,var)))
  (define (bound-encap-var? thunk)
    (handle-exceptions _ #f (thunk) #t))
  (assert (bound? +))
  (let ((result (bound? xxx-+++-xxx)))
    (assert (not result)))
  )
 (bigloo
  (cerr "Bigloo-specific tests..." nl)
  (cerr "Testing the nesting of try and with-exception-handler ..." nl)
  (let ((result
	 (bind-exit (escape)
		    (with-exception-handler
		     (lambda (exn)
		       (if (equal? exn 1)
			   (escape "Captured twice")
			   (abort "Failure")))
		     (lambda ()
		       (try
			(/ 1 0)
			(lambda (escape proc msg obj)
			  (abort 1))))))))
    (assert (equal? result "Captured twice")))
  )
 (else #f))

(cerr nl nl "All tests passed" nl)




