; Verification code for myenv.scm and other my standard "environments"

; For Bigloo, you should evaluate or compile vmyenv-bigloo.scm,
; which contains a module declaration that includes the present file.
; For SCM, (load "myenv-scm.scm") as well as env.scm and util.scm
; before evaluating this file
;
; IMPORT
; appropriate prelude: myenv.scm, myenv-bigloo.scm, myenv-scm.scm
;	depending on your system
; catch-error.scm -- for procedure, for-syntax
; env.scm
; util.scm
;
; $Id: vmyenv.scm,v 1.12 2004/11/03 22:45:29 oleg Exp $

(cerr nl "Verifying increment/decrement operators: inc, inc! etc..." nl)
(let
  ((x 0))
  (assert (= (inc x) 1))
  (assert (= (dec x) -1))
  (assert (begin (inc! x) (= x 1)))
  (assert (begin (dec! x) (dec! x) (zero? (inc x))))
)

(cerr nl "Verifying begin0..." nl)
(let
  ((x 0))
  (assert (= x (begin0 x)))
  (assert (= 0 (begin0 x 1)))
  (assert (= 1 (begin x 1)))
  (assert (= 0 (begin0 x (inc! x) x)))
  (assert (= 2 (begin x (inc! x) x)))
)


(cerr nl "Verifying extended branching instructions..." nl)
(let
  ((x 0))

  (assert (= 2 (begin (when (zero? x) (inc! x)) (inc x))))
  (assert (begin (when (zero? x) (inc! x)) (= x 1)))
  (whennot (zero? x) (dec! x))
  (assert (zero? x))
  (assert (zero? (begin (whennot (positive? x) (dec! x)) (inc x))))
  (assert (= -1 (begin (whennot (negative? x) (inc! x)) x)))
)

(cerr nl "Verifying assert..." nl)
(let
  ((x 1))

  (assert (eq? (positive? x) (assert (positive? x))))
  (assert (eq? x (assert x report: x)))
  (assert (eq? x (assert 0 x)))

  (assert (failed? (assert (zero? x))))
  (assert (failed? (assert (zero? x) report: "failure")))
  (assert (failed? (assert (zero? x) report: "failure" x (+ x 1) "!")))
  (assert (failed? 
	   (let ((y 2)) 
	     (assert (let ((z x)) (positive? z)) (positive? y) (zero? x)
		     report: "failure" x (+ x 1)))))
  (assert (failed? 
	   (let ((y 2)) 
	     (assert (zero? x) (positive? (+ y x))))))
  (assert (failed? 
	   (let ((y 2)) 
	     (assert (let ((z x)) (positive? z)) (positive? y) (zero? x)
	       (positive? (+ y x))
		     ))))
)

(cerr nl "Verifying values and let*-values" nl)
(let
  ()
  ; R5RS example
  (assert (= 5
	     (call-with-values (lambda () (values 4 5))
	       (lambda (a b) b))))
  (assert (= 4
	     (call-with-values (lambda () (values 4))
	       (lambda (b) b))))
  (assert (= 7
	     (call-with-values (lambda () (values))
	       (lambda () 7))))
  (assert (= 140
	     (call-with-values (lambda () (values 4 5 7)) *)))
  ; R5RS example
  ;(call-with-values * -)
  (assert (= -1
	     (call-with-values (lambda () (values (*))) -)))
  ; let*-values
  ; On some system, pp (pretty-printer) can print out closures
  (pp
   (lambda () (let*-values (((a) 1) ((b) 2)) (+ a b))))
  (assert (= 3
	     (let*-values (((a) 1) ((b) 2)) (+ a b))))
;   (assert (= 3
; 	     (let*-values ((a 1) (b 2)) (+ a b))))
  (pp
   (lambda () 
     (let*-values (((a) 1) ((b) 2) ((c d) (values 3 4))) (+ a b (* c d)))))
  (assert (= 15
     (let*-values (((a) 1) ((b) 2) ((c d) (values 3 4))) (+ a b (* c d)))))
  (pp
   (lambda () 
     (let*-values (((a) 1) ((b) 2) ((c d e)
				    (values 1 2 3))) (+ a b (* c d e)))))
  (assert (= 63
     (let*-values (((a) 1) ((b) 2) ((c d e)
				    (values 3 4 5))) (+ a b (* c d e)))))
  (pp
   (lambda () 
     (let*-values (((a) (values 1)) ((c d e) (values 3 4 5))
		   ((b) d)) (+ a b (* c d e)))))
  (assert (= 65
     (let*-values (((a) 1) ((c d e) (values 3 4 5))
		   ((b) d)) (+ a b (* c d e)))))
  ; Two examples from MzScheme reference
  (let ((x 0))
    (assert (= 5
	       (let*-values (((x) 5) ((y) x)) y))))
  (let ((x 0))
    (assert (= 0
	       (let*-values (((x y) (values 5 x))) y))))
  ; Examples from SRFI-11
  (let ((result (let*-values (((a b . c) (values 1 2 3 4)))
			     (list a b c))))
    (assert
     (equal? result '(1 2 (3 4)))))
  (let ((result (let*-values (((a . b) (values 1 2 3 4)))
			     (list a b))))
    (assert
     (equal? result '(1 (2 3 4)))))
  (let ((result (let*-values ((a (values 1 2 3 4)))
			     (list a))))
    (assert
     (equal? result '((1 2 3 4)))))

  (let ((result
	 (let ((a 'a) (b 'b) (x 'x) (y 'y))
	   (let*-values (((a b) (values x y))
			 ((x y) (values a b)))
			(list a b x y)))))
    (assert (equal? result '(x y x y))))
  ; An examples of 0,1,n values -> list
(cond-expand
 ((not gambit)
  (let ((result
	 (let*-values ((a (values)) (b (values 1)) (c 2)
		       (d (values 3 4)))
		      (list a b c d))))
    (assert (equal? result '(() (1) (2) (3 4)))))
  )
 (else #f))
)

(cerr nl "Verifying cond-expand: SRFI-0" nl)
(let
  ()
  (cond-expand
   (gambit (cout "Expanded in Gambit" nl))
   (else #f))
  (cond-expand
   (scm (cout "Expanded in SCM" nl))
   (else #f))
  (cond-expand
   (mit-scheme (cout "Expanded in MIT Scheme" nl))
   (else #f))
  (cond-expand
   (petite-chez (cout "Expanded in Petite Chez Scheme" nl))
   (else #f))
  (cond-expand
   (bigloo (cout "Expanded in Bigloo" nl))
   (else #f))

  (assert (cond-expand (xxx (/ 1 0)) (else #t)))
  (assert (cond-expand ((not xxx) #t)))
  (assert (cond-expand ((or xxx (not xxx)) #t)))
  (assert (cond-expand ((and (not xxx) xxx) (/ 1 0)) (else #t)))

  (cond-expand
    ((or gambit scm mit-scheme bigloo petite-chez)
      (assert (= 1
		(+
		  (cond-expand (gambit 1) (else 0))
		  (cond-expand (scm 1) (else 0))
		  (cond-expand (mit-scheme 1) (else 0))
		  (cond-expand (bigloo 1) (else 0))
		  (cond-expand (petite-chez 1) (else 0)))))
      (cond-expand
	(gambit (assert (failed? (cond-expand ((not gambit) #t)))))
	(else #t))
      (assert (memv
		(cond-expand
		  (gambit 0 1)
		  (scm 0 2)
		  (bigloo 0 3)
		  (mit-scheme 0 4)
		  (petite-chez 0 5)
		  (else 0))
		'(1 2 3 4 5)))
      (assert (memv
		(cond-expand
		  ((and bigloo srfi-0) 0 3)
		  ((and gambit srfi-0) 0 1)
		  ((and scm srfi-0) 0 2)
		  ((and mit-scheme srfi-0) 0 4)
		  ((and petite-chez srfi-0) 0 5)
		  (else 0))
		'(1 2 3 4 5)))
      (assert (memv
		(cond-expand
		  ((or xxx gambit zzz) 0 1)
		  ((or xxx scm zzz) 0 2)
		  ((or xxx bigloo zzz) 0 3)
		  ((or xxx mit-scheme zzz) 0 4)
		  ((or xxx petite-chez zzz) 0 5)
		  (else 0))
		'(1 2 3 4 5)))
      (assert (memv
		(cond-expand
		  ((not gambit) 0 1)
		  ((not scm) 0 2)
		  ((not mit-scheme) 0 4)
		  ((not bigloo) 0 3)
		  (else 0))
		'(1 2)))
      (assert (memv
		(cond-expand
		  ((or (not gambit) (and gambit gambit)) 0 1)
		  ((or (not scm) (and scm scm)) 0 2)
		  ((or (not mit-scheme) (and mit-scheme mit-scheme)) 0 4)
		  ((or (not petite-chez) (and petite-chez petite-chez)) 0 5)
		  ((or (not bigloo) (and bigloo bigloo)) 0 3)
		  (else 0))
		'(1 2 3 4 5)))
      (assert (cond-expand ((not (and gambit scm mit-scheme petite-chez)) #t)))
      (assert
	(cond-expand
	  (gambit (positive? +inf.))  ; works only in Gambit
	  (scm (procedure? try-load)) ; works only in SCM
	  (bigloo (<fx 1 2))		; works only in Bigloo
	  (mit-scheme (fix:+ 1 2))	; works only in MIT-Scheme
	  (petite-chez (procedure? compile))	; works only in Chez-Scheme
	  (else #f)))
      (assert
	(cond-expand
	  ((or gambit scm bigloo mit-scheme) 
	    (char=? #\newline (string-ref "\n" 0)))
	  ((or scm mit-scheme petite-chez) (eq? 'a 'A))
	  (else #f)))
      )
    (else
      (cerr nl "Cond-expand test skipped: platform is not known to the test"
	nl))
    )
)

(cerr nl "Verifying define-opt..." nl)
(let
  ()
  (let ()
    (define-opt (foo x (optional (y 3) (z 5))) (+ x y z))
    ;(assert (failed? (foo)))
    (assert (= (foo 1) 9))
    (assert (= (foo 1 2) 8))
    (assert (= (foo 1 2 3) 6))
    (cond-expand ((not (or gambit bigloo))
		   (assert (failed? (foo 1 2 3 4)))) (else #f))
    )
  (let ((i 1))				; The default expression is
    (define-opt (cnt (optional (x i)))	; re-evaluated every time
      (set! x (max x i))
      (set! i (+ 1 x))
      x)
    (assert (= 1 (cnt)))
    (assert (= 2 (cnt)))
    (assert (= 5 (cnt 5)))
    (assert (= 6 (cnt 4)))
    )
  (let ()				; The optional list may be empty
    (define-opt (f (optional)) 1)	; (per DSSSL spec)
    (define-opt (g x (optional)) x)
    (assert (equal? (f) 1))
    (cond-expand ((not (or gambit bigloo))
		   (assert (failed? (f 1)))) (else #f))
    ;(assert (failed? (g)))
    (assert (equal? (g 2) 2))
    (cond-expand ((not (or gambit bigloo))
		   (assert (failed? (g 1 2)))) (else #f))
    )
  (let ()				; define-opt acts as a regular define
    (define-opt (f . rest) rest)
    (define-opt g 1)
    (define-opt (h x . rest) (list x  rest))
    (assert (equal? (f) '()))
    (assert (equal? (f 1) '(1)))
    (assert (equal? (f 1 2) '(1 2)))
    (assert (equal? g 1))
    ;(assert (failed? (h)))
    (assert (equal? (h 1) '(1 ())))
    (assert (equal? (h 1 2) '(1 (2))))
    (assert (equal? (h 1 2 3) '(1 (2 3))))
    )
  ; Tests from Gambit's manual (which actually pertain to DSSSL's
  ; #!optional. We see how faithful our correspondence to DSSSL is
  (let ()
    (define-opt (f a (optional b)) (list a b))
    (assert (equal? (f 1) '(1 #f)))
    (assert (equal? (f 1 2) '(1 2)))
    (cond-expand ((not (or gambit bigloo))
		   (assert (failed? (f 1 2 3)))) (else #f))
    )
  (let ()
    (define-opt (g a (optional (b a) c (d (list a b c)))) (list a b c d))
    ;(assert (failed? (g)))
    (assert (equal? (g 3) '(3 3 #f (3 3 #f))))
    (assert (equal? (g 3 4) '(3 4 #f (3 4 #f))))
    (assert (equal? (g 3 4 5) '(3 4 5 (3 4 5))))
    (assert (equal? (g 3 4 5 6) '(3 4 5 6)))
    (cond-expand ((not (or gambit bigloo))
		   (assert (failed? (g 3 4 5 6 7)))) (else #f))
    )
)


(cerr nl "Verifying cons*..." nl)
(let ()
  (assert (equal? '(1 2 3 . 4) (cons* 1 2 3 4)))
  (assert (equal? '(1 2 3 4) (cons* 1 2 3 '(4))))
  (assert (equal? '(1 2 3 4) (cons* 1 2 '(3 4))))
  (assert (equal? '(1 2 3 4) (cons* 1 '(2 3 4))))
  (assert (equal? '(1 2 3 4) (cons* '(1 2 3 4))))
  (assert (equal? 1 (cons* 1)))
)



(cerr nl "Verifying assoc-functions with a default clause..." nl)
(let
  ((alist1 '((a 1) (b 2)))
   (alist2 '((a . 1) (b . 2)))
   (alist3 '((a (1)) (b 2 3) (c 3 . 4))))

  (assert (= 1 (lookup-def 'a alist1)))
  (assert (= 1 (lookup-def 'a alist2)))
  (assert (equal? '(1) (lookup-def 'a alist3)))
  (assert (= 2 (lookup-def 'b alist1 #f)))
  (assert (= 2 (lookup-def 'b alist2 warn: (/ 1 0))))
  (assert (equal? '(2 3) (lookup-def 'b alist3 warn: (/ 1 0))))
  (assert (failed? (lookup-def 'c alist1)))	; would fail
  (assert (not (lookup-def 'c alist1 #f)))
  (assert (= 10 (lookup-def 'c alist1 warn: 10)))
  (lookup-def 'c alist1 (lambda () (cerr "message: key not found" nl)))
  (let* ((i 0)
	 (r (lookup-def
	      (begin (inc! i) 'c)
	      (begin (inc! i) alist3))))
    (assert (= i 2) (equal? r '(3 . 4))))
  (let* ((i 0)
	 (r (lookup-def
	      (begin (inc! i) 'd)
	      (begin (inc! i) alist3)
	      warn:
	      'done)))
    (assert (= i 2) (equal? r 'done)))
)

(cerr nl "Verifying list-intersperse and list-intersperse! ..." nl)
(let ((test-l '(4 5 "7" (9)))
      (clone-list 			; Return a newly allocated copy of lst
	(lambda (lst) (append lst '()))))
  (assert (equal? '() (list-intersperse '() 1)))
  (assert (equal? '(4) (list-intersperse '(4) 1)))
  (assert (equal? '(4 1 5) (list-intersperse '(4 5) 1)))
  (assert (equal? '(4 #\, 5 #\, "7" #\, #\9)
      (list-intersperse '(4 5 "7" #\9) #\,)))
  (let ((test-clone (clone-list test-l)))
    (assert (equal? '(4 () 5 () "7" () (9))
        (list-intersperse test-clone '())))
    (assert (equal? test-l test-clone)))

  (assert (equal? '() (list-intersperse! '() 1)))
  (assert (equal? '(4) (list-intersperse! '(4) 1)))
  (assert (equal? '(4 1 5) (list-intersperse! (clone-list '(4 5)) 1)))
  (assert (equal? '(4 #\, 5 #\, "7" #\, #\9)
      (list-intersperse! (clone-list '(4 5 "7" #\9)) #\,)))
  (let ((test-clone (clone-list test-l))
        (test-result '(4 () 5 () "7" () (9))))
    (assert (equal? test-result
        (list-intersperse! test-clone '())))
    (assert (not (equal? test-l test-clone)))
    (assert (equal? test-clone test-result)))
)

(cerr nl "Verifying list-tail-diff ..." nl)
(let ((test-l '(4 5 "7" (9))))
  (assert (equal? (list-tail-diff test-l '()) test-l))
  (assert (not (eq? (list-tail-diff test-l '()) test-l)))
  (assert (equal? (list-tail-diff test-l (append test-l '())) test-l))
  (assert (equal? (list-tail-diff test-l test-l) '()))
  (assert (equal? (list-tail-diff test-l (cdr test-l))  (list (car test-l))))
  (assert (equal? (list-tail-diff test-l (cddr test-l)) (list (car test-l) (cadr test-l))))
  (assert (equal? (list-tail-diff test-l (cdddr test-l)) 
        (list (car test-l) (cadr test-l) (caddr test-l))))
  (let ((test-l-copy (append test-l '())))
    (assert (equal? test-l-copy test-l))
    (assert (not (eq? test-l-copy test-l)))
    (set-car! (list-tail-diff test-l-copy (cdr test-l-copy)) "***")
    (assert (equal? test-l-copy test-l))
    )
)

(cerr nl "Verifying any? ..." nl)
(let ()
  (define (test-driver pred? coll expected-result)
    (let ((res (any? pred? coll)))
      (if (not (eqv? res expected-result))
	  (error "computed result " res "differs from the expected one "
		 expected-result))))
  (define (eq-a? x) (if (char=? x #\a) x #f))
  (define (gt1? x) (if (> x 1) x #f))
  
  (cerr "   finding an element in a list" nl)
  (test-driver gt1? '(1 2 3 4 5) 2)
  (test-driver gt1? '(1 1 1 1 1) #f)
  (test-driver gt1? '(4 1 1 1 1) 4)
  (test-driver gt1? '(4 5 6 1 9) 4)
  (test-driver gt1? '(-4 -5 -6 1 9) 9)
  (test-driver eq-a? '(#\b #\c #\a #\k) #\a)
  
  (cerr "   finding an element in a vector" nl)
  (test-driver gt1? '#(1 2 3 4 5) 2)
  (test-driver gt1? '#(1 1 1 1 1) #f)
  (test-driver gt1? '#(4 1 1 1 1) 4)
  (test-driver gt1? '#(4 5 6 1 9) 4)
  (test-driver gt1? '#(-4 -5 -6 1 9) 9)
  (test-driver eq-a? '#(#\b #\c #\a #\k) #\a)
)

(cerr nl "Verifying our environments..." nl)
(cerr nl nl "verifying environments")
(env.print "Initial environment")
(cerr "adding a few bindings..." nl)
(%%env.bind 'a 1)
(%%env.bind 'b-1 'c)
(%%env.bind 'cc "c c")
(%%env.bind 'dd '(1 3 5 (6) ()))
(cerr nl "The resulting environment. Now trying to get the stuff back..." nl)
(assert (= 1 (%%env.find 'a)))
(assert (not (%%env.find 'b)))
; (%%env.demand 'b)
(assert (failed? (%%env.demand 'b)))
(assert (eq? 'c (%%env.demand 'b-1)))
(assert (string=? "c c" (%%env.demand 'cc)))
(assert (equal? '(1 3 5 (6) ()) (%%env.find 'dd)))
(let ((alist (env.->alist)))
  (cerr "\nThe environment exported as an assoc-list\n")
  (pp alist)
  (assert (equal? alist '((dd 1 3 5 (6) ()) (cc . "c c") (b-1 . c) (a . 1))))
)
(let
  ((mark (env.mark)) (capture #f))
  (cerr "placing mark " mark nl)
  (env.bind* '((a . 3) (b . #(1 2 1/4))))
  (assert (= 3 (%%env.find 'a)))
  (assert (equal? '#(1 2 1/4) (%%env.demand 'b)))
  (env.print "after adding the mark")
  (let ((another-mark (env.mark)))
    (%%env.bind 'a 4)
    (env.print "after adding another mark " another-mark)
    (assert (= 4 (%%env.find 'a)))
    (cerr "capturing the env" nl)
    (set! capture (env.capture! another-mark "Captured Env"))
    (assert (= 3 (%%env.demand 'a)))
    (env.extend capture)
    (env.print "after putting the captured env back")
    )
  (assert (= 4 (%%env.find 'a)))
  (cerr "flushing through the mark " mark)
  (env.flush! mark)
  (env.print)
  (assert (= 1 (%%env.find 'a)))
  (assert (failed? (env.flush! mark)))
  (assert (= -1 
      (env.with capture
        (lambda ()
          (env.print "temporarily extended env")
          (assert (= 4 (%%env.find 'a)))
          (assert (failed? (%%env.demand 'b)))
          -1))))
  (assert (= 1 (%%env.find 'a)))
  (assert (= -3 
      (env.with-exclusive capture
        (lambda ()
          (env.print "temporarily replaced env")
          (assert (= 4 (%%env.find 'a)))
          (assert (failed? (%%env.demand 'dd)))
          -3))))
  (assert (= 1 (%%env.find 'a)))
  (assert (equal? '(1 3 5 (6) ()) (%%env.find 'dd)))
)

(cerr nl "All tests passed" nl)
