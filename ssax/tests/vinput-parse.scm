#lang racket

(require srfi/13/string
         "../myenv.rkt"
         "../util.rkt"
         "../input-parse.rkt"
         "../look-for-str.rkt"
         #;"../catch-error.rkt")


;-----------------------------------------------------------------------------
;   This is a test driver for input parsing functions, to make sure they
;                       really work as intended
;
; IMPORT
; appropriate prelude: myenv.scm, myenv-bigloo.scm, myenv-scm.scm
;	depending on your system
; catch-error.scm -- for procedure, for-syntax
; util.scm
; srfi-13-local.scm if no native SRFI-13 support is available.
; look-for-str.scm
; input-parse.scm
;
; See the Makefile in this directory, which shows how to run
; this code on a variery of Scheme systems.
; 
; $Id: vinput-parse.scm,v 1.8 2004/07/08 21:53:33 oleg Exp $


; This function is imported into the input-parse.scm module
(define (parser-error port msg . specializing-msgs)
  (apply error (cons msg specializing-msgs)))

		; make sure that the 'FORM' gave upon evaluation the
		; EXPECTED-RESULT
(cond-expand
  ((or bigloo gambit)
    (define-macro (expect form expected-result)
      `(begin
	 (display "evaluating ")
	 (write ',form)
	 (let ((real-result ,form))
	   (if (equal? real-result ,expected-result)
	     (cout "... gave the expected result: "
	       (lambda () (write real-result)) nl)
	     (error "... yielded: " real-result
	       " which differs from the expected result: " ,expected-result)
	     )))))
  (else
    (define-syntax expect
      (syntax-rules ()
	((expect form expected-result)
	  (begin
	    (display "evaluating ")
	    (write 'form)
	    (let ((real-result form))
	      (if (equal? real-result expected-result)
		(cout "... gave the expected result: "
		  (lambda () (write real-result)) nl)
		(error "... yielded: " real-result
		  " which differs from the expected result: " expected-result)
		))))))))


		; apply FORM to parse the input from the STR
		; and compare the result with the EXPECTED-RESULT
		; EXPECTED-RESULT is a pair: expected result from the
		; form and the expected next character from the stream
(cond-expand
  ((or bigloo gambit)
    (define-macro (expect-parse-result str form expected-result)
      `(begin
	 (display "applying ")
	 (write ',form)
	 (display " to the string ")
	 (write ,str)
	 (newline)
	 (with-input-from-string ,str
	   (lambda ()
	     (let* ((real-result ,form) (real-next-char
					  (read-char (current-input-port))))
	       (if (equal? (cons real-result real-next-char) ,expected-result)
		 (cout "... gave the expected result: " real-result nl
		   "    the next read character, " real-next-char
		   " was expected as well" nl)
		 (error "... yielded: " real-result " and the next char "
		   real-next-char 
		   " which differ from the expected result: "
		   ,expected-result))
	       ))))))
  (else
    (define-syntax expect-parse-result
      (syntax-rules ()
	((expect-parse-result str form expected-result)
	  (begin
	    (display "applying ")
	    (write 'form)
	    (display " to the string ")
	    (write str)
	    (newline)
	    (with-input-from-string str
	      (lambda ()
		(let* ((real-result form) (real-next-char
					    (read-char (current-input-port))))
		  (if (equal? (cons real-result real-next-char)
			expected-result)
		    (cout "... gave the expected result: " real-result nl
		      "    the next read character, " real-next-char
		      " was expected as well" nl)
		    (error "... yielded: " real-result " and the next char "
		      real-next-char 
		      " which differ from the expected result: "
		      expected-result))
		  )))))))))

; Build a string out of components
; A component can be a string, a character, a number
; (converted into a character), symbols cr and lf
; We could've used a notation like "abc\n\t"
; Unfortunately, not all Scheme systems support C-like notation
; of Scheme strings
(define (s . components)
  (apply string-append
	 (map (lambda (component)
		(cond
		 ((string? component) component)
		 ((char? component) (string component))
		 ((number? component) (string (integer->char component)))
		 ((eq? 'lf component) (string #\newline))
		 ((eq? 'cr component) (string (integer->char 13)))
		 (else (error "bad component: " component))))
	      components)))


(cerr nl "Verifying string->integer ..." nl)
(let ()
  (expect (string->integer "" 0 0) #f)
  (expect (string->integer "" 0 1) #f)
  (expect (string->integer "" 1 0) #f)
  (expect (string->integer "1" 0 0) #f)
  (expect (string->integer "1" 0 1) 1)
  (expect (string->integer "1" 0 2) #f)
  (expect (string->integer "1" 1 1) #f)
  (expect (string->integer "1" 1 0) #f)
  (expect (string->integer "81234" 0 5) 81234)
  (expect (string->integer "81234" 1 5) 1234)
  (expect (string->integer "81234" -1 5) #f)
  (expect (string->integer "81234" 1 6) #f)
  (expect (string->integer "81234" 1 4) 123)
  (expect (string->integer "81234" 5 4) #f)
  (expect (string->integer "81234" 4 4) #f)
  (expect (string->integer "81234" 4 5) 4)
  (expect (string->integer "-1234" 4 5) 4)
  (expect (string->integer "-1234" 1 5) 1234)
  (expect (string->integer "-1234" 0 5) #f)
  (expect (string->integer "x12+4" 0 5) #f)
  (expect (string->integer "x12+4" 0 3) #f)
  (expect (string->integer "x12+4" 1 3) 12)
  (expect (string->integer "x12+4" 1 4) #f)
)

(cerr nl "Verifying string-split ..." nl)
(let ((tab "	")) ; This is a string of one tab character
  (expect (string-split "") '())
  (expect (string-split "" '()) '())
  (expect (string-split "" '() 0) '())
  (expect (string-split "" '() 10) '())
  (expect (string-split " " '() 0) '())
  (expect (string-split " ") '())
  (expect (string-split (string-append tab " " tab) '() 10) '())
  (expect (string-split "abcd" '() 10) '("abcd"))
  (expect (string-split "abcd") '("abcd"))
  (expect (string-split "  abcd   ") '("abcd"))
  (expect (string-split "  abcd   " '() -5) '())
  (expect (string-split "  abcd   " '() 1) '("abcd   "))
  (expect (string-split (string-append "  ab" tab "cd   ")) '("ab" "cd"))
  (expect (string-split (string-append "  ab" tab " cd   ")) '("ab" "cd"))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 1)
	  (list (string-append "ab" tab " cd   ")))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 2)
	  '("ab" "cd   "))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 3)
	  '("ab" "cd"))
  (expect (string-split " abc d e f  ") '("abc" "d" "e" "f"))
  (expect (string-split " abc d e f  " '() 1) '("abc d e f  "))
  (expect (string-split " abc d e f  " '() 3) '("abc" "d" "e f  "))

  (expect (string-split "" '(#\: #\+)) '())
  (expect (string-split "" '(#\: #\+) 0) '())
  (expect (string-split "" '(#\: #\+) 10) '())
  (expect (string-split " " '(#\: #\+)) '(" "))
  (expect (string-split " " '(#\: #\+) 1) '(" "))
  (expect (string-split " " '(#\: #\+) 0) '())
  (expect (string-split ":" '(#\: #\+)) '("" ""))
  (expect (string-split "a:" '(#\: #\+)) '("a" ""))
  (expect (string-split "a:" '(#\: #\+) 1) '("a:"))
  (expect (string-split ":a" '(#\: #\+)) '("" "a"))
  (expect (string-split ":a" '(#\: #\+) 1) '(":a"))
  (expect (string-split ":" '(#\: #\+) 1) '(":"))
  (expect (string-split ":+" '(#\: #\+)) '("" "" ""))
  (expect (string-split ":+" '(#\: #\+) 1) '(":+"))
  (expect (string-split ":+" '(#\: #\+) 2) '("" "+"))
  (expect (string-split ":+" '(#\: #\+) 3) '("" "" ""))
  (expect (string-split ":+" '(#\: #\+) 4) '("" "" ""))
  (expect (string-split ":abc:d:e:f:" '(#\:)) '("" "abc" "d" "e" "f" ""))
  (expect (string-split ":abc:d:e::f:" '(#\:)) '("" "abc" "d" "e" "" "f" ""))
  (expect (string-split "root:x:0:0:Lord" '(#\:) 2) '("root" "x:0:0:Lord"))
  (expect (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:)) 
    '("/usr/local/bin" "/usr/bin" "/usr/ucb/bin"))
  (expect (string-split "/usr/local/bin" '(#\/)) '("" "usr" "local" "bin"))
)


(cerr nl "Verifying make-char-quotator ..." nl)
(let ((string->goodHTML
       (make-char-quotator
	'((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))))
  (expect (string->goodHTML "abc!def ") "abc!def ")
  (expect (string->goodHTML "") "")
  (expect (string->goodHTML "<") '("&lt;"))
  (expect (string->goodHTML "<a") '("&lt;" "a"))
  (expect (string->goodHTML "a&b") '("a" "&amp;" "b"))
  (expect (string->goodHTML "a b>") '("a b" "&gt;"))
  (expect (string->goodHTML "<>&\"") '("&lt;" "&gt;" "&amp;" "&quot;"))
  (expect (string->goodHTML " <>&\\\"")
	  '(" " "&lt;" "&gt;" "&amp;" "\\" "&quot;"))
  (expect (string->goodHTML "&amp;") '("&amp;" "amp;"))
)


(cerr nl "Verifying assert-curr-char ..." nl)
(let ()
  (define (test-assert-curr-char str char-list)
    (with-input-from-string str
      (lambda ()
        (assert-curr-char char-list "assert curr char" (current-input-port))
        )))
  
  (expect (test-assert-curr-char " abcd" '(#\a #\space)) #\space)
  (expect (test-assert-curr-char "a bcd" '(#\a #\space)) #\a)
  (assert (failed? (expect (test-assert-curr-char "bacd" '(#\a #\space)) #\a)))
)

(cerr nl "Verifying skipping of characters ..." nl)
(let (
      (eof (with-input-from-string "" read)))

  (expect-parse-result " abcd" (skip-until 1) '(#f . #\a))
  (assert (failed? (expect-parse-result " abcd" (skip-until 10) '(#f . #f))))
  (expect-parse-result " abcd" (skip-until 5) `(#f . ,eof))
  (expect-parse-result " abcd" (skip-until '(#\a #\space)) '(#\space . #\a))
  (expect-parse-result "xxxc bcd" (skip-until '(#\a #\space #\c))
    '(#\c . #\space))
  (expect-parse-result "xxxc" (skip-until '(#\a #\space #\c) (current-input-port))
    `(#\c . ,eof))
  (assert (failed? (expect-parse-result "xxxd"
        (skip-until '(#\a #\space #\c)) '(#f . #f))))
  (expect-parse-result "xxxd" (skip-until '(#\a #\space #\c *eof*))
    `(,eof . ,eof))
  (expect-parse-result "xxxc" (skip-until '(#\a #\space #\c *eof*))
    `(#\c . ,eof))

  (expect-parse-result "xxxd" (skip-while '(#\a #\space #\x))
    '(#\d . #\d))
  (expect-parse-result "yxxxd" (skip-while '(#\a #\space #\x))
    '(#\y . #\y))
  (expect-parse-result "xxx" (skip-while '(#\a #\space #\x) (current-input-port))
    `(,eof . ,eof))
  (expect-parse-result "xxa x" (skip-while '(#\a #\space #\x))
    `(,eof . ,eof))
)   


(cerr nl "Verifying tokenizing of the input stream ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "xxxd"
    (next-token '(#\a #\space #\x) '(#\d) "next token" (current-input-port))
    '("" . #\d))
  (expect-parse-result "xxx xa cccxd"
    (next-token '(#\a #\space #\x) '(#\d))
    '("cccx" . #\d))
  (expect-parse-result "xxx xa cccxdaa"
    (next-token '() '(#\d))
    '("xxx xa cccx" . #\d))
  (expect-parse-result "xxx xa cccxdaa"
    (next-token '() '(#\d #\a))
    '("xxx x" . #\a))
  (expect-parse-result "cccxd"
    (next-token '(#\a #\space #\x) '(#\d))
    '("cccx" . #\d))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d) "next token")
    '(#f . #f))))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d))
    '(#f . #f))))
  (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d *eof*) "" (current-input-port))
    `("cccx" . ,eof))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\c #\space #\x) '(#\d))
    '(#f . #f))))
)

(cerr nl "Verifying tokenizing of the input stream, big tokens ..." nl)
(let* ((eof (with-input-from-string "" read))
	(big-token 
	  (call-with-output-string
	    (lambda (port)
	      (do ((i 0 (inc i))) ((>= i 512))
		(display (modulo i 10) port)))))
	(big-token1 (string-append 
		      big-token big-token big-token
		      (substring big-token 0 511)))
      (term-list '(#\space #\newline *eof*)))

  (call-with-input-string big-token
    (lambda (port)
      (let ((token (next-token '(#\space) term-list "" port)))
	(assert (equal? token big-token) (eof-object? (peek-char port))))))

  (call-with-input-string big-token1
    (lambda (port)
      (let ((token (next-token '() term-list "" port)))
	(assert (equal? token big-token1) (eof-object? (read-char port))))))

  (call-with-input-string (string-append "     " big-token "     ")
    (lambda (port)
      (let ((token (next-token '(#\space) term-list "comment" port)))
	(assert (equal? token big-token) 
	  (memv (peek-char port) term-list)))))

  (call-with-input-string (string-append big-token1 (string #\newline))
    (lambda (port)
      (let ((token (next-token '(#\space) term-list "" port)))
	(assert (equal? token big-token1) 
	  (memv (peek-char port) term-list)))))

  (call-with-input-string (string-append big-token)
    (lambda (port)
      (let ((token (next-token-of 
		     (lambda (c) (and (not (eof-object? c)) c))  port)))
	(assert (equal? token big-token) 
	  (eof-object? (peek-char port))))))

  (call-with-input-string (string-append big-token1 (string #\newline))
    (lambda (port)
      (let ((token (next-token-of (string->list "a0123456789") port)))
	(assert (equal? token big-token1) 
	  (memv (peek-char port) term-list)))))
)

(cerr nl "Verifying tokenizing of the input stream: next-token-of ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (next-token-of '(#\a #\space #\x))
    `("" . ,eof))
  (expect-parse-result "d" (next-token-of '(#\a #\space #\x))
    '("" . #\d))
  (expect-parse-result "a   xx " (next-token-of '(#\a #\space #\x))
    `("a   xx " . ,eof))
  (expect-parse-result (s "a   xx " 'lf)
		       (next-token-of '(#\a #\space #\x) (current-input-port))
		       '("a   xx " . #\newline))
  (expect-parse-result (s "a  " 'cr " xx ") (next-token-of '(#\a #\space #\x))
    (cons "a  " (integer->char 13)))
  (expect-parse-result (s 'lf "a  " 'cr " xx ")
		       (next-token-of '(#\a #\space #\x))
    '("" . #\newline))

  (expect-parse-result ""
    (next-token-of (lambda (c) (and (not (eof-object? c)) c)))
    `("" . ,eof))
  (expect-parse-result (s "123" 'lf 'cr 0 "!")
    (next-token-of (lambda (c) (and (not (eof-object? c)) c)))
    `(,(s "123" 'lf 'cr 0 "!") . ,eof))

  (let ((down-pred
        (lambda (c)
          (cond ((eof-object? c) #f)
            ((char-alphabetic? c) (char-downcase c))
            (else #f)))))

    (expect-parse-result "" (next-token-of down-pred)
      `("" . ,eof))
    (expect-parse-result "12abc" (next-token-of down-pred)
      '("" . #\1))
    (expect-parse-result "abc12"
			 (next-token-of down-pred (current-input-port))
      '("abc" . #\1))
    (expect-parse-result "aB c12" (next-token-of down-pred)
      '("ab" . #\space))
    (expect-parse-result "XYZ" (next-token-of down-pred)
      `("xyz" . ,eof))
    )
)

(cerr nl "Verifying read-text-line ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (read-text-line)
    `(,eof . ,eof))
  (expect-parse-result "a 1 % xx" (read-text-line)
    `("a 1 % xx" . ,eof))
  (expect-parse-result (s 'lf) (read-text-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr) (read-text-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr 'lf) (read-text-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr 'cr 'lf) (read-text-line (current-input-port))
    (cons "" (integer->char 13)))
  (expect-parse-result (s 'lf 'lf) (read-text-line)
    '("" . #\newline))
  (expect-parse-result (s #\space 'lf 'cr 'lf) (read-text-line)
    (cons " " (integer->char 13)))
  (expect-parse-result (s " 12" 'lf "3" 'cr 'lf) (read-text-line)
    '(" 12" . #\3))
  (expect-parse-result (s " 12 " 'cr "3" 'cr 'lf) (read-text-line)
    '(" 12 " . #\3))
  (expect-parse-result (s " 12 " 'cr 'lf " 4" 'cr 'lf)
		       (read-text-line (current-input-port))
    '(" 12 " . #\space))
  (expect-parse-result (s " 12 " 'cr 'lf 'cr 'lf) (read-text-line)
    (cons " 12 " (integer->char 13)))
)

(cerr nl "Verifying read-string ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (read-string 1)
    `("" . ,eof))
  (expect-parse-result "" (read-string 0)
    `("" . ,eof))
  (expect-parse-result "1234" (read-string 0)
    '("" . #\1))
  (expect-parse-result "1234" (read-string -10)
    '("" . #\1))
  (expect-parse-result (s 'lf "1234 " 'cr) 
		       (read-string 1 (current-input-port))
		       (cons (s 'lf) #\1))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 3)
    (cons (s 'lf "12") #\3))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 7)
    (cons (s 'lf "1234 " 'cr) eof))
  (expect-parse-result (s 'lf "1234 " 'cr)
		       (read-string 8 (current-input-port))
    (cons (s 'lf "1234 " 'cr) eof))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 100)
    (cons (s 'lf "1234 " 'cr) eof))
)


(cerr nl "Verifying find-string-from-port? ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "bacacabd"
    (find-string-from-port? "acab" (current-input-port) 100)
    '(7 . #\d))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acab" (current-input-port))
    '(7 . #\d))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acad" (current-input-port) 100)
    `(#f . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acad" (current-input-port))
    `(#f . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 5)
    '(#f . #\a))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 9)
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port))
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 8)
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "be" (current-input-port) 20)
    `(#f . ,eof))
)

(cerr nl nl "All tests passed" nl)
