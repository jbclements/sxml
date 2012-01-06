#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/port call-with-input-string)
         (only-in racket/pretty [pretty-write pp])
         (only-in racket/list [add-between list-intersperse])
         (only-in racket/base [quote racket:quote])
         srfi/13/string
         rackunit
         (except-in "../ssax/myenv.ss" assert)
         "../ssax/parse-error.rkt"
         "../ssax/input-parse.rkt"
         "../ssax/SSAX-code.rkt")
(provide ssax-tests)

#|
Current status: all tests pass; some print warnings.
|#

;; The tests in this file were originally written to work with or
;; without reader case sensitivity for symbols. The form '<string> is
;; used to indicate a *symbol* with the case as written in <string>.
(define-syntax (quote stx)
  (syntax-case stx ()
    [(quote s)
     (let ([d (syntax-e #'s)])
       (cond [(string? d) #`(racket:quote #,(string->symbol d))]
             [else #'(racket:quote s)]))]))
(define equal_? equal?)

;; ryanc: catch exceptions (simplified from original lib/catch-error.scm)
(define-syntax failed?
  (syntax-rules ()
    ((failed? . stmts)
     (with-handlers ([exn? (lambda (e) #t)])
       (let () . stmts)
       #f))))

;; ryanc: specialize assert for better error messages
(define-syntax assert
  (syntax-rules ()
    [(assert (=? expected actual))
     (check =? actual expected)]
    [(assert e ...)
     (check-not-false (and e ...))]))

;; ----

(define ssax-tests
  (test-suite "SSAX"

(test-case "parsing names"
  (assert (eq? '_
               (call-with-input-string "_" ssax:read-NCName)))
  (assert (eq? '_
               (call-with-input-string "_" ssax:read-QName)))
  (assert (eq? (string->symbol "_abc_")
               (call-with-input-string "_abc_;" ssax:read-NCName)))
  (assert (eq? (string->symbol "_abc_")
               (call-with-input-string "_abc_;" ssax:read-QName)))
  (assert (eq? (string->symbol "_a.b")
               (call-with-input-string "_a.b " ssax:read-QName)))
  (assert (equal? (cons (string->symbol "_a.b") (string->symbol "d.1-ef-"))
                  (call-with-input-string "_a.b:d.1-ef-;" ssax:read-QName)))
  (assert (equal? (cons (string->symbol "a") (string->symbol "b"))
                  (call-with-input-string "a:b:c" ssax:read-QName)))
  (assert (failed? (call-with-input-string ":abc" ssax:read-NCName)))
  (assert (failed? (call-with-input-string "1:bc" ssax:read-NCName)))
  )

(test-case "name-compare"
  (assert (eq? '= (name-compare 'ABC 'ABC)))
  (assert (eq? '< (name-compare 'ABC 'ABCD)))
  (assert (eq? '> (name-compare 'XB 'ABCD)))
  (assert (eq? '> (name-compare '(HTML . PRE) 'PRE)))
  (assert (eq? '< (name-compare 'HTML '(HTML . PRE))))
  (assert (eq? '= (name-compare '(HTML . PRE) '(HTML . PRE))))
  (assert (eq? '< (name-compare '(HTML . PRE) '(XML . PRE))))
  (assert (eq? '> (name-compare '(HTML . PRE) '(HTML . P))))
  (assert (eq? '< (name-compare '(HTML . PRE) ssax:largest-unres-name)))
  (assert (eq? '< (name-compare '(ZZZZ . ZZZ) ssax:largest-unres-name)))
  (assert (eq? '> (name-compare ssax:largest-unres-name '(ZZZZ . ZZZ) )))
  )

(test-case "PI parsing"
  (assert (equal? "p1 content "
                  (call-with-input-string "<?pi1  p1 content ?>"
                    (lambda (port)
                      (ssax:read-markup-token port)
                      (ssax:read-pi-body-as-string port)))))
  (assert (equal? "pi2? content? ?"
                  (call-with-input-string "<?pi2 pi2? content? ??>"
                    (lambda (port)
                      (ssax:read-markup-token port)
                      (ssax:read-pi-body-as-string port)))))
  )

(let ()
  (define (consumer fragment foll-fragment seed)
    (cons* (if (equal? foll-fragment (string #\newline))
               " NL" foll-fragment) fragment seed))
  (define-check (test str expected-result)
    (let ((result
           (reverse
            (let ([port (open-input-string str)])
              (ssax:read-cdata-body port consumer '())))))
      (check-equal? result expected-result)))
  (test-case "cdata body parsing"
    (test "]]>" '())
    (test "abcd]]>" '("abcd" ""))
    (test "abcd]]]>" '("abcd" "" "]" ""))
    (test "abcd]]]]>" '("abcd" "" "]" "" "]" ""))
    (test "abcd]]]]]>" '("abcd" "" "]" "" "]" "" "]" ""))
    (test "abcd]]]a]]>" '("abcd" "" "]" "" "]]" "" "a" ""))
    (test "abc\r\ndef\n]]>" '("abc" " NL" "def" " NL"))
    (test "\r\n\r\n]]>" '("" " NL" "" " NL"))
    (test "\r\n\r\na]]>" '("" " NL" "" " NL" "a" ""))
    (test "\r\r\r\na]]>" '("" " NL" "" " NL" "" " NL" "a" ""))
    (test "abc&!!!]]>" '("abc" "&" "" "" "!!!" ""))
    (test "abc]]&gt;&gt&amp;]]]&gt;and]]>"
          '("abc" "" "]]" "" "" ">" "" "&" "gt" "" "" "&" "amp" "" ";" "" "]" ""
            "]]" "" "" ">" "and" ""))
    ))

(let ()
  (define (f str decl-entities)
    (ssax:read-attributes (open-input-string str) decl-entities))
  (define-check (test str decl-entities expected-res)
    (let ((result (f str decl-entities)))
      (check-equal? result expected-res)))
  (define-check (test-fails str decl-entities)
    (assert (failed? (f str decl-entities))))
  (test-case "read-attributes"
    (test "" '() '())
    (test "href='http://a\tb\r\n\r\n\nc'" '()
	  `((,(string->symbol "href") . "http://a b   c")))
    (test "href='http://a\tb\r\r\n\rc'" '()
	  `((,(string->symbol "href") . "http://a b   c")))
    (test "_1 ='12&amp;' _2= \"\r\n\t12&#10;3\">" '()
	  `((_1 . "12&") (_2 . "  12\n3")))
    (test "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&ent;34' />" 
	  '((ent . "&lt;xx&gt;"))
	  `((,(string->symbol "Abc") . "<&>\n")
	    (,(string->symbol "Next") . "12<xx>34")))
    (test "\tAbc='&lt;&amp;&gt;&#x0d;'\nNext='12&ent;34' />" 
	  '((ent . "&lt;xx&gt;"))
	  `((,(string->symbol "Abc") . "<&>\r")
	    (,(string->symbol "Next") . "12<xx>34")))
    (test "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&en;34' />" 
	  `((en . ,(lambda () (open-input-string "&quot;xx&apos;"))))
	  `((,(string->symbol "Abc") . "<&>\n")
	    (,(string->symbol "Next") . "12\"xx'34")))
    (test "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&ent;34' />" 
	  '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&amp;"))
	  `((,(string->symbol "Abc") . "<&>\n")
	    (,(string->symbol "Next") . "12<&T;>34")))
    (test-fails "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&ent;34' />"
                '((ent . "<&ent1;T;&gt;") (ent1 . "&amp;")))
    (test-fails "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&ent;34' />"
                '((ent . "&lt;&ent;T;&gt;") (ent1 . "&amp;")))
    (test-fails "\tAbc='&lt;&amp;&gt;&#x0A;'\nNext='12&ent;34' />"
                '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&ent;")))
    (test "html:href='http://a\tb\r\n\r\n\nc'" '()
	  `(((,(string->symbol "html") . ,(string->symbol "href"))
	     . "http://a b   c")))
    (test "html:href='ref1' html:src='ref2'" '()
	  `(((,(string->symbol "html") . ,(string->symbol "href"))
	     . "ref1")
	    ((,(string->symbol "html") . ,(string->symbol "src"))
	     . "ref2")))
    (test "html:href='ref1' xml:html='ref2'" '()
	  `(((,(string->symbol "html") . ,(string->symbol "href"))
	     . "ref1")
	    ((,ssax:Prefix-XML . ,(string->symbol "html"))
	     . "ref2")))
    (test-fails "html:href='ref1' html:href='ref2'" '())
    (test-fails "html:href='<' html:href='ref2'" '())
    (test-fails "html:href='ref1' html:href='&ref2;'" '())))

(let* ((namespaces
	'((HTML UHTML . URN-HTML)
	  (HTML UHTML-1 . URN-HTML)
	  (A    UHTML . URN-HTML)))
       (namespaces-def
        (cons
         '(*DEFAULT* DEF . URN-DEF) namespaces))
       (namespaces-undef
        (cons
         '(*DEFAULT* #f . #f) namespaces-def))
       (port (current-input-port)))
  (test-case "resolve-name"
    (assert (equal? 'ABC 
                    (ssax:resolve-name port 'ABC namespaces #t)))
    (assert (equal? '(DEF . ABC)
                    (ssax:resolve-name port 'ABC namespaces-def #t)))
    (assert (equal? 'ABC
                    (ssax:resolve-name port 'ABC namespaces-def #f)))
    (assert (equal? 'ABC
                    (ssax:resolve-name port 'ABC namespaces-undef #t)))
    (assert (equal? '(UHTML . ABC)
                    (ssax:resolve-name port '(HTML . ABC) namespaces-def #t)))
    (assert (equal? '(UHTML . ABC)
                    (ssax:resolve-name port '(HTML . ABC) namespaces-def #f)))
    (assert (equal? `(,ssax:Prefix-XML . space)
                    (ssax:resolve-name port 
                                       `(,(string->symbol "xml") . space)
                                       namespaces-def #f)))
    (assert (failed?
             (ssax:resolve-name port '(XXX . ABC) namespaces-def #f)))
    ))

(let* ((urn-a (string->symbol "urn:a"))
       (urn-b (string->symbol "urn:b"))
       (urn-html (string->symbol "http://w3c.org/html"))
       (namespaces
        `((#f UHTML . ,urn-html)
          (A  UA . ,urn-a)))
       (test
        (lambda (tag-head-name elems str)
          (call-with-input-string str
            (lambda (port)
              (call-with-values
                  (lambda ()
                    (ssax:complete-start-tag
                     (call-with-input-string tag-head-name ssax:read-QName)
                     port
                     elems '() namespaces))
                list))))))
  (test-case "read-QName"
    ;; First test with no validation of elements
    ;;(test "TAG1" #f "")
    (assert (equal? `(TAG1 () ,namespaces ANY)
                    (test "TAG1" #f ">")))
    (assert (equal? `(TAG1 () ,namespaces EMPTY-TAG)
                    (test "TAG1" #f "/>")))
    (assert (equal? `(TAG1 ((HREF . "a")) ,namespaces EMPTY-TAG)
                    (test "TAG1" #f "HREF='a'/>")))
    (assert (equal? `((UA . TAG1) ((HREF . "a"))
                      ,(cons `(*DEFAULT* UA . ,urn-a) namespaces) ANY)
                    (test "TAG1" #f "HREF='a' xmlns='urn:a'>")))
    (assert (equal? `(TAG1 ((HREF . "a"))
                      ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
                    (test "TAG1" #f "HREF='a' xmlns=''>")))
    (assert (failed? (test "UA:TAG1" #f "HREF='a' xmlns=''/>")))
    (assert (equal? `((UA . TAG1) (((UA . HREF) . "a"))
                      ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
                    (test "A:TAG1" #f "A:HREF='a' xmlns=''>")))
    (assert (equal? `((UA . TAG1) (((UA . HREF) . "a"))
                      ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) ANY)
                    (test "A:TAG1" #f "A:HREF='a' xmlns='urn:b'>")))
    (assert (failed? (test "B:TAG1" #f "A:HREF='a' xmlns:b=''/>")))
    (assert (equal? `((,urn-b . TAG1) (((UA . HREF) . "a"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) ANY)
                    (test "B:TAG1" #f "A:HREF='a' xmlns:B='urn:b'>")))
    (assert (equal? `((,urn-b . TAG1) (((UA . HREF) . "a")
                                          ((,urn-b . SRC) . "b"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) ANY)
                    (test "B:TAG1" #f 
                          "B:SRC='b' A:HREF='a' xmlns:B='urn:b'>")))
    (assert (equal? `((,urn-b . TAG1) (((UA . HREF) . "a")
                                          ((,urn-b . HREF) . "b"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) ANY)
                    (test "B:TAG1" #f 
                          "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:b'>")))
    ;; must be an error! Duplicate attr
    (assert (failed? (test "B:TAG1" #f
                           "HREF=\"b\" HREF='a' xmlns:B='urn:a'/>")))
    ;; must be an error! Duplicate attr after ns expansion
    (assert (failed? (test "B:TAG1" #f 
                           "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:a'/>")))
    (assert (equal? `((UA . TAG1) ((HREF . "a")
                                         ((UA . HREF) . "b"))
                      ,(cons `(*DEFAULT* UA . ,urn-a) namespaces) ANY)
                    (test "TAG1" #f 
                          "A:HREF=\"b\" HREF='a' xmlns='urn:a'>")))
    (assert (equal? `(TAG1 (((UHTML . HREF) . "a")
                               ((,urn-b . HREF) . "b"))
                      ,(append `((HTML UHTML . ,urn-html)
                                 (B ,urn-b . ,urn-b))
                               namespaces) ANY)
                    (test "TAG1" #f 
                          "B:HREF=\"b\" xmlns:B='urn:b' xmlns:HTML='http://w3c.org/html' HTML:HREF='a' >")))
    ;; Now test the validating parsing
    ;; No decl for tag1
    (assert (failed? (test "TAG1" '((TAG2 ANY ()))
                           "B:HREF='b' xmlns:B='urn:b'>")))
    ;; No decl for HREF elem
    (assert (failed?
             (test "TAG1" '((TAG1 ANY ((HREF1 CDATA IMPLIED #f))))
                   "B:HREF='b' xmlns:B='urn:b'>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces EMPTY-TAG)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
                          "HREF='b'/>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
                          "HREF='b'>")))
    ;; Req'd attribute not given error
    (assert (failed? 
             (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
                   ">")))
    ;; Wrong content-type of the attribute
    (assert (failed? 
             (test "TAG1" '((TAG1 PCDATA ((HREF ("c") REQUIRED #f))))
                   "HREF='b'>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF ("c" "b") IMPLIED #f))))
                          "HREF='b'>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED "c"))))
                          "HREF='b'>")))
    ;; Bad fixed attribute
    (assert (failed? 
             (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "c"))))
                   "HREF='b'>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "b"))))
                          "HREF='b'>")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "b")))) ">")))
    (assert (equal? `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED "b")))) ">")))
    (assert (equal? `(TAG1 () ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED #f)))) ">")))
    ;; Undeclared attr
    (assert (failed? 
             (test "TAG1"
                   '((TAG1 PCDATA (((A . HREF) CDATA IMPLIED "c"))))
                   "HREF='b'>")))
    (assert (equal? `(TAG1 ((HREF . "b") ((UA . HREF) . "c"))
                      ,namespaces PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                                                    ((A . HREF) CDATA IMPLIED "c"))))
                          "HREF='b'>")))
    (assert (equal? `((UA . TAG1)
                      ((HREF . "b") ((UA . HREF) . "c"))
                      ,namespaces PCDATA)
                    (test "A:TAG1" '(((A . TAG1) PCDATA
                                      ((HREF NMTOKEN REQUIRED #f)
                                       ((A . HREF) CDATA IMPLIED "c"))))
                          "HREF='b'>")))
    (assert (equal? `((,urn-b . TAG1) ((HREF . "b"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) PCDATA)
                    (test "B:TAG1"
                          '(((B . TAG1)
                             PCDATA
                             ((HREF CDATA REQUIRED #f)
                              ((xmlns . B) CDATA IMPLIED "urn:b"))))
                          "HREF='b'>")))
    (assert (equal? `((,urn-b . TAG1) (((,urn-b . HREF) . "b"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) PCDATA)
                    (test "B:TAG1" '(((B . TAG1) PCDATA
                                      (((B . HREF) CDATA REQUIRED #f)
                                       ((xmlns . B) CDATA IMPLIED "urn:b"))))
                          "B:HREF='b'>")))
    (assert (equal? `((,urn-b . TAG1) ((HREF . "b"))
                      ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                                                    (xmlns CDATA IMPLIED "urn:b"))))
                          "HREF='b'>")))
    ;; xmlns not declared
    (assert (equal? `((,urn-b . TAG1) ((HREF . "b"))
                      ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
                    (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                                                    )))
                          "HREF='b' xmlns='urn:b'>")))
    ;; xmlns:B not declared
    (assert (equal? `((,urn-b . TAG1) (((,urn-b . HREF) . "b"))
                      ,(cons `(B ,urn-b . ,urn-b) namespaces) PCDATA)
                    (test "B:TAG1" '(((B . TAG1) PCDATA
                                      (((B . HREF) CDATA REQUIRED #f)
                                       )))
                          "B:HREF='b' xmlns:B='urn:b'>")))
    ))


(letrec
  ((a-tag (make-xml-token 'START (string->symbol "BR")))
   (a-ref (make-xml-token 'ENTITY-REF (string->symbol "lt")))
   (eof-object (lambda () eof-object)) ; a unique value
   (str-handler (lambda (fragment foll-fragment seed)
                  (if (string-null? foll-fragment) (cons fragment seed)
                      (cons* foll-fragment fragment seed)))))
  (define-check (test str expect-eof? expected-data expected-token)
    (let*-values
        (((seed token)
          (ssax:read-char-data (open-input-string str) expect-eof? str-handler '()))
         ((result) (reverse seed)))
      (assert (equal? result expected-data))
      (assert (if (eq? expected-token eof-object)
                  (eof-object? token)
                  (equal? token expected-token)))))
  (test-case "read-char-data"
    (test "" #t '() eof-object)
    (assert (failed? (test "" #f '() eof-object)))
    (test "  " #t '("  ") eof-object)
    (test "<BR/>" #f '() a-tag)
    (test " <BR  />" #f '(" ") a-tag)

    (test " &lt;" #f '(" ") a-ref)
    (test " a&lt;" #f '(" a") a-ref)
    (test " a &lt;" #f '(" a ") a-ref)

    (test " <!-- comment--> a  a<BR/>" #f '(" " " a  a") a-tag)
    (test " <!-- comment-->\ra  a<BR/>" #f '(" " "" "\n" "a  a") a-tag)
    (test " <!-- comment-->\r\na  a<BR/>" #f '(" " "" "\n" "a  a") a-tag)
    (test " <!-- comment-->\r\na\t\r\r\na<BR/>" #f
          '(" " "" "\n" "a\t" "\n" "" "\n" "a") a-tag)
    (test "a<!-- comment--> a  a<BR/>" #f '("a" " a  a") a-tag)
    (test "&#x21;<BR/>" #f '("" "!") a-tag)
    (test "&#x21;\n<BR/>" #f '("" "!" "\n") a-tag)
    (test "\t&#x21;\n<BR/>" #f '("\t" "!" "\n") a-tag)
    (test "\t&#x21;\na a<BR/>" #f '("\t" "!" "\na a") a-tag)
    (test "\t&#x21;\ra a<BR/>" #f '("\t" "!" "" "\n" "a a") a-tag)
    (test "\t&#x21;\r\na a<BR/>" #f '("\t" "!" "" "\n" "a a") a-tag)

    (test " \ta &#x21;   b <BR/>" #f '(" \ta " "!" "   b ") a-tag)
    (test " \ta &#x20;   b <BR/>" #f '(" \ta " " " "   b ") a-tag)

    (test "<![CDATA[<]]><BR/>" #f '("<") a-tag)
    (test "<![CDATA[]]]><BR/>" #f '("]") a-tag)
    (test "\t<![CDATA[<]]><BR/>" #f '("\t" "<") a-tag)
    (test "\t<![CDATA[<]]>a b<BR/>" #f '("\t" "<" "a b") a-tag)
    (test "\t<![CDATA[<]]>  a b<BR/>" #f '("\t" "<" "  a b") a-tag)

    (test "\td <![CDATA[  <\r\r\n]]>  a b<BR/>" #f 
          '("\td " "  <" "\n" "" "\n" "  a b") a-tag)
    ))

#|
(pp (ssax:make-pi-parser ()))
(pp (ssax:make-pi-parser ((xml . (lambda (port target seed) seed)))))
(pp (ssax:make-pi-parser ((xml . (lambda (port target seed) seed))
                          (html . list)
                          (*DEFAULT* . ssax:warn))))
|#

(let ()
  (define (simple-parser str doctype-fn)
    (call-with-input-string str
      (lambda (port)
        ((ssax:make-parser
          NEW-LEVEL-SEED 
          (lambda (elem-gi attributes namespaces expected-content seed) '())

          FINISH-ELEMENT
          (lambda (elem-gi attributes namespaces parent-seed seed)
            (let ((seed (if (null? namespaces) (reverse seed)
                            (cons (list '*NAMESPACES* namespaces)
                                  (reverse seed)))))
              (let ((seed (if (attlist-null? attributes) seed
                              (cons 
                               (cons '@ 
                                (map (lambda (attr)
                                       (list (car attr) (cdr attr)))
                                     (attlist->alist attributes)))
                               seed))))
                (cons (cons elem-gi seed) parent-seed))))

          CHAR-DATA-HANDLER
          (lambda (string1 string2 seed)
            (if (string-null? string2) (cons string1 seed)
                (cons* string2 string1 seed)))

          DOCTYPE
          (lambda (port docname systemid internal-subset? seed)
            (when internal-subset?
              (ssax:warn port "Internal DTD subset is not currently handled ")
              (ssax:skip-internal-dtd port))
            (ssax:warn port "DOCTYPE DECL " docname " "
                       systemid " found and skipped")
            (doctype-fn docname seed))

          UNDECL-ROOT
          (lambda (elem-gi seed) (doctype-fn elem-gi seed)))
         port '()))))

  (define (dummy-doctype-fn elem-gi seed) (values #f '() '() seed))
  (define-check (test str doctype-fn expected)
    (let ((result (simple-parser str doctype-fn)))
      (assert (equal? result expected))))

  (test-case "simple parser"
    (test "<BR/>" dummy-doctype-fn '((BR)))
    (assert (failed? (test "<BR>" dummy-doctype-fn '())))
    (test "<BR></BR>" dummy-doctype-fn '((BR)))
    (assert (failed? (test "<BR></BB>" dummy-doctype-fn '())))

    (test "   <A HREF='URL'> link <I>itlink </I> &amp;amp;</A>"
          dummy-doctype-fn 
          '((A (@ (HREF "URL")) " link " (I "itlink ")
             " " "&" "amp;")))

    (test
     "   <A HREF='URL' xml:space='preserve'> link <I>itlink </I> &amp;amp;</A>" dummy-doctype-fn 
     '((A (@ (HREF "URL") ((xml . space) "preserve"))
        " link " (I "itlink ") " " "&" "amp;")))

    (test "   <A HREF='URL' xml:space='preserve'> link <I xml:space='default'>itlink </I> &amp;amp;</A>" dummy-doctype-fn
          '((A (@ (HREF "URL") ((xml . space) "preserve"))
             " link "
             (I (@ ((xml . space) "default")) "itlink ")
             " " "&" "amp;")))
    (test "<itemize><item>This   is item 1 </item>\n<!-- Just:a comment --><item>Item 2</item>\n </itemize>" dummy-doctype-fn 
          `((itemize (item "This   is item 1 ")
             "\n" (item "Item 2") "\n ")))
    (test " <P><![CDATA[<BR>\n<![CDATA[<BR>]]&gt;]]></P>"
          dummy-doctype-fn  `((P "<BR>" ,nl "<![CDATA[<BR>" "]]" "" ">")))

    (test " <P><![CDATA[<BR>\r<![CDATA[<BR>]]&gt;]]></P>"
          dummy-doctype-fn `((P "<BR>" ,nl "<![CDATA[<BR>" "]]" "" ">")))

    (test "<?xml version='1.0'?>\n\n<Reports TStamp='1'></Reports>"
          dummy-doctype-fn '((Reports (@ (TStamp "1")))))
    (test "\n<?PI xxx?><!-- Comment \n -\r-->\n<?PI1 zzz?><T/>" 
          dummy-doctype-fn '((T)))
    (test "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->\n<T/>"
          (lambda (elem-gi seed) (assert (equal? elem-gi 'T))
                  (values #f '() '() seed))
          '((T)))
    (test "<!DOCTYPE T PUBLIC '//EN/T' \"system1\" [ <!ELEMENT a 'aa'> ]>\n<?pi?><T/>" 
          (lambda (elem-gi seed) (assert (equal? elem-gi 'T))
                  (values #f '() '() seed))
          '((T)))
    (test "<BR/>"
          (lambda (elem-gi seed)
            (values '((BR EMPTY ())) '() '() seed)) '((BR)))
    (test "<BR></BR>"
          (lambda (elem-gi seed)
            (values '((BR EMPTY ())) '() '() seed)) '((BR)))
    (assert (failed? (test "<BR>aa</BR>"
                           (lambda (elem-gi seed)
                             (values '((BR EMPTY ())) '() '() seed)) '())))
    (test "<BR>aa</BR>"
          (lambda (elem-gi seed)
            (values '((BR PCDATA ())) '() '() seed)) '((BR "aa")))
    (assert (failed? (test "<BR>a<I>a</I></BR>"
                           (lambda (elem-gi seed)
                             (values '((BR PCDATA ())) '() '() seed)) '())))
    (test "<BR>a<I>a</I></BR>"
          (lambda (elem-gi seed)
            (values '((BR ANY ()) (I PCDATA ())) '() '() seed))
	  '((BR "a" (I "a"))))

    (test "<DIV>Example: \"&example;\"</DIV>"
          (lambda (elem-gi seed)
            (values #f '((example . "<P>An    ampersand (&#38;) may   be escaped numerically (&#38;#38;) or with a general entity (&amp;amp;).</P>")) '() seed))
          '((DIV "Example: \""
             (P "An    ampersand (" "&" ") may   be escaped numerically (" "&" "#38;) or with a general entity (" "&" "amp;).") "\"")))
    (test "<DIV>Example: \"&example;\" <P/></DIV>"
          (lambda (elem-gi seed)
            (values #f '((quote . "<I>example:</I> ex")
                         (example . "<Q>&quote;!</Q>?")) '() seed))
	  '((DIV "Example: \"" (Q (I "example:") " ex" "!") "?"
             "\" "  (P))))
    (assert (failed?
             (test "<DIV>Example: \"&example;\" <P/></DIV>"
                   (lambda (elem-gi seed)
                     (values #f '((quote . "<I>example:")
                                  (example . "<Q>&quote;</I>!</Q>?")) '() seed))
                   '())))

    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          (lambda (elem-gi seed)
            (values #f '() '() seed))
          '(((URI1 . DIV) (@ (B "B") ((URI1 . B) "A"))
             (*NAMESPACES* ((A URI1 . URI1)
                            (*DEFAULT* URI1 . URI1)))
             ((URI1 . P)
              (*NAMESPACES* ((*DEFAULT* #f . #f) (A URI1 . URI1)
                             (*DEFAULT* URI1 . URI1)))
              (BR
               (*NAMESPACES* ((*DEFAULT* #f . #f)
                              (A URI1 . URI1)
                              (*DEFAULT* URI1 . URI1))))))))
    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          (lambda (elem-gi seed)
            (values #f '() '((#f UA . URI1)) seed))
          '(((UA . DIV) (@ (B "B") ((UA . B) "A"))
             (*NAMESPACES* ((A UA . URI1)
                            (*DEFAULT* UA . URI1) (#f UA . URI1)))
             ((UA . P)
              (*NAMESPACES* ((*DEFAULT* #f . #f) (A UA . URI1)
                             (*DEFAULT* UA . URI1) (#f UA . URI1)))
              (BR
               (*NAMESPACES* ((*DEFAULT* #f . #f) (A UA . URI1)
                              (*DEFAULT* UA . URI1)
                              (#f UA . URI1))))))))
    ;; uniqattr should fail
    (assert (failed?
             (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                   (lambda (elem-gi seed)
                     (values 
                      `((DIV ANY ((B CDATA IMPLIED #f)
                                     ((A . B) CDATA IMPLIED #f)
                                     ((C . B) CDATA IMPLIED "xx")
                                     ((xmlns . C) CDATA IMPLIED "URI1")
                                     ))
                        ((A . P) ANY ()) (BR EMPTY ()))
                      '() '((#f UA . URI1)) seed))
                   '())))
    ;; prefix C undeclared
    (assert (failed?
             (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                   (lambda (elem-gi seed)
                     (values 
                      '((DIV ANY ((B CDATA IMPLIED #f)
                                     (xmlns  CDATA IMPLIED "URI1")
                                     ((A . B) CDATA IMPLIED #f)
                                     ((C . B) CDATA IMPLIED "xx")
                                     ))
                        ((A . P) ANY ()) (BR EMPTY ()))
                      '() '((#f UA . URI1)) seed))
                   '())))
    ;; contradiction to xmlns declaration
    (assert (failed?
             (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                   (lambda (elem-gi seed)
                     (values 
                      '((DIV ANY ((B CDATA IMPLIED #f)
                                     (xmlns  CDATA FIXED "URI2")
                                     ((A . B) CDATA IMPLIED #f)
                                     ))
                        ((A . P) ANY ()) (BR EMPTY ()))
                      '() '((#f UA . URI1)) seed))
                   '())))

    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          (lambda (elem-gi seed)
            (values 
             '((DIV ANY ((B CDATA IMPLIED #f)
                            (xmlns  CDATA FIXED "URI1")
                            ((A . B) CDATA IMPLIED #f)
                            ))
               ((A . P) ANY ()) (BR EMPTY ()))
             '() '((#f UA . URI1)) seed))
          '(((UA . DIV) (@ (B "B") ((UA . B) "A"))
             (*NAMESPACES* ((*DEFAULT* UA . URI1)
                            (A UA . URI1) (#f UA . URI1)))
             ((UA . P)
              (*NAMESPACES* ((*DEFAULT* #f . #f) 
                             (*DEFAULT* UA . URI1)
                             (A UA . URI1) (#f UA . URI1)))
              (BR
               (*NAMESPACES* ((*DEFAULT* #f . #f) (*DEFAULT* UA . URI1)
                              (A UA . URI1) (#f UA . URI1))))))))

    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          (lambda (elem-gi seed)
            (values 
             '((DIV ANY ((B CDATA IMPLIED #f)
                            ((A . B) CDATA IMPLIED #f)
                            ((C . B) CDATA IMPLIED "xx")
                            ((xmlns . C) CDATA IMPLIED "URI2")
                            ))
               ((A . P) ANY ()) (BR EMPTY ()))
             '() '((#f UA . URI1)) seed))
          '(((UA . DIV) (@ (B "B") ((UA . B) "A")
                                 ((URI2 . B) "xx"))
             (*NAMESPACES* ((*DEFAULT* UA . URI1)
                            (A UA . URI1)
                            (C URI2 . URI2)
                            (#f UA . URI1)))
             ((UA . P)
              (*NAMESPACES* ((*DEFAULT* #f . #f) (*DEFAULT* UA . URI1)
                             (A UA . URI1)
                             (C URI2 . URI2) (#f UA . URI1)))
              (BR 
               (*NAMESPACES* ((*DEFAULT* #f . #f)
                              (*DEFAULT* UA . URI1)
                              (A UA . URI1)
                              (C URI2 . URI2)
                              (#f UA . URI1))))))))
    ))

(let ()
  (define-check (test str namespace-assig expected-res)
    (let ((result (ssax:xml->sxml (open-input-string str) namespace-assig)))
      (assert (equal_? result expected-res))))
  (test-case "ssax:xml->sxml"
    (test " <BR/>" '() '(*TOP* (BR)))
    (test "<BR></BR>" '() '(*TOP* (BR)))
    (test " <BR CLEAR='ALL'\nCLASS='Class1'/>" '()
	  '(*TOP* (BR (@ (CLEAR "ALL") (CLASS "Class1")))))
    (test "   <A HREF='URL'>  link <I>itlink </I> &amp;amp;</A>" '()
	  '(*TOP* (A (@ (HREF "URL")) "  link " (I "itlink ") " &amp;")))
    (test "   <A HREF='URL' xml:space='preserve'>  link <I>itlink </I> &amp;amp;</A>"
          '()
	  '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
		     "  link " (I "itlink ") " &amp;")))
    (test "   <A HREF='URL' xml:space='preserve'>  link <I xml:space='default'>itlink </I> &amp;amp;</A>" '()
	  '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
		     "  link " (I (@ (xml:space "default"))
				  "itlink ") " &amp;")))
    (test " <P><?pi1  p1 content ?>?<?pi2 pi2? content? ??></P>" '()
	  '(*TOP* (P (*PI* pi1 "p1 content ") "?"
		     (*PI* pi2 "pi2? content? ?"))))
    (test " <P>some text <![CDATA[<]]>1\n&quot;<B>strong</B>&quot;\r</P>"
	  '()
	  `(*TOP* (P "some text <1\n\""
                     (B "strong") "\"\n")))
    (test " <P><![CDATA[<BR>\n<![CDATA[<BR>]]&gt;]]></P>" '()
	  `(*TOP* (P "<BR>\n<![CDATA[<BR>]]>")))
    ;; (test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
    ;;       '(*TOP* (T1 (T2 "it's\nand   that\n") "\n\n\n")))
    (test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
	  `(*TOP* (T1 (T2 "it's\nand   that\n"))))
    (test "<T1><T2>it&apos;s\rand   that\n</T2>\r\n\r\n\n</T1>" '()
	  `(*TOP* (T1 (T2 "it's\nand   that\n"))))
    (test "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->\n<T/>" '()
	  '(*TOP* (T)))
    (test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
	  '(*TOP* (*PI* xml "version='1.0'")
                  (WEIGHT (@ (unit "pound"))
                          (NET (@ (certified "certified")) " 67 ")
                          (GROSS " 95 "))
		  ))
    ;;     (test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
    ;; 	  '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
    ;;                "\n" (NET (@ (certified "certified")) " 67 ")
    ;;                "\n" (GROSS " 95 ") "\n")
    ;; 		  ))
    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          '()
	  '(*TOP* (URI1:DIV (@ (URI1:B "A") (B "B")) (URI1:P (BR)))))
    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
          '((UA . "URI1"))
	  '(*TOP* (@ (*NAMESPACES* (UA "URI1")))
		  (UA:DIV (@ (UA:B "A") (B "B")) (UA:P (BR)))))

    ;; A few tests from XML Namespaces Recommendation
    (test (string-append
	   "<x xmlns:edi='http://ecommerce.org/schema'>"
           "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
           "<lineItem edi:taxClass='exempt'>Baby food</lineItem>" nl
           "</x>") '()
	   '(*TOP* 
	     (x (lineItem
		 (@ (http://ecommerce.org/schema:taxClass "exempt"))
                 "Baby food"))))
    (test (string-append 
	   "<x xmlns:edi='http://ecommerce.org/schema'>"
           "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
           "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
           "</x>") '((EDI . "http://ecommerce.org/schema"))
	   '(*TOP*
	     (@ (*NAMESPACES* (EDI "http://ecommerce.org/schema")))
	     (x (lineItem
		 (@ (EDI:taxClass "exempt"))
                 "Baby food"))))
    
    (test (string-append
	   "<bk:book xmlns:bk='urn:loc.gov:books' "
           "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	   "<bk:title>Cheaper by the Dozen</bk:title>"
           "<isbn:number>1568491379</isbn:number></bk:book>")
	  '()
	  '(*TOP* (urn:loc.gov:books:book
		   (urn:loc.gov:books:title "Cheaper by the Dozen")
		   (urn:ISBN:0-395-36341-6:number "1568491379"))))

    (test (string-append
	   "<!-- initially, the default namespace is 'books' -->"
           "<book xmlns='urn:loc.gov:books' "
           "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
           "<title>Cheaper by the Dozen</title>"
           "<isbn:number>1568491379</isbn:number>"
	   "<notes>"
	   "<!-- make HTML the default namespace for some commentary -->"
	   "<p xmlns='urn:w3-org-ns:HTML'>"
	   "This is a <i>funny</i> book!"
           "</p>"
           "</notes>"
           "</book>") '()
           '(*TOP* (urn:loc.gov:books:book
                    (urn:loc.gov:books:title "Cheaper by the Dozen")
                    (urn:ISBN:0-395-36341-6:number "1568491379")
                    (urn:loc.gov:books:notes
                     (urn:w3-org-ns:HTML:p 
                      "This is a " (urn:w3-org-ns:HTML:i "funny")
                      " book!")))))

    (test (string-append
	   "<Beers>"
           "<!-- the default namespace is now that of HTML -->"
           "<table xmlns='http://www.w3.org/TR/REC-html40'>"
           "<th><td>Name</td><td>Origin</td><td>Description</td></th>"
           "<tr>"
           "<!-- no default namespace inside table cells -->"
           "<td><brandName xmlns=\"\">Huntsman</brandName></td>"
           "<td><origin xmlns=''>Bath, UK</origin></td>"
           "<td>"
           "<details xmlns=''><class>Bitter</class><hop>Fuggles</hop>"
           "<pro>Wonderful hop, light alcohol, good summer beer</pro>"
           "<con>Fragile; excessive variance pub to pub</con>"
           "</details>"
	   "</td>"
           "</tr>"
           "</table>"
           "</Beers>")
          '((html . "http://www.w3.org/TR/REC-html40"))
          '(*TOP*
            (@ (*NAMESPACES* (html "http://www.w3.org/TR/REC-html40")))
            (Beers (html:table
                    (html:th (html:td "Name")
                             (html:td "Origin")
                             (html:td "Description"))
                    (html:tr (html:td (brandName "Huntsman"))
                             (html:td (origin "Bath, UK"))
                             (html:td 
                              (details 
                               (class "Bitter")
                               (hop "Fuggles")
                               (pro "Wonderful hop, light alcohol, good summer beer")
                               (con "Fragile; excessive variance pub to pub"))))))))

    (test (string-append
           "<!-- 1 --><RESERVATION xmlns:HTML='http://www.w3.org/TR/REC-html40'>"
           "<!-- 2 --><NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
           "<!-- 3 --><SEAT CLASS='Y' HTML:CLASS=\"largeMonotype\">33B</SEAT>"
           "<!-- 4 --><HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>"
           "<!-- 5 --><DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
	  '((HTML . "http://www.w3.org/TR/REC-html40"))
	  '(*TOP*
	    (@ (*NAMESPACES* (HTML "http://www.w3.org/TR/REC-html40")))
            (RESERVATION
             (NAME (@ (HTML:CLASS "largeSansSerif")) "Layman, A")
             (SEAT (@ (HTML:CLASS "largeMonotype") (CLASS "Y")) "33B")
             (HTML:A (@ (HREF "/cgi-bin/ResStatus")) "Check Status")
             (DEPARTURE "1997-05-24T07:55:00+1"))))
    ; Part of RDF from the XML Infoset
    (test (string-concatenate/shared
           (list-intersperse
            '("<?xml version='1.0' encoding='utf-8' standalone='yes'?>"
              "<!-- this can be decoded as US-ASCII or iso-8859-1 as well,"
              "  since it contains no characters outside the US-ASCII repertoire -->"
              "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'"
              "         xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'"
              "          xmlns='http://www.w3.org/2001/02/infoset#'>"
              "<rdfs:Class ID='Boolean'/>"
              "<Boolean ID='Boolean.true'/>"
              "<Boolean ID='Boolean.false'/>"
              "<!--Info item classes-->"
              "<rdfs:Class ID='InfoItem'/>"
              "<rdfs:Class ID='Document' rdfs:subClassOf='#InfoItem'/>"
              "<rdfs:Class ID='Element' rdfs:subClassOf='#InfoItem'/>"
              "<rdfs:Class ID='Attribute' rdfs:subClassOf='#InfoItem'/>"
              "<rdfs:Class ID='InfoItemSet'
      rdfs:subClassOf='http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag'/>"
              "<rdfs:Class ID='AttributeSet' rdfs:subClassOf='#InfoItemSet'/>"
              "<!--Info item properties-->"
              "<rdfs:Property ID='allDeclarationsProcessed'>"
              "<rdfs:domain resource='#Document'/>"
              "<rdfs:range resource='#Boolean'/></rdfs:Property>"
              "<rdfs:Property ID='attributes'>"
              "<rdfs:domain resource='#Element'/>"
              "<rdfs:range resource='#AttributeSet'/>"
              "</rdfs:Property>"
              "</rdf:RDF>")
            (string #\newline)))
          '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
            (RDFS . "http://www.w3.org/2000/01/rdf-schema#")
            (ISET . "http://www.w3.org/2001/02/infoset#"))
          '(*TOP* (@ (*NAMESPACES*
                      (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                      (RDFS "http://www.w3.org/2000/01/rdf-schema#")
                      (ISET "http://www.w3.org/2001/02/infoset#")))
                  (*PI* xml "version='1.0' encoding='utf-8' standalone='yes'")
                  (RDF:RDF
                   (RDFS:Class (@ (ID "Boolean")))
                   (ISET:Boolean (@ (ID "Boolean.true")))
                   (ISET:Boolean (@ (ID "Boolean.false")))
                   (RDFS:Class (@ (ID "InfoItem")))
                   (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Document")))
                   (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Element")))
                   (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Attribute")))
                   (RDFS:Class
                    (@ (RDFS:subClassOf
                        "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
                       (ID "InfoItemSet")))
                   (RDFS:Class
                    (@ (RDFS:subClassOf "#InfoItemSet") (ID "AttributeSet")))
                   (RDFS:Property
                    (@ (ID "allDeclarationsProcessed"))
                    (RDFS:domain (@ (resource "#Document")))
                    (RDFS:range (@ (resource "#Boolean"))))
                   (RDFS:Property
                    (@ (ID "attributes"))
                    (RDFS:domain (@ (resource "#Element")))
                    (RDFS:range (@ (resource "#AttributeSet")))))))
	  
    ; Part of RDF from RSS of the Daemon News Mall
    (test (string-concatenate/shared
           (list-intersperse
            '("<?xml version='1.0'?><rdf:RDF "
              "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' "
              "xmlns='http://my.netscape.com/rdf/simple/0.9/'>"
              "<channel>"
              "<title>Daemon News Mall</title>"
              "<link>http://mall.daemonnews.org/</link>"
              "<description>Central source for all your BSD needs</description>"
              "</channel>"
              "<item>"
              "<title>Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95</title>"
              "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=880</link>"
              "</item>"
              "<item>"
              "<title>The Design and Implementation of the 4.4BSD Operating System $54.95</title>"
              "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=912&amp;category_id=1761</link>"
              "</item>"
              "</rdf:RDF>")
            (string #\newline)
            ))
          '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
            (RSS . "http://my.netscape.com/rdf/simple/0.9/")
            (ISET . "http://www.w3.org/2001/02/infoset#"))
          '(*TOP* (@ (*NAMESPACES*
                      (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                      (RSS "http://my.netscape.com/rdf/simple/0.9/")
                      (ISET "http://www.w3.org/2001/02/infoset#")))
                  (*PI* xml "version='1.0'")
                  (RDF:RDF (RSS:channel
                            (RSS:title "Daemon News Mall")
                            (RSS:link "http://mall.daemonnews.org/")
                            (RSS:description "Central source for all your BSD needs"))
                           (RSS:item
                            (RSS:title
                             "Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95")
                            (RSS:link
                             "http://mall.daemonnews.org/?page=shop/flypage&product_id=880"))
                           (RSS:item
                            (RSS:title
                             "The Design and Implementation of the 4.4BSD Operating System $54.95")
                            (RSS:link
                             "http://mall.daemonnews.org/?page=shop/flypage&product_id=912&category_id=1761")))))

    (test (string-concatenate/shared
           (list-intersperse 
            '("<Forecasts TStamp='958082142'>"
              "<TAF TStamp='958066200' LatLon='36.583, -121.850' BId='724915'"
              "  SName='KMRY, MONTEREY PENINSULA'>"
              "<VALID TRange='958068000, 958154400'>111730Z 111818</VALID>"
              "<PERIOD TRange='958068000, 958078800'>"
              "<PREVAILING>31010KT P6SM FEW030</PREVAILING>"
              "</PERIOD>"
              "<PERIOD TRange='958078800, 958104000' Title='FM2100'>"
              "<PREVAILING>29016KT P6SM FEW040</PREVAILING>"
              "</PERIOD>"
              "<PERIOD TRange='958104000, 958154400' Title='FM0400'>"
              "<PREVAILING>29010KT P6SM SCT200</PREVAILING>"
              "<VAR Title='BECMG 0708' TRange='958114800, 958118400'>VRB05KT</VAR>"
              "</PERIOD></TAF>"
              "</Forecasts>")
            (string #\newline)
            ))
	  '()
	  '(*TOP* (Forecasts
		   (@ (TStamp "958082142"))
		   (TAF (@ (TStamp "958066200")
			   (SName "KMRY, MONTEREY PENINSULA")
			   (LatLon "36.583, -121.850")
			   (BId "724915"))
                        (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
                        (PERIOD (@ (TRange "958068000, 958078800"))
                                (PREVAILING "31010KT P6SM FEW030"))
                        (PERIOD (@ (Title "FM2100") (TRange "958078800, 958104000"))
                                (PREVAILING "29016KT P6SM FEW040"))
                        (PERIOD (@ (Title "FM0400") (TRange "958104000, 958154400"))
                                (PREVAILING "29010KT P6SM SCT200")
                                (VAR (@ (Title "BECMG 0708")
                                        (TRange "958114800, 958118400"))
                                     "VRB05KT"))))))
    ))
))
