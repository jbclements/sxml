#lang racket/base
(require racket/port)
(provide find-string-from-port?)

; -- Function: find-string-from-port? STR IN-PORT
;    Looks for a string STR within the input port IN-PORT
;    When the STR is found, the function returns the number of
;    characters it has read from the port, and the port is set
;    to read the first char after that (that is, after the STR)
;    The function returns #f when the string wasn't found
; Note the function reads the port *STRICTLY* sequentially, and does not
; perform any buffering. So the function can be used even if the port is open
; on a pipe or other communication channel.

(define (find-string-from-port? str input-port)
  (let ([output-counting-port (open-output-nowhere)])
    ;; Could just use file-position, but that returns byte count, not char count
    ;; Could use output string port, but don't need contents (memory use)
    (port-count-lines-enabled output-counting-port)
    (let ([result
           (regexp-match? (regexp-quote str)
                          input-port
                          0 #f
                          output-counting-port)])
      (and result
           (let-values ([(_line _col pos) (port-next-location output-counting-port)])
             (sub1 pos))))))
