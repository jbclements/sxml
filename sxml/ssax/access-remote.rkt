#lang racket/base
(require net/url
         net/head
         racket/path
         srfi/13/string
         "errors-and-warnings.rkt")
(provide open-input-resource
         ar:resolve-uri-according-base
         ar:resource-type)

;; Uniform access to local and remote resources
;; Resolution for relative URIs in accordance with RFC 2396
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; Accessing (remote) resources

; Opens an input port for a resource
;  REQ-URI - a string representing a URI of the resource
; An input port is returned if there were no errors. In case of an error,
; the function returns #f and displays an error message as a side effect.
; Doesn't raise any exceptions.
;; ryanc: Why not?!
(define (open-input-resource req-uri)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (sxml:warn 'open-input-resource "~a: ~a"
                                req-uri (exn-message e))
                     #f)])
    (get-pure-port (string->url req-uri))))

;=========================================================================
; Determining resource type

; Determines the type of a resource
;  REQ-URI - a string representing a URI of the resource
; For a local resource, its type is determined by its file extension
; One of the following is returned:
;  #f - if the requested resource doesn't exist
;  'xml - for a resource that is an XML document
;  'html - for a resource that is an HTML document
;  'unknown - for any other resource type
(define (ar:resource-type req-uri)
  (cond [(string-prefix? "http://" req-uri)  ; HTTP scheme is used in REQ-URI
         (with-handlers ([exn:fail? (lambda (exn) #f)])
           (call/input-url (string->url req-uri) head-impure-port
             (lambda (port)
               (let* ([headers (purify-port port)]
                      [content-type (extract-field "content-type" headers)])
                 (cond [(not content-type) ;; no content-type specified
                        'unknown]
                       [(string-prefix? "text/xml" content-type)
                        'xml]
                       [(string-prefix? "text/html" content-type)
                        'html]
                       [(string-prefix? "text/plain" content-type)
                        'plain]
                       [else 'unknown])))))]
        [else  ; a local file
         (cond [(not (file-exists? req-uri))  ; file doesn't exist
                #f]
               [(assoc (filename-extension req-uri)
                       '((#"xml" . xml) (#"html" . html) (#"htm" . html)))
                => cdr]
               [else 'unknown])]))

;=========================================================================
; Working on absolute/relative URIs
; This section is based on RFC 2396

;-------------------------------------------------
; Resolves a relative URI with respect to the base URI

;  base-uri - base URI for the requiested one
; Returns the resolved URI
(define (ar:resolve-uri-according-base base-uri req-uri)
  (url->string (combine-url/relative (string->url base-uri) req-uri)))
