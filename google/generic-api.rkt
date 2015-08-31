#lang racket/base

(provide (struct-out exn:fail:google:api)
         parse-json-response
         json-api-get)

(require json)
(require net/url)
(require racket/match)
(require racket/port)

(require "oauth.rkt")
(require "exn.rkt")

(struct exn:fail:google:api exn:fail:google (detail) #:transparent)

(define (parse-json-response json-blob)
  (match (string->jsexpr json-blob)
    [(hash-table ['error detail] _ ...)
     (raise (exn:fail:google:api (hash-ref detail
                                           'message
                                           (lambda ()
                                             (format "Google API error: ~v" detail)))
                                 (current-continuation-marks)
                                 detail))]
    [other other]))

(define (json-api-get u
                      #:token [t #f]
                      #:headers [headers '()])
  (parse-json-response
   (port->string
    (get-pure-port u
                   (if t (cons (token->authorization-header t) headers) headers)))))
