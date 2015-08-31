#lang racket/base
;; Generic JSON-based API calls.

(provide (struct-out exn:fail:google:api)
         string->url ;; from net/url
         string->url/query
         append-optional-fields
         parse-json-response
         json-api-get
         invalid-token-handler
         GET-string
         GET-bytes)

(require json)
(require net/url)
(require racket/match)
(require racket/port)

(require "oauth.rkt")
(require "exn.rkt")
(require "json-utils.rkt")

(struct exn:fail:google:api exn:fail:google (detail) #:transparent)

(define (string->url/query u fields)
  (struct-copy url (string->url u) [query fields]))

(define (append-optional-fields fields . kvs0)
  (append fields
          (let walk ((kvs kvs0))
            (match kvs
              ['() '()]
              [(list* k #f rest) (walk rest)]
              [(list* k v rest) (cons (cons k v) (walk rest))]
              [_ (error 'append-optional-fields "Uneven key/value list: ~v" kvs0)]))))

(define (raise-api-error detail)
  (raise (exn:fail:google:api (hash-ref detail
                                        'message
                                        (lambda ()
                                          (format "Google API error: ~v" detail)))
                              (current-continuation-marks)
                              detail)))

(define (parse-json-response json-blob retry t retry-counter)
  (match (string->jsexpr json-blob)
    [(hash-table ['error detail] _ ...)
     (match detail
       [(hash-table ['code 401]
                    ['errors (list (hash-table ['reason "authError"] _ ...))]
                    _ ...)
        (if (invalid-token-handler)
            (let ((maybe-new-token ((invalid-token-handler) t retry-counter)))
              (if maybe-new-token
                  (retry maybe-new-token (+ retry-counter 1))
                  (raise-api-error detail)))
            (raise-api-error detail))]
       [_
        (raise-api-error detail)])]
    [other other]))

(define (json-api-get u
                      #:token [t #f]
                      #:headers [headers '()])
  (let retry ((t t) (retry-counter 0))
    (parse-json-response
     (GET-string u (if t (cons (token->authorization-header t) headers) headers))
     retry
     t
     retry-counter)))

(define invalid-token-handler (make-parameter #f))

(define (GET-string u headers)
  (port->string (get-pure-port u headers)))

(define (GET-bytes u headers)
  (port->bytes (get-pure-port u headers)))
