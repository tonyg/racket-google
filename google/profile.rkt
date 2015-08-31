#lang racket/base
;; https://developers.google.com/+/web/api/rest/latest/people/get#response
;; https://developers.google.com/+/web/api/rest/latest/people#resource

(provide email-scope
         profile-scope
         get-profile)

(require net/uri-codec)
(require net/url)
(require "oauth.rkt")
(require "generic-api.rkt")

(define email-scope "email")
(define profile-scope "profile")

(define (get-profile [user-id "me"] #:token [t #f])
  (json-api-get (string->url
                 (format "https://www.googleapis.com/plus/v1/people/~a"
                         (form-urlencoded-encode user-id)))
                #:token t))
