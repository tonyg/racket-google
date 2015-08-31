#lang racket/base
;; Simple framework-preferences storage of OAuth tokens

(provide oauth-preference-name
         oauth-token-store?
         forget-all-google-tokens!
         forget-google-token!
         all-google-tokens
         lookup-google-token
         store-google-token!)

(require framework/preferences)
(require racket/serialize)

(require "oauth.rkt")

(define oauth-preference-name 'google:oauth-tokens)
;; Preference contains a hash table that maps a client ID string to a
;; two-entry list containing a human-friendly client name string and
;; and authorization token.

(define (oauth-token-store? t)
  (and (hash? t)
       (for/and [((k v) (in-hash t))]
         (and (string? k)
              (list? v)
              (= (length v) 2)
              (string? (car v))
              (token? (cadr v))))))

(preferences:set-default oauth-preference-name (hash) oauth-token-store?)
(preferences:set-un/marshall oauth-preference-name serialize deserialize)

(define (forget-all-google-tokens!)
  (preferences:set oauth-preference-name
                   (hash)))

(define (forget-google-token! c)
  (preferences:set oauth-preference-name
                   (hash-remove (preferences:get oauth-preference-name)
                                (if (client? c) (client-id c) c))))

(define (all-google-tokens)
  (hash->list (preferences:get oauth-preference-name)))

(define (lookup-google-token c)
  (define entry
    (hash-ref (preferences:get oauth-preference-name)
              (if (client? c) (client-id c) c)
              #f))
  (and entry (cadr entry)))

(define (store-google-token! c client-name t)
  (preferences:set oauth-preference-name
                   (hash-set (preferences:get oauth-preference-name)
                             (if (client? c) (client-id c) c)
                             (list client-name t))))

(module+ main
  (require racket/pretty)
  (pretty-print (all-google-tokens)))
