#lang racket/base

(provide simple-cli-oauth-login
         (all-from-out "../oauth.rkt"))

(require net/sendurl)

(require "../oauth.rkt")

;; Client (Listof String)
;; [#:login-hint (Option String)] ;; email address, if present
;; [#:include-granted-scopes? Boolean]
;; -> Token
(define (simple-cli-oauth-login c scopes
                                #:login-hint [login-hint #f]
                                #:include-granted-scopes? [include-granted-scopes? #f])
  (printf "Opening a browser to request authorization from Google for the following scopes:\n")
  (for [(s scopes)] (printf " - ~a\n" s))
  (define u (authentication-request-url c scopes
                                        #:login-hint login-hint
                                        #:include-granted-scopes? include-granted-scopes?))
  (send-url u)
  (sleep 5)
  (printf "\n(If a browser window doesn't open, visit the following URL by hand:)\n~a\n\n" u)
  (printf "After completing the authorization in the browser, please paste your code here:\n")
  (define code (read-line))
  (authorization-code->token c code))

(module+ main
  (require racket/pretty)
  (require "../profile.rkt")
  (require "../drive.rkt")
  (require "../calendar.rkt")
  (require "../simple-token-store.rkt")

  (define c (file->client "client_secret.json"))
  (define t (simple-cli-oauth-login c
                                    (list email-scope
                                          profile-scope
                                          drive-scope
                                          calendar-scope)
                                    #:include-granted-scopes? #t))
  (store-google-token! c "Test application" t)
  (pretty-print (all-google-tokens)))
