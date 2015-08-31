#lang racket/base
;; Google API OAuth

(provide (struct-out exn:fail:google:oauth)

         (struct-out client)
         json-string->client
         file->client

         (struct-out token)
         authentication-request-url
         authorization-code->token
         refresh-token
         token-expired?
         token->authorization-header)

(require json)
(require net/uri-codec)
(require net/url)
(require racket/file)
(require racket/match)
(require racket/port)
(require racket/set)
(require racket/string)

(require "exn.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct exn:fail:google:oauth exn:fail:google (error description) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application credentials for interacting with Google APIs.
(struct client (id ;; client_id : String
                secret ;; client_secret : String
                auth-uri ;; auth_uri : String
                token-uri ;; token_uri : String
                )
  #:prefab)

(define (json-string->client json-blob)
  (match-define (hash-table [client-kind
                             (hash-table ['client_id id]
                                         ['auth_uri auth-uri]
                                         ['token_uri token-uri]
                                         ['client_secret secret]
                                         _ ...)])
    (string->jsexpr json-blob))
  (client id secret auth-uri token-uri))

(define (file->client path)
  (json-string->client (file->string path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct token (access ;; access_token : String
               refresh ;; refresh_token : String
               expiry-time ;; retrieval time + expires_in : Float, epoch seconds
               type ;; token_type : String
               )
  #:prefab)

;; Client (Listof String)
;; [#:state (Option String)]
;; [#:login-hint (Option String)] ;; email address, if present
;; [#:include-granted-scopes? Boolean]
;; -> UrlString
(define (authentication-request-url c scopes
                                    #:state [state #f]
                                    #:login-hint [login-hint #f]
                                    #:include-granted-scopes? [include-granted-scopes? #f])
  (let* ((fields (list (cons 'response_type "code")
                       (cons 'client_id (client-id c))
                       (cons 'redirect_uri "urn:ietf:wg:oauth:2.0:oob")
                       (cons 'scope (string-join scopes))))
         (fields (if state (cons (cons 'state state) fields) fields))
         (fields (if login-hint (cons (cons 'login_hint login-hint) fields) fields))
         (fields (if include-granted-scopes?
                     (cons (cons 'include_granted_scopes "true") fields)
                     fields)))
    (url->string (struct-copy url (string->url (client-auth-uri c))
                              [query fields]))))

(define (decode-oauth-response json-blob)
  (match (string->jsexpr json-blob)
    [(hash-table ['error error]
                 ['error_description description]
                 _ ...)
     (raise (exn:fail:google:oauth (format "Google OAuth error: ~a: ~a" error description)
                                   (current-continuation-marks)
                                   error
                                   description))]
    [other other]))

(define (token-uri-operation c fields)
  (decode-oauth-response
   (port->string (post-pure-port (string->url (client-token-uri c))
                                 (string->bytes/utf-8 (alist->form-urlencoded fields))
                                 (list "Content-Type: application/x-www-form-urlencoded")))))

;; Client String -> Token
(define (authorization-code->token c code)
  (match (token-uri-operation c
                              (list (cons 'code code)
                                    (cons 'client_id (client-id c))
                                    (cons 'client_secret (client-secret c))
                                    (cons 'redirect_uri "urn:ietf:wg:oauth:2.0:oob")
                                    (cons 'grant_type "authorization_code")))
    [(hash-table ['access_token access-token]
                 ['refresh_token refresh-token]
                 ['expires_in remaining-lifetime-seconds]
                 ['token_type token-type]
                 _ ...)
     (token access-token
            refresh-token
            (+ (current-seconds) remaining-lifetime-seconds)
            token-type)]))

;; Client Token -> (Option Token)
(define (refresh-token c t)
  (with-handlers [((lambda (e)
                     (and (exn:fail:google:oauth? e)
                          (equal? (exn:fail:google:oauth-error e) "invalid_grant")))
                   (lambda (e)
                     #f))]
    (match (token-uri-operation c
                                (list (cons 'client_id (client-id c))
                                      (cons 'client_secret (client-secret c))
                                      (cons 'refresh_token (token-refresh t))
                                      (cons 'grant_type "refresh_token")))
      [(hash-table ['access_token access-token]
                   ['expires_in remaining-lifetime-seconds]
                   ['token_type token-type]
                   _ ...)
       (token access-token
              (token-refresh t)
              (+ (current-seconds) remaining-lifetime-seconds)
              token-type)])))

(define (token-expired? t)
  (>= (current-seconds) (token-expiry-time t)))

(define (token->authorization-header t)
  (format "Authorization: Bearer ~a" (token-access t)))
