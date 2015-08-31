#lang racket/base
;; https://developers.google.com/drive/web/scopes
;; https://developers.google.com/drive/web/search-parameters
;; https://developers.google.com/drive/v2/reference/files
;; https://developers.google.com/drive/v2/reference/files#resource

(provide drive.file-scope
         drive-scope
         drive.apps.readonly-scope
         drive.readonly-scope
         drive.metadata.readonly-scope
         drive.metadata-scope
         drive.install-scope
         drive.appfolder-scope
         drive.scripts-scope

         drive-folder-mime-type

         search-query->string

         drive-list
         drive-get
         drive-export)

(require racket/date)
(require racket/format)
(require racket/match)
(require racket/string)

(require "oauth.rkt")
(require "generic-api.rkt")
(require "json-utils.rkt")

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define drive.file-scope "https://www.googleapis.com/auth/drive.file")
;; Per-file access to files created or opened by the app

(define drive-scope "https://www.googleapis.com/auth/drive")
;; Full, permissive scope to access all of a user's files. Request
;; this scope only when it is strictly necessary. Tokens with scope
;; https://docs.google.com/feeds are accepted and treated the same as
;; tokens with scope https://www.googleapis.com/auth/drive.

(define drive.apps.readonly-scope "https://www.googleapis.com/auth/drive.apps.readonly")
;;Allows apps read-only access to the list of Drive apps a user has installed.

(define drive.readonly-scope "https://www.googleapis.com/auth/drive.readonly")
;; Allows read-only access to file metadata and file content

(define drive.metadata.readonly-scope "https://www.googleapis.com/auth/drive.metadata.readonly")
;; Allows read-only access to file metadata, but does not allow any
;; access to read or download file content

(define drive.metadata-scope "https://www.googleapis.com/auth/drive.metadata")
;; Allows read-write access to file metadata, but does not allow any
;; access to read, download, write or upload file content. Does not
;; support file creation, trashing or deletion. Also does not allow
;; changing folders or sharing in order to prevent access escalation.

(define drive.install-scope "https://www.googleapis.com/auth/drive.install")
;; Special scope used to let users approve installation of an app

(define drive.appfolder-scope "https://www.googleapis.com/auth/drive.appfolder")
;; Allows access to the Application Data folder

(define drive.scripts-scope "https://www.googleapis.com/auth/drive.scripts")
;; Allows access to Apps Script files

(define drive-folder-mime-type "application/vnd.google-apps.folder")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (escape-query-value-string s)
  (let* ((s (string-replace s "\\" "\\\\"))
         (s (string-replace s "'" "\\'")))
    (string-append "'" s "'")))

(define (query-value->string value)
  (match value
    [#t "true"]
    [#f "false"]
    [(? string?) (escape-query-value-string value)]
    [(? date?) (escape-query-value-string
                (parameterize ((date-display-format 'iso-8601))
                  (define offset (date-time-zone-offset value))
                  (string-append
                   (date->string value #t)
                   (if (zero? offset)
                       ""
                       (let* ((sign (if (negative? offset) "-" "+"))
                              (offset (abs offset))
                              (hours (quotient offset 3600))
                              (minutes (quotient (remainder offset 3600) 60)))
                         (string-append sign
                                        (~r hours #:min-width 2 #:pad-string "0")
                                        ":"
                                        (~r minutes #:min-width 2 #:pad-string "0")))))))]
    [`(exact ,(? string? exactstr)) (escape-query-value-string (format "\"~a\"" exactstr))]
    [_ (error 'search-query->string "Unsupported query value: ~v" value)]))

(define (search-query->string q)
  (let walk ((q q))
    (match q
      [`(and ,q ...) (string-join (map walk q) " and " #:before-first "(" #:after-last ")")]
      [`(or ,q ...) (string-join (map walk q) " or " #:before-first "(" #:after-last ")")]
      [`(not ,q) (format "not ~a" (walk q))]
      [`(properties ,q ...)
       (string-join (map walk q) " and " #:before-first "properties has { " #:after-last " }")]
      [`(in ,value ,field) (format "~a in ~a" (query-value->string value) field)]
      [`(,op ,field ,value) (format "~a ~a ~a" field op (query-value->string value))]
      [(? symbol?) (symbol->string q)])))

(module+ test
  (check-equal? (search-query->string `(and (> modifiedDate ,(date 0 0 12 4 6 2012 0 0 #f 0))
                                            (or (contains mimeType "image/")
                                                (contains mimeType "video/"))))
                "(modifiedDate > '2012-06-04T12:00:00' and (mimeType contains 'image/' or mimeType contains 'video/'))")

  (check-equal? (search-query->string `(= title "hello"))
                "title = 'hello'")

  (check-equal? (search-query->string `(= mimeType ,drive-folder-mime-type))
                "mimeType = 'application/vnd.google-apps.folder'")

  (check-equal? (search-query->string `(!= mimeType ,drive-folder-mime-type))
                "mimeType != 'application/vnd.google-apps.folder'")

  (check-equal? (search-query->string `(and (contains title "hello")
                                            (contains title "goodbye")))
                "(title contains 'hello' and title contains 'goodbye')")

  (check-equal? (search-query->string `(not (contains title "hello")))
                "not title contains 'hello'")

  (check-equal? (search-query->string `(contains fullText "hello"))
                "fullText contains 'hello'")

  (check-equal? (search-query->string `(not (contains fullText "hello")))
                "not fullText contains 'hello'")

  (check-equal? (search-query->string `(contains fullText (exact "hello world")))
                "fullText contains '\"hello world\"'")

  (check-equal? (search-query->string `(contains fullText (exact "hello_world")))
                "fullText contains '\"hello_world\"'")

  (check-equal? (search-query->string `(contains fullText "\\authors"))
                "fullText contains '\\\\authors'")

  (check-equal? (search-query->string `(in "test@example.org" writers))
                "'test@example.org' in writers")

  (check-equal? (search-query->string `(in "1234567" parents))
                "'1234567' in parents")

  (check-equal? (search-query->string `(in "appfolder" parents))
                "'appfolder' in parents")

  (check-equal? (search-query->string `(and (in "test@example.org" writers)
                                            (in "test2@example.org" writers)))
                "('test@example.org' in writers and 'test2@example.org' in writers)")

  (check-equal? (search-query->string `(and (contains fullText "important")
                                            (= trashed #t)))
                "(fullText contains 'important' and trashed = true)")

  (check-equal? (search-query->string `(> modifiedDate ,(date 0 0 12 4 6 2012 0 0 #f 0)))
                "modifiedDate > '2012-06-04T12:00:00'")

  (check-equal? (search-query->string `(> modifiedDate ,(date 0 0 12 4 6 2012 0 0 #f (* -8 3600))))
                "modifiedDate > '2012-06-04T12:00:00-08:00'")

  (check-equal? (search-query->string `(and sharedWithMe
                                            (contains title "hello")))
                "(sharedWithMe and title contains 'hello')")

  (check-equal? (search-query->string `(properties (= key "additionalID")
                                                   (= value "8e8aceg2af2ge72e78")
                                                   (= visibility "PRIVATE")))
                "properties has { key = 'additionalID' and value = '8e8aceg2af2ge72e78' and visibility = 'PRIVATE' }")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (drive-list #:token t
                    #:corpus [corpus #f]
                    #:max-results [max-results #f]
                    #:page-token [page-token #f]
                    #:query [query #f])
  (json-api-get (string->url/query "https://www.googleapis.com/drive/v2/files"
                                   (append-optional-fields
                                    '()
                                    'corpus corpus
                                    'maxResults (and max-results (number->string max-results))
                                    'pageToken page-token
                                    'q (and query (search-query->string query))))
                #:token t))

(define (drive-get file-id
                   #:token t
                   #:acknowledge-abuse? [acknowledge-abuse? #f]
                   #:download? [download? #f]
                   #:revision-id [revision-id #f]
                   #:update-viewed-date? [update-viewed-date? #f])
  (json-api-get (string->url/query (format "https://www.googleapis.com/drive/v2/files/~a" file-id)
                                   (append-optional-fields
                                    '()
                                    'acknowledgeAbuse (and acknowledge-abuse? "true")
                                    'alt (and download? "media")
                                    'revisionId revision-id
                                    'updateViewedDate (and update-viewed-date? "true")))
                #:token t))

(define (drive-export file-metadata mime-type #:token t)
  (GET-bytes (string->url (@ file-metadata exportLinks ,mime-type))
             (if t (list (token->authorization-header t)) '())))
