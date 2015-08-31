#lang racket/base

(require google)
(require google/oauth)
(require google/profile)
(require google/drive)
(require google/simple-token-store)
(require racket/pretty)

;; Download client_secret.json from your application's credentials
;; console, https://console.developers.google.com/
(define c (file->client "client_secret.json"))

;; You can use google/oauth/cli's "main" submodule to get a token for
;; yourself for dev/test purposes and store it in the racket prefs
;; (which is where lookup-google-token gets it from).
(define t (lookup-google-token c))

(with-handlers [(exn:fail:google?
                 (lambda (e)
                   ;; Printing the exception shows all of the detail
                   ;; of it in a way that the default exception
                   ;; printer does not; conversely, letting the
                   ;; default exception printer print it too gets us a
                   ;; pretty stack trace.
                   (pretty-print e)
                   (raise e)))]
  (parameterize ((invalid-token-handler
                  (lambda (old-token retry-counter)
                    ;; When a configured token fails, the
                    ;; invalid-token-handler parameter is called. Here
                    ;; we can try to refresh the token, and if that
                    ;; fails we could also ask the user to sign in
                    ;; again if we wanted. In this instance, we only
                    ;; try the refresh, and if that fails, we fail
                    ;; permanently.
                    (printf "Failed token (retry ~a): ~v\n" retry-counter old-token)
                    (if (zero? retry-counter)
                        (let ((new-token (refresh-token c old-token)))
                          (when new-token
                            (replace-google-token! c new-token)
                            (set! t new-token))
                          new-token)
                        #f))))

    (pretty-print (get-profile #:token t))

    (let ((r (drive-list #:token t
                         #:query `(in "root" parents))))
      (for [(item (@ r items))]
        (printf " - ~a: ~a\n" (@ item title) (@ item id))
        ;; (for [(parent (@ item parents))]
        ;;   (printf "    - ~a\n" (@ parent id)))
        ;; (for [(kid (@ (drive-list #:token t #:query `(in ,(@ item id) parents)) items))]
        ;;   (printf "    = ~a: ~a\n" (@ kid title) (@ kid id)))
        ))

    (pretty-print (drive-get "root" #:token t))

    ;; (display
    ;;  (bytes->string/utf-8
    ;;   (drive-export (drive-get "<<SOME DOCUMENT FILE ID>>" #:token t)
    ;;                 'text/plain
    ;;                 #:token t)))
    ;; (newline)

    ))
