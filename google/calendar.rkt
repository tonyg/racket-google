#lang racket/base
;; https://developers.google.com/google-apps/calendar/auth

(provide calendar-scope
         calendar.readonly-scope
         get-calendar-list
         list-calendar-list
         get-calendars
         list-events)

(require net/uri-codec
         "oauth.rkt"
         "generic-api.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define calendar-scope "https://www.googleapis.com/auth/calendar")
;; Full calendar access including read+write

(define calendar.readonly-scope "https://www.googleapis.com/auth/calendar.readonly")
;; Read only calendar access

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://developers.google.com/google-apps/calendar/v3/reference/calendarList/get#auth
(define (get-calendar-list calendar-id #:token [t #f])
  (json-api-get (string->url
                 (format "https://www.googleapis.com/calendar/v3/users/me/calendarList/~a"
                         (form-urlencoded-encode calendar-id)))
                #:token t))

;; https://developers.google.com/google-apps/calendar/v3/reference/calendarList/list
;; All optional parameters are currently ignored, there for future compatibility
(define (list-calendar-list #:token [t #f]
                            #:max-results [max-results #f]
                            #:min-access-role [min-access-role #f]
                            #:page-token [page-token #f]
                            #:show-hidden [show-hidden #f]
                            #:sync-token [sync-token #f])
  (json-api-get (string->url
                 "https://www.googleapis.com/calendar/v3/users/me/calendarList")
                #:token t))

; https://developers.google.com/google-apps/calendar/v3/reference/calendars/get
(define (get-calendars calendar-id #:token [t #f])
  (json-api-get (string->url
                 (format "https://www.googleapis.com/calendar/v3/calendars/~a"
                         (form-urlencoded-encode calendar-id)))
                #:token t))

;; https://developers.google.com/google-apps/calendar/v3/reference/events/list#http-request
;; All optional parameters are currently ignored, there for future compatibility
(define (list-events calendar-id
                     #:token [t #f]
                     #:always-include-email [always-include-email #f]
                     #:i-cal-uid [i-cal-uid #f]
                     #:max-attendees [max-attendees #f]
                     #:max-results [max-results #f]
                     #:order-by [order-by #f]
                     #:page-token [page-token #f]
                     #:private-extended-property [private-extended-property #f]
                     #:q [q #f]
                     #:shared-extended-property [shared-extended-property #f]
                     #:show-deleted [show-delted #f]
                     #:show-hidden-invitations [show-hidden-invitations #f]
                     #:single-events [single-events #f]
                     #:sync-token [sync-token #f]
                     #:time-max [time-max #f]
                     #:time-min [time-min #f]
                     #:time-zone [time-zone #f]
                     #:updated-min [updated-min #f])
  (json-api-get (string->url
                 (format
                  "https://www.googleapis.com/calendar/v3/calendars/~a/events"
                  (form-urlencoded-encode calendar-id)))
                #:token t))