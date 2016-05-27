#lang racket/base
;; https://developers.google.com/google-apps/calendar/auth

(provide calendar-scope
         calendar.readonly-scope)

(require "oauth.rkt")

(define calendar-scope "https://www.googleapis.com/auth/calendar")
;; Full calendar access including read+write

(define calendar.readonly-scope "https://www.googleapis.com/auth/calendar.readonly")
;; Read only calendar access

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
