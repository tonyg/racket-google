#lang racket/base
;; Base exception struct used by the modules in this package.

(provide (struct-out exn:fail:google))

(struct exn:fail:google exn:fail () #:transparent)
