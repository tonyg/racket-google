#lang racket/base

(provide (struct-out exn:fail:google))

(struct exn:fail:google exn:fail () #:transparent)
