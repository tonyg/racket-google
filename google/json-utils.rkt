#lang racket/base
;; Utilities for convenient access to fields of jsexpr objects (and hashtables generally)

(provide @
         @ref)

(define-syntax @
  (syntax-rules (unquote)
    [(_ v) v]
    [(_ v ,k rest ...) (@ (@ref v k) rest ...)]
    [(_ v k rest ...) (@ (@ref v 'k) rest ...)]))

(define (@ref v k)
  (and v (hash-ref v k (lambda () #f))))
