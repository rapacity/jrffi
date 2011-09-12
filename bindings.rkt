#lang racket/base

(require ffi/unsafe racket/runtime-path racket/function racket/match racket/path racket/file racket/string)

(require "auto.rkt")
(require racket/pretty)

(define-runtime-path bindings-path "bindings")

(define (split-class-name str)
  (match (regexp-split #rx"[/.]" str)
    [(list head ... tail) (values head tail)]
    [(list) (error "not a valid package name")]))


(define (relative-binding-file name base)
  (define-values (package class) (split-class-name name))
  (find-relative-path 
   base
   (build-path (apply build-path bindings-path package) (string-append ($->+ class) ".rkt"))))


(define (absolute-binding-file name)
  (define-values (package class) (split-class-name name))
  (build-path (apply build-path bindings-path package) (string-append ($->+ class) ".rkt")))

(define (binding-exists? name)
  (file-exists? (absolute-binding-file name)))

(define (slash->dot str)
  (regexp-replace* #rx"[/]" str "."))

(define ($->+ str)
  (regexp-replace* #rx"[$]" str "+"))

(define (generate-bindings name #:exists [exists 'error])
  (define-values (package class) (split-class-name name))
  (define package-path (apply build-path bindings-path package))
  (define bindings (jrequire (string->symbol (slash->dot name))))
  (make-directory* package-path)
  (let* ([file   (build-path package-path (format "~a.rkt" ($->+ class)))]
         [output (open-output-file file #:exists exists)])
    (displayln "#lang racket/base" output)
    (displayln "(provide (all-defined-out))" output)
    (for ([i (in-list (list "core.rkt" "funtype.rkt" "fieldtype.rkt" "jvector.rkt"))])
      (displayln
       (string-append 
        "(require \"" (string-append* (build-list (add1 (length package)) (λ (_) "../"))) i "\")") output))
    (pretty-write bindings output)
    (flush-output output)))





(provide (all-defined-out))

