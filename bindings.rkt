#lang racket/base

(require racket/runtime-path racket/path file/sha1 racket/pretty "auto.rkt")

(define-runtime-path bindings-path "bindings")

; makes the file-name, java classes can have all sorts of weird characters in their names
; and those characters may not be allowed in a racket module-path
(define (make-name name)
  (string-append (sha1 (open-input-string name)) ".rkt"))

(define (valid-name? name)
  (regexp-match "[.]" name))

(define (relative-binding-file name base)
  (find-relative-path base (build-path bindings-path (make-name name))))

(define (slash->dot str)
  (regexp-replace* #rx"[/]" str "."))

(define (absolute-binding-file name)
  (build-path bindings-path (make-name name)))

(define (binding-exists? name)
  (file-exists? (absolute-binding-file name)))

(define (generate-bindings name #:exists [exists 'error])
  (define bindings (jrequire (string->symbol (slash->dot name))))
  (let* ([file   (absolute-binding-file name)]
         [output (open-output-file file #:exists exists)])
    (displayln "#lang racket/base" output)
    (displayln "(provide (all-defined-out))" output)
    (for ([i (in-list (list "core.rkt" "funtype.rkt" "fieldtype.rkt" "jvector.rkt"))])
      (displayln
       (string-append 
        "(require \"../" i "\")") output))
    (pretty-write bindings output)
    (flush-output output)))

(provide (all-defined-out))