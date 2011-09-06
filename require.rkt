#lang racket/base

(require (for-syntax "bindings.rkt" syntax/parse racket/base)
         racket/require-syntax)


 (require (for-syntax syntax/srcloc))



(define-require-syntax (java stx)
  (define (directory-path p)
    (define-values (dir _1 _2) (split-path p))
    dir)
  (define (current-source-location)
    (let ([loc (source-location-source stx)])
      (if (symbol? loc) (current-directory)
          (directory-path loc))))
  (syntax-parse stx
    [(java package:id ...)
     (let* ([packages (syntax->datum #`(package ...))])
       (quasisyntax/loc stx
         (combine-in
          #,@(for/list ([package (in-list packages)])
               (let ([name (symbol->string package)]
                     [file (relative-binding-file (symbol->string package) (current-source-location))])
                 (unless (binding-exists? name)
                   (generate-bindings name))
                 (datum->syntax stx (path->string file) stx stx))))))]))

  



(provide (all-defined-out))




