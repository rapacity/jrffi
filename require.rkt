#lang racket/base

(require (for-syntax "bindings.rkt" syntax/parse racket/base syntax/srcloc
                     racket/require-transform racket/provide-transform))

(begin-for-syntax
  ; attempt to generate if module isn't already generated
  (define (java-module-path stx)
    (define (directory-path p)
      (define-values (dir _1 _2) (split-path p))
      dir)
    (define (current-source-location)
      (let ([loc (source-location-source stx)])
        (if (symbol? loc) (current-directory)
            (directory-path loc))))
    (let* ([package (syntax->datum stx)]
           [name (symbol->string package)]
           [file (relative-binding-file (symbol->string package) (current-source-location))])
      (unless (binding-exists? name)
        (generate-bindings name))
    (path->string file)))
  
  (define (requirer stx)
    (syntax-parse stx
      [(_ package:id ...
          (~optional (~and #:unsafe unsafe-kw))
          (~optional (~and #:full full-kw))
          (~optional (~and #:racketify racketify-kw))
          (~optional (~seq #:namer namer)))
       (for*/fold ([imports null] [sources null])
                  ([package-stx  (in-list (syntax-e #`(package ...)))]
                   [package-path (in-value (java-module-path package-stx))])
         (values
          (for*/fold ([new-imports imports])
                     ([names (in-list (syntax-local-module-exports package-path))]
                      [mode  (in-value (car names))]
                      [name  (in-list (cdr names))])
            (cons (make-import (datum->syntax stx name stx) name package-path mode 0 mode stx)
                  new-imports))
          (cons (make-import-source (datum->syntax stx package-path stx stx) 0) sources)))]))
  
  (define (provider stx modes)
    (syntax-parse stx
      [(_ package:id ...)
       (for*/list ([package-stx  (in-list (syntax-e #`(package ...)))]
                   [package-path (in-value (java-module-path package-stx))]
                   [mode         (in-list (if (null? modes) '(0) modes))]
                   [names        (or (syntax-local-module-required-identifiers package-path mode)
                                     (raise-syntax-error #f "no corresponding require" stx))]
                   [name         (in-list (cdr names))])
         (make-export name (syntax->datum name) mode #f stx))]))
  
  )

(define-syntax java
  (let ()
    (struct require+provide ()
      #:property prop:provide-transformer (λ (_) provider)
      #:property prop:require-transformer (λ (_) requirer)
      #:property prop:procedure
      (λ (_ stx)
        (raise-syntax-error #f "can only be used inside require or provide context" stx)))
    (require+provide)))



(provide java)