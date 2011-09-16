#lang racket/base

(require (for-syntax "bindings.rkt" syntax/parse racket/base syntax/srcloc
                     racket/require-transform racket/provide-transform racket/match
                     racket/list))

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
  
  (define (call-namer package-name object-name namer)
    (match-let ([(regexp #rx"^<([^>]+)>(.*)$" (list _ type name)) (symbol->string object-name)])
      (string->symbol 
       (namer (symbol->string package-name)
              (string->symbol type)
              (or name (symbol->string name))))))
  
  (define ((default-namer full?) package-name type object-name)
    (define class-name (if full? package-name (last (regexp-split #rx"[/]" package-name))))
    (case type
      [(type)        class-name]
      [(predicate)   (format "~a?" class-name)]
      [(mutator)     (format "set-~a-~a!" class-name object-name)]
      [(accessor)    (format "get-~a-~a" class-name object-name)]
      [(constructor) (format "new-~a" class-name)]
      [(method)      (format "~a-~a" class-name object-name)]))
  
  (define (requirer stx)
    (syntax-parse stx
      [(_ package:id ...
          (~optional (~and #:unsafe unsafe-kw))
          (~optional (~and #:full full-kw))
          (~optional (~and #:racketify racketify-kw))
          (~optional (~seq #:namer namer-kw)))
       (define namer (or (and (attribute namer-kw) (eval-syntax #'namer-kw))
                         (default-namer (attribute full-kw))))
       (for*/fold ([imports null] [sources null])
                  ([package-stx  (in-list (syntax-e #`(package ...)))]
                   [package-name (in-value (syntax->datum package-stx))]
                   [package-path (in-value (java-module-path package-stx))])
         (values
          (for*/fold ([new-imports imports])
                     ([names (in-list (syntax-local-module-exports package-path))]
                      [mode  (in-value (car names))]
                      [name  (in-list (cdr names))])
            (let ([new-name (call-namer package-name name namer)])
              (cons (make-import (datum->syntax stx new-name stx) name package-path mode 0 mode stx)
                    new-imports)))
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