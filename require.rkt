#lang racket/base

(require (for-syntax "bindings.rkt" syntax/parse racket/base syntax/srcloc
                     racket/require-transform racket/provide-transform racket/match
                     racket/list srfi/26/cut racket/string))

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
  
  (define (nxor a b)
    (and (or (not a) b) (or a (not b))))
  
  (define (call-namer package-name object-name unsafe? namer)
    (match-let ([(regexp #rx"^(<contract>)?<([^>]+)>(.*)$" (list _ contract? type name))
                 (symbol->string object-name)])
      (define (make-name)
        (string->symbol 
         (namer (symbol->string package-name)
                (string->symbol type)
                (or name (symbol->string name)))))
      ; FIXME don't make type, predicate as exceptions
      (cond [(or (string=? type "type") (string=? type "predicate")) (make-name)]
            [(nxor unsafe? contract?) #f]
            [else (make-name)])))
  

(define (split-dots str)
  (regexp-split #rx"[.]" str))

(define (split-camel-case str)
  (regexp-split #rx"(?<=[a-z])(?=[A-Z])" str))

(define (hyphenate lst)
  (string-join lst "-"))

(define (replace-first-to str)
  (match str
    [(list-rest "to" rest) (list* "->" rest)]
    [(list-rest "To" rest) (list* "->" rest)]
    [else str]))


(define (fix-hyphens str)
  ((compose (cut regexp-replace* #rx"-+" <> "-")
            (cut regexp-replace* #rx">-+" <> ">")
            (cut regexp-replace* #rx"_+" <> "-"))
   str))

(define (racketify str)
  (string-downcase (hyphenate (replace-first-to (append* (map split-camel-case (split-dots str)))))))

(define ((racketify-namer full?) package-name type object-name)
  (define class-name (if full? package-name (last (regexp-split #rx"[/]" package-name))))
  (fix-hyphens  
   (case type
     [(type)        (racketify class-name)]
     [(predicate)   (format "~a?" (racketify class-name))]
     [(mutator)     (format "set-~a-~a!" (racketify class-name) (racketify object-name))]
     [(accessor)    (format "get-~a-~a" (racketify class-name) (racketify object-name))]
     [(constructor) (format "make-~a" (racketify class-name))]
     [(method)      (format "~a-~a" (racketify class-name) (racketify object-name))])))
  
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
          (~optional (~and #:rktfy rktfy-kw))
          (~optional (~seq #:namer namer-kw)))
       (define namer (or (and (attribute rktfy-kw) (racketify-namer (attribute full-kw)))
                         (and (attribute namer-kw) (eval-syntax #'namer-kw))
                         (default-namer (attribute full-kw))))
       (for*/fold ([imports null] [sources null])
                  ([package-stx  (in-list (syntax-e #`(package ...)))]
                   [package-name (in-value (syntax->datum package-stx))]
                   [package-path (in-value (java-module-path package-stx))])
         (values
          (for*/fold ([new-imports imports])
                     ([names (in-list (syntax-local-module-exports package-path))]
                      [mode  (in-value (car names))]
                      [name  (in-list (cdr names))]
                      [new-name (in-value (call-namer package-name name (attribute unsafe-kw) namer))]
                      #:when new-name)
            (cons (make-import (datum->syntax stx new-name stx) name package-path mode 0 mode stx)
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