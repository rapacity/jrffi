#lang racket/base

(require "query.rkt"  racket/match srfi/26/cut racket/list "private/list.rkt"
           racket/string)

(define default-autobind-typemap
  (make-hash
   `([vector  . _jvector]
     [boolean . _jboolean]
     [byte    . _jbyte]
     [char    . _jchar]
     [short   . _jshort]
     [int     . _jint]
     [long    . _jlong]
     [float   . _jfloat]
     [double  . _jdouble]
     [void    . _jvoid]
     [string  . _jstring]
     [object  . _jobject]
     [vararg  . _jlist])))

(define (type-mapper typemap)
  (define (ref id) (hash-ref typemap id))
  (define (map-type token)
    (match token
      [(list 'object class-name) 
       (cond
         [(string=? class-name "java/lang/String") (ref 'string)]
         [(string=? class-name "java/lang/Object") (ref 'object)]
         [else `(,(ref 'object) ,class-name)])]
      [(list-rest 'vector rest) `(,(ref 'vector) ,(map-type rest))]
      [(list-rest 'vararg rest) `(,(ref 'vararg) ,(map-type rest))]
      [(list type) (ref type)]))
  map-type)

(define (import-renamer name)
  (last (regexp-split #rx"[.]" name)))

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

(define (enum? str)
  (regexp-match? #rx"[$]" (format "~a" str)))

(define (fix-hyphens str)
  ((compose (cut regexp-replace* #rx"-+" <> "-")
            (cut regexp-replace* #rx">-+" <> ">")
            (cut regexp-replace* #rx"_+" <> "-"))
   str))

(define (racketify str)
  (string-downcase (hyphenate (replace-first-to (append* (map split-camel-case (split-dots str)))))))

(define (racketify-namer type class-name field-or-method-name)
  (with-handlers ([void (λ (e) (error (format "error: racketify: ~a ~a ~a~%"
                                              type class-name field-or-method-name)))])
    (case type
      [(class)       (fix-hyphens (racketify class-name))]
      [(predicate)   (fix-hyphens (format "~a?" (racketify class-name)))]
      [(mutator)     (fix-hyphens (format "set-~a-~a!"
                                          (racketify class-name)
                                          (racketify field-or-method-name)))]
      [(accessor)    (fix-hyphens (format "get-~a-~a"
                                          (racketify class-name)
                                          (racketify field-or-method-name)))]
      [(constructor) (fix-hyphens (format "make-~a" (racketify class-name)))]
      [(method)      (fix-hyphens (format "~a-~a"
                                          (racketify class-name)
                                          (racketify field-or-method-name)))])))

(define (default-namer type class-name field-or-method-name)
  (case type
    [(class)       class-name]
    [(predicate)   (format "~a?" class-name)]
    [(mutator)     (format "set-~a-~a!" class-name field-or-method-name)]
    [(accessor)    (format "get-~a-~a" class-name field-or-method-name)]
    [(constructor) (format "new-~a" class-name)]
    [(method)      (format "~a-~a" class-name field-or-method-name)]))


(define (require-namer type class-name field-or-method-name)
  (case type
    [(class)       "<type>"]
    [(predicate)   "<predicate>"]
    [(mutator)     (format "<mutator>~a!" field-or-method-name)]
    [(accessor)    (format "<accessor>~a" field-or-method-name)]
    [(constructor) "<constructor>"]
    [(method)      (format "<method>~a" field-or-method-name)]))


(define (construct-syntax stx token->type import? racketify? custom-namer)
  (define namer (or (and racketify? racketify-namer)
                    custom-namer
                    default-namer
                    ))
  (define (namer-stx type class-name field-or-method-name)
    (string->symbol (namer type class-name field-or-method-name)))
  (define package+class-name (symbol->string stx))
  (define package+class-name-forward-slash (regexp-replace* #rx"[.]" package+class-name "/"))
  (define class-info (find-class-signature package+class-name))
  (define class-methods
    (partition-by method-signature-name (jclass-signature-methods class-info)))
  (define class-constructors (jclass-signature-constructors class-info))
  (define class-fields       (jclass-signature-fields class-info))
  (define class-name         (if import? (import-renamer package+class-name) package+class-name))
  (define class-identifier   (namer-stx 'class class-name #f))
  `(begin
     (define ,class-identifier
       (_jobject ,package+class-name-forward-slash))
     
     (define ,(namer-stx 'predicate class-name #f) (jtype-predicate ,class-identifier))
     ,@(if (null? class-constructors) `()
           `((define ,(namer-stx 'constructor class-name #f)
               (get-java-constructor ,class-identifier
                 (_jconstructor ,@(map (match-lambda [(constructor-signature vararg? args return)
                                              `(,@(map token->type args)
                                                ,@(if vararg? `(#:vararg) `()))])
                               class-constructors))))))
      
     ,@(for/list ([i (in-list class-methods)])
         (match i
           [(list-rest method-name methods)
            `(define ,(namer-stx 'method class-name method-name)
               (get-java-method ,class-identifier ,method-name 
                 (_jmethod
                  ,@(map (match-lambda [(method-signature _ _ static? _ vararg? args return)
                                        `(,@(map token->type args)
                                          ->
                                          ,(token->type return)
                                          ,@(if static? `(#:static) `())
                                          ,@(if vararg? `(#:vararg) `()))]) methods))))]))
     ,@(for/list ([i (in-list class-fields)])
         (match i
           [(field-signature field-name final? static? type)
            `(define ,(namer-stx 'accessor class-name field-name)
                  (get-java-accessor ,class-identifier ,field-name
                                     (_jfield ,(token->type type)
                                              ,@(if static? `(#:static) `()))))]))
     ,@(for/fold ([output null]) ([i (in-list class-fields)])
         (match i
           [(field-signature field-name final? static? type)
            (if final? output
                (cons `(define ,(namer-stx 'mutator class-name field-name)
                         (get-java-mutator ,class-identifier ,field-name
                                           (_jfield ,(token->type type)
                                                    ,@(if static? `(#:static) `()))))
                      output))]))))

;(construct-syntax 'java.lang.String (type-mapper default-autobind-typemap) #t #t #f)
(define (jrequire name)
  (construct-syntax name (type-mapper default-autobind-typemap) #t #f require-namer))







(provide (all-defined-out))