#lang racket/base

(require "query.rkt" racket/match racket/list "private/list.rkt")

(define default-typemap
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
     [object  . _jobject/null]
     [vararg  . _jlist])))

(define (default-tokenizer typemap)
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

(define (require-namer type class-name field-or-method-name)
  (case type
    [(class)       "<type>"]
    [(predicate)   "<predicate>"]
    [(mutator)     (format "<mutator>~a!" field-or-method-name)]
    [(accessor)    (format "<accessor>~a" field-or-method-name)]
    [(constructor) "<constructor>"]
    [(method)      (format "<method>~a" field-or-method-name)]))


(define (construct-syntax stx token->type namer)
  (define (namer-stx type class-name field-or-method-name)
    (string->symbol (namer type class-name field-or-method-name)))
  (define package+class-name (symbol->string stx))
  (define package+class-name-forward-slash (regexp-replace* #rx"[.]" package+class-name "/"))
  (define package+class-symbol (string->symbol package+class-name-forward-slash))
  (define class-info (find-class-signature package+class-name))
  (define class-methods
    (partition-by method-signature-name (jclass-signature-methods class-info)))
  (define class-constructors (jclass-signature-constructors class-info))
  (define class-fields       (jclass-signature-fields class-info))
  (define class-name         (import-renamer package+class-name))
  (define class-identifier   (namer-stx 'class class-name #f))
  (define predicate-name (namer-stx 'predicate class-name #f))
  `((define ,class-identifier
      ,(token->type (list 'object package+class-name-forward-slash)))
     (define ,predicate-name (jtype-predicate ,class-identifier))
     (provide ,class-identifier ,predicate-name)
     ,@(if (null? class-constructors) `()
           (let* ([name (namer-stx 'constructor class-name #f)]
                  [contract (gensym)]
                  [name/contract (string->symbol (format "<contract>~a" name))])
             `((define-values (,name ,contract)
                 (get-java-constructor ,class-identifier #:output-contract? #t
                   (_jconstructor ,@(map (match-lambda
                                           [(constructor-signature vararg? args return)
                                            `(,@(map token->type args)
                                              ,@(if vararg? `(#:vararg) `()))])
                                         class-constructors))))
                (define ,name/contract
                  (contract ,contract ,name '(java ,package+class-symbol) '(function a)))
                (provide ,name/contract ,name)
                )))
      
     ,@(append*
        (for/list ([i (in-list class-methods)])
         (match i
           [(list-rest method-name methods)
            (let* ([name (namer-stx 'method class-name method-name)]
                   [contract (gensym)]
                   [name/contract (string->symbol (format "<contract>~a" name))])
              `((define-values (,name ,contract)
                  (get-java-method ,class-identifier ,method-name #:output-contract? #t
                     (_jmethod
                      ,@(map (match-lambda
                               [(method-signature _ _ static? _ vararg? args return)
                                `(,@(map token->type args)
                                  ->
                                  ,(token->type return)
                                  ,@(if static? `(#:static) `())
                                  ,@(if vararg? `(#:vararg) `()))]) methods))))
                 (define ,name/contract
                   (contract ,contract ,name '(java ,package+class-symbol) '(function a)))
                 (provide ,name/contract ,name)))])))
     ,@(append*
        (for/list ([i (in-list class-fields)])
         (match i
           [(field-signature field-name final? static? type)
            (let* ([name (namer-stx 'accessor class-name field-name)]
                   [contract (gensym)]
                   [name/contract (string->symbol (format "<contract>~a" name))])
            `((define-values (,name ,contract)
                (get-java-accessor ,class-identifier ,field-name #:output-contract? #t
                                   (_jfield ,(token->type type)
                                            ,@(if static? `(#:static) `()))))
               (define ,name/contract
                 (contract ,contract ,name '(java ,package+class-symbol) '(function a)))
               (provide ,name/contract ,name)
               ))])))
     ,@(append*
        (for/fold ([output null]) ([i (in-list class-fields)])
         (match i
           [(field-signature field-name final? static? type)
            (if final? output
                (cons
                 (let* ([name (namer-stx 'mutator class-name field-name)]
                        [contract (gensym)]
                        [name/contract (string->symbol (format "<contract>~a" name))])
                   `((define-values (,name ,contract)
                       (get-java-mutator ,class-identifier ,field-name #:output-contract? #t
                                         (_jfield ,(token->type type)
                                                  ,@(if static? `(#:static) `()))))))
                      output))])))))

(define (jrequire name)
  (construct-syntax name (default-tokenizer default-typemap) require-namer))


(provide (all-defined-out))