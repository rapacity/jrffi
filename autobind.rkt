#lang racket

(require "core.rkt" "misc.rkt" "c.rkt" "jvector.rkt"
         (for-syntax "query.rkt" racket/syntax syntax/parse racket/match))

(begin-for-syntax
  (define default-autobind-typemap
    (make-hash
     `([vector  . ,#`_jvector]
       [boolean . ,#`_jboolean]
       [byte    . ,#`_jbyte]
       [char    . ,#`_jchar]
       [short   . ,#`_jshort]
       [int     . ,#`_jint]
       [long    . ,#`_jlong]
       [float   . ,#`_jfloat]
       [double  . ,#`_jdouble]
       [void    . ,#`_jvoid]
       [string  . ,#`_jstring]
       [object  . ,#`_jobject]
       [vararg  . ,#`_jlist])))
  
  (define (type-mapper typemap)
    (define (ref id) (hash-ref typemap id))
    (define (map-type token)
      (match token
        [(list 'object class-name) 
         (cond
           [(string=? class-name "java/lang/String") (ref 'string)]
           [(string=? class-name "java/lang/Object") (ref 'object)]
           [else #`(#,(ref 'object) #,class-name)])]
        [(list-rest 'vector rest) #`(#,(ref 'vector) #,(map-type rest))]
        [(list-rest 'vararg rest) #`(#,(ref 'vararg) #,(map-type rest))]
        [(list type) (ref type)]))
    map-type)
  
  (define (construct mapper lst)
    (for/list ([i (in-list lst)])
      (match i
        [(list-rest name (method-signature _ static? vararg? args return) rest)
         (list* name static? (list vararg? args return)
                (map (match-lambda [(method-signature _ _ vararg? args return)
                                    (list vararg? args return)]) rest))])))
  
  (define (construct-syntax class-name stx token->type)
    (define class-name-forward-slash (regexp-replace* #rx"[.]" class-name "/"))
    (define class-info (find-class-signature class-name))
    (define class-methods
      (partition-by method-signature-name (jclass-signature-methods class-info)))
    (define class-identifier (format-id stx "~a" class-name))
    #`(begin
        (define #,class-identifier (#,(format-id stx "find-class") #,class-name-forward-slash))
        #,@(for/list ([i (in-list class-methods)])
             (match i
               [(list-rest method-name (method-signature _ static? vararg? args return) rest)
                #`(define #,(format-id stx "~a-~a" class-name method-name)
                    (#,(format-id stx "jmethod/overload/check")
                     #,class-identifier #,method-name #,static?
                     (#,vararg? #,(map token->type args) #,(token->type return))
                     #,@(map (match-lambda [(method-signature _ _ vararg? args return)
                                            (list vararg? (map token->type args)
                                                  (token->type return))]) rest)))]))))
  
  
  )


(define-syntax (jimport stx)
  (syntax-parse stx
    [(_ class-name:id)
     (let ([token->type (type-mapper default-autobind-typemap)])
       (construct-syntax (symbol->string (syntax-e #'class-name)) #'class-name token->type))]))


(provide (all-defined-out))