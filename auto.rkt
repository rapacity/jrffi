#lang racket

(require "core.rkt" "misc.rkt" "c.rkt" "jvector.rkt"
         (for-syntax "query.rkt" racket/syntax syntax/parse racket/match srfi/26/cut racket/list
           racket/string))

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
  
  (define (import-renamer name)
    (last (regexp-split #rx"[.]" name)))
  
  (define (split-dots str)
    (regexp-split #rx"[.]" str))
  
  (define (split-camel-case str)
    (regexp-split #rx"(?<=[a-z])(?=[A-Z])" str))
  
  (define (hyphenate lst)
    (string-join lst "-"))
  
  (define (translate-words str)
    (match str
      ["to" "->"]
      ["To" "->"]
      [else str]))
  
  (define (fix-hyphens str)
    ((compose (cut regexp-replace* #rx"-+" <> "-")
              (cut regexp-replace* #rx">-+" <> ">")
              (cut regexp-replace* #rx"_+" <> "-"))
     str))
    
  (define (racketify str)
    (string-downcase(hyphenate (map translate-words (append* (map split-camel-case (split-dots str)))))))
  
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
  
  
  (define (construct-syntax stx token->type import? racketify? custom-namer)
    (define namer (or (and racketify? racketify-namer)
                        custom-namer
                        default-namer
                        ))
    (define (namer-stx type class-name field-or-method-name)
      (format-id stx (namer type class-name field-or-method-name)))
    (define package+class-name (symbol->string (syntax-e stx)))
    (define package+class-name-forward-slash (regexp-replace* #rx"[.]" package+class-name "/"))
    (define class-info (find-class-signature package+class-name))
    (define class-methods
      (partition-by method-signature-name (jclass-signature-methods class-info)))
    (define class-constructors (jclass-signature-constructors class-info))
    (define class-fields       (jclass-signature-fields class-info))
    (define class-name         (if import? (import-renamer package+class-name) package+class-name))
    (define class-identifier   (namer-stx 'class class-name #f))
    #`(begin
        (define #,class-identifier
          (#,(format-id stx "find-class") #,package+class-name-forward-slash ))
        
        (define (#,(namer-stx 'predicate class-name #f) obj)
          (instance-of? obj #,class-identifier))
        
        #,@(if (null? class-constructors) #`()
               #`((define #,(namer-stx 'constructor class-name #f)
                    (jconstructor/overload/check #,class-identifier 
                     #,@(map (match-lambda [(constructor-signature vararg? args return)
                                            (list vararg? (map token->type args)
                                                  (token->type return))]) class-constructors)))))
        #,@(for/list ([i (in-list class-methods)])
             (match i
               [(list-rest method-name methods)
                #`(define #,(namer-stx 'method class-name method-name)
                    (jmethod/overload/check
                     #,class-identifier #,method-name 
                     #,@(map (match-lambda [(method-signature _ _ static? _ vararg? args return)
                                            (list static? vararg? (map token->type args)
                                                  (token->type return))]) methods)))]))
        #,@(for/list ([i (in-list class-fields)])
             (match i
               [(field-signature field-name final? static? type)
                #`(define #,(namer-stx 'accessor class-name field-name)
                    (get-jaccessor #,class-identifier #,field-name #,(token->type type)
                                   #:static? #,static?))]))
        #,@(for/fold ([output null]) ([i (in-list class-fields)])
             (match i
               [(field-signature field-name final? static? type)
                (if final? output
                    (cons #`(define #,(namer-stx 'mutator class-name field-name)
                              (get-jmutator #,class-identifier #,field-name #,(token->type type)
                                            #:static? #,static?))
                          output))])))))
  
  


(define-syntax (jrequire stx)
  (syntax-parse stx
    [(_ class-name:id ...
        (~optional (~and #:import import-kw))
        (~optional (~or (~and #:racketify racketify-kw)
                        (~seq #:renamer renamer-proc)))
        (~optional (~seq #:typemap typemap-proc)))
     (let* ([token->type (type-mapper default-autobind-typemap)]
            [unpacker (cut construct-syntax <> token->type
                           (attribute import-kw) 
                           (attribute racketify-kw)
                           (and (attribute renamer-proc) (eval-syntax #'renamer-proc))
                           )])
       (with-syntax ([(unpacked ...) (map unpacker (syntax-e #`(class-name ...)))])
         #`(begin unpacked ...)))]))


(provide (all-defined-out))





























