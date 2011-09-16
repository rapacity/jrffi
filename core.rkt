#lang racket/base

(require ffi/unsafe racket/function "start.rkt" "c.rkt")

(struct jtype (signature tag predicate ctype racket->java java->racket))
(struct jtype/object jtype (class))
(struct jtype/vector jtype/object (element))
(struct jvector (cpointer type length))

(define current-java-throw-handler (make-parameter raise))

(define ((single-compose f1 f2) e) (f1 (f2 e)))

(define (make-jtype obj racket->java java->racket)
  (let ([composed-racket->java (single-compose (jtype-racket->java obj) racket->java)]
        [composed-java->racket (single-compose java->racket (jtype-java->racket obj))])
    ; due to limitation in racket's struct-copy
    (cond
      [(jtype/vector? obj)
       (struct-copy jtype/vector obj
         [racket->java #:parent jtype composed-racket->java]
         [java->racket #:parent jtype composed-java->racket])]
      [(jtype/object? obj)
       (struct-copy jtype/object obj
         [racket->java #:parent jtype composed-racket->java]
         [java->racket #:parent jtype composed-java->racket])]
      [else
       (struct-copy jtype obj
         [racket->java                composed-racket->java]
         [java->racket                composed-java->racket])])))

(define (jtype->ctype obj)
  (make-ctype (jtype-ctype obj) (jtype-racket->java obj) (jtype-java->racket obj)))

; --- signature makers ---
(define (make-class-signature c)  (string-append "L" c ";"))
(define (make-vector-signature s) (string-append "[" s))

; --- predicates for java types on racket ---
(define jboolean?   boolean?)
(define jbyte?      byte?)
(define jchar?      char?)
(define (jshort? n) (and (exact-integer? n) (<= -32768 n 32767)))
(define (jint? n)   (and (exact-integer? n) (<= -2147483648 n 2147483647)))
(define (jlong? n)  (and (exact-integer? n) (<= -9223372036854775808 n 9223372036854775807)))
(define jfloat?     single-flonum?)
(define jdouble?    flonum?)
(define jstring?    string?)
(define ((make-jobject-predicate clss) o) (instance-of? o clss))
(define ((make-jlist-predicate element?) o) (andmap element? o))

; --- java types ---
(define _jboolean (jtype "Z" 'boolean jboolean? __jboolean #f            #f))
(define _jchar    (jtype "C" 'char    jchar?    __jchar    char->integer integer->char))
(define _jbyte    (jtype "B" 'byte    jbyte?    __jbyte    #f            #f))
(define _jshort   (jtype "S" 'short   jshort?   __jshort   #f            #f))
(define _jint     (jtype "I" 'int     jint?     __jint     #f            #f))
(define _jlong    (jtype "J" 'long    jlong?    __jlong    #f            #f))
(define _jfloat   (jtype "F" 'float   jfloat?   __jfloat   #f            #f))
(define _jdouble  (jtype "D" 'double  jdouble?  __jdouble  #f            #f))
(define _jvoid    (jtype "V" 'void    #f        __jvoid    #f            #f))
; hack for _jobject and _jlist so that they dual as a jtype and function
(define _jobject
  ((λ ()
     (struct _jobject jtype/object ()
       #:property prop:procedure 
       (λ (self class-name [racket->java #f] [java->racket #f] [predicate #f])
         (let ([class-id (find-class class-name)])
           (struct-copy jtype/object self
             [signature    #:parent jtype (make-class-signature class-name)]
             [predicate    #:parent jtype (or predicate 
                                              (procedure-rename
                                               (make-jobject-predicate class-id)
                                               (string->symbol (format "~a?" class-name))))]
             [racket->java #:parent jtype racket->java]
             [java->racket #:parent jtype java->racket]
             [class                 class-id]))))
     (let ([class-id (find-class "Ljava/lang/Object;")])
       (_jobject "Ljava/lang/Object;" 'object (make-jobject-predicate class-id)
                 __jobject #f #f class-id)))))
(define _jstring (_jobject "java/lang/String" new-string get-string jstring?))
(define _jlist    
  ((λ ()
     (struct _jlist jtype/vector ()
       #:property prop:procedure
       (λ (self element)
         (define-values (make-array array-ref array-set!) (tag->array-info (jtype-tag element)))
         (when (jtype/object? element)
           (let ([clss (jtype/object-class element)])
             (set! make-array (λ (n) (new-object-array n clss #f)))))
         (let* ([signature (make-vector-signature (jtype-signature element))]
                [element-racket->java (or (jtype-racket->java element) identity)]
                [element-java->racket (or (jtype-java->racket element) identity)]
                [element? (or (jtype-predicate element) (λ (_) #t))])
           (struct-copy jtype/vector self
             [signature    #:parent jtype signature]
             [predicate    #:parent jtype (make-jlist-predicate element?)]
             [ctype        #:parent jtype __jobject]
             [racket->java #:parent jtype
              (λ (c)
                (let ([array (make-array (length c))])
                  (for ([e (in-list c)] [i (in-naturals)])
                    (array-set! array i (element-racket->java e)))
                  array))]
             [java->racket #:parent jtype
              (λ (c)
                (for/list ([i (in-range (get-array-length c))])
                  (element-java->racket (array-ref c i))))]
             [class        #:parent jtype/object (find-class signature)]
             [element               element]))))
     (let ([class-id (find-class "[Ljava/lang/Object;")]
           [element-class-id (jtype/object-class _jobject)])
       (_jlist "[Ljava/lang/Object;" 'object (make-jobject-predicate element-class-id) __jobject
               (λ (c)
                 (let ([array (new-object-array (length c) element-class-id #f)])
                   (for ([e (in-list c)]
                         [i (in-naturals)])
                     (set-object-array-element array i e))
                   array))
               (λ (c)
                 (for/list ([i (in-range (get-array-length c))])
                   (get-object-array-element c i)))
               class-id
               _jobject)))))



(provide _jboolean _jbyte _jchar _jshort _jint _jlong _jfloat _jdouble _jvoid
         _jobject _jstring _jlist)

(provide jboolean? jbyte? jchar? jshort? jint? jlong? jfloat? jdouble? jstring?)

(provide
 make-jlist-predicate
 make-jobject-predicate
 make-class-signature
 make-vector-signature
 jtype->ctype
 current-java-throw-handler
 (struct-out jtype) 
 (struct-out jtype/object) 
 (struct-out jtype/vector)
 (struct-out jvector))