#lang racket/base

(require (only-in ffi/unsafe _cprocedure)
         "core.rkt" "c.rkt"
         (prefix-in contract: racket/contract/base))

(require (for-syntax syntax/parse racket/syntax racket/base) racket/string racket/contract/region)

(require (for-syntax "private/list.rkt" racket/list racket/dict "private/stx.rkt"))

(struct jtype/fun
  (signature
   export-procedure-wrapper
   import-procedure-wrapper
   contract-wrapper
   args))

(struct jtype/method jtype/fun (return static?)
  #:transparent)

(struct jtype/constructor jtype/fun ()
  #:transparent)

(struct jtype/fun/overload
  (methods 
   predicate-wrappers
   export-procedure-wrapper
   import-procedure-wrapper
   contract-wrapper)
  #:transparent)

(struct jtype/method/overload jtype/fun/overload ()
  #:transparent)

(struct jtype/constructor/overload jtype/fun/overload ()
  #:transparent)

(struct jfun (signature procedure contract) #:transparent)
(struct jfun/overload (methods) #:transparent)


(define (make-fun-signature args return)
  (let ([args-signature (string-append* (map jtype-signature args))]
        [return-signature (jtype-signature return)])
    (string-append "(" args-signature ")" return-signature)))

(define make-method-signature make-fun-signature)

(define (make-constructor-signature args)
  (make-fun-signature args _jvoid))

(define (make-jinst-signature args)
  (string-append* (map jtype-signature args)))

(define-syntax (-> stx)
  (raise-syntax-error #f "can only be used within a java method type definition"))

  
(define-syntax (_jmethod stx)
  (define-syntax-class method-type-stx
    (pattern
     ((~and (~not (~or (~literal ->) (~literal ...))) arg-type:expr) ...
      (~optional (~seq vararg-type:expr (~literal ...)))
      (~literal ->) return-type
      (~optional (~and #:static static-kw))
      (~optional (~and #:vararg vararg-kw)))
     #:with (arg+novar-id ...) (generate-temporaries #`(arg-type ...))
     #:with vararg-id          (if (or (attribute vararg-kw) (attribute vararg-type))
                                   (generate-temporary) #`())
     #:with (vararg-id* ...)   (if (attribute vararg-type) #`(vararg-id) #`())
     #:with (arg ...)     #`(arg-type ... #,@(if (attribute vararg-type) #`((_jlist vararg-type)) #`()))
     #:with return          (if (attribute return-type) #`return-type #`_jvoid)
     ;-------
     #:with args (generate-temporary)
     #:with vararg? (if (attribute vararg-kw) #`#t #`#f)
     #:with argc (+ (if (attribute static-kw) 0 1)
                    (if (attribute vararg-kw) -1 0)
                    (length (syntax-e #`(arg ...))))
     #:with (pred?-id ...)  (generate-temporaries #`(arg ...))
     #:with (arg-id ...)  #`(arg+novar-id ... vararg-id* ...)
     #:with return-id       (generate-temporary)
     #:with (binding ...) #`([arg-id arg] ...
                             [return-id return]
                             [args (list arg-id ...)] 
                             [pred?-id (jtype-predicate arg-id)] ...)
     #:with [static? (a ...) (o ...)] (if (attribute static-kw) #`[#t () (class-id)] #`[#f (obj) (obj)])
     #:with pred     #`(λ (obj?-id)
                        (λ (a ... arg+novar-id ... . vararg-id)
                          (and (obj?-id a) ... (pred?-id arg-id) ... )))
     #:with cont     #`(λ (obj)
                         (contract:-> a ... pred?-id ... any))
     #:with proc
     (begin
       (when (and (attribute vararg-type) (attribute vararg-kw))
         (raise-syntax-error #f "cannot use ... notation with #:vararg flag"))
       #`(lambda (ffi-func class-id method-id)
           (λ (a ... arg+novar-id ... . vararg-id)
             (begin0
               (ffi-func current-jnienv o ... method-id arg+novar-id ... vararg-id* ...)
               (when (has-exception? current-jnienv)
                 ((current-java-throw-handler)
                  (begin0 (exception-occurred current-jnienv)
                          (exception-clear current-jnienv))))))))
     #:with type #`(jtype/method (make-method-signature args return-id) #f proc cont args return-id static?)))
  (syntax-parse stx
    [(_ single:method-type-stx)
     (quasisyntax/loc stx
       (let* (single.binding ...)
         single.type))]
    [(_ overload:method-type-stx ...+)
     (quasisyntax/loc stx
       (let* (overload.binding ... ...)
         (jtype/method/overload
          (list overload.type ...)
          (list overload.pred ...)
          #f #;export-procedure-wrapper
          (expand-overload-import (overload.vararg? overload.argc) ...)
          contract:any/c)))]))



(define-syntax (_jconstructor stx)
  (define-syntax-class method-type-stx
    (pattern
     ((~and (~not (~or (~literal ...))) arg-type:expr) ...
      (~optional (~seq vararg-type:expr (~literal ...)))
      (~optional (~and #:vararg vararg-kw)))
     #:with (arg+novar-id ...) (generate-temporaries #`(arg-type ...))
     #:with vararg-id          (if (or (attribute vararg-kw) (attribute vararg-type))
                                   (generate-temporary) #`())
     #:with (vararg-id* ...)   (if (attribute vararg-type) #`(vararg-id) #`())
     #:with (arg ...)     #`(arg-type ... #,@(if (attribute vararg-type) #`((_jlist vararg-type)) #`()))
     ;-------
     #:with args (generate-temporary)
     #:with vararg? (if (attribute vararg-kw) #`#t #`#f)
     #:with argc  (+ (if (attribute vararg-kw) -1 0)
                     (length (syntax-e #`(arg ...))))
     #:with (pred?-id ...)  (generate-temporaries #`(arg ...))
     #:with (arg-id ...)  #`(arg+novar-id ... vararg-id* ...)
     #:with (binding ...) #`([arg-id arg] ...  [pred?-id (jtype-predicate arg-id)] ...
                                          [args (list arg-id ...)])
     #:with pred     #`(λ (obj)
                         (λ (arg+novar-id ... . vararg-id)
                           (and (pred?-id arg-id) ... )))
     #:with cont     #`(λ (obj)
                         (contract:-> pred?-id ... any))
     #:with proc
     (begin
       (when (and (attribute vararg-type) (attribute vararg-kw))
         (raise-syntax-error #f "cannot use ... notation with #:vararg flag"))
       #`(lambda (ffi-func class-id method-id)
           (λ (arg+novar-id ... . vararg-id)
             (begin0
               (ffi-func current-jnienv class-id method-id arg+novar-id ... vararg-id* ...)
               (when (has-exception? current-jnienv)
                 ((current-java-throw-handler)
                  (begin0 (exception-occurred current-jnienv)
                          (exception-clear current-jnienv))))))))
     #:with type #`(jtype/constructor (make-constructor-signature args) #f proc cont args)))
  (syntax-parse stx
    [(_ single:method-type-stx)
     (quasisyntax/loc stx
       (let* (single.binding ...)
         single.type))]
    [(_ overload:method-type-stx ...+)
     (quasisyntax/loc stx
       (let* (overload.binding ... ...)
         (jtype/constructor/overload
          (list overload.type ...)
          (list overload.pred ...)
          #f #;export-procedure-wrapper
          (expand-overload-import (overload.vararg? overload.argc) ...)
          contract:any/c)))]))

(define-syntax (expand-overload-import stx)
  (struct funrow (name vararg? arity pred) #:transparent)
  (define-syntax-class imaginary-fun
    (pattern (vararg?:boolean argc:integer)
             #:with name (generate-temporary)
             #:with pred (generate-temporary)
             #:with funrow (funrow #'name
                                   (syntax-e #'vararg?)
                                   (syntax-e #'argc)
                                   #'pred)))
  (syntax-parse stx
    [(_ f:imaginary-fun ...)
     (let*-values ([(funcs) (syntax->datum #`(f.funrow ...))]
                   [(max-argc) (apply max 0 (map funrow-arity funcs))]
                   [(min-argc) (apply min (map funrow-arity funcs))]
                   [(vararg fixed) (partition funrow-vararg? funcs)])
       ; partitions the fixed arg functions into buckets depending on parity
       (define (partition-by-parity lst)
         (partition-by funrow-arity lst))
       ; appends the vararg functions to the end of the lists in the bucket whenever the
       ; vararg function has a parity that is at least the bucket key
       ; +inf.0 for last vararg case
       (define (bucket-push buckets key value)
         (dict-update buckets key (λ (f) (append f (list value))) null))
       (define (append-vararg buckets lst)
         (for/fold ([buckets buckets]) ([v (in-list lst)])
           (for/fold ([buckets (bucket-push buckets +inf.0 v)])
                     ([i (in-range (funrow-arity v) (add1 max-argc))])
             (bucket-push buckets i v))))
       (define completed-buckets (sort (append-vararg (partition-by-parity fixed) vararg) < #:key car))
        (quasisyntax/loc stx
          (lambda (f.name ... f.pred ...)
            (case-lambda
            #,@(for/list ([(n funcs) (in-dict (dict-remove completed-buckets +inf.0))])
                 (with-syntax ([(arg ...) (generate-n-temporaries n)])
                   #`[(_ arg ...)
                      (cond
                        #,@(for/list ([func (in-list funcs)])
                             (with-syntax ([pred? (funrow-pred func)]
                                           [name  (funrow-name func)])
                               #`[(pred? arg ...) (name arg ...)]))
                        [else (error "signature not found")])]))
            #,@(if (null? vararg) #`()
                   (with-syntax ([(arg ...) (generate-n-temporaries max-argc)])
                     #`([(_ arg ... . rest)
                         (cond
                           #,@(for/list ([func (in-list (dict-ref completed-buckets +inf.0))])
                                (with-syntax ([pred? (funrow-pred func)]
                                              [name  (funrow-name func)])
                                  #`[(apply pred? arg ... rest) (apply name arg ... rest)]))
                           [else (error "signature not found")])])))
            [else (error "signature not found")]))))]))

(define (get-java-method class-type method-name method-type
                         #:output-contract? [output-contract? #f]
                         #:apply-contract?  [apply-contract? #f])
  (let ([objpred? (jtype-predicate class-type)]
        [class-id (jtype/object-class class-type)])
    (define (get-single method-type)
      (let* ([signature   (jtype/fun-signature method-type)]
             [arg-types   (jtype/fun-args method-type)]
             [return-type (jtype/method-return method-type)]
             [static?     (jtype/method-static? method-type)]
             [method-id (get-method-id class-id method-name signature static?)]
             [ffi-func 
              (get-jrffi-obj 
               (format "call-~a~a-method" (if static? "static-" "") (jtype-tag return-type))       
               (_cprocedure (append (list __jnienv (if static? __jclass __jobject)
                                          __jmethodID) (map jtype->ctype arg-types))
                            (jtype->ctype return-type)))]
             [wrapper     (jtype/fun-import-procedure-wrapper method-type)]
             [contract    (jtype/fun-contract-wrapper method-type)])
        (values (contract objpred?) (wrapper ffi-func class-id method-id))))
    (get-fun get-single class-type method-type output-contract? apply-contract?)))


(define (get-java-constructor class-type method-type 
                              #:output-contract? [output-contract? #f]
                              #:apply-contract?  [apply-contract? #f])
    (let ([objpred? (jtype-predicate class-type)]
          [class-id (jtype/object-class class-type)])
  (define (get-single method-type)
    (let* ([signature   (jtype/fun-signature method-type)]
           [arg-types   (jtype/fun-args method-type)]
           [method-id   (get-method-id class-id "<init>" signature #f)]
           [ffi-func    (get-jrffi-obj "new-object"
                          (_cprocedure (list* __jnienv __jclass __jmethodID (map jtype->ctype arg-types))
                                       __jobject))]
           [wrapper     (jtype/fun-import-procedure-wrapper method-type)]
           [contract    (jtype/fun-contract-wrapper method-type)])
      (values (contract objpred?) (wrapper ffi-func class-id method-id))))
  (get-fun get-single class-type method-type output-contract? apply-contract?)))
  

(define (get-fun get-single class-type method-type output-contract? apply-contract?)
  (let ([objpred? (jtype-predicate class-type)])
    (define (get-multi method-type)
      (let* ([procedure-wrapper  (jtype/fun/overload-import-procedure-wrapper method-type)]
             [predicate-wrappers (jtype/fun/overload-predicate-wrappers method-type)]
             [predicates         (map (λ (o) (o objpred?)) predicate-wrappers)]
             [method-types       (jtype/fun/overload-methods method-type)]
             [methods            (for/list ([m (in-list method-types)])
                                   (let*-values ([(args) (jtype/fun-args m)]
                                                 [(signature) (make-jinst-signature args)]
                                                 [(contract procedure) (get-single m)])
                                     (jfun signature procedure contract)))])
        (struct jfun/overload jfun/overload ()
          #:property prop:procedure
          (apply procedure-wrapper (append (map jfun-procedure methods) predicates)))
        (values contract:any/c (jfun/overload methods))))
    (let-values ([(contract procedure) (if (jtype/fun/overload? method-type) 
                                           (get-multi method-type)
                                           (get-single method-type))])
      (if output-contract? (values procedure contract) procedure))))


(define-syntax (jinst stx)
  (syntax-parse stx
    [(_ obj-stx:expr arg:expr ... (~optional (~and #:unsafe unsafe-kw)))
     (quasisyntax/loc stx
       (let ([obj obj-stx])
         (unless (jfun/overload? obj)
           (error 'obj-stx "is not an overloaded function"))
         (let* ([signature  (make-jinst-signature (list arg ...))]
                [check?     #,(if (attribute unsafe-kw) #`#f #`#t)]
                [rows       (jfun/overload-methods obj)]
                [maybe-row  (findf (λ (o) (string=? signature (jfun-signature o))) rows)])
           (if maybe-row
               (let ([procedure          (jfun-procedure maybe-row)]
                     [procedure-contract (jfun-contract maybe-row)])
                 (if check? (contract:contract procedure-contract procedure 
                                      (current-contract-region) '(function a))
                     procedure))
               (error 'jinst "overloaded method signature not found")))))]))

(provide get-java-method get-java-constructor jinst ... -> _jmethod _jconstructor)



