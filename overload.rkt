#lang racket/base

(require (except-in ffi/unsafe ->) (except-in "core.rkt" ->) "c.rkt"  racket/contract/base)

(require (for-syntax syntax/parse racket/syntax racket/base) racket/string racket/contract/region)

(require "query.rkt" "auto.rkt")


(struct joverload-row (pred? signature method contract))

(struct joverload (check? methods)
  #:property prop:procedure
  (λ (self . args)
    (let loop ([rows (joverload-methods self)])
      (if (null? rows) (error "signature for provided args not found")
          (let* ([row   (car rows)]
                 [pred? (joverload-row-pred? row)]
                 [arity (procedure-arity pred?)]
                 [argc  (length args)])
            (if (and (or (and (arity-at-least? arity) (>= argc (arity-at-least-value arity)))
                         (= argc arity))
                     (apply pred? args))
                (apply (joverload-row-method row) args)
                (loop (cdr rows))))))))

(define (make-jinst-signature args)
  (string-append* (map jtype-signature args)))


(define-syntax (_jfun stx)
  ; the mother of all messes
  (define-syntax-class jfunction
    (pattern
     ((~and (~not (~or (~datum ->) (~datum ...))) arg-type:expr) ...
      (~optional (~seq vararg-type:expr (~datum ...)))
      (~optional (~seq (~datum ->) return-type))
      (~optional (~and #:static static-kw))
      (~optional (~and #:error error-kw))
      (~optional (~and #:vararg vararg-kw))
      )
     #:with (arg+novar-id ...) (generate-temporaries #`(arg-type ...))
     #:with vararg-id          (if (or (attribute vararg-kw) (attribute vararg-type))
                                   (generate-temporary) #`())
     #:with (vararg-id* ...)   (if (attribute vararg-type) #`(vararg-id) #`())
     #:with (arg ...)     #`(arg-type ... #,@(if (attribute vararg-type) #`((_jlist vararg-type)) #`()))
     #:with return          (if (attribute return-type) #`return-type #`_jvoid)
     #:with (pred?-id ...)  (generate-temporaries #`(arg ...))
     
     #:with static?         (if (attribute static-kw) #`#t #`#f)
     #:with (arg-id ...)  #`(arg+novar-id ... vararg-id* ...)
     #:with return-id       (generate-temporary)
     #:with (binding ...) #`([arg-id arg] ... [return-id return] [pred?-id (jtype-predicate arg-id)] ...)
     #:with proc
     (begin
       (when (and (attribute vararg-type) (attribute vararg-kw))
         (raise-syntax-error #f "cannot use ... notation with #:vararg flag"))
       #`(lambda (ffi-func jobject-type method-id force-static?)
           (let ([obj?     (jtype-predicate jobject-type)]
                 [class-id (jtype/object-class jobject-type)])
           (if (or static? force-static?)
               (values
                (-> pred?-id ... any)
                (λ (arg+novar-id ... . vararg-id)
                  (and (pred?-id arg-id) ... ))
                (λ (arg+novar-id ... . vararg-id)
                  (begin0
                    (ffi-func current-jnienv class-id method-id arg+novar-id ... vararg-id* ...)
                    (when (has-exception? current-jnienv)
                      (raise (begin0 (exception-occurred current-jnienv)
                                     (exception-clear current-jnienv)))))))
               (values
                (-> obj? pred?-id ... any)
                (λ (obj arg+novar-id ... . vararg-id)
                  (and (obj? obj) (pred?-id arg-id) ... ))
                (λ (obj arg+novar-id ... . vararg-id)
                  (begin0
                    (ffi-func current-jnienv obj method-id arg+novar-id ... vararg-id* ...)
                    (when (has-exception? current-jnienv)
                      (raise (begin0 (exception-occurred current-jnienv)
                                     (exception-clear current-jnienv)))))))))))))
  (syntax-parse stx
    [(_ single:jfunction
        (~optional (~and #:check check-kw))
        (~optional (~and #:static static-kw)))
     (with-syntax ([check? (if (attribute check-kw) #`#t #`#f)]
                   [global-static? (if (attribute static-kw) #`#t #`#f)])
       (quasisyntax/loc stx
         (lambda (get-method)
           (let* (single.binding ...)
             (let-values ([(procedure-contract pred? procedure)
                           (get-method single.proc (list single.arg-id ...) single.return-id
                                       (or global-static? single.static?))])
               (if check? (contract procedure-contract procedure 
                                    (current-contract-region) '(function a))
                   procedure))))))]
    [(_ overload:jfunction ...
        (~optional (~and #:check check-kw))
        (~optional (~and #:static static-kw)))
     (with-syntax ([check? (if (attribute check-kw) #`#t #`#f)]
                   [global-static? (if (attribute static-kw) #`#t #`#f)])
       (quasisyntax/loc stx
         (lambda (get-method)
           (joverload check?
            (list
             (let* (overload.binding ... [args (list overload.arg-id ...)])
               (let-values ([(procedure-contract pred? procedure)
                             (get-method overload.proc args overload.return-id
                                         (or global-static? overload.static?))])
                 (joverload-row pred? (make-jinst-signature args)
                                procedure procedure-contract))) ...)))))]))

(define (get-jrffi-method jobject-type method-name function-type)
  (define (get-method unpacker args return static?)
    (let* ([signature (make-signature args return)]
           [class-id  (jtype/object-class jobject-type)]
           [method-id (get-method-id class-id method-name signature static?)]
           [ffi-func  (get-jrffi-obj 
                       (format "call-~a~a-method" (if static? "static-" "") (jtype-tag return))       
                       (_cprocedure (append (list __jnienv (if static? __jclass __jobject)
                                                  __jmethodID) (map jtype->ctype args))
                                    (jtype->ctype return)))])
      (unpacker ffi-func jobject-type method-id static?)))
  (function-type get-method))

(define (get-jrffi-constructor jobject-type function-type)
  (define (get-method unpacker args return _)
    (let* ([signature (make-signature args return)]
           [class-id  (jtype/object-class jobject-type)]
           [method-id (get-method-id class-id "<init>" signature #f)]
           [ffi-func  (get-jrffi-obj "new-object"
                        (_cprocedure (list* __jnienv __jclass __jmethodID (map jtype->ctype args))
                                     __jobject))])
      (unpacker ffi-func jobject-type method-id #t)))
  (function-type get-method))


(define-syntax (jinst stx)
  (syntax-parse stx
    [(_ obj-stx:expr arg:expr ...)
     (quasisyntax/loc stx
       (let ([obj obj-stx])
         (unless (joverload? obj)
           (error 'obj-stx "is not an overloaded function"))
         (let* ([signature  (make-jinst-signature (list arg ...))]
                [check?     (joverload-check? obj)]
                [rows       (joverload-methods obj)]
                [maybe-row  (findf (λ (o) (string=? signature (joverload-row-signature o))) rows)])
           (if maybe-row
               (let ([procedure          (joverload-row-method maybe-row)]
                     [procedure-contract (joverload-row-contract maybe-row)])
                 (if check? (contract procedure-contract procedure 
                                      (current-contract-region) '(function a))
                     procedure))
               (error 'jinst "overloaded method signature not found")))))]))


(provide (all-defined-out))
