#lang racket

(require "core.rkt" (for-syntax syntax/parse racket/syntax srfi/26/cut) "private/stx.rkt")

; <convenience macros>
; interfacing with standard java methods
(define-syntax jmethod
  (syntax-rules (:)
    [(_ class-id method-name : args ... #:static)
     (get-jmethod class-id (symbol->string 'method-name) #:static? #t (_jmethod  args ...))]
    [(_ class-id method-name : args ...)
     (get-jmethod class-id (symbol->string 'method-name)  (_jmethod args ...))]))

; interfacing with java constructors
; usage (jconstructor class-id arg-type ...)
(define-syntax-rule (jconstructor clss args ...)
  (get-jconstructor clss (_jmethod args ...)))

; macro to make interface uniform with jmethod
(define-syntax jparameter
  (syntax-rules ()
    [(_ class-id field-name type #:static)
     (make-jparameter class-id (symbol->string 'field-name) type #:static? #t)]
    [(_ class-id field-name type)
     (make-jparameter class-id (symbol->string 'field-name) type #:static? #f)]))

(define-syntax (jmethod/overload/check stx)
  (syntax-parse stx
    [(_ clss-id mthd-name static? (vararg? (arg-type ...) return) ...)
     (with-syntax* ([((pred? ...) ...) #'(((jtype-predicate arg-type) ...) ...)]
                    [((evaluated-pred? ...) ...) (generate-temporaries* #'((pred? ...) ...))]
                    [(method-id ...) (generate-temporaries #`(return ...))]
                    [(ffi-func ...)  (generate-temporaries #`(return ...))]
                    [(matcher ...)   (map (compose (cut if <> #`list-rest #`list) syntax-e)
                                          (syntax-e #`(vararg? ...)))]
                    [((arg ...) ...) (generate-temporaries* #`((arg-type ...) ...))])
       #`(values
          (let ([class-id clss-id]
                [method-name mthd-name])
            (let-values ([(evaluated-pred? ...) (values pred? ...)] ...
                         [(method-id ffi-func )
                          (get-jmethod/id+ffi-func class-id method-name 
                            (list arg-type ...) return #:static? static?)] ...)
             (match-lambda*
               [(matcher #,@(maybe-not-syntax #'static? #'o) (? evaluated-pred? arg) ...)
                (ffi-func current-jnienv  #,@(maybe-not-syntax #'static? #'(o) #'(class-id)) 
                          method-id arg ...)] ...
               [else (error 'name "No matching type signature for provided arguments")])))))]))


(provide jmethod jconstructor jparameter jmethod/overload/check)



