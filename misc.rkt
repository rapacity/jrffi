#lang racket/base

(require "core.rkt")

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


(provide jmethod jconstructor jparameter)