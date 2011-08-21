#lang racket/base

(require "core.rkt" (for-syntax syntax/parse racket))


(define-syntax (jclass stx)
  (syntax-parse stx
    [(_ class-name
        (~optional ((~literal constructors) [constructor-bind-name:id constructor-properties ...] ...))
        (~optional ((~literal methods) [method-bind-name:id method-properties ...] ...))
        (~optional ((~literal fields) [field-bind-name:id field-properties] ...)))
     #`(begin 
         (define class-id (find-class class-name))
         #,@(if (attribute constructor-bind-name)
                #`((define constructor-bind-name (jconstructor class-id constructor-properties ...)) ...)
                #`())
         #,@(if (attribute method-bind-name)
                #`((define method-bind-name (jinvoker class-id method-properties ...)) ...)
                #`())
         #,@(if (attribute field-bind-name)
                #`((define field-bind-name (jparameter class-id field-properties ...)) ...)
                #`()))]))


(provide jclass)