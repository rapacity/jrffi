#lang racket
(require rackunit)

(require "../main.rkt")


(define java.util.Arrays (find-class "java/util/Arrays"))
(define java.lang.Integer (find-class "java/lang/Integer"))


(define array->string (jmethod java.util.Arrays toString : _jobject ... -> _jstring #:static))
(define array->string2 (get-jmethod java.util.Arrays "toString" #:static? #t
                           (_jprocedure (list _jlist) _jstring #:repeat-last-arg? #t)))


(test-equal? "static jmethod vararg no-args"
             "[]" (array->string))

(test-equal? "static jmethod vararg with-args"
             "[1, 35]" (array->string (new-integer 1) (new-integer 35)))

(test-equal? "static jprocedure vararg no-args"
             "[]" (array->string2))

(test-equal? "static jprocedure vararg no-args"
             "[1]" (array->string2 (new-integer 1)))

(test-equal? "static jprocedure vararg with-args"
             "[1, 35]" (array->string2 (new-integer 1) (new-integer 35)))


(define integerToString (jmethod java.lang.Integer toString : -> _jstring))


(test-equal? "jmethod single-argument"
             "35" (integerToString (new-integer 35)))

