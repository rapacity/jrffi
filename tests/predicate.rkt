#lang racket
(require rackunit "../jvector.rkt"  "../core.rkt" "../funtype.rkt")

(define integer (_jobject "java/lang/Integer"))
(define integer? (jtype-predicate integer))

(define new-integer
  (get-java-constructor
   (_jobject "java/lang/Integer")
   (_jconstructor [_jint]
                  [_jstring])))

(test-true "jtype predicate, true" (integer? (new-integer 33333)))
(test-false "jtype predicate, false" (integer? "fffff"))

(define _jdouble-list  (_jlist _jdouble))
(define _jdouble-list?  (jtype-predicate _jdouble-list))

(test-true "jlist predicate, true" (_jdouble-list? (map real->double-flonum (list 1 2 4))))



(define _jboolean-list  (_jlist _jboolean))
(define _jboolean-list?  (jtype-predicate _jboolean-list))

(test-false "jlist predicate, false" (_jboolean-list? (list 1 2 3 4)))