#lang scribble/doc
@(require 
  (for-label racket
    (except-in "../main.rkt" ->))
  
  
  scribble/racket
  scribble/eval
  scribble/base
  scribble/manual
  
  "../main.rkt" )


@title[#:style 'quiet #:tag "vector"]{Java Vectors}


@section[#:tag "jvector"]{Generic Vectors}
@defproc[(jvector? [v any/c]) boolean?]
@defthing[_jvector jtype/vector?]
@defproc[(make-jvector [type jtype?] [size exact-positive-integer?]) jvector?]
@defproc[(jvector-ref [v jvector?] [index exact-positive-integer?]) any/c]
@defproc[(jvector-set! [v jvector?] [index exact-positive-integer?] [value any/c]) any/c]



@section[#:tag "jvector-homogeneous"]{Homogeneous vectors}
@defproc[(jobject-vector? [v any/c]) boolean?]
@defthing[_jobject-vector jtype/vector?]
@defproc[(make-jobject-vector [type jtype/jobject?] [size exact-positive-integer?]) jvector?]
@defproc[(jobject-vector-ref [index exact-nonnegative-integer?]) any/c]
@defproc[(jobject-vector-set! [index exact-nonnegative-integer?] [value any/c]) void?]



@defproc[(jboolean-vector? [v any/c]) boolean?]
@defthing[_jboolean-vector jtype/vector?]
@defproc[(make-jboolean-vector [size exact-positive-integer?]) jvector?]
@defproc[(jboolean-vector-ref [index exact-nonnegative-integer?]) jboolean?]
@defproc[(jboolean-vector-set! [index exact-nonnegative-integer?] [value jboolean?]) void?]


@defproc[(jbyte-vector? [v any/c]) boolean?]
@defthing[_jbyte-vector jtype/vector?]
@defproc[(make-jbyte-vector [size exact-positive-integer?]) jvector?]
@defproc[(jbyte-vector-ref [index exact-nonnegative-integer?]) jbyte?]
@defproc[(jbyte-vector-set! [index exact-nonnegative-integer?] [value jbyte?]) void?]


@defproc[(jchar-vector? [v any/c]) boolean?]
@defthing[_jchar-vector jtype/vector?]
@defproc[(make-jchar-vector [size exact-positive-integer?]) jvector?]
@defproc[(jchar-vector-ref [index exact-nonnegative-integer?]) jchar?]
@defproc[(jchar-vector-set! [index exact-nonnegative-integer?] [value jchar?]) void?]


@defproc[(jshort-vector? [v any/c]) boolean?]
@defthing[_jshort-vector jtype/vector?]
@defproc[(make-jshort-vector [size exact-positive-integer?]) jvector?]
@defproc[(jshort-vector-ref [index exact-nonnegative-integer?]) jshort?]
@defproc[(jshort-vector-set! [index exact-nonnegative-integer?] [value jshort?]) void?]


@defproc[(jint-vector? [v any/c]) boolean?]
@defthing[_jint-vector jtype/vector?]
@defproc[(make-jint-vector [size exact-positive-integer?]) jvector?]
@defproc[(jint-vector-ref [index exact-nonnegative-integer?]) jint?]
@defproc[(jint-vector-set! [index exact-nonnegative-integer?] [value jint?]) void?]


@defproc[(jlong-vector? [v any/c]) boolean?]
@defthing[_jlong-vector jtype/vector?]
@defproc[(make-jlong-vector [size exact-positive-integer?]) jvector?]
@defproc[(jlong-vector-ref [index exact-nonnegative-integer?]) jlong?]
@defproc[(jlong-vector-set! [index exact-nonnegative-integer?] [value jlong?]) void?]


@defproc[(jfloat-vector? [v any/c]) boolean?]
@defthing[_jfloat-vector jtype/vector?]
@defproc[(make-jfloat-vector [size exact-positive-integer?]) jvector?]
@defproc[(jfloat-vector-ref [index exact-nonnegative-integer?]) jfloat?]
@defproc[(jfloat-vector-set! [index exact-nonnegative-integer?] [value jfloat?]) void?]


@defproc[(jdouble-vector? [v any/c]) boolean?]
@defthing[_jdouble-vector jtype/vector?]
@defproc[(make-jdouble-vector [size exact-positive-integer?]) jvector?]
@defproc[(jdouble-vector-ref [index exact-nonnegative-integer?]) jdouble?]
@defproc[(jdouble-vector-set! [index exact-nonnegative-integer?] [value jdouble?]) void?]