#lang scribble/doc
@(require 
  (for-label racket
    (except-in "../main.rkt" ->))
  
  
  scribble/racket
  scribble/eval
  scribble/base
  scribble/manual
  
  "../main.rkt" )

@title[#:style 'quiet #:tag "types"]{Java Types}

@defmodule[jrffi]



@; ---------------------------------------------------------------------------
                

@section[#:tag "jtype"]{Type Constructors}

@defproc[(make-jtype [type jtype?]
                     [racket->java (any/c . -> . any/c)]
                     [java->racket (any/c . -> . any/c)])
         jtype?]{
Creates a new jtype,                  
                 
}

@; ---------------------------------------------------------------------------

@section[#:tag "numeric-type-predicates"]{Numeric Type Predicates}

@defproc[(jbyte? [v any/c]) boolean?]
Returns @racket[(byte? v)]

@defproc[(jshort? [v any/c]) boolean?]
Returns @racket[(and (exact-integer? v) (< -32768 v 32767))]

@defproc[(jint? [v any/c]) boolean?]
Returns @racket[(and (exact-integer? v) (< -2147483648 v 2147483647))]

@defproc[(jlong? [v any/c]) boolean?]
Returns @racket[(and (exact-integer? v)
                     (< -9223372036854775808 v 9223372036854775807))]

@defproc[(jfloat? [v any/c]) boolean?]
Returns @racket[(single-flonum? v)]

@defproc[(jdouble? [v any/c]) boolean?]
Returns @racket[(flonum? v)]


@; ---------------------------------------------------------------------------

@section[#:tag "other-type-predicates"]{Other Type Predicates}

@defproc[(jboolean? [v any/c]) boolean?]
Returns @racket[(boolean? v)]


@defproc[(jchar? [v any/c]) boolean?]
Returns @racket[(char? v)]


@defproc[(jstring? [v any/c]) boolean?]
Returns @racket[(string? v)]



@; ---------------------------------------------------------------------------


@section[#:tag "java-types"]{Java Types}

@deftogether[(@defthing[_jboolean jtype?])]{
  When used in an unchecked function it translates #f to a 0 and any other value to 1
  If type checking is turned on, it will ony accept a #t or #f value
}

@defthing[_jchar jtype?]
@defthing[_jstring jtype?]


@defthing[_jbyte jtype?]
@defthing[_jshort jtype?]
@defthing[_jint jtype?]
@defthing[_jlong jtype?]
@defthing[_jfloat jtype?]
@defthing[_jdouble jtype?]

@defthing[_jvoid jtype?]{
void type, cannot appear in an argument position
}



@defform/subs[(_jmethod [arg-type ... -> return-type local-option ...] ...)
              ([local-option code:blank
                            (code:line #:static)
                            (code:line #:vararg)])
              #:contracts ([arg-type jtype?]
                           [return-type jtype?])]{
                                                 (or/c jtype/method? jtype/method/overload?)
Create an ffi function type
}

                                                 
                                                 

@defform/subs[(_jconstructor [arg-type ... local-option ...] ...)
              ([local-option code:blank
                             (code:line #:vararg)])
              #:contracts ([arg-type jtype?])]{
                                               (or/c jtype/constructor? jtype/constructor/overload?)
Create an ffi constructor type
}
                                                 
@defform/subs[(_jfield type option ...)
              ([local-option code:blank
                             (code:line #:static)])
              #:contracts ([type jtype?])]{
                                           jtype/field?
Create an ffi field type
}


@defform/subs[(jinst func arg-type ... option ...)
              ([local-option code:blank
                             (code:line #:unsafe)])
              #:contracts ([func jfun/overload?]
                           [arg-type jtype?])]{
Instantiate an overloaded function by providing it with argument types. jinst will return a non-overloaded version of the function, assuming one with the argument types it is provided with exists.
}
                                                 
@section[#:tag "importing-java-objects"]{Importing Java Objects}
                                                                                         
@defproc[(get-java-method [object-type jtype/object?]
                          [name string?]
                          [function-type procedure?]
                          [#:output-contract output-contract? boolean? #f])
         procedure?]{
Retrieves a java method
                     
}


@defproc[(get-java-constructor [object-type jtype/object?]
                               [function-type procedure?]
                               [#:output-contract output-contract? boolean? #f])
         procedure?]{
Retrieves a java constructor
}

@defproc[(get-java-accessor [object-type jtype/object?]
                            [name string?]
                            [field-type jtype?]
                            [#:output-contract output-contract? boolean? #f])
         procedure?]{
Retrieves a java accessor
}

@defproc[(get-java-mutator [object-type jtype/object?]
                           [name string?]
                           [function-type procedure?]
                           [#:output-contract output-contract? boolean? #f])
         procedure?]{
Retrieves a java mutator
}






