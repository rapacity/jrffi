#lang scribble/manual
@(require scribble/eval)


@title[#:style 'quiet #:tag "auto"]{Autobinding Module}

@defmodule[jrffi/auto]



@defform/subs[(jrequire java-class ... class-option ...)
              ([class-option code:blank
                             (code:line #:import)
                             (code:line #:racketify)
                             (code:line #:renamer renamer-proc)
                             (code:line #:typemap typemap-proc)])
              #:contracts ([java-class id?]
                           [renamer-proc (type string? string? . -> . string?)]
                           [type (or/c 'predicate 'class 'mutator 'accessor 'constructor 'method)])]{
                                                                     
Unpacks the bindings for the jrequired java classes. For each class the following are binded:

@itemize[
         
 @item{@racketidfont{java-class}, the name of the java-class is binded to the java class' id}
  
 @item{@racketidfont{new-java-class}, it is binded only if the java-class has a public constructor}

 @item{@racketidfont{java-class?}, takes in an object and checks if it is an instance of java-class}
 
 @item{@racket[java-class-method], it is binded for each method, if the method is overloaded}
 
 @item{@racket[get-java-class-field], for each public @racket[java-class-field];
       an @deftech{accessor} procedure is binded, if the @racket[java-class-field] is static
       the @deftech{accessor} does not take any arguments, if the @racket[java-class-field] is
       non-static the @deftech{accessor} takes in an instance object of
       type @racketidfont{java-class}}
         
 @item{@racket[set-java-class-field!], for each public non-final @racket[java-class-field]
       a @deftech{mutator} procedure is binded, if the @racket[java-class-field] is static
       the @deftech{mutator} takes a new field value, if the @racket[java-class-field] is
       non-static the @deftech{mutator} takes in an instance object of
       type @racketidfont{java-class} and a new field-value}
               
                                                            
         ]

If the @racket[#:import] option is specified the java class namespace is stripped from the
resulting generated names.

Note, the @racket[#:racketify] and the @racket[#:renamer renamer-proc] options cannot be used at the 
same time

If the @racket[#:racketify] option is specified:
  @itemize[
    @item{the word "to" is converted to "->"}
    @item{the position before the beginning of a change in character case, will become split 
          up by a hyphen}
    @item{all characters are lower-cased}
    @item{dots are converted to hyphens}
    @item{"make-" instead of "new-" is used in the constructor name}
   ]

For the @racket[#:renamer renamer] option to be used, the renamer function must be
present for-syntax. The provided renamer will be passed by the autobinder 
a symbol representing the type being one of object to be binded
('predicate 'class 'mutator, 'accessor, 'constructor, or 'method).
The renamer is expected to return a string which will be used by the autobinder
to bind the object. a string for class-name, a string representing the method or field name or
#f if type 'class or 'predicate is passed.



For the @racket[#:typemap typemap] TODO



@examples[
(require jrffi jrffi/auto)
          
(jrequire java.lang.Integer #:import #:racketify)
(integer->string (make-integer 100))
(get-integer-max-value)
(integer? (make-integer 100))

]


}                                                
                                                                    
                                                                    