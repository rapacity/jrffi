#lang scribble/doc
@(require 
  (for-label racket
    (except-in "../main.rkt" ->))
  
  
  scribble/racket
  scribble/eval
  scribble/base
  scribble/manual
  
  "../main.rkt" )


@title[#:style 'quiet #:tag "auto"]{Autobinding Module}

@defmodule[jrffi]


@specsubform/subs[(java name ... class-option ...)
                  ([class-option code:blank
                           (code:line #:full)
                           (code:line #:rktfy)
                           (code:line #:namer namer-proc)
                           (code:line #:unsafe)])
              #:contracts ([name id?]
                           [namer-proc (type string? string? . -> . string?)]
                           [type (or/c 'predicate 'jtype 'mutator 'accessor 'constructor 'method)])]{
                                                                                               
The above form is a require sub-form, it imports the bindings for the specified java classes.
The @racketidfont{name} must be specify the class along with its package path, "." should
be written as "/" except when the class you want to import is an inner then the final separator should be "$"

For example requiring the java.lang.Integer class

@code[#:lang "at-exp racket"]{(require jrffi (java java/lang/Integer))}

an example of importing an inner class enum for an imaginary class named "an.imaginary.Class" with the enum "Toaster"

@code[#:lang "at-exp racket"]{(require jrffi (java an/imaginary/Class$Toaster))}


For each class specified the following are imported:

@itemize[
         
 @item{@racketidfont{name}, the name of the java-class is binded to a jtype}
  
 @item{@racketidfont{new-java-class}, it is binded only if the java-class has a public constructor}

 @item{@racketidfont{class-name?}, takes in an object and checks if it is an instance of java-class}
 
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

If the @racket[#:full] option is specified the java class' package is kept in the names imported

Note, the @racket[#:rktfy] and the @racket[#:namer renamer-proc] options cannot be used at the 
same time

If the @racket[#:rktfy] option is specified:
  @itemize[
    @item{if the beginning of a method name is the word "to", the "to" is converted to "->"}
    @item{the position before the beginning of a change in character case, will become split 
          up by a hyphen}
    @item{all characters are lower-cased}
    @item{dots are converted to hyphens}
    @item{"make-" instead of "new-" is used in the constructor name}
   ]

For the @racket[#:namer namer] option to be used, the renamer function must be
present for-syntax. The provided renamer will be passed by the autobinder 
a symbol representing the type being one of object to be binded
('predicate 'class 'mutator, 'accessor, 'jtype, or 'method). a string for class-name, a string
representing the method or field name or #f if type 'jtype or 'predicate is passed.
The renamer is expected to return a string which will be used by the autobinder to bind the object.

If the @racket[#:unsafe] option is used, no contract checking will 
be done on the bindings, however checking will still be done for both overloaded 
methods and constructors
                                                   
                                                                    
}