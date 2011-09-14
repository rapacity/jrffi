#lang scribble/doc
@(require 
  (for-label racket
    (except-in "../main.rkt" ->))
  
  
  scribble/racket
  scribble/eval
  scribble/base
  scribble/manual
  
  "../main.rkt" )




@larger{Warning: The library is experimental. The documentation is filled with errors, specification of incomplete and deprecated features, etc.... The examples section should have working code though.}


@defmodule[jrffi]


@title[#:style 'quiet #:tag "jrffi"]{jrffi - Java FFI}

@table-of-contents[]

@include-section["examples.scrbl"]
@include-section["require.scrbl"]
@include-section["types.scrbl"]
@include-section["vector.scrbl"]
