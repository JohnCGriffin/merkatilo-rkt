#lang scribble/manual

@(require (for-label racket))

@title{Merkatilo Time-Series}

@margin-note{This is tested on Racket 6.11}


@section{Background}

 After having creating a considerable amount of very fast, complete, and huge
 technical analysis tools in Java, C++ and Zentech, I just wanted something simple.
 Thus, the merkatilo
 libraries are
 my most-used subset of functionality in single language implementations.
 This is the Racket implementation.
 
@;--------------------------------------

@section{Overview}

The basic data structures are a Time Series structure that wraps a simple date-to-number
function, a date, and a dateset which is an ordered collection of dates.  A time series is represented by struct @tt{series} which takes only a date->optional-number function and a name.  It
is the argument to many functions that beget new @tt{series} instances.

The functions operating transforming series to new series come in two styles, those oriented
to a sequence of dates, and those that require no @tt{dateset}.  For example, the @tt{sma}
procedure creates a new series representing a running average of the input series over some
@tt{dateset}.  However, @tt{add} adds two input series on a date without respect to any
date sequence.

@;--------------------------------------



