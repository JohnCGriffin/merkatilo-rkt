#lang scribble/manual

@(require (for-label racket))

@title{Merkatilo Time-Series}

@margin-note{This is tested on Racket 6.11}

@defmodule[merkatilo]

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

Speaking of dates, with merkatilo, they are called @tt{jdate}, meaning julian date.  The
julian date coincides with Postgres' idea of a julian.

Here's an example that loads the SPY ETF adjusted closing price, does a cross of that series
with its 200-period moving average, generating buy (+1) and sell (-1) signals.  Finally, it
dumps them out in date order, like a dumped spreadsheet.

@racketblock[
(require merkatilo)
(define SPY (lo-set-dates 'SPY))
(define smoothed (sma SPY 200))
(define my-signals
   (cross #:slower smoothed #:faster SPY))
(dump SPY smoothed my-signals)
]

@;--------------------------------------

@section{Dates}

A jdate? is simply an integer between MIN-DATE and MAX-DATE, covering the days of the
years from 1700 through 2100.  Because it is just an integer, finding the jdate? before or
after another jdate? is simple.  Common jdate? operations include finding @tt{today}, and
transforming from and to text representations.  Also note that "text representation" means
ISO 8601 YYYY-MM-DD.

@deftogether[(@defthing[MIN-DATE jdate?]
              @defthing[MAX-DATE jdate?])]{
First and last valid jdate? instances.
I figure that no finance stuff before 1700 or
after I die really matters.
}


@defproc[(jdate->text [date jdate?]) string?]{
Transform a julian date integer to text form.
For instance, 2451545 -> "2001-01-01". 
}

@defproc[(text->jdate [date-text string?]) jdate?]{
Parse and ISO 8601 YYYY-MM-DD text string into a jdate?.  The content
will be verified as a legal date or raise and exception.
}

@defproc[(->jdate [date-like string?/symbol?/jdate?]) jdate?]{
Given a symbol or string representation of a jdate?, parse it.  If it is
a jdate?, it passes through.
}

@defstruct*[ymd ([year integer?][month integer?][day integer?])]{
The immutable ymd checks its year, month, and day arguments for
validity.  Typically used with ymd->jdate to construct a jdate?.
}

@defproc[(jdate->ymd [date jdate?]) ymd?]{
Return ymd struct from jdate?.
}



@;----------------------------------------

@section{Date Sets}

@defstruct*[dateset ([vector vector?])]{
A structure containing an ordered set of jdate? (i.e. julian integers).  More commonly
a user will construct a dateset with the @tt{dates} operator.
}



@;--------------------------------------

