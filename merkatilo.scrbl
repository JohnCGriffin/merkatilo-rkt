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

Please note that if you attempt to do the example above, it will not work.  That is because
this library manipulates times series; it does not provide financial data.  You have to come up
with that yourself.  If you are just studying, investigate using the St. Louis
Fedeeral Reserve FRED database, OECD, and Quandl.

@;--------------------------------------

@section{Dates}

A @tt{jdate?} is simply an integer in [@tt{MIN-DATE} @tt{MAX-DATE}], covering the days of the
years from 1700 through 2100.  Because it is just an integer, finding the @tt{jdate?} before or
after another @tt{jdate?} is simple.  Common @tt{jdate?} operations include finding @tt{today}, and
transforming from and to text representations.  Also note that "text representation" means
ISO 8601 YYYY-MM-DD.

@deftogether[(@defthing[MIN-DATE jdate?]
              @defthing[MAX-DATE jdate?])]{
First and last valid jdate? instances.
I figure that no finance stuff before 1700 or
after I die matters enough to generate unit tests for irrelevent values.
}

@defproc[(today [optional-offset integer?]) jdate?]{
@tt{(today)} is today, @tt{(today -1)} is yesterday and
@tt{(today 2)} is the day after tomorrow.
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

@defproc[(ymd->jdate [ymd ymd?]) jdate?]{
From year, month, and day, create a jdate?
}

@defproc[(jdate->ymd [date jdate?]) ymd?]{
Return ymd struct from jdate?.
}

@defproc[(jdate? [object any]) boolean?]{
Is this thing a jdate?
}

@defproc[(jdate-year [date jdate?]) integer?]{
Return the year.
}

@defproc[(jdate-month [date jdate?]) integer?]{
Return the month (1-12).
}

@defproc[(jdate-day [date jdate?]) integer?]{
Return the day of month (1-31).
}

@defproc[(jdate-weekday [date jdate?]) integer?]{
Return the day of the week, 0=Sunday, 6=Saturday.
}



@;----------------------------------------

@section{Date Sets}

A @tt{dateset} is a wrapper around a strictly ascending immutable vector of @tt{jdate?}.
Many operations require a dateset to traverse, such as @tt{ema}, @tt{sma}, and even
@tt{dump}.  In general, the @tt{dateset} is optional and usually found through a
paramter @tt{current_dates}.  Constructing a @tt{dateset} is best done via the
@tt{dates} operation which flexibly intersects dates associated with series and
other entities, including other @tt{dateset} instances.

@defstruct*[dateset ([vector vector?])]{
A structure containing an ordered set of @tt{jdate?} (i.e. julian integers).  More commonly
a user will construct a dateset with the @tt{dates} operator.
}

@defparam[current-dates dateset dateset?]{
The current-dates is used throughout the API, although most procedure permit overriding
it with a @tt{#dates} option.  As with any parameter, @tt{parameterize} can set a scoped
binding, but an easier way is the @tt{with-dates} macro.
}
 
@defproc[(dates [spec any] ... [#:first first jdate? MIN-DATE][#:last last jdate? MAX-DATE]
[#:union union boolean? #t][#:expanded expanded boolean? #f]) dateset?]{
Construct a dateset from other entities and constraints.  Usually wanting to work with
compatible data, the union of the specifications is appropriate. Occasionally, one needs
full calendar dates with endpoints constrained by the specs; that is handled with @tt{expanded}.
A typical usage would be:

@racketblock[
(dates SPY DJIA MSFT #:first '1998-1-1)
]

This example will find the intersection of SPY, DJIA and MSFT dates starting in 1998.  
}

@defstruct*[date-range([first optional-jdate?][last optional-jdate?])]{
Handy specification for date bounds, either end of which can be #f.  A date-range is
one of the available types to pass to @tt{dates}.
}
 
@defproc*[([(first-date) jdate?]
           [(first-date [ dates dateset? ]) jdate?])]{
First date of specified dates or first date of @tt{current-dates} parameter.		    
}
 
@defproc*[([(last-date) jdate?]
           [(last-date [ dates dateset? ]) jdate?])]{
Last date of specified dates or last date of @tt{current-dates} parameter.		    
}

@;--------------------------------------

@section{Sequenced Series}

Many series generating functions require traversal of a dateset.  These are sequenced series.

@subsection{Series Smoothings}

To dampen daily movements in a series, two common smoothing operations are the simple
moving average @tt{sma} and the exponential moving average @tt{ema}.  Like other sequential
operations, a dateset is required to perform the operation.  If not supplied, the
@tt{current-dates} parameter is used.

@defproc[(ema [input=series series?] [N integer?] [#:dates dates dateset? (current-dates)])
series?]{
From N, a fraction is derived, F=2/(N+1), requiring N > 1.  Each observation in the output series is that fraction
F times the value plus (1-F) times the preceding value.  Thus ema(IBM,10) will smooth each price
of IBM such that the current value is weighted 2/11 and 9/11 is multiplied by the previous value.
The first value or any value following a missing observation is simply the input value.  The number
of output observations equals the number of input observations. That feature plus the weighting of
new input heavier than older input makes this a more useful smoothing operator than @tt{sma}.

The default value for @tt{#:dates} is @tt{(current-dates)}.
}

@defproc[(sma [input=series series?] [Period integer?] [#:dates dates dateset? (current-dates)])
series?]{
Each output value represents the average of the most current @tt{Period} values.  Until that
number of values is met, there is no output.  Upon encountering a missing observation, the
output average will be delayed again.  The maximum number of output values in the produced
series is therefore (Period-1) less than the input.  This function matches what most people
expect of a moving average.  Otherwise, @tt{ema} is a generally better choice.

The default value for @tt{#:dates} is @tt{(current-dates)}.
}


@subsection{Signal Generation}

Signal series are those that have non-repeating instances of buy and sell signals,
respectively respresented as 1 and -1.  Building trading models with thise library,
one strives to find signal generation that says something like "Buy low, sell high."
By passing any manipulated series through @tt{to-signals}, any sequence of values will
be translated into non-repeating -1 for negative values and 1 for non-negative values.

A very common signal is a cross, generating a signal as one series moves above or below
another.  The press often talks of a market index passing its 200-day moving average as
an important event.  By the way, "200-day" in the press usually means 200 market days,
not calendar days.

Reversals identify when a series has experienced a drop after a local peak or risen after
a local trough.

@defproc[(cross [#:slower slower series?]
[#:faster faster series?]
[#:upside-factor upside-factor number? 1.0]
[#:downside-factor downside-factor number? 1.0]
[#:dates dates dateset? current-dates]) series?]{
When the faster series surpasses the slower series, a buy signal (1) fires
and a sell (-1) when it goes below.  These boundaries are altered with
upside-factor and downside-factor.  For instance, the following
generates signals then the SPY ETF price series passes 1% above and 1% below
its 200-period average.

@racketblock[
(cross #:slower (sma SPY 200) #:faster SPY #:upside-factor 1.01 #:downside-factor .99)
]
                                         }

@defproc[(reversals [s series?][#:down-factor down-factor number?][#:up-factor up-factor number?][#:dates dates dateset? current-dates]) series?]{
When a series ascends above up-factor multiplied by a preceding local minimum, a buy (1) signal is produced.  Upon descending below the product of a local maximum and down-factor, a sell (-1) signal is produced.  For instance, if you want to get out of the stock market after 15% corrections and back in after 10% reversal back up.

@racketblock[
(reversals DJIA #:down-factor .85 #:up-factor 1.1)
]

It is worth checking out various combinations of this strategy as a confirmation of the soundness of buy-hold.
}

@defproc[(conviction [input-signals series?][periods integer?][#:dates dates dateset current-dates]) series?]{
The conviction operator reduces whipsaw signals by delaying the signal then producing it only if
it is not flipped with the specified number of periods.  As a small side benefit, any practical
trading strategy receives a "heads-up" that a signal is imminent.
}

@defproc[(to-signals [series series?][#:dates dates dateset current-dates]) series?]{
Any series that generates positive and negative numbers indicating buys and sells respectively
may be converted to a proper signal series (non-repeated sparse 1 and -1 values).  For instance,
one can generate signals based upon the periodic momentum of a series as in:

@racketblock[
(to-signals (mo-days IBM 365))
]
}

@;-----------------------------------

@;-----------SEQUENCED ---------------





@section{Unsequenced Series}

When a series can be generated without respect to a dateset, it is unsequenced. These include
constant-value series, arithmetic on series, and/or logic, filters, mappings and calibration.

@subsection{Arithmetic}

@defproc[(constant [value real?]) series?]{
A series is created that always responds to a date query with the single @tt{value}.
}

@defproc[(add [a series-or-real?][b series-or-real?]) series?]{
Creates a series of the sum of @tt{a} and @tt{b} at each date for which both series
generate values. If either @tt{a} or @tt{b} is a number, it is converted to a series
first via @tt{constant}.
}

@defproc[(sub [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{add}.
}

@defproc[(mul [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{add}.
}

@defproc[(div [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{add}, but no value is generated when @tt{b} is zero.
}

@;-----------------------------


@subsection{Inequalities}

@defproc[(gt [a series-or-real?] [b series-or-real?]) series?]{
   After converting any numeric argument to a series via @tt{constant},
   the value of @tt{a} is copied to the output series if that value
   if greater than the value of @tt{b} at that date.  Most commonly,
   a numeric argument is second as in this example which looks for
   periods where the 1-year gain is greater than 20%.

@racketblock[
(gt (mo-days a-price-series 365) 1.2)
]
}

@defproc[(ge [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{gt} but uses >=.
}

@defproc[(lt [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{gt} but uses <.
}

@defproc[(ge [a series-or-real?][b series-or-real?]) series?]{
Analogous to @tt{le} but uses <=.
}

@;------------------------------------------

@subsection{Logical}

@defproc[(series-and [a series?][b series?]) series?]{
At any date query for which @tt{a} has a value, the output series produces the value produced
by @tt{b} at that date.
}

@defproc[(series-or [a series?][b series?]) series?]{
At any date query for which @tt{a} has a value, that value is returned.  Otherwise, whatever
is returned by @tt{b} is the response.
}

@;---------------------------------------------


@subsection{Functional}

@defproc[(series-map [proc procedure?][input series?]...[#:missing-data-permitted missing-data-permitted boolean? #f]) series?]{
The procedure @tt{proc} must handle arity that matches the number of input series, analogous
to @tt{map} on lists.  Generally, the @tt{missing-data-permitted} option is false. If the
given mapping procedure handles missing values, then missing-data-permitted should be set true.
For instance to take the square root of a series:

@racketblock[
(map-series sqrt my-series)
]
}

@defproc[(series-filter [predicate procedure?][input series?]) series?]{
Each value produced by the input series is passed to the output series unchanged if that value
passes a predicate function.  For instance, @tt{(gt IBM 100)} is equivalent to
@racketblock[
(series-filter (curryr > 100) IBM)
]
}

@;--------------------------------------------------

@;-------------- UNSEQUENCED ------------------