#lang scribble/manual

@(require (for-label racket))

@title{Merkatilo Time-Series}

@margin-note{This is tested on Racket 6.11}

@defmodule[merkatilo]

@section{Background}

The merkatilo time-series library implementations address my 
personal financial computing research requirements.
This is the Racket implementation.

The purpose of this library is financial computing research and education, not trading
with material funds at risk.  Use it accordingly.

The code resides at @(hyperlink "https://github.com/JohnCGriffin/merkatilo-rkt" "Github") under MIT licensing.
Corrections, improvements, or other comments are welcome.  Please include merkatilo
in the subject line.

John Griffin, griffinish at gmail
 
@;--------------------------------------

@section{Overview}

The basic data structures are a time series structure that wraps a simple date-to-number
function, a date, and a dateset which is an ordered collection of dates.  A time series
is represented by struct @tt{series} which takes only a date->optional-number function and a name.
It is the argument to many functions that beget further @tt{series} instances.

Series creations come in two styles, those requiring 
a sequence of dates implemented as @tt{dateset}, and those that do not.  For example, the @tt{ma}
procedure creates a new series representing a running average of the input series over a
@tt{dateset}.  However, @tt{add} sums two input series on a date without reference to a
date sequence.

Speaking of dates, with merkatilo, they are called @tt{jdate}, meaning julian date.  The
merkatilo julian date coincides with Postgres' julian date value.

Here's an example that loads the SPY ETF adjusted closing price, does a cross of that series
with its 200-period moving average, generating buy (+1) and sell (-1) signals.  Finally, it
dumps them out in date order, like a dumped spreadsheet.

@racketblock[
(require merkatilo)

(define SPY (lo-set-dates 'SPY))
(define smoothed (ma SPY 200))
(define my-signals
   (cross #:slower smoothed #:faster SPY))

(dump SPY smoothed my-signals)
]

Please note that if you attempt to do the example above,
@italic{it will not work}.  That is because
this library manipulates times series; it does not provide financial data.  You have to come up
with that yourself.  If you have no data source, investigate using the St. Louis
Federal Reserve @(hyperlink "https://fred.stlouisfed.org/" "FRED database").


@;--------------------------------------

@section{Dates}

A @tt{jdate?} is simply an integer in [@tt{MIN-DATE} @tt{MAX-DATE}], covering the days of the
years from 1700 through 2100.  Because it is just an integer, finding the @tt{jdate?} before or
after another @tt{jdate?} is simple.  Common @tt{jdate?} operations include finding @tt{today}, and
transforming from and to text representations.  Also note that "text representation" means
ISO 8601 YYYY-MM-DD.  Text output includes zero-padded month and days for single-digit values;
all text representations of @tt{jdate?} are ten characters in length.

@deftogether[(@defthing[MIN-DATE jdate?]
              @defthing[MAX-DATE jdate?])]{
First and last valid @tt{jdate?} instances.
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
Parse an ISO 8601 YYYY-MM-DD text string into a @tt{jdate?}.  The content
will be verified as a legal date or raise an exception.
}

@defproc[(->jdate [date-like string?/symbol?/jdate?]) jdate?]{
Given a symbol or string representation of a @tt{jdate?}, parse it.  If it is
a @tt{jdate?}, it passes through.
}

@defstruct*[ymd ([year integer?][month integer?][day integer?])]{
The immutable @tt{ymd} checks its year, month, and day arguments for
validity.  Typically used with @tt{ymd->jdate} to construct a @tt{jdate?}.
}

@defproc[(ymd->jdate [ymd ymd?]) jdate?]{
From year, month, and day, create a @tt{jdate?}.
}

@defproc[(jdate->ymd [date jdate?]) ymd?]{
Return a @tt{ymd?} from @tt{jdate?}.
}

@defproc[(jdate? [object any]) boolean?]{
Is this thing a @tt{jdate?}
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
Many operations require a dateset to traverse, such as @tt{ema}, @tt{ma}, and 
@tt{dump}.  In general, the @tt{dateset} is optional and usually found through a
parameter @tt{current_dates}.  Constructing a @tt{dateset} is best done via the
@tt{dates} operation which flexibly intersects dates associated with series and
other entities, including other @tt{dateset} instances.

@defstruct*[dateset ([vector vector?])]{
A structure containing an ordered set of @tt{jdate?} (i.e. julian integers).  More commonly
a user will construct a dateset with the @tt{dates} operator.
}

@defparam[current-dates dateset dateset?]{
The current-dates is used throughout the API, although most procedures permit overriding
it with a @tt{#dates} option.  As with any parameter, @tt{parameterize} can set a scoped
binding, but an easier way is the @tt{with-dates} macro.
}

@racketblock[
(with-dates SPY
  (do-something)
  (do-something-else))
]

 
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
@tt{date-range} is a handy specification for date bounds, either end of which can be #f.  It is 
one of the available specification types to pass to @tt{dates}.
}
 
@defproc*[([(first-date) jdate?]
           [(first-date [ dates dateset? ]) jdate?])]{
First date of specified dates or first date of @tt{current-dates} parameter.		    
}
 
@defproc*[([(last-date) jdate?]
           [(last-date [ dates dateset? ]) jdate?])]{
Last date of specified dates or last date of @tt{current-dates} parameter.		    
}

@defproc[(nearest [dt jdate?][#:dates dates dateset? current-dates]) optional-jdate?]{
Return the latest date in the @tt{dateset} that is not greater than the requested date. Requests
outside the given dates will return #f.  
}

@defproc[(nearest+ [dt jdate?][#:dates dates dateset? current-dates]) optional-jdate?]{
Return the earliest date in the @tt{dateset} that is not less than the requested date.  Requests
outside the given dates will return #f.
}
          



@;--------------------------------------

@section{Sequenced Series}

A series that references a sequence of dates is sequenced.
These include smoothing, signal generation, and time shifts. For each of these operations,
the needed @tt{dateset}
is supplied via the @tt{#:dates} parameter which defaults to the @tt{current-dates}
parameter.

@subsection{Series Smoothings}

To dampen daily movements in a series, merkatilo has smoothing operations: @tt{ma} (moving average), @tt{ema} (exponential moving average), and @tt{fractional}.  As with other sequential
operations, a dateset is required.  If not supplied, the
@tt{current-dates} parameter is used.

@defproc[(fractional [input-series series?] [fraction (0 < fraction < 1)] [#:dates dates dateset? (current-dates)])
series?]{
Fractional smoothing multiples the fraction by the current observation plus (1-fraction) times
the previous result.  The first value or any value following a missing observation is simply the input value.  The number
of output observations equals the number of input observations. That feature plus stronger weighting of newer
input makes @tt{fractional} more useful than @tt{ma}.

The default value for @tt{#:dates} is @tt{(current-dates)}.
}

@defproc[(ema [input-series series?] [N integer?] [#:dates dates dateset? (current-dates)])
series?]{
From N, a fraction F is derived, F=2/(N+1), requiring N > 1.
Each observation in the output series is that fraction
F times the value plus (1-F) times the preceding value.  Thus @tt{ema(IBM,10)}
will smooth each price
of IBM such that the current value is weighted 2/11 and 9/11 is multiplied by the previous value.
The fraction F is passed to the more general @tt{fractional} operator.

The default value for @tt{#:dates} is @tt{(current-dates)}.

@italic{Note: EMA stands for 'exponential moving average', 
a term used by technical analysts to describe
a process that utilizes neither exponents nor averages.}
}

@defproc[(ma [input-series series?] [period integer?] [#:dates dates dateset? (current-dates)])
series?]{
Each output value represents the average of the most current @tt{period} values.  Until that
number of values is met, there is no output.  Upon encountering a missing observation, the
output average will be delayed again.  The maximum number of output values in the produced
series is therefore (period-1) less than the input.  

The default value for @tt{#:dates} is @tt{(current-dates)}.
}


@subsection{Signal Generation}

Signal series are those that have non-repeating instances of buy and sell signals,
respectively respresented as 1 and -1.  Building trading models with this library,
one strives to find signal generation that says something like "Buy low, sell high."
By passing any manipulated series through @tt{to-signals}, any sequence of values will
be translated into non-repeating -1 for negative values and 1 for non-negative values.

A very common signal is a cross, generating a signal as one series moves above or below
another.  The market press often talks of a market index passing its 200-day moving average as
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
(cross #:slower (ma SPY 200)
       #:faster SPY
       #:upside-factor 1.01
       #:downside-factor .99)
]
                                         }

@defproc[(reversals [s series?]
                    [#:down-factor down-factor number?]
                    [#:up-factor up-factor number?]
                    [#:dates dates dateset? current-dates]) series?]{
When a series ascends above up-factor multiplied by a preceding local minimum, a buy (1) signal is produced.
Upon descending below the product of a local maximum and down-factor, a sell (-1) signal is produced.
For instance, if you want to get out of the stock market after 15% corrections and back in after 10% reversal back up.

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

@subsection{Other Sequenced Series}

@defproc[(warp [input series?][amount integer?][#:dates dates dateset current-dates]) series]{
  The @tt{warp} command shifts the values forward or backward along the dateset.  A valid use
  of warp is to shift signals one-day later to generate an equity-line to reflect the reality
  that one cannot trade signals after a market has delivered end-of-day closing values.  Sadly,
  many great trading ideas do not perform as well when traded after the signals arrive.
}

@defproc[(calibrate [input series?][#:init init real? 100][#:date date jdate? first-date]) series?]{
The @tt{calibrate} operator restates a series proportionally to a new value at a specified date.
This is most useful when comparing the progress of one or more price series.  Such a use is
exemplified by a spider chart.
}

@defproc[(prepend [primary series?][#:with-surrogate surrogate series?][#:dates dates dateset current-dates]) series?]{
When a primary series has insufficient history for analytics and a reasonable surrogate
exists, prepend joins them, normalizing the values to the primary one at the point
where they are siamesed.  For example, to do some sector analysis with
a First Trust sector ETF, one might prepend it with a similar iShares sector ETF 
to approximate history for an analysis the precedes the First Trust inception.
}

@defproc[(window-series [input series?]
                        [n-periods integer?]
                        [proc procedure?]
                        [#:dates dates dateset current-dates]
                        [#:missing-data-permitted missing-data-permitted boolean? #f]) series?]{
Collect sliding view of input values represented as a vector of length @tt{n-periods}.
The supplied procedure receives the vector and responds with a number.
For instance to find 22-day maxima:
@racketblock[
(window-series input-series 22 (λ (v) (apply max (vector->list v))))
]
Although flexible, window-series has quadratic big-O.  Treat it accordingly.
}

@;-----------SEQUENCED ---------------





@section{Unsequenced Series}

When a series can be generated without referencing a dateset, it is unsequenced. These include
constant-value series, arithmetic on series, and/or logic, filters, and mappings.

@subsection{Arithmetic}

@defproc[(constant [value real?]) series?]{
A series is created that always responds to a date query with the single @tt{value}.
}

@defproc[(add [a series-or-real?][b series-or-real?]) series?]{
Creates a series of the sum of @tt{a} and @tt{b} at each date for which both series
generate values. If either @tt{a} or @tt{b} is a number, it is first converted to a series
via @tt{constant}.  Thus, these two expressions are equivalent:
@tt{(add some-series 1)} and @tt{(add some-series (constant 1))}
}

@defproc[(sub [a series-or-real?][b series-or-real?]) series?]{
subtraction analogous to @tt{add}
}

@defproc[(mul [a series-or-real?][b series-or-real?]) series?]{
multiplication analogous to @tt{add}
}

@defproc[(div [a series-or-real?][b series-or-real?]) series?]{
division analogous to @tt{add}, when @tt{b} is non-zero
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
analogous to @tt{gt} using >= comparision
}

@defproc[(lt [a series-or-real?][b series-or-real?]) series?]{
analogous to @tt{gt} using < comparision
}

@defproc[(le [a series-or-real?][b series-or-real?]) series?]{
analogous to @tt{gt} using <= comparision
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
The procedure @tt{proc} must handle arity that matches the number of input series, as
@tt{map} does on lists.  Generally, the @tt{missing-data-permitted} option is false. If the
given mapping procedure handles missing values, then missing-data-permitted should be set true.
For instance to take the square root of a series:

@racketblock[
(map-series sqrt my-series)
]
}

@defproc[(series-filter [predicate procedure?][input series?]) series?]{
Each value produced by the input series is passed to the output series unchanged if that value
passes the predicate.  For instance, @tt{(gt IBM 100)} is equivalent to
@racketblock[
(series-filter (curryr > 100) IBM)
]
}

@;--------------------------------------------------

@;-------------- UNSEQUENCED ------------------

@section{Performance Measurement}

If you are developing signals, you must be interested in how "good" those
signals are. Likewise, if you are developing an asset allocation system,
you need to know how good the rebalanced asset system is.
Good means many things to many people; some value 
the total money earned over the lifetime of a model, while others value risk
avoidance.  That quantification of goodness is likely constructed from a combination
of aiming for low return volatility, high returns and mimimizing unrealized
losses.

@subsection{Equity Line Generation}

Whether developing signals or allocations, evaluation of goodness requires that
you build an equity line reflecting the result of changing investment states.
That functionality
is provided by @tt{equity-line} when using signals and @tt{allocation-equity-line}
for asset allocation or rebalancing.  Both types of equity lines start on the first
signal or allocation.

@defproc[(equity-line [input series?]
                      [signals series?]
                      [#:init initial-value real? 100]
                      [#:alternate-investment alternate-investment optional-series? constant-value-1]
                      [#:dates dates dateset current-dates]) series?]{
    If an alternate investment is not specified, it is set to @tt{(constant 1)} and funds are
    set to @tt{initial-value} dollars.  When a buy signal arrives, all funds purchase the series
    at its price (the value of the series at that signal date), resulting in shares.  Upon
    a sell signal, the shares are liquidated and used to purchase the alternate investment at
    its price.  With @tt{initial-value} at its default of 100, the value of the output series
    should the accumulated percentage value related to the original investment.  Until the
    first signal is encountered, funds are idle, invested in neither the primary nor alternate
    investment.  A graphed equity line will therefore always start with a flat zone before
    the first signal, as funds are in neither in the primary nor alternate investments.

    The output of @tt{equity-line} is what you will likely use with the performance measures
    of gain, drawdown and volatility.
}

@defproc[(allocation-equity-line [allocations (listof allocation?) ]
[#:init initial-value real? 100]) series?]{
  Rather than @tt{equity-line}, which uses signals, an @tt{allocation-equity-line} specifies
  portfolio weightings on dates and generates an equity line with respect to the specified
  rebalancings of a portfolio.
  An allocation structure contains a @tt{jdate?} and a @tt{portion?} list where @tt{portion}
  contains a series and a weight.  For instance, to double-weight a financial ETF (FXO)
  and single-weight an energy sector (FXN) which is rebalanced on the 15th of each month, the
  creation of the equity line might look like:

@racketblock[
(define 15ths (some-way-to-get-15ths ...))
(define weightings (list (portion FXO 2) (portion FXN 1)))
(define allocations
   (map (λ (dt) (allocation dt weightings)) 15ths))
(define rebalanced-equity-line
   (allocation-equity-line allocations))
]

}

@;----------- EQUITY LINE GENERATION ---------------

@subsection{Basic Measurements}


@defproc[(volatility [input series?][#:days days integer? 365][#:dates dates dateset current-dates]) real?]{
   Given a running ratio of new/old over @tt{days} (default 365), return the
   standard deviation of those ratios.
}

@defproc[(gpa [input series?][#:dates dates dateset current-dates]) real?]{
   Gain per annum is calculated as @italic{gains@superscript{(1/Y)}-1} where Y=days/365.2425.
}

@subsubsection{Drawdown}
Drawdown is the description of an unrealized loss in equity value. 
If you persevered through the downtime of the 2008 S&P 500 market decline and its recovery,
you experienced a period of maximum
unrealized loss between October 2007 and March of 2009 of 55%.  That 55% was your drawdown.

@defstruct*[drawdown ([max observation?][min observation?])]{
  The drawdown struct contains two observations, the earlier being the higher valued observation and
  the latter being a lesser valued observation such that that combination of
  observed date-value pairs represents the greatest loss in value over the history
  of a series.
}

@defproc[(drawdown-residual [ drawdown drawdown? ]) real?]{
Given a drawdown, calculate the residual after the drop of the drawdown.  For instance, a drawdown of 24% will
have a residual of 0.74.
}

@defproc[(series-drawdown [input series?][#:dates dates dateset current-dates]) drawdown?]{
   For a given series, return its drawdown, i.e. the pair of observations representing the
   greatest loss in series value.
}

@defproc[(series-drawdowns [input series?][#:dates dates dateset current-dates][#:max-residual max-residual real? 1]) (listof drawdown?)]{
Return a list of all drawdowns, ordered by drawdown-residual, i.e. worst drawdown first.
The max-residual can be used to limit the results
to consequential drawdowns.  For instance, to find the drawdowns of at least 15% for the SPY ETF:

@racketblock[
(series-drawdowns SPY #:max-residual .85)
]
}

@;---------- drawdown --------

@;---------- basic ------------

@subsection{Summarized Measurement}

@defstruct*[performance ([volatility-residual real?][drawdown-residual real?][annualized-gain real?][long-ratio real?][trades integer?])]{
    The @tt{performance} struct is a handy collection of performance measures generated by
    @tt{investment-performance}.  The volatility-residual respresents 1 minus the volatility, thus
    a series with a high standard deviation of returns has a lower volatility-residual.
    The drawdown-residual represents 1 minus the drawdown amount.  For instance, the SP500 lost
    about 55% back in 2008, so its drawdown-residual is about 0.45.

The volatility-residual, drawdown-residual
and annualized-return values are arranged such that greater values are better
values.  These components will likely suffice to create a goodness
function that fits your needs.  The long-ratio and trades are
potentially useful as filters.  For instance, you may decide that any
model should be invested at least half the time or you want enough
signals to convince yourself that the system is not based upon flukes
of market history, but not so many that you trade too often.

}

@defproc[(investment-performance [investment series?]
                                 [#:alternate-investment alternate-investment series? constant]
                                 [#:signals signals series? #f]
                                 [#:dates dates dateset? current-dates]) performance?]{
Return the performance structure of trading based upon signals or
simply return the performance of buy/hold if no signals are supplied.  Don't forgot
to @tt{warp} the signals by one if you want realistic results to daily end-of-day signals.
}

@;--------- Summarized -----------

@subsection{Benchmark Capture}

Capture describes  how amplified or diminished periodic returns are relative to some benchmark.  In calculating
a capture against the returns of a benchmark series, e.g. the S&P 500 total return, the benchmark gains/losses
are divided into positive and negative observations (zero being excluded).
For all values where the benchmark return observations are positive, the sum of the matching investment returns
divided by the sum of the benchmark returns yields the up-capture value.
Conversely, negative benchmark return observations are utilized to calculate
the down-capture value.  A capture result will be centered about 1.0, with values greater than one indicating
amplified gains or losses and values less then one indicating diminished gains or losses.  An up-capture greater
than one is viewed positively, as is a down-capture less than one.


@defproc[(up-capture [investment series?]
[#:benchmark benchmark series?]
[#:period period positive-integer?]
[#:dates dates dateset? current-dates]) real?]{
 Using the result of @tt{(mo investment period)} to define changes to an investment and @tt{(mo benchmark period)}
to define changes to the benchmark, each positive change in the benchmark is summed as well as summing the investment
changes on the same dates. The result of up-capture is the ratio of the investment's sum of changes over the benchmark's.
An up-capture > 1 indicates that on market gains, the investment performs better on average.  Inversely, an
up-capture < 1 indicates underperformance of the benchmark over the selected positive return benchmark observations.
}

@defproc[(down-capture [investment series?]
[#:benchmark benchmark series?]
[#:period period positive-integer?]
[#:dates dates dateset? current-dates]) real?]{
 Using the result of @tt{(mo investment period)} to define changes to an investment and @tt{(mo benchmark period)}
to define changes to the benchmark, each negative change in the benchmark is summed as well as summing the investment
changes on the same dates.  The result of down-capture is the ratio of the investment's sum of changes over the benchmark's.
A down-capture > 1 indicates that on market losses, the investment performs worse than the benchmark on average.
Inversely, a down-capture < 1 indicates that an investment outperforms the benchmark over the selected negative
return benchmark observations..
}

@;------- CAPTURE ----------

@;--------- PERF MEASURES ---------------

@section{Utilities}

@defparam[current-loader procedure procedure?]{
This tells the @tt{lo} operator how to fetch series from an id.  The id is normalized to
a string in the form of TICKER::SUBJECT, e.g. IBM::VOLUME.  Without the double colon
second part, ::CLOSE is assumed.  Thus, @tt{'IBM} or @tt{"IBM"} is normalized to @tt{"IBM::CLOSE"}
and your
loader returns the series.

The supplied default loader reads from ASCII streams containing date-value lines
at URL locations
specified in @tt{/etc/merkatilo/default-loader-config.json}.  Here's a sample config file:

@codeblock|{
{
  "cache-seconds" : 3600,
  "data-source" : {
    "regex"       : "^([A-Z][A-Z0-9]*)::([A-Z0-9]+)$",
    "replacement" : "https://mydata-provider.net/ticker=?\\1&record=\\2"
  }
}
}|

The supplied loader works fine for the original author's needs.
If the supplied loader will not work for you, then simply make your own loader and set the
@tt{current-loader} parameter to make @tt{lo} fit your needs.

}

@defproc[(lo [id symbol-or-string?]) series?]{
Given an id such as @tt{'SPY}, @tt{"SPY"}, @tt{'SPY::HIGH}, or @tt{"IBM::OPEN"}, return a series.
}

@defproc*[([(first-ob [s series?][#:dates dates dateset current-dates]) observation?]
	   [(last-ob [s series?][#:dates dates dateset current-dates]) observation?])]{
	   @tt{first-ob} delivers the first valid observation in a series while @tt{first-value}
	   returns that observation's value. 
}

@defproc*[([(min-max-obs [s series?][#:dates dates dateset current-dates]) (values observation? observation?)]
[(min-ob [s series?][#:dates dates dateset current-dates]) observation?]
[(max-ob [s series?][#:dates dates dateset current-dates]) observation?])]{
The @tt{min-max-obs} operator returns both the minimum and maximum observations.  @tt{min-ob} and
@tt{max-ob} are derived.
}

@defproc[(dump [s series?]...[#:first first optional-jdate? #f][#:last last optional-jdate? #f][#:dates dates dateset? current-dates]) void?]{
The @tt{dump} procedure simply prints one or more series on separate dates lines where at least
one of the series must have a value on each date shown.
}

@defproc[(series-count [s series?][#:dates dates dateset current-dates]) integer?]{
Returns the number of observations in a series for the specified dates.
}



@;-------------------------------------------






