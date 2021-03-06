# merkatilo-rkt
Racket implementation of [merkatilo libraries](https://merkatilo.org).

## Purpose

This library serves two goals.  First, it is used to perform active research in the stock market.  Secondly, it's a
nice size for programming research.  Other implementations exist in C++ and Python. This version is implemented in Racket.  It is a library, not a complete system.  Specifically, you must add a data source.

## Setup

1) You need Racket 6.11 or higher.
2) ``cd the-place-you-like-personal-racket-libraries``
3) ``git clone this-repository-url merkatilo``
4) ``cd merkatilo``
5) ``make test``
6) ``cd ..``
7) ``raco link merkatilo``

_Note that when running the test for the first time, test files are downloaded from another
github repository and placed in ``/tmp/merkatio-test-data/``.  The same tests are used across equivalent code
in C++, Racket, and Python._

## Get some data

You will eventually need a data source.  Time series data are loaded into the system via the ``lo`` operator, e.g. ``(lo 'SPY)``.
That procedure references a parameter ``current-series-loader`` which defaults to ``default-loader`` found in the private/ subdirectory.  The ``default-loader`` looks for ASCII series files in ~/TIME_SERIES/TICKER/RECORD.

Also note that a built-in series exists for testing - use ``(require merkatilo/private/test-support)`` and then reference ``TEST-SERIES``.  You can play with that until you decide where to get your data.  By the way, if you edit the test series, almost all unit tests will fail.  

## Usage Overview

Major operations typically associated with technical market analysis involve time series that map dates to numeric values.  The mapping function is handled by the ```series``` struct and dates are contained within the ```dateset``` struct.  In merkatilo, dates are simply julian date integers.  For instance, the first day of the year 2000 can be constructed via ``(->jdate '2000-1-1)`` yielding 2451545.  The date integers where chosen to match Postgres julians.  A special but common time series in merkatilo is a signal series which answers date queries with only (#f, 1, -1).  The following example loads the SPY ETF, sets the active dates parameter, and does a 200 period cross to create buy and sell signals.  The utility operator dump prints out the SPY series, the 200 period smoothing and the cross.

```
(require merkatilo)

(define SPY (lo 'SPY))
(set-dates SPY)
(define smoothed (sma SPY 200))
(dump SPY smoothed (cross #:slower smoothed #:faster SPY))
```



- - -
MIT License

Copyright (c) 2018 John C. Griffin, 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


  







