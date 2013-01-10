HaskellEvolutionProfile
=======================

### Profiling ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html) using [Criterion](http://hackage.haskell.org/package/criterion)

This project aims to profile the various factorial solutions presented in ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html)
by using the [Criterion](http://hackage.haskell.org/package/criterion) profiling library.

## Omissions

I had issues compiling the examples for

* Static

... so these have been excluded for the present. Please don't hesitate to send
a pull-request that modifies these solutions to work :-)

## Running

Generate a profiling report by running `make`.  The report should be generated at "images/report1.html".

I reran the report twice with the worst offenders removed to
show the variance between good solutions. These results can be
found at "images/report2.html" and "images/report3.html"
respectively.

## Results

The memoizing solution seems to be the clear winner at this point, with the
list-encoding, followed by the interpretive solutions being the clear losers:

X-Axis in milliseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/1.png" alt="All results" />

X-Axis in microseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/2.png" alt="Partial results 2" />

X-Axis in nanoseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/3.png" alt="Partial results 3" />

X-Axis in nanoseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/4.png" alt="Partial results 4" />
