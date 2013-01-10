HaskellEvolutionProfile
=======================

### Profiling ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html) using [Criterion](http://hackage.haskell.org/package/criterion)

This project aims to profile the various factorial solutions presented in ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html)
by using the [Criterion](http://hackage.haskell.org/package/criterion) profiling library.

## Omissions

The static solution won't have a run-time component worth profiling ;-)

## Running

The runtimes are calculated for an average of 100 evaluations of `fac 8`.

Generate a profiling report by running `make`.  The report should be generated
at "images/report1.html".

I reran the report three-times with the worst offenders removed to
show the variance between good solutions. These results can be
found at "images/report2.html", "images/report3.html" and "images/report4.html"
respectively.

## Results

The memoizing solution seems to be the clear winner at this point, with PHD
solution being the clear loser:

X-Axis in milliseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/1.png" alt="All results" />

X-Axis in microseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/2.png" alt="Partial results 2" />

X-Axis in nanoseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/3.png" alt="Partial results 3" />

X-Axis in nanoseconds:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/4.png" alt="Partial results 4" />
