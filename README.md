HaskellEvolutionProfile
=======================

### Profiling ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html)

This project aims to profile the various factorial solutions presented in ["Evolution of a Haskell Programmer"](http://www.willamette.edu/~fruehr/haskell/evolution.html)
by using the [Criterion](http://hackage.haskell.org/package/criterion) profiling library.

## Omissions

I Had issues compiling the examples for

* Static
* BeginningGraduate
* Origamist
* PHD
* PostDoc

... so these have been excluded for the present. Please don't hesitate to send
a pull-request that modify these solutions to work :-)

## Running

Generate a profiling report by running `make`.  The report should be generated at "images/report1.html".

I reran the report twice with the worst offenders removed to
show the variance between good solutions. These results can be
found at "images/report2.html" and "images/report3.html"
respectively.

## Results

The memoizing solution seems to be the clear winner at this point, with the
list-encoding, followed by the interpretive solutions being the clear losers:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/1.png" alt="All results" />

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/2.png" alt="All results" />

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/3.png" alt="All results" />
