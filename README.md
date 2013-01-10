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
* TenuredProfessor

... so these have been excluded for the present.

## Running

Generate a profiling report by running `make`.

## Results

The memoizing solution seems to be the clear winner at this point, with the
list-encoding, followed by the interpretive solutions being the clear losers:

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/1.png" alt="All results" />

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/2.png" alt="All results" />

<img src="https://raw.github.com/sordina/HaskellEvolutionProfile/master/images/3.png" alt="All results" />
