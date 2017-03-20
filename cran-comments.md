## Changes and notes
* fixed DOI format in description, as requested by Uwe Ligges.
* this upload also addresses the problem reported by Brian Ripley: the offending test was changed so that integers are compared instead of doubles, and the values compared are much smaller (without compromising the validity of the test).

## Test environments
* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci v 2.5.0), R 3.3.2
* win-builder (release and devel)

## R CMD check results
0 errors | 0 warnings | 0 notes
R CMD check succeeded
