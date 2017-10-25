## Changes and notes
* Fixed minor error in vignette "Fine tuning MOEADr using irace";
* Fixed minor error in final Archive composition in `moead()`;
* `moead()` output objects now contain the input configuration used 
  for running the algorithm, for easier reproducibility;
* Fixed problem in `moead()` that was compromising reproducibility when running
  with `irace`;
* Changed output class of moead() to _moead_ (instead of _moeadoutput_). All 
  related functions (plot, summary) and documentation were updated.
* Added S3 print function, changed class of the outputs of 
  _moead()_ to **moead** (instead of **moeadoutput**).

## Test environments
* local OS X 10.12.6 install, R 3.4.1
* Ubuntu 14.04.5 LTS (on travis-ci v 3.1.0), R version 3.4.2
* win-builder (release and devel)

## R CMD check results
0 errors | 0 warnings | 0 notes
R CMD check succeeded
