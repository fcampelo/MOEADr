# MOEADr 1.1.1
* Maintenance update: 
  * Minor update in `variation_polymut()` to allow probability of mutation as 
  (1 / problem dimension) (by setting `pm = "n"`)
  * Minor change in `preset_moead()`
* Added CITATION information to paper in the Journal of Statistical Software. 



# MOEADr 1.0.3
* Changed output class of moead() to _moead_ (instead of _moeadoutput_). All 
  related functions (plot, summary) and documentation were updated.
* Added S3 print function for _moead_ objects

# MOEADr 1.0.2
* Corrected the version for the two fixes below;
* `moead()` output objects now contain the input configuration used 
  for running the algorithm, for easier reproducibility;
* Fixed problem in `moead()` that was compromising reproducibility when running
  with `irace`;
  
# MOEADr 1.0.1 
* Fixed minor error in vignette "Fine tuning MOEADr using irace";
* Fixed minor error in final Archive composition in `moead()`;

# MOEADr 1.0.0  
* Moved package **smoof** back to _Suggestions_;
* Updated package examples to reflect looser dependency on **smoof**;
* Added summary and plot functions (S3);
* Added three more Vignettes to explain different aspects of the package: basic 
  usage; automated algorithm assembling and tuning using _irace_; and using a 
  user-defined operator with the MOEADr framework;
* Added warning / user confirmation in decomposition_sld() to prevent huge 
  population sizes due to mis-specification of a user parameter;
* Added preset_moead() to provide fast access to standard configurations from 
  the MOEA/D literature (two "standard MOEA/D" versions and one "MOEA/D-DE");


# MOEADr 0.2.2  
* Moved package **smoof** from _Suggestions_ to _Imports_
* _Depends_ updated to R (>= 3.4.0)


# MOEADr 0.2.1  
* Added `NEWS.md` to track changes to the package.
* _R.utils_ is no longer imported by the **MOEADr** package
* Skipped some tests on Solaris platforms, which were throwing spurious errors.


# MOEADr 0.2.0
* Initial release on CRAN
