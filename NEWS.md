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
