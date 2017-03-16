## MOEADr package
[![Build Status](https://api.travis-ci.org/fcampelo/MOEADr.png)](https://travis-ci.org/fcampelo/MOEADr)

***

[Felipe Campelo](mailto:fcampelo@ufmg.br) and [Lucas Batista](mailto:lusoba@ufmg.br)  
Operations Research and Complex Systems Laboratory - ORCS Lab  
Universidade Federal de Minas Gerais  
Belo Horizonte, Brazil

  
[Claus Aranha](mailto:caranha@cs.tsukuba.ac.jp)  
Faculty of Engineering, Information and Systems  
University of Tsukuba  
Tsukuba, Japan

***

**R** package containing a component-based, modular implementation of the Multiobjective Evolutionary Algorithm with Decomposition (MOEA/D) framework. 

The MOEA/D framework is seen as a combination of specific design decisions regarding several independent modules:

- Decomposition strategy;  
- Aggregation function;  
- Objective scaling strategy;  
- Neighborhood assignment strategy;  
- Variation Stack;  
- Update strategy;  
- Constraint handling method;  
- Termination criteria.

This package provides several options for each module, as explained in the documentation of its main function, `MOEADr::moead()`. The input structure of this function is also explained in its documentation.

To install the current release version in your system, simply use:

```
install.packages("MOEADr")
```

For the most up-to-date development version, install the github version using:

```
# install.packages("devtools")
devtools::install_github("fcampelo/MOEADr")
```

Or, if you are interested in the specific version used to generate the results reported in our paper (Name and journal to be included here as soon as they are decided), use:

```
devtools::install_github("fcampelo/MOEADr/MOEADr@Manuscript-Version")
```

and follow the instructions provided in the README section of the [Manuscript-Version](https://github.com/fcampelo/MOEADr/tree/Manuscript-Version).


Have fun!  
Felipe
