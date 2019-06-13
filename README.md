# MOEADr
R package MOEADr, a modular implementation of the Multiobjective Evolutionary Algorithm with Decomposition (MOEA/D) framework modified to include Resource Allocation. 

Please [read the paper](diversityMOEAD.pdf)
Please [read the poster](Poster_GECCO_Priority_Functions.pdf.pdf)

Branch containing the code and the paper (diversityMOEAD.pdf) of the GECCO poster **Using Diversity as a Priority Function for Resource Allocation on MOEA/D**

## ABSTRACT

The key characteristic of the Multi-Objective Evolutionary Algorithm Based on Decomposition (MOEA/D) is that a multi-objective problem is decomposed into multiple single-objective subproblems. In standard MOEA/D, all subproblems receive the same computational effort. However, as each subproblem relates to different areas of the objective space, it is expected that some subproblems are more difficult than others. Resource Allocation techniques allocates computational effort proportional to each subproblem’s difficulty. This difficulty is estimated by a priority function. Using Resource Allocation, MOEA/D could spend less effort on easier subproblems and more on harder ones, improving efficiency. We propose that using diversity as the priority criteria results in better allocation of computational effort. Therefore we propose a new priority function: decision space diversity. We compare the proposed diversity based priority with previous approaches on the UF benchmarks. The proposed decision space priority achieved high IGD values, excellent rate of non-dominated solutions on the benchmark problem.
