#ifndef MOON_MOP_H_
#define MOON_MOP_H_

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <utility>

#include "MoonDB.h"

namespace benchmark_problem {

class MoonMOP {
private:
	MoonDB f1, f2, f3;

public:
  const int nobj = 3;
  const int ncon = 2;
  const int nvar = 2;

  std::vector< std::pair<double, double> > varRanges;

public:
	MoonMOP();

	void evaluate(std::vector<double>& var,
				  std::vector<double>& obj,
				  std::vector<double>& con);

};

} // end of namespace benchmark_problem

#endif /* MOON_MOP_H_ */
