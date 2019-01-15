#include "moon_mop.h"
namespace bp = benchmark_problem;

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <utility>

bp::MoonMOP::MoonMOP() {
	std::pair<double, double> p = std::make_pair(0, 1);
	varRanges.push_back(p);
	varRanges.push_back(p);

	const std::string f1_fname = "DB/continue_night_nml_full.dat";
	const std::string f2_fname = "DB/total_comm_nml_full.dat";
	const std::string f3_fname = "DB/slope_nml_full.dat";

	f1.init(f1_fname);
	f2.init(f2_fname);
	f3.init(f3_fname);
}

void bp::MoonMOP::evaluate(std::vector<double>& var, std::vector<double>& obj, std::vector<double>& con) {
	obj[0] = f1.get(var);
	obj[1] = -f2.get(var);
	obj[2] = f3.get(var);

	con[0] = 0.05 - obj[0];
	con[1] = 0.3 - obj[2];
}
