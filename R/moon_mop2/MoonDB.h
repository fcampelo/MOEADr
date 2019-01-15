#ifndef MOONDB_H_
#define MOONDB_H_

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

namespace benchmark_problem {

class MoonDB {
private:
	int jmax, kmax;
	int offset0;
	std::ifstream ifs;

public:
	MoonDB();
	~MoonDB();

	void init(const std::string& fname);
	double get(std::vector<double>& var);

protected:
	template <typename T>
	void read(std::ifstream &ifs, T& s, std::streamsize n, bool is_swap = false) {
		if (is_swap) {
			std::vector<char> dst(n);
			ifs.read(&dst[0], n);
			std::reverse_copy(&dst[0], &dst[0] + n, reinterpret_cast<char *>(&s));
		} else {
			ifs.read(reinterpret_cast<char *>(&s), n);
		}
	}
};

} // end of namespace benchmark_problem

#endif /* MOONDB_H_ */
