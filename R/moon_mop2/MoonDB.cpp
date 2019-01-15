/*
 * MoonDB.cpp
 *
 *  Created on: Sep 28, 2018
 *      Author: Tomoaki Tatsukawa<tatsukawa@rs.tus.ac.jp>
 */

#include "MoonDB.h"
namespace bp = benchmark_problem;

#include <iostream>
#include <fstream>
#include <vector>
#include <string>

bp::MoonDB::MoonDB() {
	jmax = 0;
	kmax = 0;
	offset0 = 0;
}

bp::MoonDB::~MoonDB() {
}

void bp::MoonDB::init(const std::string& fname) {
	ifs.open(fname.c_str(), std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "[ERR] Cannot open " << fname << "." << std::endl;
		return;
	}

	unsigned long long marker;
	float tmp;

	read(ifs, marker, sizeof(marker));
	read(ifs, jmax, sizeof(jmax));
	read(ifs, kmax, sizeof(jmax));
	read(ifs, marker, sizeof(marker));

	read(ifs, marker, sizeof(marker));
	read(ifs, tmp, sizeof(tmp));
	read(ifs, tmp, sizeof(tmp));
	read(ifs, marker, sizeof(marker));

	read(ifs, marker, sizeof(marker));

	offset0 = ifs.tellg();
}

double bp::MoonDB::get(std::vector<double>& var) {
	if (!ifs.is_open()) {
		std::cerr << "[ERR] DB is not opened" << std::endl;
		return 0.0f;
	}

	double j = var[0] * (jmax - 1);
	double k = var[1] * (kmax - 1);
	if (j < 0 || k < 0 || j > jmax - 1 || k > kmax - 1) {
		std::cerr << "[ERR] Invalid variable range" << std::endl;
		return 0.0f;
	}

	int j0 = (int)j;
	int k0 = (int)k;

	int j1 = (j0 >= (jmax - 1)) ? j0 : j0 + 1;
	int k1 = (k0 >= (kmax - 1)) ? k0 : k0 + 1;

	double jm = (j0 >= (jmax - 1)) ? 0.0 : j - j0;
	double km = (k0 >= (kmax - 1)) ? 0.0 : k - k0;

	unsigned long long offset;
	float fa, fb, fc, fd, f;

	offset = (k0 * jmax + j0) * sizeof(float);
	ifs.seekg(offset + offset0, ifs.beg);
	read(ifs, fa, sizeof(fa));

	offset = (k0 * jmax + j1) * sizeof(float);
	ifs.seekg(offset + offset0, ifs.beg);
	read(ifs, fb, sizeof(fb));

	offset = (k1 * jmax + j0) * sizeof(float);
	ifs.seekg(offset + offset0, ifs.beg);
	read(ifs, fc, sizeof(fc));

	offset = (k1 * jmax + j1) * sizeof(float);
	ifs.seekg(offset + offset0, ifs.beg);
	read(ifs, fd, sizeof(fd));

	if (jm + km - 1.0 < 0) {
		f = jm * fb + km * fc - (jm + km - 1) * fa;
	} else {
		f = (1.0 - jm) * fc + (1.0 - km) * fb + (jm + km - 1) * fd;
	}

	return static_cast<double>(f);
}
