#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstdlib>
using namespace std;

#include "moon_mop.h"
namespace bp = benchmark_problem;

typedef vector<double> dvec;
typedef vector<dvec> dmat;

const string varsFileName = "pop_vars_eval.txt";
const string objsFileName = "pop_objs_eval.txt";
const string consFileName = "pop_cons_eval.txt";

void evaluate(bp::MoonMOP &mop, dmat &vars, dmat &objs, dmat &cons)
{
  const size_t N = vars.size();
	for (size_t i = 0; i < N; ++i) {
		mop.evaluate(vars[i], objs[i], cons[i]);
	}
}

bool write(const string &filename, const dmat &mat, char delim = '\t')
{
  ofstream os(filename.c_str(), ios::out);
  if (os.fail()) return false;

  const int prec = 8;

  const size_t N = mat.size();
  for (size_t i = 0; i < N; ++i) {
    const dvec &v = mat[i];

    if (v.size() == 0) continue;

    const size_t M = v.size();
    os << fixed << setprecision(prec) << v[0];
    for (size_t j = 1; j < M; ++j) {
      os << delim << fixed << setprecision(prec) << v[j];
    }
    os << endl;
  }

  return true;
}

bool read(const string &filename, dmat &mat, char delim = '\t')
{
  ifstream is(filename.c_str(), ios::in);
  if (is.fail()) return false;

  for (string line_in; getline(is, line_in);) {
    if (line_in.size() == 0) continue;

    string token;
    istringstream ss(line_in);
    vector<double> v;
    while (getline(ss, token, delim)) {
      v.push_back(atof(token.c_str()));
    }
    mat.push_back(v);
  }

  return true;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cerr << "Usage: ./moon_bench file_path" << endl;
    cerr << endl;
    cerr << "file_path: file path to pop_vars_eval.txt" << endl;
    cerr << "output : pop_objs_eval.txt, pop_cons_eval.txt" << endl;
    return 0;
  }

  const string interfacePath = argv[1];
  const string sep = "/";
  dmat vars, objs, cons;

  // setup //////////////////////////////////////////////////////////////////////////////////
  bp::MoonMOP mop;

  string filePath = interfacePath + sep + varsFileName;
  if (!read(filePath, vars)) return 1;

  if (!vars.size()) {
    cerr << "[Error] The number of samples is zero." << endl;
    return 0;
  }

  if (vars[0].size() != (size_t)mop.nvar) {
    cerr << "[Error] The number of variables is different from the benchmark problem." << endl;
    cerr << "pop_vars_eval.txt: " << vars[0].size() << endl;
    cerr << "benchmark problem: " << mop.nvar << endl;
    return 0;
  }

  objs.resize(vars.size());
  cons.resize(vars.size());
  for (size_t i=0;i<objs.size();++i) objs[i].resize(mop.nobj);
  for (size_t i=0;i<cons.size();++i) cons[i].resize(mop.ncon);

  // evaluate solutions  ////////////////////////////////////////////////////////////////////
  evaluate(mop, vars, objs, cons);

  // write results  /////////////////////////////////////////////////////////////////////////
  filePath = interfacePath + sep + objsFileName;
  write(filePath, objs);

  filePath = interfacePath + sep + consFileName;
  write(filePath, cons);

  return 0;
}
