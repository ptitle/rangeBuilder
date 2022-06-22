#include <Rcpp.h>
#include <numeric>
using namespace Rcpp;

// return index of shortest distance
// [[Rcpp::export(name = shortDistInd, rng = false)]]
IntegerVector shortDistInd(NumericMatrix mat1, NumericMatrix mat2) {

	int nrow1 = mat1.nrow();
	int nrow2 = mat2.nrow();

	IntegerVector out(nrow1);

	for (int i = 0; i < nrow1; i++) {
		double x1 = mat1(i,0);
		double y1 = mat1(i,1);
		NumericVector ret(nrow2);
		for (int j = 0; j < nrow2; j++) {
			double tmp = pow((pow(x1 - mat2(j,0), 2) + pow(y1 - mat2(j,1), 2)), 0.5);
			ret[j] = tmp;
		}
		out[i] = which_min(ret) + 1;
	}

	return out;
}

