// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// shortDistInd
IntegerVector shortDistInd(NumericMatrix mat1, NumericMatrix mat2);
RcppExport SEXP _rangeBuilder_shortDistInd(SEXP mat1SEXP, SEXP mat2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat1(mat1SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat2(mat2SEXP);
    rcpp_result_gen = Rcpp::wrap(shortDistInd(mat1, mat2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rangeBuilder_shortDistInd", (DL_FUNC) &_rangeBuilder_shortDistInd, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rangeBuilder(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
