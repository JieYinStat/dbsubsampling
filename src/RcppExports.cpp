// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// getIdxR_cpp
IntegerVector getIdxR_cpp(int r, NumericVector z, IntegerVector rdel);
RcppExport SEXP _dbsubsampling_getIdxR_cpp(SEXP rSEXP, SEXP zSEXP, SEXP rdelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type rdel(rdelSEXP);
    rcpp_result_gen = Rcpp::wrap(getIdxR_cpp(r, z, rdel));
    return rcpp_result_gen;
END_RCPP
}
// getIdx_cpp
IntegerVector getIdx_cpp(int r, NumericVector z);
RcppExport SEXP _dbsubsampling_getIdx_cpp(SEXP rSEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(getIdx_cpp(r, z));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dbsubsampling_getIdxR_cpp", (DL_FUNC) &_dbsubsampling_getIdxR_cpp, 3},
    {"_dbsubsampling_getIdx_cpp", (DL_FUNC) &_dbsubsampling_getIdx_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dbsubsampling(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}