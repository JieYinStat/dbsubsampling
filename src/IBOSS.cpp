#include <Rcpp.h>
#include <algorithm>
#include <iostream>

using namespace Rcpp;

//' Get subsample index of other column(except the first column) (IBOSS)
//'
//' @param r Subsample size of the column.
//' @param z A numeric vector. the column.
//' @param rdel Subsample index of the first column.
//' @return Subsample index of the column.
// [[Rcpp::export]]
IntegerVector getIdxR_cpp(int r, NumericVector z, IntegerVector rdel) {
  int m = rdel.size(), n = z.size();
  int* del = INTEGER(rdel);
  std::sort( del, del + m);
  // This code use half memory
  double* y = new double [n-m];
  int j = 0, k = 0;
  for ( int i = 0; i < n; i++) {
    if ( j >= m) {
      y[k++] = z[i];
    }
    else if ( del[j] != i + 1) {
      y[k++] = z[i];
    }
    else
      j++;
  }
  std::nth_element(y, y + r - 1, y + n - m);
  double  yrl = y[r-1];
  j = 0, k = 0;
  for ( int i = 0; i < n; i++) {
    if ( j >= m) {
      y[k++] = -z[i];
    }
    else if ( del[j] != i + 1) {
      y[k++] = -z[i];
    }
    else
      j++;
  }
  std::nth_element(y, y + r - 1, y + n - m);
  double yru = -y[r-1];
  delete [] y;

  int jl = 0, ju = 0;
  std::vector<int> locl(r);
  std::vector<int> locu(r);
  j = 0;
  for ( int i = 0; i < n; i++) {
    if ( j >= m) {
      if ( z[i] <= yrl && jl < r)
        locl[jl++] = i + 1;
      if ( z[i] >= yru && ju < r)
        locu[ju++] = i + 1;
    }
    else if ( del[j] != i + 1) {
      if ( z[i] <= yrl && jl < r)
        locl[jl++] = i + 1;
      if ( z[i] >= yru && ju < r)
        locu[ju++] = i + 1;
    }
    else
      j++;
    if ( jl >= r && ju >= r)
      break;
  }
  /*******************************************/
  IntegerVector idx(2 * r);
  for ( int i = 0; i < r; i++) {
    idx[i] = locl[i];
    idx[r+i] = locu[i];
  }
  return idx;
}

//' Get subsample index of the first column(IBOSS)
//'
//' @param r Subsample size of the first column.
//' @param z A numeric vector. the first column.
//' @return Subsample index of the first column.
// [[Rcpp::export]]
IntegerVector getIdx_cpp(int r, NumericVector z) {
  int n = z.size();
  // This code use half memory
  double* y = new double [n];
  for ( int i = 0; i < n; i++)
    y[i] = z[i];
  std::nth_element(y, y + r - 1, y + n);
  double yrl = y[r-1];
  for ( int i = 0; i < n; i++)
    y[i] = -z[i];
  std::nth_element(y, y + r - 1, y + n);
  double yru = -y[r-1];
  delete [] y;

  int jl = 0, ju = 0;
  std::vector<int> locl(r);
  std::vector<int> locu(r);
  for ( int i = 0; i < n; i++) {
    if ( z[i] <= yrl && jl < r)
      locl[jl++] = i + 1;
    if ( z[i] >= yru && ju < r)
      locu[ju++] = i + 1;
    if ( jl >= r && ju >= r)
      break;
  }
  IntegerVector idx(2 * r);
  // PROTECT(idx = allocVector(INTSXP, 2 * r));
  for ( int i = 0; i < r; i++) {
    idx[i] = locl[i];
    idx[r+i] = locu[i];
  }
  return idx;
}
