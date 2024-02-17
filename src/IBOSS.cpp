#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
//
// C++ main function `getIdxR_cpp` `getIdx_cpp`.
//
// R function `IBOSS` core code, `Rcpp`-C++-style by `Wang`.
//
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

//' Get subsample index of other column(except the first column) (IBOSS core code, `Rcpp`-C++-style by `Wang`)
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

//' Get subsample index of the first column (IBOSS core code, `Rcpp`-C++-style by `Wang`)
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


// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
//
// C++ main function `rcpp_cstyle_IBOSS`
//
// R function `myRcpp_cstyle_IBOSS` core code, `Rcpp`-C++-style by `the package itself`.
//
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
std::vector<int> Append_Index(std::vector<int> index, IntegerVector new_index){
  for(int i=0; i<new_index.size(); i++) {
    index.push_back(new_index[i]);
  }
  return index;
}
// [[Rcpp::export]]
NumericVector GetQuantile(std::vector<double> temp, int r){
  int len = temp.size();
  std::nth_element(temp.begin(), temp.begin()+r-1, temp.end());
  double lower = temp[r-1];
  std::nth_element(temp.begin(), temp.begin()+len-r, temp.end());
  double upper = temp[len-r];
  return NumericVector::create(lower, upper);
}
// [[Rcpp::export]]
List Get_Candi_Quan(NumericVector x, NumericVector next_x, IntegerVector candi, double lower, double upper, int r){
  int N = x.size();
  std::vector<int> index;
  std::vector<int> next_candi;
  std::vector<double> temp;
  for (int i=0, ii=0; i<N && ii<candi.size(); i++){
    if(i==candi[ii]) {
      if ((x[i] <= lower) || (x[i] >= upper)) {
        index.push_back(i);
      } else{
        next_candi.push_back(i);
        temp.push_back(next_x[i]);
      }
      ii++;
    }
  }
  NumericVector quan = GetQuantile(temp, r);
  double new_lower = quan[0];
  double new_upper = quan[1];
  return List::create(_["index"] = wrap(index), _["candi"] = wrap(next_candi), _["lower"] = new_lower, _["upper"] = new_upper);
}
IntegerVector Get_Index(NumericVector x, IntegerVector candi, double lower, double upper){
  int N = x.size();
  std::vector<int> index;
  for (int i=0, ii=0; i<N && ii<candi.size(); i++){
    if(i==candi[ii]) {
      if ((x[i] <= lower) || (x[i] >= upper)) {
        index.push_back(i);
      }
      ii++;
    }
  }
  return wrap(index);
}
//' IBOSS with `Rcpp`-C++-style by `the package itself` (`myRcpp_cstyle_IBOSS` core c++ code)
//' @param n Subsample size.
//' @param X A data.frame or matrix consists of explanatory variables.
//' @return Subsample index.
// [[Rcpp::export]]
NumericVector rcpp_cstyle_IBOSS(NumericMatrix X, int n){
  int N = X.nrow();
  int p = X.ncol();
  int r = floor(n/2/p);

  NumericVector x = X(_,0);
  std::vector<double> temp = as<std::vector<double>>(x);

  std::vector<int> index;
  NumericVector quan = GetQuantile(temp, r);
  double lower = quan[0];
  double upper = quan[1];

  std::vector<int> initial_candi(N);
  for(int i=0; i<N; i++) {initial_candi[i] = i;}
  List result = Get_Candi_Quan(X(_,0), X(_,1), wrap(initial_candi), lower, upper, r);
  index = Append_Index(index, result["index"]);

  for(int j=1; j<p; j++){
    if (j<(p-1)){
      result = Get_Candi_Quan(X(_,j), X(_,j+1), result["candi"], result["lower"], result["upper"], r);
      index = Append_Index(index, result["index"]);
    } else {
      IntegerVector temp_index = Get_Index(X(_,j), result["candi"], result["lower"], result["upper"]);
      index = Append_Index(index, temp_index);
    }

  }
  NumericVector return_index = wrap(index);
  return return_index + 1;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
//
// C++ main function `rcppIBOSS`
//
// R function `myRcppIBOSS` core code, `Rcpp`-r-style by `the package itself`.
//
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
IntegerVector bottom_r_index(NumericVector x, int r){
  IntegerVector index(r);
  NumericVector xcopy = clone(x);
  std::nth_element(xcopy.begin(), xcopy.begin() + r - 1, xcopy.end());
  for(int i=0, ii=0; i<x.length(); i++){
    if (x[i] <= xcopy[r-1]) index[ii++] = i;
  }
  return(index);
}

IntegerVector top_r_index(NumericVector x, int r){
  IntegerVector index = bottom_r_index(-x, r);
  return index;
}

//' IBOSS with `Rcpp`-r-style by `the package itself` (`myRcppIBOSS` core c++ code)
//' @param n Subsample size.
//' @param X A data.frame or matrix consists of explanatory variables.
//' @return Subsample index.
// [[Rcpp::export]]
IntegerVector rcppIBOSS(NumericMatrix X, int n){
  int N = X.nrow();
  int p = X.ncol();
  int r = floor(n/2/p);
  IntegerVector wholeindex = seq(0,N-1);
  IntegerVector index = top_r_index(X(_,0), r);
  index = union_(index, bottom_r_index(X(_,0), r));

 for(int j=1; j<p; j++){
   IntegerVector tempindex = setdiff(wholeindex, index);
   NumericVector tempvector = X(_,j);
   tempvector = tempvector[tempindex];

   IntegerVector jbottom_index = tempindex[bottom_r_index(tempvector, r)];
   IntegerVector jtop_index = tempindex[top_r_index(tempvector, r)];

   index = union_(index, jbottom_index);
   index = union_(index, jtop_index);
  }
  return index + 1;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
//
// C++ main function `armarcppIBOSS`
//
// R function `myArma_IBOSS` core code, `RcppArmadillo`-r-style by `the package itself`.
//
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
arma::uvec arma_bottom_r_index(arma::vec x, int r){
  arma::uvec index(r);
  arma::vec xcopy = x;
  std::nth_element(xcopy.begin(), xcopy.begin() + r - 1, xcopy.end());
  for(int i=0, ii=0; i<x.n_elem; i++){
    if (x[i] <= xcopy[r-1]) index[ii++] = i;
  }
  return index;
}

//' IBOSS with `RcppArmadillo` by `the package itself` (`myArma_IBOSS` core c++ code)
//' @param n Subsample size.
//' @param X A data.frame or matrix consists of explanatory variables.
//' @return Subsample index.
// [[Rcpp::export]]
arma::uvec armarcppIBOSS(arma::mat X, int n){
  int N = X.n_rows;
  int p = X.n_cols;
  int r = floor(n/2/p);
  arma::uvec candi = linspace<arma::uvec>(0,N-1,N);
  arma::uvec index = ones<uvec>(r*2*p);

  for(int j=0; j<p; j++){
    if (j==0) {
      index( linspace<arma::uvec>(0,r-1,r) ) = arma_bottom_r_index(X.col(0),r);
      index( linspace<arma::uvec>(r,2*r-1,r) ) = arma_bottom_r_index(-X.col(0),r);
      X.shed_rows( index(linspace<arma::uvec>(0, 2*r -1, 2*r)) );
      candi.shed_rows( index(linspace<arma::uvec>(0, 2*r -1, 2*r)) );
      continue;
    }

    arma::uvec temp_bottom = arma_bottom_r_index(X.col(j),r);
    arma::uvec temp_top = arma_bottom_r_index(-X.col(j),r);
    index( linspace<arma::uvec>( 2*j*r, (2*j+1) *r -1, r) ) = candi(temp_bottom);
    index( linspace<arma::uvec>( (2*j+1) *r, (2*j+2) *r -1, r) ) = candi(temp_top);

    if (j == (p-1)) {
     break;
    }
    X.shed_rows(join_cols(temp_bottom, temp_top));
    candi.shed_rows(join_cols(temp_bottom, temp_top));
  }

  return index + 1;
}
