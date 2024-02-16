#include <Rcpp.h>
using namespace Rcpp;

// Scale a matrix
//
// @param X A matrix.
//
// @return Scaled matrix.
//
// NumericMatrix ScaleMatrix(NumericMatrix X){
//   int p = X.cols();
//   for(int j = 0; j < p; j++){
//     X(_,j) = (X(_,j) - mean(X(_,j))) / sd(X(_,j));
//   }
//   return X;
// }

//' Get L2 norm
//'
//' Get L2 norm of a matrix or data frame.
//'
//' @param X A matrix or data.frame.
//'
//' @return L2 norm of `X`(every row).
// [[Rcpp::export]]
NumericVector L2norm(NumericMatrix X){
  int N = X.rows();
  NumericVector norm(N);
  for(int i=0; i<N; i++){
    norm[i] = sum(X(i,_) * X(i,_));
    // norm[i] = std::inner_product(X(i,_).begin(), X(i,_).end(), X(i,_).begin(),0);
  }
  return norm;
}

//' Find t smallest index of a vector
//'
//' @param loss A vector.
//' @param t A int.
//'
//' @return The index of the t smallest element of the vector.
// [[Rcpp::export]]
IntegerVector bottom_t_index(NumericVector loss, int t){
  IntegerVector remain(t);
  NumericVector losscopy = clone(loss);
  std::nth_element(losscopy.begin(), losscopy.begin() + t - 1, losscopy.end());
  for(int i=0, ii=0; i<loss.length(); i++){
    if (loss[i] <= losscopy[t-1]) remain[ii++] = i;
  }
  return(remain);
}

//' Compute loss function for OSS
//'
//' @param candi The index of the candidate set.
//' @param last_index The index of the seleted point in last iteration.
//' @param X The whole data.
//' @param norm Norm of the whole data.
//'
//' @return Loss of every point in candidate set.
// [[Rcpp::export]]
NumericVector ComputeLoss(IntegerVector candi, int last_index, NumericMatrix X, NumericVector norm){
  int p = X.cols();
  int k = candi.length();
  NumericVector loss(k);
  for(int i=0; i<k; i++){
    int delta = sum(sign(X(candi[i],_)) == sign(X(last_index,_)));
    loss[i] = pow(p - norm[candi[i]]/2 - norm[last_index]/2 + delta, 2);
  }
  return loss;
}

//' Rcpp version OSS (core code of `OSS`)
//'
//' @param X A matrix.
//' @param n Subsample size.
//'
//' @return Subsample index.
// [[Rcpp::export]]
IntegerVector rcppOSS(NumericMatrix X, int n) {
  // X = ScaleMatrix(X);  // Standardize in R
  int N = X.rows();

  IntegerVector index(n);
  IntegerVector candi = seq_len(N)-1;
  NumericVector norm = L2norm(X);
  double r = log(N) / log(n);

  index[0] = which_max(norm);
  candi.erase(index[0]);
  NumericVector loss = ComputeLoss(candi, index[0], X, norm);
  // Rcout << "i = 0" << "\n";
  // Rcout << "index: " << index[0] << " \n";
  // Rcout << "length of candi: " << candi.length() << " \n";
  // Rcout << "candi: " << candi << "\n";
  // Rcout << "length of loss: " << loss.length() << " \n";
  // Rcout << "loss: " << loss << "\n" << "\n";

  for(int i=1; i<n; i++){

    int tmp = which_min(loss);
    index[i] = candi[tmp];
    // Rcout << "i = " << i << "\n";
    // Rcout << "tmp: " << tmp << "\n";
    // Rcout << "index: " << index[i] << "\n";

    candi.erase(tmp);
    loss.erase(tmp);
    // Rcout << "length of candi: " << candi.length() << "\n";
    // Rcout << "candi: " << candi << "\n";
    // Rcout << "length of loss: " << loss.length() << "\n";
    // Rcout << "loss: " << loss << "\n";

    double t = 0;
    if (N > pow(n,2)) {
      t = N / (i+1);
    } else {
      t = N / pow(i+1, r-1);
    }
    // Rcout << "t = " << t << "\n";
    if (candi.length() > t) {
      IntegerVector remain = bottom_t_index(loss, floor(t));
      //   Rcout << "length of remain: " << remain.length() << "\n";
      //   Rcout << "remain:" << remain << "\n";
      candi = candi[remain];
      loss = loss[remain];
    }

//    if (candi.length() == 0) {
//      index = index[seq(0,i)];
//      break;
//    }
    // Rcout << "After eliminate:" << "\n";
    // Rcout << "length of candi: " << candi.length() << "\n";
    // Rcout << "candi: " << candi << "\n";
    // Rcout << "length of loss: " << loss.length() << "\n";
    // Rcout << "loss: " << loss << "\n";
    loss = loss + ComputeLoss(candi, index[i], X, norm); // loss can't located.
    // Rcout << "After Update loss:" << "\n";
    // Rcout << "length of loss: " << loss.length() << "\n";
    // Rcout << "loss: " << loss << "\n" << "\n";
  }
  return index + 1;
}


