#include <RcppArmadillo.h>
using namespace arma;

arma::vec Compute_IES_Loss(arma::uvec candi, int last_index, arma::mat X, int q){
  arma::vec loss = zeros(candi.n_elem);
  arma::rowvec x_last = floor(X.row(last_index)*q);
  for (int i=0; i<candi.n_elem; i++) {
    loss[i] = pow(accu(floor(X.row(candi[i])*q) == x_last),2);
  }
  return loss;
}

//' IES Core Code Using `RcppArmadillo`.
//'
//' @param X A data.frame or matrix consists of explanatory variables.
//' @param n Subsample size.
//' @param q Hyperparamter of how to divide the axes.
// [[Rcpp::export]]
arma::uvec armaIES(arma::mat X, int n, int q){
  int N = X.n_rows;
  arma::uvec index(n);
  arma::uvec candi = linspace<uvec>(0, N-1, N);
  arma::vec loss = zeros(N);

  index[0] = randi(distr_param(0, N-1));
  // index[0] = 0;
  candi.shed_row(index[0]);
  loss.shed_row(index[0]);
  loss = Compute_IES_Loss(candi, index[0], X, q);
  // Rcout << "---Step: 1---" << "\n";
  // Rcout << conv_to< arma::rowvec >::from(loss) << "\n";
  for (int i=1; i<n; i++) {
    arma::uvec temp_index_vec = find(loss == min(loss));
    int temp = temp_index_vec[randi(distr_param(0, temp_index_vec.n_elem-1))];
    // int temp = temp_index_vec[0];
    // Rcout << "temp: " << temp+1 << "\n";
    index[i] = candi[temp];
    candi.shed_row(temp);
    loss.shed_row(temp);
    loss = loss + Compute_IES_Loss(candi, index[i], X, q);
    // Rcout << "---Step: " << i+1 << "---" <<"\n";
    // Rcout << conv_to< arma::rowvec >::from(loss) << "\n";
  }
  return index + 1;
}

//' IES C++-Version for Benchmarking (C++ Core Code)
//'
//' There is no randomness, all parts that need to be randomly selected are selected first indexed.
//'
//' @param X A data.frame or matrix consists of explanatory variables.
//' @param n Subsample size.
//' @param q Hyperparamter of how to divide the axes.
// [[Rcpp::export]]
arma::uvec armaIES_compare(arma::mat X, int n, int q){
  int N = X.n_rows;
  arma::uvec index(n);
  arma::uvec candi = linspace<uvec>(0, N-1, N);
  arma::vec loss = zeros(N);

  // index[0] = randi(distr_param(0, N-1));
  index[0] = 0;
  candi.shed_row(index[0]);
  loss.shed_row(index[0]);
  loss = Compute_IES_Loss(candi, index[0], X, q);
  // Rcout << "---Step: 1---" << "\n";
  // Rcout << conv_to< arma::rowvec >::from(loss) << "\n";
  for (int i=1; i<n; i++) {
    arma::uvec temp_index_vec = find(loss == min(loss));
    // int temp = temp_index_vec[randi(distr_param(0, temp_index_vec.n_elem-1))];
    int temp = temp_index_vec[0];
    // Rcout << "temp: " << temp+1 << "\n";
    index[i] = candi[temp];
    candi.shed_row(temp);
    loss.shed_row(temp);
    loss = loss + Compute_IES_Loss(candi, index[i], X, q);
    // Rcout << "---Step: " << i+1 << "---" <<"\n";
    // Rcout << conv_to< arma::rowvec >::from(loss) << "\n";
  }
  return index + 1;
}
