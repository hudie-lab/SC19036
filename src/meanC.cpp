#include <Rcpp.h>
using namespace Rcpp;

//' @title A mean function using Rcpp
//' @description A mean function using Rcpp
//' @param x the vector to get the mean 
//' @return the mean of x
//' @examples
//' \dontrun{
//' meanC(c(1,2,3))
//' }
//' @export
// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}
