#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

double logapprox4(double x, int k)
{
  x = 1.0 - x;
  
  double sum = 0;  
  for (int i = 1; i <= k; i++)
  {
    double term = -1.0 / i * pow(x, i);
    sum += term;
  }
  
  return sum;
}