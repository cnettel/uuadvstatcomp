#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox6(double x, int k) 
{
  x = 1.0 - x;
  double c = 0.0;
  double sum = 0.0;
  double y;
  double t;
  for (int i = k; i >= 1; i--)
  {
      y = -1.0 / i * pow(x, i) - c;
      t = sum + y;
      c = (t - sum) - y;
      sum = t;
  }
  
  return sum;
}
