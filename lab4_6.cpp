#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox5(double x, int k) 
{
  x = 1.0 - x;
  double c = 0;
  double sum = 0;
  double y;
  double t;
  for (int i = k; i >= 1; i--)
  {
      double term = -1.0 / i * pow(x, i);
      y = term - c;
      t = sum + y;
      c = (t - sum) - y;
      sum = t;
  }
  
  return sum;
}
