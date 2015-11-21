#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::export]]
double logapproxC(double x, int k)
{
  x = 1.0 - x;
  
  double sum = 0;	
  double y = 1.0;
  for (int i = 1; i <= k; i++)
  {
    y *= x;
    double term = -1.0 / i * y;
    sum += term;
  }
  
  return sum;
}



