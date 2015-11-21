#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

double logapprox3(double x, int k)
{
  x = 1.0 - x;
  
  float sum = 0;	
  for (int i = 1; i <= k; i++)
  {
    float term = -1.0 / i * pow(x, i);
    sum += term;
  }
  
  return sum;
}
