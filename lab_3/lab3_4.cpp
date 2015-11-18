#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapproxk(double x, int k)
{
  x = 1.0 - x;
  
  float sum = 0;
  float c = 0;
  for (int i = 1; i <= k; i++)
  {
    float term = -1.0 / i * pow(x, i);

    float y = term - c;
    term = sum + y;
    c = (term - sum) - y;
    sum = term;
    
  }
  
  return sum;
}
