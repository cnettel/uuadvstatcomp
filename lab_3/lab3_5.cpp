#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapproxkd(double x, int k)
{
  x = 1.0 - x;
  
  double sum = 0;	
  double c   = 0;
  for (int i = 1; i <= k; i++)
  {
    double term = -1.0 / i * pow(x, i);

    double y = term - c;
    term = sum + y;
    c = (term - sum) - y;
    sum = term;
  }
  
  return sum;
}
