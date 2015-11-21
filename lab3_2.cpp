#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double logapprox(double x, int k)
{
  x = 1.0 - x;

	float sum = 0;	
	for (int i = 1; i <= k; i++)
	{
		float term = pow(x,i);
		sum -= 1.0 / i * term;
	}

	return sum;
}

