#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double ourFunc()
{
    return runif(1)[0];
}
