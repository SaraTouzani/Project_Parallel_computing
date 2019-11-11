#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]] 
List vecmin(NumericVector x) {
  double n = x.size();
  NumericVector tempM24(n);
  NumericVector tempm24(n);
  for(int i = 0; i < n; ++i){
    if((i-23)<0){
      tempM24[i] = max(x[Rcpp::Range(0,i)]);
      tempm24[i] = min(x[Rcpp::Range(0,i)]);
    }else{
      tempM24[i] = max(x[Rcpp::Range((i-23),i)]);
      tempm24[i] = min(x[Rcpp::Range((i-23),i)]);
    }
  }
  List ret;
  ret["tempM24"] = tempM24;
  ret["tempm24"] = tempm24;
  return ret;
}

/*** R
to_test = vecmin(DAT$temp)
*/

