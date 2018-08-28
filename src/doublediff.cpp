#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<bool> doublediff(NumericVector x, NumericVector y,
                          double tolerance, double scale = 0) {
    int n = x.size();
    const bool initial_value = false;
    std::vector<bool> out(n, initial_value);
    double diff;
    
    for (int i =0; i < n; ++i){
        if (NumericVector::is_na(x[i]) & NumericVector::is_na(y[i]) ){
            out[i] = FALSE;
        } else if (NumericVector::is_na(x[i]) | NumericVector::is_na(y[i])){
            out[i] = TRUE;
        } else{ 
        diff = std::abs(x[i] - y[i]);
        if (scale != 0){
            diff = diff/scale;
        }
        out[i] = diff>tolerance;
        }
    }
    return out;
}

