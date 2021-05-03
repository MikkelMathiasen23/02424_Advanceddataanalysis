#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector test2(NumericVector sex_male,NumericVector subjId,NumericVector unique_subjId){
  
  int nsubj = unique_subjId.length();
  IntegerVector arr(nsubj);
  
  for(int i=0; i < nsubj; i++){
      LogicalVector idx(subjId == (unique_subjId[i]));
      IntegerVector sex(sex_male[idx]);
      arr[i] = sex[0]; 
  }
  
  return arr;
}
for(int i=0; i < nsubj; i++){
  
  // u~N(0,sigma_u^2*alpha(sex_i)*exp(-gamma_i))
  vector<bool> idx(subjId == (unique_subjId[i]));
  vector<int> sex(sex_male[idx]);
  f -= dnorm(u[i], mean_ran , exp(sigma_u+(sex[1])*alpha-gamma[i]), true);
  // gamma~N(0,sigma_G^2)
  f -= dnorm(gamma[i],mean_ran,exp(sigma_G),true);
  
}