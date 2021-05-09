#include <TMB.hpp>                                // Links in the TMB libraries


template<class Type>
  Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(clo);                                 // Data vector transmitted from R
  DATA_VECTOR(sex_male);                            // Data vector transmitted from R
  DATA_FACTOR(subjId);                              // Data vector transmitted from R
  DATA_FACTOR(subjId_day);                          // Data vector transmitted from R
  DATA_FACTOR(subjId_u);                            // Data vector transmitted from R
  DATA_FACTOR(subjId_v);                            // Data vector transmitted from R
  
  PARAMETER_VECTOR(u);                              // Random effects
  PARAMETER_VECTOR(v);                              // Random effects
  // Parameters
  PARAMETER(mu);                                    // Parameter value transmitted from R
  PARAMETER(beta);                                  // Parameter value transmitted from R
  PARAMETER(sigma_u);                               // Parameter value transmitted from R
  PARAMETER(sigma);                                 // Parameter value transmitted from R
  PARAMETER(sigma_v);                               // Parameter value transmitted from R
  
  using namespace density;
  int nobs = clo.size();
  int nsubj = u.size();
  int nsubj_day = v.size();
  
  Type mean_ran = Type(0);
  vector<Type> pred(nobs);
  // vector<Type> res(nobs);
  int i;
  int j;
  
  Type f = Type(0);                     // Declare the "objective function" (neg. log. likelihood)
  // subject random effect for u and gamma
  for(int i=0; i < nsubj; i++){
    // u~N(0,sigma_u^2*alpha(sex_i)*exp(-gamma_i))
    f -= dnorm(u(i), mean_ran , exp(sigma_u), true);
  }
  // subject ID and day interaction random effect for v
  for(int j=0; j < nsubj_day; j++){
    // v~N(0,sigma_v^2*alpha(sex_i)*exp(-gamma_i))
    i = subjId_v(j);
    f -= dnorm(v(j),mean_ran,exp(sigma_v),true);
    
  }
  // clo_{k,i,j}|u,j,gamma = N (mu+beta(sex_i)+u_i+v_{i,j}, sigma^2*alpha(sex_i)*exp(-gamma))
  for(int k =0; k < nobs; k++){
    j = subjId_day[k];
    i = subjId[k];
    
    pred[k] = mu+beta*sex_male[k]+u(i)+v(j);
    // res[k] = clo[k]-pred[k];
    f -= dnorm(clo(k),pred(k),exp(sigma),true);
  }
  return f;
  }

