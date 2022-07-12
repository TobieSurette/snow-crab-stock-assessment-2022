#include <TMB.hpp>
template<class Type> Type objective_function<Type>::operator()(){
   // Data declarations:
   DATA_VECTOR(x);                             // Size values.
   DATA_VECTOR(f);                             // Frequencies of size values.
   DATA_INTEGER(n_instar);                     // Number of instars participating in recruitment.
   
   // Recruitment growth and abundance parameters:                       
   PARAMETER(mu0_rec);                         // Size of first instar.
   PARAMETER(log_sigma0_rec);                  // Error for first instar.
   PARAMETER(log_hiatt_intercept_rec);         // Hiatt intercept parameter.
   PARAMETER(log_hiatt_slope_rec);             // Hiatt slope parameter.
   PARAMETER(log_growth_error_rec);            // Growth increment error inflation parameter.

   // Constants, accumulators and transformed parameters:
   Type nll = 0;                                            // Initialize negative log-likelihood.
   
   // Recruitment instar sizes and errors:
   vector<Type> mu_rec(n_instar);
   vector<Type> log_sigma_rec(n_instar);
   mu_rec[0] = mu0_rec;
   log_sigma_rec[0] = log_sigma0_rec; 
   for (int k = 1; k < n_instar; k++){
      mu_rec[k] = exp(log_hiatt_intercept_rec) + mu_rec[k-1] + exp(log_hiatt_slope_rec) * mu_rec[k-1];
      log_sigma_rec[k] = log(1 + exp(log_hiatt_slope_rec) + exp(log_growth_error_rec)) + log_sigma_rec[k-1];
   }
   vector<Type> sigma_rec = exp(log_sigma_rec);
   
   // Output:
   REPORT(mu_rec);
   REPORT(sigma_rec);

   return nll;
}

