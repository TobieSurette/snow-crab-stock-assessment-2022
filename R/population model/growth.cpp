#include <TMB.hpp>
template<class Type> Type objective_function<Type>::operator()(){
   // Data declarations:
   DATA_VECTOR(x);                   // Size values.
   DATA_VECTOR(f);                   // Frequencies of size values.

   // Model parameters:
   PARAMETER(intercept_growth);      // Growth intercept parameter.
   PARAMETER(xp_growth);             // Growth pivot point parameter.
   PARAMETER(log_window_growth);     // Growth transition window parameter.
   PARAMETER_VECTOR(slope_growth);   // Growth slope parameters.
   PARAMETER(log_sigma_growth);      // Growth error parameter.
 
   // Constants, accumulators and transformed parameters:
   int n = x.size(); // Number of observations.
   Type nll = 0;     // Initialize negative log-likelihood.
   Type dx = 1.0;    // Size variable precision.
   
   // Smoothed piece-wise linear model:
   vector<Type> mu_growth(n);
   vector<Type> sigma_growth(n);
   vector<Type> phi_growth(n);
   vector<Type> k_growth(n);
   for (int i = 0; i < n; i++){
      // Growth increment:
      mu_growth[i] = intercept_growth +
                     slope_growth[0] * x[i] +
                     exp(log_window_growth) * (slope_growth[1] - slope_growth[0]) *
                     log(1 + exp((x[i] - xp_growth) / exp(log_window_growth)));

      // Growth error:
      sigma_growth[i] = exp(log_sigma_growth) * mu_growth[i];

      // Gamma parameters:
      phi_growth[i] = (sigma_growth[i] * sigma_growth[i]) / mu_growth[i];  // Gamma scale parameters.
      k_growth[i]   = mu_growth[i] / phi_growth[i];                        // Gamma shape parameters.
   }

   // Growth increment matrix:
   matrix<Type> G(n, n);
   G.fill(0);
   for (int i = 0; i < n; i++){
      for (int j = i; j < (n-1); j++){
         G(i,j) = pgamma(x[j] - x[i] + (dx / 2.0), k_growth[i], phi_growth[i]) - 
                  pgamma(x[j] - x[i] - (dx / 2.0), k_growth[i], phi_growth[i]);
      }
      G(i,n-1) = 1.0 - pgamma(x[n-1] - x[i] - (dx / 2.0), k_growth[i], phi_growth[i]);
   }

   // Apply growth to size-frequencies:
   vector<Type> fp(n);
   fp.fill(0);
   for (int i = 0; i < n; i++){
      for (int j = 0; j < n; j++){
         fp[j] += f[i] * G(i,j);
      }
   }
   
   // Output:
   REPORT(mu_growth);
   REPORT(sigma_growth);
   REPORT(phi_growth);
   REPORT(k_growth);
   REPORT(G);
   REPORT(fp);
   
   return nll;
}

