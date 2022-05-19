#include <TMB.hpp>
template<class Type> Type objective_function<Type>::operator()(){
   // Data:
   DATA_VECTOR(landings);                      // Observed landings.
   DATA_VECTOR(trap_number);                   // Number of traps.
   DATA_VECTOR(day);                           // Fishing day since start of season.
   DATA_IVECTOR(zone);                         // Fishing zone indicator.
   DATA_VECTOR(soak_time);                     // Soak time.

   // Instar growth parameters:
   PARAMETER_VECTOR(alpha_zone);               // Intercept parameter by zone.
   PARAMETER_VECTOR(beta_day_0);               // Fishing day coefficient.
   PARAMETER_VECTOR(beta_day_1);
   PARAMETER_VECTOR(beta_day_2);
   PARAMETER(beta_setting);                    // Setting period coefficient.

   PARAMETER_VECTOR(log_scale_soak_time);
   PARAMETER_VECTOR(log_rate_soak_time);

   // Error distribution parameters:
   PARAMETER(gamma);                           // Johnson's SU distribution skewness parameter.
   PARAMETER(log_delta);                       // Johnson's SU distribution shape parameter.
   PARAMETER(log_lambda);                      // Johnson's SU distribution scale parameter.

   using namespace density;

   // Declare constants:
   Type v = 0;
   int n = landings.size();

   vector<Type> log_mu(n);
   vector<Type> residuals(n);
   for (int i = 0; i < n; i++){
      // Calculate mean:
      log_mu[i] = alpha_zone[zone[i]] +                                            // Scale effects by zone.
                  log(trap_number[i]) +                                            // Number of traps offset term.
                  beta_day_0[zone[i]] * day[i] +                                   // Linear day effect.
                  beta_day_1[zone[i]] * (1 - exp(-beta_day_2[zone[i]] * day[i])) + // Exponential decay component of day effect.
                  log(pgamma(soak_time[i], exp(log_scale_soak_time[zone[i]]), exp(log_rate_soak_time[zone[i]])));   // Soak time effect.

      // Log-likelihood for Johnson's SU distribution:
      Type U = (log(landings[i]) - log_mu[i]) / exp(log_lambda);
      Type asinh_U = log(U + sqrt(1 + U*U));
      v -= log_delta - log_lambda - 0.5 * log(2*3.141592653) - 0.5 * log(1 + U*U) - 0.5 * pow(gamma + exp(log_delta) * asinh_U,2);
   }

   return v;
}

