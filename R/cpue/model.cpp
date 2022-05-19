#include <TMB.hpp>
template<class Type> Type objective_function<Type>::operator()(){
   // Data:
   DATA_VECTOR(landings);                      // Observed landings.
   DATA_VECTOR(trap_number);                   // Number of traps.
   DATA_VECTOR(day);                           // Fishing day since start of season.
   DATA_VECTOR(setting);                       // Indicator for setting traps.
   DATA_IVECTOR(zone);                         // Fishing zone indicator.
   DATA_VECTOR(soak_time);                     // Soak time.

   // Instar growth parameters:
   PARAMETER_VECTOR(alpha_zone);               // Intercept parameter by zone.
   PARAMETER_VECTOR(beta_day_0);               // Fishing day coefficient.
   PARAMETER_VECTOR(beta_day_1);
   PARAMETER(beta_setting);                    // Setting period coefficient.
   PARAMETER(logit_eps);
   PARAMETER_VECTOR(log_scale_soak_time);
   PARAMETER_VECTOR(log_rate_soak_time);

   // Error parameters:
   PARAMETER_VECTOR(log_sigma_eps);            // Observation error parameter.

   using namespace density;

   // Declare constants:
   Type v = 0;
   int n = landings.size();
   Type p_eps = 1.0 / (1.0 + exp(-logit_eps));

   vector<Type> log_mu(n);
   vector<Type> residuals(n);
   for (int i = 0; i < n; i++){
      // Calculate mean:
      log_mu[i] = alpha_zone[zone[i]] +        // Scale effects by zone.
                  log(trap_number[i]) +        // Number of traps offset term.
                  beta_day_0[zone[i]] * day[i] +
                  beta_day_1[zone[i]] * day[i]*day[i] +        // Linear day effect.
                  beta_setting * setting[i] +  // Trap setting effect.
                  log(pgamma(soak_time[i], exp(log_scale_soak_time[zone[i]]), exp(log_rate_soak_time[zone[i]])));   // Soak time effect.

      // Calculate residuals:
      residuals[i] = log(landings[i]) - log_mu[i];

      // Log-likelihood:
      v -= log(p_eps * dnorm(log(landings[i]), log_mu[i], exp(log_sigma_eps[0]), false) +
               (1-p_eps) * dnorm(log(landings[i]), log_mu[i], exp(log_sigma_eps[0]) + exp(log_sigma_eps[1]), false)) ;
   }

   // Prediction:
   vector<Type> log_mu_pred(4);
   for (int j = 0; j < 4; j++){
      log_mu_pred[j] = alpha_zone[j] +        // Scale effects by zone.
                       beta_day_0[j] * 7 +
                       beta_day_1[j] * 7*7 +        // Linear day effect.
                       log(pgamma(24, exp(log_scale_soak_time[j]), exp(log_rate_soak_time[j])));   // Soak time effect.

   }

   // Report results:
   REPORT(residuals);
   REPORT(log_mu_pred);

   return v;
}

