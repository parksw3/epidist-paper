functions {
#include functions/utils.stan
#include functions/zero_truncated_normal.stan
#include functions/regression.stan
#include functions/log_expected_latent_from_r.stan
#include functions/log_expected_obs_from_latent.stan
#include functions/discretised_logit_hazard.stan
#include functions/hazard.stan
#include functions/expected_obs.stan
#include functions/combine_logit_hazards.stan
#include functions/expected_obs_from.stan
#include functions/filt_obs_indexes.stan
#include functions/obs_lpmf.stan
#include functions/allocate_observed_obs.stan
#include functions/apply_missing_reference_effects.stan
#include functions/log_expected_by_report.stan
#include functions/delay_lpmf.stan
}

data {
  // Indexes and lookups
  int n; // total number of observations (obs)
  int t; // total number of time points 
  int s; // total number of snapshots
  int g; // total number of groups
  array[g] int groups; // array of group ids
  array[s] int st; // time index of each snapshot (snapshot time)
  array[s] int sg; // group index of each snapshot (snapshot group)
  array[t, g] int ts; // snapshot index by time and group
  array[s] int sl; // number of reported obs per snapshot (snapshot length)
  array[s] int csl; // cumulative version of sl
  int dmax; // maximum possible reporting delay
  array[s] int sdmax; // maximum delay by snapshot (snapshot dmax)
  array[s] int csdmax; // cumulative version of sdmax

  // Observations
  array[s, dmax] int obs; // obs by reference date (row) and delay (column)
  array[n] int flat_obs; // obs stored as a flat vector
  array[t, g] int latest_obs; // latest obs by time and group

  // Expectation model
  // ---- Growth rate submodule ----
  int expr_r_seed; // number of time points with seeded/initial latent cases
  int expr_gt_n; // maximum generation time
  int expr_t; // number of time points with modeled growth rate per group
  int expr_ft; // number of time points with latent cases (expr_r_seed + expr_t)
  // PMF of the generation time distribution (reversed and on log scale)
  vector[expr_gt_n] expr_lrgt; 
  // Model for growth rate process. Currently, 0 = none
  int expr_obs;
  int expr_fnindex;
  int expr_fintercept; // Should an intercept be included
  int expr_fncol;
  int expr_rncol;
  matrix[expr_fnindex, expr_fncol + expr_fintercept] expr_fdesign;
  matrix[expr_fncol, expr_rncol + 1] expr_rdesign;
  array[g] int expr_g; // starting time points for growth of each group
  // Priors for growth rate and initial log latent cases
  array[2, g * expr_r_seed] real expr_lelatent_int_p; 
  array[2, 1] real expr_r_int_p;
  array[2, 1] real expr_beta_sd_p;
  // ---- Latent case submodule ----
  int expl_lrd_n; // maximum latent delay (from latent case to obs at ref time)
  // Partial PMF of the latent delay distribution as a sparse convolution matrix
  int expl_lrd_nw;
  vector[expl_lrd_nw] expl_lrd_w;
  int expl_lrd_nv;
  array[expl_lrd_nv] int expl_lrd_v;
  int expl_lrd_nu;
  array[expl_lrd_nu] int expl_lrd_u;
  // Model for latent-to-obs proportion. Currently, 0 = none
  // --> proportion of latent cases that will become observations
  // --> e.g.: infection fatality rate for death data
  int expl_obs;
  int expl_fnindex;
  int expl_fncol;
  int expl_rncol;
  matrix[expl_fnindex, expl_fncol + 1] expl_fdesign;
  matrix[expl_fncol, expl_rncol + 1] expl_rdesign;
  array[2, 1] real expl_beta_sd_p;

  // Reference date model
  int model_refp;
  int refp_fnrow;
  array[s] int refp_findex;
  int refp_fncol;
  matrix[refp_fnrow, refp_fncol + 1] refp_fdesign;
  int refp_rncol;
  matrix[refp_fncol, refp_rncol + 1] refp_rdesign; 
  array[2, 1] real refp_mean_int_p;
  array[2, 1] real refp_sd_int_p;
  array[2, 1] real refp_mean_beta_sd_p;
  array[2, 1] real refp_sd_beta_sd_p; 

  // Reporting time model
  int model_rep;
  int rep_t; // total number of reporting times (t + dmax - 1)
  int rep_fnrow; 
  array[g, rep_t] int rep_findex; 
  int rep_fncol;
  matrix[rep_fnrow, rep_fncol + 1] rep_fdesign;
  int rep_rncol; 
  matrix[rep_fncol, rep_rncol + 1] rep_rdesign; 
  array[2, 1] real rep_beta_sd_p;

  // Missing reference date model
  int model_miss;
  int miss_obs;
  int miss_fnindex;
  int miss_fncol;
  int miss_rncol;
  matrix[miss_fnindex, miss_fncol + 1] miss_fdesign;
  matrix[miss_fncol, model_miss ? miss_rncol + 1 : 0] miss_rdesign;
  // Observations reported without a reference date (by reporting time)
  // --> Since the first dmax-1 obs are not used here, the reporting time
  // --> starts at dmax, not at 1
  array[miss_obs] int missing_reference;
  // Lookup for the obs index by reference date of entries in missing_reference:
  // --> If observations from entry i of missing_reference had a reporting
  // --> delay of d, their index in the flat vector of obs by reference date
  // --> (e.g. flat_obs) would be: obs_by_report[i, d]
  array[miss_obs, dmax] int obs_by_report;
  // Observations by group (and cumulative) for missing_reference and
  // obs_by_report
  array[miss_obs ? g : 0] int miss_st;
  array[miss_obs ? g : 0] int miss_cst;
  array[2, 1] real miss_int_p;
  array[2, 1] real miss_beta_sd_p;

  // Observation model
  int model_obs; // control parameter for the observation model
  array[2, 1] real sqrt_phi_p; // 1/sqrt (overdispersion)

  // Control parameters
  int debug; // should debug information be shown?
  int likelihood; // should the likelihood be included?
  // type of aggregation scheme (0 = snaps, 1 = groups)
  int likelihood_aggregation;
  int pp; // should posterior predictions be produced?
  int cast; // should a nowcast be produced?
  int ologlik; // should the pointwise log likelihood be calculated?
}

transformed data{
  real logdmax = 5*log(dmax); // scaled maxmimum delay to log for crude bound
  // if no reporting time effects use native probability for reference date
  // effects, i.e. do not convert to logit hazard
  int ref_as_p = (model_rep > 0 || model_refp == 0) ? 0 : 1; 
  // Type of likelihood aggregation to use
  int ll_aggregation = likelihood_aggregation + model_miss;
}

parameters {
  // Expectation model
  // ---- Growth rate submodule ----
  matrix[expr_r_seed, g] expr_lelatent_int; // initial observations by group (log)
  array[expr_fintercept ? 1 : 0] real expr_r_int; // growth rate intercept
  vector[expr_fncol] expr_beta;
  vector<lower=0>[expr_rncol] expr_beta_sd;
  // ---- Latent case submodule ----
  vector[expl_fncol] expl_beta;
  vector<lower=0>[expl_rncol] expl_beta_sd;
    
  // Reference model
  array[model_refp ? 1 : 0] real<lower=-10, upper=logdmax> refp_mean_int;
  array[model_refp > 1 ? 1 : 0]real<lower=1e-3, upper=2*dmax> refp_sd_int; 
  vector[model_refp ? refp_fncol : 0] refp_mean_beta; 
  vector[model_refp > 1 ? refp_fncol : 0] refp_sd_beta; 
  vector<lower=0>[refp_rncol] refp_mean_beta_sd;
  vector<lower=0>[model_refp ? refp_rncol : 0] refp_sd_beta_sd; 

  // Report model
  vector[rep_fncol] rep_beta;
  vector<lower=0>[rep_rncol] rep_beta_sd; 

  // Missing reference date model
  array[model_miss] real miss_int;
  vector[miss_fncol] miss_beta; 
  vector<lower=0>[miss_rncol] miss_beta_sd; 

  // Observation model
  array[model_obs > 0 ? 1 : 0] real<lower=0> sqrt_phi; // overdispersion
}

transformed parameters{
  // Expectation model
  vector[expr_t * g] r; // growth rate of observations (log)
  array[g] vector[expr_ft]  exp_llatent; // expected latent cases (log)
  vector[expl_obs ? expl_fnindex : 0] expl_prop; // latent-to-obs proportion
  array[g] vector[t]  exp_lobs; // expected obs by reference date (log)
  
  // Reference model
  vector[refp_fnrow] refp_mean;
  vector[refp_fnrow] refp_sd;
  matrix[dmax, refp_fnrow] ref_lh; // sparse report logit hazards
  
  // Report model
  vector[rep_fnrow] srdlh; // sparse reporting time logit hazards
  
  // Missing reference date model
  vector[miss_fnindex] miss_ref_lprop;

  // Observation model
  array[model_obs > 0 ? 1 : 0] real phi; // Transformed overdispersion

  // Expectation model
  
  // Get log growth rates and map to expected latent cases
  r = combine_effects(
    expr_r_int, expr_beta, expr_fdesign, expr_beta_sd, expr_rdesign, 
    expr_fintercept
  );
  exp_llatent = log_expected_latent_from_r(
    expr_lelatent_int, r, expr_g, expr_t, expr_r_seed, expr_gt_n, expr_lrgt,
    expr_ft, g
  );
  // Get latent-to-obs proportions and map expected latent cases to expected
  // observations
  if (expl_obs) {
    expl_prop = combine_effects(
      {0}, expl_beta, expl_fdesign, expl_beta_sd, expl_rdesign, 1
    );
    exp_lobs = log_expected_obs_from_latent(
      exp_llatent, expl_lrd_n, expl_lrd_w, expl_lrd_v, expl_lrd_u,
      t, g, expl_prop
    );
  } else {
    exp_lobs = exp_llatent; // assume latent cases and obs are identical
  }

  

  // Reference model
  
  if (model_refp) {
    // calculate sparse reference date effects
    
    refp_mean = combine_effects(
      refp_mean_int, refp_mean_beta, refp_fdesign, refp_mean_beta_sd,
      refp_rdesign, 1
    );
    if (model_refp > 1) {
      refp_sd = combine_effects(
        log(refp_sd_int), refp_sd_beta, refp_fdesign, refp_sd_beta_sd,
        refp_rdesign, 1
      ); 
      refp_sd = exp(refp_sd);
    } 
    
    // calculate reference date logit hazards (unless no reporting effects)
    
    for (i in 1:refp_fnrow) {
      ref_lh[, i] = discretised_logit_hazard(
        refp_mean[i], refp_sd[i], dmax, model_refp, 2, ref_as_p
      );
    }
    
  }  
  

  // Report model
  
  srdlh =
    combine_effects({0}, rep_beta, rep_fdesign, rep_beta_sd, rep_rdesign, 1);
  

  // Missing reference model
  if (model_miss) {
    miss_ref_lprop = log_inv_logit(
      combine_effects(
        miss_int, miss_beta, miss_fdesign, miss_beta_sd, miss_rdesign, 1
      )
    );
  }
  
  // Observation model
  if (model_obs) {
    
    phi = inv_square(sqrt_phi);
  
  } 
  // debug issues in truncated data if/when they appear
  if (debug) {
#include /chunks/debug.stan
  }
} 
  
model {
  
  // Expectation model
  // ---- Growth rate submodule ----
  // intercept/initial latent cases (log)
  to_vector(expr_lelatent_int) ~ normal(
    expr_lelatent_int_p[1], expr_lelatent_int_p[2]
  );
  // intercept of growth rate
  if (expr_fintercept) {
    expr_r_int[expr_fintercept]  ~ normal(expr_r_int_p[1], expr_r_int_p[2]); 
  }
  
  // growth rate effect priors
  effect_priors_lp(
    expr_beta, expr_beta_sd, expr_beta_sd_p, expr_fncol, expr_rncol
  );
  // ---- Latent case submodule ----
  // latent-to-obs proportion
  effect_priors_lp(
    expl_beta, expl_beta_sd, expl_beta_sd_p, expl_fncol, expl_rncol
  );
  
  // Reference model
  if (model_refp) {
    refp_mean_int ~ normal(refp_mean_int_p[1], refp_mean_int_p[2]);
    if (model_refp > 1) {
      refp_sd_int ~ normal(refp_sd_int_p[1], refp_sd_int_p[2]);
    }
    effect_priors_lp(
      refp_mean_beta, refp_mean_beta_sd, refp_mean_beta_sd_p, refp_fncol,
       refp_rncol
    );
    if (model_refp > 1) {
      effect_priors_lp(
        refp_sd_beta, refp_sd_beta_sd, refp_sd_beta_sd_p, refp_fncol,
        refp_rncol
      );
    }
  }
  // Report model
  effect_priors_lp(rep_beta, rep_beta_sd, rep_beta_sd_p, rep_fncol, rep_rncol);
  
  // Missing reference date model
  if (model_miss) {
    miss_int ~ normal(miss_int_p[1], miss_int_p[2]);
    effect_priors_lp(
      miss_beta, miss_beta_sd, miss_beta_sd_p, miss_fncol, miss_rncol
    );
  }
  
  // Observation model
  // overdispersion (1/sqrt)
  if (model_obs) {   
    sqrt_phi[1] ~ normal(sqrt_phi_p[1], sqrt_phi_p[2]) T[0,]; 
  }
  
  
  // Log-Likelihood either by snapshot or group depending on settings/model
  if (likelihood) {
    
    if (ll_aggregation) {
      target += reduce_sum(
        delay_group_lupmf, groups, 1, flat_obs, sl, csl, exp_lobs, t, sg, ts,
        st, rep_findex, srdlh, ref_lh, refp_findex, model_refp, rep_fncol, ref_as_p, phi, model_obs, model_miss, miss_obs, missing_reference,
        obs_by_report, miss_ref_lprop, sdmax, csdmax, miss_st, miss_cst
      );
    } else {
      target += reduce_sum(
        delay_snap_lupmf, st, 1, flat_obs, sl, csl,  exp_lobs, sg, st,
        rep_findex, srdlh, ref_lh, refp_findex, model_refp, rep_fncol,
        ref_as_p, phi, model_obs
      );
    }
    
  }
}

generated quantities {
  array[pp ? sum(sl) : 0] int pp_obs;
  array[pp ? miss_obs : 0] int pp_miss_ref;
  vector[ologlik ? s : 0] log_lik;
  array[cast ? dmax : 0, cast ? g : 0] int pp_inf_obs;
  
  if (cast) {
    vector[csdmax[s]] log_exp_obs;
    array[csdmax[s]] int pp_obs_tmp;
    vector[miss_obs]  log_exp_miss_ref;
    vector[miss_obs ? csdmax[s] : 0] log_exp_miss_by_ref;

    // Posterior predictions for observations
    
    log_exp_obs = expected_obs_from_snaps(
      1, s,  exp_lobs, rep_findex, srdlh, ref_lh, refp_findex, model_refp,
      rep_fncol, ref_as_p, sdmax, csdmax, sg, st, csdmax[s]
    );
    
    if (model_miss) {
      // Allocate proportion that are not reported
      log_exp_miss_by_ref = apply_missing_reference_effects(
        1, s, log_exp_obs, sdmax, csdmax, miss_ref_lprop
      );
      log_exp_miss_ref = log_expected_by_report(
        log_exp_miss_by_ref, obs_by_report
      );
      // Allocate proportion that are reported
      log_exp_obs = apply_missing_reference_effects(
        1, s, log_exp_obs, sdmax, csdmax, log1m_exp(miss_ref_lprop)
      );
    }
    // Draw from observation model for observed counts with report and reference
    pp_obs_tmp = obs_rng(log_exp_obs, phi, model_obs);
     

    // Likelihood by snapshot (rather than by observation)
    
    if (ologlik) {
      for (i in 1:s) {
        array[3] int l = filt_obs_indexes(i, i, csl, sl);
        array[3] int f = filt_obs_indexes(i, i, csdmax, sdmax);
        log_lik[i] = 0;
        for (j in 1:sl[i]) {
          log_lik[i] += obs_lpmf(
            flat_obs[l[1] + j - 1]  | log_exp_obs[f[1] + j - 1], phi, model_obs
          );
        }
      }
      // Add log lik component for missing reference model
      if (miss_obs) {
        for (i in 1:miss_obs) {
          log_lik[dmax + i - 1] += obs_lpmf(
            missing_reference[i] | log_exp_miss_ref[i], phi, model_obs
          );
        }
      }
    }
    
    // Posterior prediction for final reported data (i.e at t = dmax + 1)
    // Organise into a grouped and time structured array
    
    for (k in 1:g) {
      int start_t = t - dmax;
      for (i in 1:dmax) {
        int i_start = ts[start_t + i, k];
        array[3] int l = filt_obs_indexes(i_start, i_start, csl, sl);
        array[3] int f = filt_obs_indexes(i_start, i_start, csdmax, sdmax);
        pp_inf_obs[i, k] = sum(segment(flat_obs, l[1], l[3]));
        if (sl[i_start] < dmax) {
          pp_inf_obs[i, k] += sum(
            segment(pp_obs_tmp, f[1] + sl[i_start], f[3] - sl[i_start])
          );
        }
      }
    }
    if (pp) {
      // If posterior predictions for all observations are needed copy
      // from a temporary object to a permanent one
      // drop predictions without linked observations
      pp_obs = allocate_observed_obs(1, s, pp_obs_tmp, sl, csl, sdmax, csdmax);
      // Posterior predictions for observations missing reference dates
      if (miss_obs) {
        pp_miss_ref = obs_rng(log_exp_miss_ref, phi, model_obs);
      }
    }
    
  }
  
}
