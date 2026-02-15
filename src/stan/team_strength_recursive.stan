// Recursive update model for NFL team strength
// Designed for 1-week (or short window) updates using priors from previous state.

functions {
  matrix compute_eta_home_away(array[] int home_team, array[] int away_team,
                               array[] int week_idx, array[] int hfa,
                               array[] vector team_off_strength,
                               array[] vector team_def_strength,
                               vector team_hfa, real alpha_log, int N_games) {
    matrix[N_games, 2] eta;
    for (g in 1 : N_games) {
      int w = week_idx[g];
      int h = home_team[g];
      int a = away_team[g];
      
      // Note: team_hfa is now a single vector (static within window) or we index by season?
      // For short window recursive, we assume parameters are for the current season/frame.
      
      eta[g, 1] = alpha_log
                  + (team_off_strength[w][h] - team_def_strength[w][a])
                  + (hfa[g] == 1 ? 0.5 * team_hfa[h] : 0);
      
      eta[g, 2] = alpha_log
                  + (team_off_strength[w][a] - team_def_strength[w][h])
                  - (hfa[g] == 1 ? 0.5 * team_hfa[h] : 0);
    }
    return eta;
  }
}
data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_weeks; // Usually 1 for recursive update
  
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_weeks> week_idx;
  array[N_games] int<lower=0, upper=1> hfa;
  
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  
  // --- Priors (from previous step) ---
  // Input: Mean and SD for the "Starting" state of this window
  vector[N_teams] prior_off_mean;
  vector[N_teams] prior_off_sd;
  vector[N_teams] prior_def_mean;
  vector[N_teams] prior_def_sd;
  
  // HFA Logic: Can be fixed or updated. 
  // If updated, provide prior. If fixed, provide exact values and set prior_sd very small?
  // Let's allow updating.
  vector[N_teams] prior_team_hfa_mean;
  vector[N_teams] prior_team_hfa_sd;
  
  // Intercept Prior
  real prior_alpha_log_mean;
  real prior_alpha_log_sd;
  
  // --- Fixed Hyperparameters (Offline Learned) ---
  real<lower=0> sigma_weekly_off;
  real<lower=0> sigma_weekly_def;
  real<lower=0, upper=1> phi_weekly_off;
  real<lower=0, upper=1> phi_weekly_def;
  
  // Dispersion (Fixed)
  real<lower=0> phi_home;
  real<lower=0> phi_away;
}
parameters {
  // States
  // For week 1, these are "initialized" by the prior. 
  // For week > 1, they evolve.
  array[N_weeks] vector[N_teams] team_off_strength;
  array[N_weeks] vector[N_teams] team_def_strength;
  
  vector[N_teams] team_hfa;
  real alpha_log;
}
model {
  // --- Priors ---
  
  // Week 1: Direct prior from input (representing Proj(t-1 -> t))
  team_off_strength[1] ~ normal(prior_off_mean, prior_off_sd);
  team_def_strength[1] ~ normal(prior_def_mean, prior_def_sd);
  
  // Subsequent weeks (Transition)
  for (w in 2 : N_weeks) {
    team_off_strength[w] ~ normal(phi_weekly_off * team_off_strength[w - 1],
                                  sigma_weekly_off);
    team_def_strength[w] ~ normal(phi_weekly_def * team_def_strength[w - 1],
                                  sigma_weekly_def);
  }
  
  // Static/Slow parameters within window
  team_hfa ~ normal(prior_team_hfa_mean, prior_team_hfa_sd);
  alpha_log ~ normal(prior_alpha_log_mean, prior_alpha_log_sd);
  
  // Optional: Soft centering constraints to prevent drift if priors are weak
  sum(team_off_strength[1]) ~ normal(0, 0.001 * N_teams);
  sum(team_def_strength[1]) ~ normal(0, 0.001 * N_teams);
  sum(team_hfa) ~ normal(sum(prior_team_hfa_mean), 0.001 * N_teams);
  
  // --- Likelihood ---
  {
    matrix[N_games, 2] eta = compute_eta_home_away(home_team, away_team,
                               week_idx, hfa, team_off_strength,
                               team_def_strength, team_hfa, alpha_log,
                               N_games);
    
    home_score ~ neg_binomial_2_log(eta[ : , 1], phi_home);
    away_score ~ neg_binomial_2_log(eta[ : , 2], phi_away);
  }
}
generated quantities {
  // Generate one-step-ahead forecast for NEXT week (after the window)
  // This assumes the user might want a prediction for N_weeks + 1
  // Using the same evolution logic as the main model
  
  vector[N_teams] predicted_team_off_strength;
  vector[N_teams] predicted_team_def_strength;
  
  int last_w = N_weeks;
  
  // Prediction step (Evolution)
  // Note: We use the FIXED Params for evolution
  for (t in 1 : N_teams) {
    predicted_team_off_strength[t] = normal_rng(
                                                phi_weekly_off
                                                * team_off_strength[last_w][t],
                                                sigma_weekly_off);
    predicted_team_def_strength[t] = normal_rng(
                                                phi_weekly_def
                                                * team_def_strength[last_w][t],
                                                sigma_weekly_def);
  }
  
  // Pass through static/slow parameters for next prior
  vector[N_teams] predicted_team_hfa = team_hfa;
  real predicted_alpha_log = alpha_log;
}
