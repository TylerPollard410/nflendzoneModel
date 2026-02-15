// Joint home/away count model (NegBinomial-2 on log scale) with offense/defense latents
// ROBUST VERSION: Uses Scaling Factors to prevent initialization explosions.

functions {
  matrix compute_eta_home_away(array[] int home_team, array[] int away_team,
                               array[] int week_idx, array[] int season_idx,
                               array[] int hfa,
                               array[] vector team_off_strength,
                               array[] vector team_def_strength,
                               array[] vector team_hfa, vector alpha_log,
                               int N_games) {
    matrix[N_games, 2] eta;
    for (g in 1 : N_games) {
      int w = week_idx[g];
      int s = season_idx[g];
      int h = home_team[g];
      int a = away_team[g];
      
      eta[g, 1] = alpha_log[s]
                  + (team_off_strength[w][h] - team_def_strength[w][a])
                  + (hfa[g] == 1 ? 0.5 * team_hfa[s][h] : 0);
      
      eta[g, 2] = alpha_log[s]
                  + (team_off_strength[w][a] - team_def_strength[w][h])
                  - (hfa[g] == 1 ? 0.5 * team_hfa[s][h] : 0);
    }
    return eta;
  }
  
  array[] int compute_week_to_season(int N_weeks, int N_games,
                                     array[] int week_idx,
                                     array[] int season_idx) {
    array[N_weeks] int week_to_season = rep_array(1, N_weeks);
    for (g in 1 : N_games) {
      int w = week_idx[g];
      int s = season_idx[g];
      if (s > week_to_season[w]) 
        week_to_season[w] = s;
    }
    return week_to_season;
  }
  
  array[] int compute_is_first_week(int N_weeks, int N_games,
                                    array[] int week_idx,
                                    array[] int fw_season_idx) {
    array[N_weeks] int is_first_week = rep_array(0, N_weeks);
    for (g in 1 : N_games) 
      if (fw_season_idx[g] == 1) 
        is_first_week[week_idx[g]] = 1;
    if (N_weeks > 0) 
      is_first_week[1] = 1;
    return is_first_week;
  }
  
  array[] int compute_is_last_week(int N_weeks, int N_games,
                                   array[] int week_idx,
                                   array[] int lw_season_idx) {
    array[N_weeks] int is_last_week = rep_array(0, N_weeks);
    for (g in 1 : N_games) 
      if (lw_season_idx[g] == 1) 
        is_last_week[week_idx[g]] = 1;
    return is_last_week;
  }
}
data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;
  
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx;
  array[N_games] int<lower=1, upper=N_weeks> week_idx;
  
  array[N_games] int<lower=0, upper=1> fw_season_idx;
  array[N_games] int<lower=0, upper=1> lw_season_idx;
  array[N_games] int<lower=0, upper=1> hfa;
  
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
}
transformed data {
  array[N_weeks] int week_to_season = compute_week_to_season(N_weeks,
                                        N_games, week_idx, season_idx);
  array[N_weeks] int is_first_week = compute_is_first_week(N_weeks, N_games,
                                       week_idx, fw_season_idx);
  array[N_weeks] int is_last_week = compute_is_last_week(N_weeks, N_games,
                                      week_idx, lw_season_idx);
  
  // SCALING CONSTANTS
  // We use these to multiply the raw parameters.
  // Even if raw_sigma is initialized to 5.0, result is 0.5.
  real SCALE_HFA = 0.1;
  real SCALE_INIT = 0.1;
  real SCALE_WEEK = 0.05;
  real SCALE_SEAS = 0.1;
  real SCALE_PHI = 0.1;
}
parameters {
  // --- League HFA ---
  real league_hfa_init;
  vector[N_seasons - 1] z_league_hfa_innovation;
  real<lower=0> sigma_league_hfa_std; // Init range ~[0.1, 7], Physical ~[0.01, 0.7] due to scale
  real<lower=0, upper=1> phi_league_hfa;
  
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation;
  real<lower=0> sigma_team_hfa_std;
  
  // --- Offense/Defense Init ---
  sum_to_zero_vector[N_teams] z_team_off_init;
  sum_to_zero_vector[N_teams] z_team_def_init;
  real<lower=0> sigma_team_off_init_std;
  real<lower=0> sigma_team_def_init_std;
  
  // --- Weekly Innovations ---
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_off_innov;
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_def_innov;
  real<lower=0> sigma_weekly_off_std;
  real<lower=0> sigma_weekly_def_std;
  real<lower=0, upper=1> phi_weekly_off;
  real<lower=0, upper=1> phi_weekly_def;
  
  // --- Season Innovations ---
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_off_innov;
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_def_innov;
  real<lower=0> sigma_season_off_std;
  real<lower=0> sigma_season_def_std;
  real<lower=0, upper=1> phi_season_off;
  real<lower=0, upper=1> phi_season_def;
  
  // --- Scoring Environment ---
  // Centered parameterization: 22 + 3 * std
  real alpha_score_std;
  
  real alpha_log_dev_init;
  vector[N_seasons - 1] z_alpha_log_dev_innovation;
  real<lower=0> sigma_alpha_log_std;
  real<lower=0, upper=1> phi_alpha_log;
  
  // --- Dispersion ---
  real log_phi_league_std;
  real log_phi_home_raw;
  real log_phi_away_raw;
  real<lower=0> sigma_log_phi_std;
}
transformed parameters {
  // 1. Rescale Sigmas to Physical Scale
  real sigma_league_hfa = sigma_league_hfa_std * SCALE_HFA;
  real sigma_team_hfa = sigma_team_hfa_std * SCALE_HFA;
  
  real sigma_team_off_init = sigma_team_off_init_std * SCALE_INIT;
  real sigma_team_def_init = sigma_team_def_init_std * SCALE_INIT;
  
  real sigma_weekly_off_innov = sigma_weekly_off_std * SCALE_WEEK;
  real sigma_weekly_def_innov = sigma_weekly_def_std * SCALE_WEEK;
  
  real sigma_season_off_innov = sigma_season_off_std * SCALE_SEAS;
  real sigma_season_def_innov = sigma_season_def_std * SCALE_SEAS;
  
  real sigma_alpha_log = sigma_alpha_log_std * SCALE_SEAS;
  real sigma_log_phi_ha = sigma_log_phi_std * SCALE_PHI;
  
  // 2. Build Model Components
  // Derived scoring environment (season-level)
  // Center Alpha around 22 points
  real alpha_score_raw = 22 + 3 * alpha_score_std;
  
  real alpha_log_base = log(fmax(1.0, alpha_score_raw));
  vector[N_seasons] alpha_log_dev;
  vector[N_seasons] alpha_log;
  
  alpha_log_dev[1] = alpha_log_dev_init;
  for (s in 2 : N_seasons) 
    alpha_log_dev[s] = phi_alpha_log * alpha_log_dev[s - 1]
                       + z_alpha_log_dev_innovation[s - 1] * sigma_alpha_log;
  alpha_log = alpha_log_base + alpha_log_dev;
  
  // League HFA
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) 
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + z_league_hfa_innovation[s - 1] * sigma_league_hfa;
  
  // Team HFA
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1 : N_seasons) 
    team_hfa[s] = league_hfa[s] + z_team_hfa_deviation[s] * sigma_team_hfa;
  
  // Offense/Defense states
  array[N_weeks] vector[N_teams] team_off_strength;
  array[N_weeks] vector[N_teams] team_def_strength;
  array[N_weeks] vector[N_teams] team_strength;
  
  team_off_strength[1] = z_team_off_init * sigma_team_off_init;
  team_def_strength[1] = z_team_def_init * sigma_team_def_init;
  
  for (w in 2 : N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_off_strength[w] = phi_season_off * team_off_strength[w - 1]
                             + z_season_off_innov[s - 1]
                               * sigma_season_off_innov;
      team_def_strength[w] = phi_season_def * team_def_strength[w - 1]
                             + z_season_def_innov[s - 1]
                               * sigma_season_def_innov;
    } else {
      team_off_strength[w] = phi_weekly_off * team_off_strength[w - 1]
                             + z_weekly_off_innov[w - 1]
                               * sigma_weekly_off_innov;
      team_def_strength[w] = phi_weekly_def * team_def_strength[w - 1]
                             + z_weekly_def_innov[w - 1]
                               * sigma_weekly_def_innov;
    }
  }
  
  for (w in 1 : N_weeks) 
    team_strength[w] = team_off_strength[w] + team_def_strength[w];
  
  // Hierarchical dispersion
  // Centered on log(12) ~ 2.48
  real log_phi_league = 2.5 + 0.5 * log_phi_league_std;
  real<lower=0> phi_home = fmax(1e-6,
                                exp(
                                    log_phi_league
                                    + sigma_log_phi_ha * log_phi_home_raw));
  real<lower=0> phi_away = fmax(1e-6,
                                exp(
                                    log_phi_league
                                    + sigma_log_phi_ha * log_phi_away_raw));
}
model {
  // ---------------------------
  // Priors on Standardized Parameters
  // ---------------------------
  // Prior scale is now relative to the hard-coded SCALE constant.
  // e.g., std ~ N(0, 2) * SCALE(0.1) => physical ~ N(0, 0.2)
  
  // HFA
  league_hfa_init ~ normal(0, 0.2);
  z_league_hfa_innovation ~ std_normal();
  sigma_league_hfa_std ~ student_t(3, 0, 1.0); // Resulting scale ~ 0.1
  phi_league_hfa ~ beta(8, 2);
  
  for (s in 1 : N_seasons) 
    z_team_hfa_deviation[s] ~ std_normal();
  sigma_team_hfa_std ~ student_t(3, 0, 1.0); // Resulting scale ~ 0.1
  
  // Off/Def init
  z_team_off_init ~ std_normal();
  z_team_def_init ~ std_normal();
  sigma_team_off_init_std ~ student_t(3, 0, 2.0); // Resulting scale ~ 0.2
  sigma_team_def_init_std ~ student_t(3, 0, 2.0);
  
  // Weekly innovations
  for (w in 1 : (N_weeks - 1)) {
    z_weekly_off_innov[w] ~ std_normal();
    z_weekly_def_innov[w] ~ std_normal();
  }
  sigma_weekly_off_std ~ student_t(3, 0, 1.0); // Resulting scale ~ 0.05
  sigma_weekly_def_std ~ student_t(3, 0, 1.0);
  phi_weekly_off ~ beta(9, 1);
  phi_weekly_def ~ beta(9, 1);
  
  // Seasonal innovations
  for (s in 1 : (N_seasons - 1)) {
    z_season_off_innov[s] ~ std_normal();
    z_season_def_innov[s] ~ std_normal();
  }
  sigma_season_off_std ~ student_t(3, 0, 2.0); // Resulting scale ~ 0.2
  sigma_season_def_std ~ student_t(3, 0, 2.0);
  phi_season_off ~ beta(6, 4);
  phi_season_def ~ beta(6, 4);
  
  // Intercept (centered)
  alpha_score_std ~ std_normal(); // ~ N(22, 3) because of transform
  
  alpha_log_dev_init ~ normal(0, 0.1);
  z_alpha_log_dev_innovation ~ std_normal();
  sigma_alpha_log_std ~ student_t(3, 0, 1.0);
  phi_alpha_log ~ beta(8, 2);
  
  // Dispersion shrinkage
  log_phi_league_std ~ std_normal();
  log_phi_home_raw ~ std_normal();
  log_phi_away_raw ~ std_normal();
  sigma_log_phi_std ~ student_t(3, 0, 2.0); // Resulting scale ~ 0.2
  
  // ---------------------------
  // Likelihood
  // ---------------------------
  {
    matrix[N_games, 2] eta = compute_eta_home_away(home_team, away_team,
                               week_idx, season_idx, hfa, team_off_strength,
                               team_def_strength, team_hfa, alpha_log,
                               N_games);
    
    home_score ~ neg_binomial_2_log(eta[ : , 1], phi_home);
    away_score ~ neg_binomial_2_log(eta[ : , 2], phi_away);
  }
}
generated quantities {
  // Last observed global week and season
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  // Filtered snapshots
  vector[N_teams] filtered_team_off_strength = team_off_strength[last_w];
  vector[N_teams] filtered_team_def_strength = team_def_strength[last_w];
  vector[N_teams] filtered_team_hfa = team_hfa[last_s];
  real filtered_league_hfa = league_hfa[last_s];
  real filtered_alpha_log = alpha_log[last_s];
  
  // One-step-ahead state draws (for R-side forecasting)
  vector[N_teams] predicted_team_off_strength;
  vector[N_teams] predicted_team_def_strength;
  vector[N_teams] predicted_team_hfa;
  real predicted_league_hfa;
  real predicted_alpha_log;
  
  {
    int next_is_first = is_last_week[last_w];
    
    vector[N_teams - 1] off_raw;
    vector[N_teams - 1] def_raw;
    for (t in 1 : (N_teams - 1)) {
      off_raw[t] = normal_rng(0, 1);
      def_raw[t] = normal_rng(0, 1);
    }
    vector[N_teams] z0_off = sum_to_zero_constrain(off_raw);
    vector[N_teams] z0_def = sum_to_zero_constrain(def_raw);
    
    // If NEXT week starts a new season
    if (next_is_first == 1) {
      predicted_team_off_strength = phi_season_off
                                    * team_off_strength[last_w]
                                    + z0_off * sigma_season_off_innov;
      predicted_team_def_strength = phi_season_def
                                    * team_def_strength[last_w]
                                    + z0_def * sigma_season_def_innov;
      
      predicted_league_hfa = phi_league_hfa * league_hfa[last_s]
                             + normal_rng(0, sigma_league_hfa);
      
      predicted_alpha_log = alpha_log_base
                            + phi_alpha_log * alpha_log_dev[last_s]
                            + normal_rng(0, sigma_alpha_log);
      
      vector[N_teams - 1] hfa_raw;
      for (t in 1 : (N_teams - 1)) 
        hfa_raw[t] = normal_rng(0, 1);
      vector[N_teams] hfa_dev = sum_to_zero_constrain(hfa_raw);
      predicted_team_hfa = predicted_league_hfa + hfa_dev * sigma_team_hfa;
    } else {
      predicted_team_off_strength = phi_weekly_off
                                    * team_off_strength[last_w]
                                    + z0_off * sigma_weekly_off_innov;
      predicted_team_def_strength = phi_weekly_def
                                    * team_def_strength[last_w]
                                    + z0_def * sigma_weekly_def_innov;
      
      predicted_league_hfa = league_hfa[last_s];
      predicted_alpha_log = alpha_log[last_s];
      predicted_team_hfa = team_hfa[last_s];
    }
  }
  
  // Per-game log-lik (sum of two NB2 logs)
  vector[N_games] log_lik;
  {
    matrix[N_games, 2] eta = compute_eta_home_away(home_team, away_team,
                               week_idx, season_idx, hfa, team_off_strength,
                               team_def_strength, team_hfa, alpha_log,
                               N_games);
    
    for (g in 1 : N_games) 
      log_lik[g] = neg_binomial_2_log_lpmf(home_score[g] | eta[g, 1],
                     phi_home)
                   + neg_binomial_2_log_lpmf(away_score[g] | eta[g, 2],
                       phi_away);
  }
}
