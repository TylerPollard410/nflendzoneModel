// Joint home/away count model (NegBinomial-2 on log scale) with offense/defense latents
functions {
  // Return N_games x 2 matrix: log-rate [home, away]
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
}
parameters {
  // League & team HFA
  real league_hfa_init;
  vector[N_seasons - 1] z_league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;
  
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation;
  real<lower=0> sigma_team_hfa;
  
  // Offense/Defense states
  sum_to_zero_vector[N_teams] z_team_off_init;
  sum_to_zero_vector[N_teams] z_team_def_init;
  real<lower=0> sigma_team_off_init;
  real<lower=0> sigma_team_def_init;
  
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_off_innov;
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_def_innov;
  real<lower=0> sigma_weekly_off_innov;
  real<lower=0> sigma_weekly_def_innov;
  real<lower=0, upper=1> phi_weekly_off;
  real<lower=0, upper=1> phi_weekly_def;
  
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_off_innov;
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_def_innov;
  real<lower=0> sigma_season_off_innov;
  real<lower=0> sigma_season_def_innov;
  real<lower=0, upper=1> phi_season_off;
  real<lower=0, upper=1> phi_season_def;
  
  // Scoring intercept on natural scale -> transformed to log
  real<lower=5> alpha_score_raw;
  
  // Season-level scoring environment on log scale (AR(1) deviations)
  real alpha_log_dev_init;
  vector[N_seasons - 1] z_alpha_log_dev_innovation;
  real<lower=0> sigma_alpha_log_dev_innovation;
  real<lower=0, upper=1> phi_alpha_log;
  
  // Overdispersion (NegBinomial-2) on log scale for better geometry
  real log_phi_home;
  real log_phi_away;
}
transformed parameters {
  // Derived intercept and scales
  real alpha_log_base = log(alpha_score_raw);
  vector[N_seasons] alpha_log_dev;
  vector[N_seasons] alpha_log;
  real<lower=0> phi_home = exp(log_phi_home);
  real<lower=0> phi_away = exp(log_phi_away);
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;
  
  array[N_weeks] vector[N_teams] team_off_strength;
  array[N_weeks] vector[N_teams] team_def_strength;
  array[N_weeks] vector[N_teams] team_strength;
  
  alpha_log_dev[1] = alpha_log_dev_init;
  for (s in 2 : N_seasons) 
    alpha_log_dev[s] = phi_alpha_log * alpha_log_dev[s - 1]
                       + z_alpha_log_dev_innovation[s - 1]
                         * sigma_alpha_log_dev_innovation;
  alpha_log = alpha_log_base + alpha_log_dev;
  
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) 
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + z_league_hfa_innovation[s - 1]
                      * sigma_league_hfa_innovation;
  for (s in 1 : N_seasons) 
    team_hfa[s] = league_hfa[s] + z_team_hfa_deviation[s] * sigma_team_hfa;
  
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
}
model {
  // Priors (similar to Poisson file)
  // HFA on log-rate: small centered prior
  league_hfa_init ~ normal(0, 0.3);
  z_league_hfa_innovation ~ std_normal();
  sigma_league_hfa_innovation ~ student_t(3, 0, 0.4);
  phi_league_hfa ~ beta(8, 2);
  
  for (s in 1 : N_seasons) 
    z_team_hfa_deviation[s] ~ std_normal();
  sigma_team_hfa ~ student_t(3, 0, 0.4);
  
  z_team_off_init ~ std_normal();
  z_team_def_init ~ std_normal();
  sigma_team_off_init ~ student_t(3, 0, 5);
  sigma_team_def_init ~ student_t(3, 0, 5);
  
  for (w in 1 : (N_weeks - 1)) {
    z_weekly_off_innov[w] ~ std_normal();
    z_weekly_def_innov[w] ~ std_normal();
  }
  sigma_weekly_off_innov ~ student_t(3, 0, 2);
  sigma_weekly_def_innov ~ student_t(3, 0, 2);
  phi_weekly_off ~ beta(9, 1);
  phi_weekly_def ~ beta(9, 1);
  
  for (s in 1 : (N_seasons - 1)) {
    z_season_off_innov[s] ~ std_normal();
    z_season_def_innov[s] ~ std_normal();
  }
  sigma_season_off_innov ~ student_t(3, 0, 5);
  sigma_season_def_innov ~ student_t(3, 0, 5);
  phi_season_off ~ beta(6, 4);
  phi_season_def ~ beta(6, 4);
  
  // Intercept on natural scale; weakly informative around NFL scoring
  alpha_score_raw ~ normal(21, 5);
  
  // Season-level scoring environment (log scale)
  alpha_log_dev_init ~ normal(0, 0.2);
  z_alpha_log_dev_innovation ~ std_normal();
  sigma_alpha_log_dev_innovation ~ student_t(3, 0, 0.2);
  phi_alpha_log ~ beta(8, 2);
  
  // Overdispersion with weakly-informative log-normal prior
  log_phi_home ~ normal(log(12), 0.8);
  log_phi_away ~ normal(log(12), 0.8);
  
  // Likelihood (independent NegBinomial-2 given latents)
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
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  vector[N_teams] filtered_team_off_strength = team_off_strength[last_w];
  vector[N_teams] filtered_team_def_strength = team_def_strength[last_w];
  vector[N_teams] filtered_team_hfa = team_hfa[last_s];
  real filtered_league_hfa = league_hfa[last_s];
  vector[N_teams] filtered_team_strength = team_strength[last_w];
  
  // One-step-ahead offense/defense (season-first vs within-season)
  vector[N_teams] predicted_team_off_strength;
  vector[N_teams] predicted_team_def_strength;
  vector[N_teams] predicted_team_hfa;
  real predicted_league_hfa;
  vector[N_teams] predicted_team_strength;
  
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
    
    if (next_is_first == 1) {
      predicted_team_off_strength = phi_season_off
                                    * team_off_strength[last_w]
                                    + z0_off * sigma_season_off_innov;
      predicted_team_def_strength = phi_season_def
                                    * team_def_strength[last_w]
                                    + z0_def * sigma_season_def_innov;
      predicted_league_hfa = phi_league_hfa * league_hfa[last_s]
                             + normal_rng(0, sigma_league_hfa_innovation);
      
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
      predicted_team_hfa = team_hfa[last_s];
    }
    
    predicted_team_strength = predicted_team_off_strength
                              + predicted_team_def_strength;
  }
  
  matrix[N_games, 2] eta = compute_eta_home_away(home_team, away_team,
                             week_idx, season_idx, hfa, team_off_strength,
                             team_def_strength, team_hfa, alpha_log, N_games);
  
  // Per-game log-lik (sum of two NegBinomial-2 logs)
  vector[N_games] log_lik;
  for (g in 1 : N_games) 
    log_lik[g] = neg_binomial_2_log_lpmf(home_score[g] | eta[g, 1], phi_home)
                 + neg_binomial_2_log_lpmf(away_score[g] | eta[g, 2],
                     phi_away);
  
  // Integer predictive draws per game and derived outcomes
  // These provide a posterior distribution over scores/result/total/winner
  array[N_games] int sim_home_score;
  array[N_games] int sim_away_score;
  array[N_games] int sim_result;
  array[N_games] int sim_total;
  array[N_games] int sim_home_win;
  for (g in 1 : N_games) {
    int y_h = neg_binomial_2_log_rng(eta[g, 1], phi_home);
    int y_a = neg_binomial_2_log_rng(eta[g, 2], phi_away);
    sim_home_score[g] = y_h;
    sim_away_score[g] = y_a;
    sim_result[g] = y_h - y_a;
    sim_total[g] = y_h + y_a;
    sim_home_win[g] = (y_h > y_a);
  }
  
  // Expose per-game log-rates for R-side computation
  matrix[N_games, 2] eta_home_away = eta;
  
  // Expose per-game log-rates (kept for backward compatibility with prior week-shock output)
  matrix[N_games, 2] eta_plus_week_shock = eta;
}
