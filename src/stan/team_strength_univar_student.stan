// Global-week team-strength model with Student-t likelihood on result
functions {
  vector compute_mu(array[] int home_team, array[] int away_team,
                    array[] int week_idx, array[] int season_idx,
                    array[] int hfa, array[] vector team_strength,
                    array[] vector team_hfa, int N_games) {
    vector[N_games] mu;
    for (g in 1 : N_games) {
      int w = week_idx[g];
      int s = season_idx[g];
      int h = home_team[g];
      int a = away_team[g];
      mu[g] = team_strength[w][h] - team_strength[w][a];
      if (hfa[g] == 1) 
        mu[g] += team_hfa[s][h];
    }
    return mu;
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
  array[N_games] int<lower=0> total;
  array[N_games] int result;
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
  // League HFA AR(1)
  real league_hfa_init;
  vector[N_seasons - 1] z_league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;
  
  // Team HFA deviations per season
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation;
  real<lower=0> sigma_team_hfa;
  
  // Team strengths
  sum_to_zero_vector[N_teams] z_team_strength_init;
  real<lower=0> sigma_team_strength_init;
  
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_team_strength_innovation;
  real<lower=0> sigma_weekly_team_strength_innovation;
  real<lower=0, upper=1> phi_weekly_team_strength_innovation;
  
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_team_strength_innovation;
  real<lower=0> sigma_season_team_strength_innovation;
  real<lower=0, upper=1> phi_season_team_strength_innovation;
  
  // Student-t observation noise
  real<lower=2> nu_obs;
  real<lower=0> sigma_obs;
}
transformed parameters {
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;
  array[N_weeks] vector[N_teams] team_strength;
  
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) 
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + z_league_hfa_innovation[s - 1]
                      * sigma_league_hfa_innovation;
  
  for (s in 1 : N_seasons) 
    team_hfa[s] = league_hfa[s] + z_team_hfa_deviation[s] * sigma_team_hfa;
  
  team_strength[1] = z_team_strength_init * sigma_team_strength_init;
  for (w in 2 : N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_strength[w] = phi_season_team_strength_innovation
                         * team_strength[w - 1]
                         + z_season_team_strength_innovation[s - 1]
                           * sigma_season_team_strength_innovation;
    } else {
      team_strength[w] = phi_weekly_team_strength_innovation
                         * team_strength[w - 1]
                         + z_weekly_team_strength_innovation[w - 1]
                           * sigma_weekly_team_strength_innovation;
    }
  }
}
model {
  // Priors
  league_hfa_init ~ normal(3, 2);
  z_league_hfa_innovation ~ std_normal();
  sigma_league_hfa_innovation ~ student_t(3, 0, 2);
  phi_league_hfa ~ beta(8, 2);
  
  for (s in 1 : N_seasons) 
    z_team_hfa_deviation[s] ~ std_normal();
  sigma_team_hfa ~ student_t(3, 0, 2);
  
  z_team_strength_init ~ std_normal();
  sigma_team_strength_init ~ student_t(3, 0, 5);
  
  for (w in 1 : (N_weeks - 1)) 
    z_weekly_team_strength_innovation[w] ~ std_normal();
  sigma_weekly_team_strength_innovation ~ student_t(3, 0, 2);
  phi_weekly_team_strength_innovation ~ beta(9, 1);
  
  for (s in 1 : (N_seasons - 1)) 
    z_season_team_strength_innovation[s] ~ std_normal();
  sigma_season_team_strength_innovation ~ student_t(3, 0, 5);
  phi_season_team_strength_innovation ~ beta(6, 4);
  
  nu_obs ~ gamma(2, 0.1);
  sigma_obs ~ student_t(3, 0, 10);
  
  // Likelihood
  {
    vector[N_games] mu = compute_mu(home_team, away_team, week_idx,
                                    season_idx, hfa, team_strength, team_hfa,
                                    N_games);
    result ~ student_t(nu_obs, mu, sigma_obs);
  }
}
generated quantities {
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  vector[N_teams] filtered_team_strength = team_strength[last_w];
  vector[N_teams] filtered_team_hfa = team_hfa[last_s];
  real filtered_league_hfa = league_hfa[last_s];
  
  // One-step predictive snapshots (same as your normal version)
  vector[N_teams] predicted_team_strength;
  vector[N_teams] predicted_team_hfa;
  real predicted_league_hfa;
  
  {
    int next_is_first = is_last_week[last_w];
    vector[N_teams - 1] z_raw;
    for (t in 1 : (N_teams - 1)) 
      z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    
    if (next_is_first == 1) 
      predicted_team_strength = phi_season_team_strength_innovation
                                * team_strength[last_w]
                                + z0 * sigma_season_team_strength_innovation;
    else 
      predicted_team_strength = phi_weekly_team_strength_innovation
                                * team_strength[last_w]
                                + z0 * sigma_weekly_team_strength_innovation;
    
    if (next_is_first == 1) {
      predicted_league_hfa = phi_league_hfa * league_hfa[last_s]
                             + normal_rng(0, sigma_league_hfa_innovation);
      vector[N_teams - 1] hfa_raw;
      for (t in 1 : (N_teams - 1)) 
        hfa_raw[t] = normal_rng(0, 1);
      vector[N_teams] hfa_dev = sum_to_zero_constrain(hfa_raw);
      predicted_team_hfa = predicted_league_hfa + hfa_dev * sigma_team_hfa;
    } else {
      predicted_league_hfa = league_hfa[last_s];
      predicted_team_hfa = team_hfa[last_s];
    }
  }
  
  vector[N_games] log_lik;
  {
    vector[N_games] mu = compute_mu(home_team, away_team, week_idx,
                                    season_idx, hfa, team_strength, team_hfa,
                                    N_games);
    for (g in 1 : N_games) 
      log_lik[g] = student_t_lpdf(result[g] | nu_obs, mu[g], sigma_obs);
  }
}
