// Global-week state-space model (Glickman-style) - ENHANCED
// - Uses sum_to_zero_vector for efficient identifiability
// - Non-centered innovations for better sampling
// - Leverages provided global week_idx and season boundary indicators
// - ENHANCEMENTS: Function for mu computation, complete predictions, log-likelihood
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
      if (hfa[g] == 1) {
        mu[g] += team_hfa[s][h];
      }
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
      if (s > week_to_season[w]) {
        week_to_season[w] = s;
      }
    }
    return week_to_season;
  }
  
  array[] int compute_is_first_week(int N_weeks, int N_games,
                                    array[] int week_idx,
                                    array[] int fw_season_idx) {
    array[N_weeks] int is_first_week = rep_array(0, N_weeks);
    for (g in 1 : N_games) {
      if (fw_season_idx[g] == 1) {
        is_first_week[week_idx[g]] = 1;
      }
    }
    if (N_weeks > 0) {
      is_first_week[1] = 1;
    }
    return is_first_week;
  }
  
  array[] int compute_is_last_week(int N_weeks, int N_games,
                                   array[] int week_idx,
                                   array[] int lw_season_idx) {
    array[N_weeks] int is_last_week = rep_array(0, N_weeks);
    for (g in 1 : N_games) {
      if (lw_season_idx[g] == 1) {
        is_last_week[week_idx[g]] = 1;
      }
    }
    return is_last_week;
  }
}
data {
  int<lower=1> N_games;
  int<lower=2> N_teams; // number of teams = 32
  int<lower=1> N_seasons; // global season index starting at 1 (e.g. 2002)
  int<lower=1> N_weeks; // global week index starting at 1 (e.g. 2002 week 1)
  
  // Indexing variables
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx; // season index per game
  array[N_games] int<lower=1, upper=N_weeks> week_idx; // week index per game
  
  // Indicator variables for season transitions
  array[N_games] int<lower=0, upper=1> fw_season_idx; // first week of season (by game)
  array[N_games] int<lower=0, upper=1> lw_season_idx; // last week of season (by game)
  array[N_games] int<lower=0, upper=1> hfa; // true home-stadium indicator
  
  // Response variables (some unused here but kept as in your data)
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  array[N_games] int<lower=0> total;
  
  // Margin: home_score - away_score
  array[N_games] int result;
}
transformed data {
  // Map global weeks to seasons and mark season boundaries
  array[N_weeks] int week_to_season = compute_week_to_season(N_weeks,
                                        N_games, week_idx, season_idx);
  array[N_weeks] int is_first_week = compute_is_first_week(N_weeks, N_games,
                                       week_idx, fw_season_idx);
  array[N_weeks] int is_last_week = compute_is_last_week(N_weeks, N_games,
                                      week_idx, lw_season_idx);
}
parameters {
  // League HFA AR(1) across seasons
  real league_hfa_init; // initial league HFA at season 1
  vector[N_seasons - 1] z_league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  // CHANGED: unconstrained AR parameterization for better optimizer geometry
  real phi_league_hfa_raw;
  
  // Team HFA deviations per season (sum-to-zero around league HFA)
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation;
  real<lower=0> sigma_team_hfa;
  
  // Team strengths: initial
  sum_to_zero_vector[N_teams] z_team_strength_init;
  real<lower=0> sigma_team_strength_init;
  
  // Weekly innovations and season-carryover innovations (sum-to-zero)
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_team_strength_innovation;
  real<lower=0> sigma_weekly_team_strength_innovation;
  // CHANGED
  real phi_weekly_team_strength_innovation_raw;
  
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_team_strength_innovation;
  real<lower=0> sigma_season_team_strength_innovation;
  // CHANGED
  real phi_season_team_strength_innovation_raw;
  
  // Observation noise
  real<lower=0> sigma_obs;
}
transformed parameters {
  // CHANGED: map raw -> (0,1)
  real<lower=0, upper=1> phi_league_hfa = inv_logit(phi_league_hfa_raw);
  real<lower=0, upper=1> phi_weekly_team_strength_innovation = inv_logit(
                                                                    phi_weekly_team_strength_innovation_raw);
  real<lower=0, upper=1> phi_season_team_strength_innovation = inv_logit(
                                                                    phi_season_team_strength_innovation_raw);
  
  // League HFA
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + z_league_hfa_innovation[s - 1]
                      * sigma_league_hfa_innovation;
  }
  
  // Team HFA
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1 : N_seasons) {
    team_hfa[s] = league_hfa[s] + z_team_hfa_deviation[s] * sigma_team_hfa;
  }
  
  // Team strength
  array[N_weeks] vector[N_teams] team_strength;
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
  
  // CHANGED: tighten/regularize innovation SD priors (replaces heavy-tailed student_t)
  // Rationale: reduces funnel geometry and makes Laplace/Pathfinder/ADVI far more stable,
  // while still allowing reasonable variability in points.
  sigma_league_hfa_innovation ~ normal(0, 1); // half-normal due to <lower=0>
  
  // CHANGED: prior on raw scale; centered near your previous Beta(8,2) mean (~0.8)
  phi_league_hfa_raw ~ normal(logit(0.8), 0.7);
  
  for (s in 1 : N_seasons) 
    z_team_hfa_deviation[s] ~ std_normal();
  
  // CHANGED: tighten/regularize SD prior (replaces heavy-tailed student_t)
  sigma_team_hfa ~ normal(0, 1); // half-normal due to <lower=0>
  
  z_team_strength_init ~ std_normal();
  
  // CHANGED: tighten/regularize SD prior (replaces heavy-tailed student_t)
  sigma_team_strength_init ~ normal(0, 5); // half-normal; init strength can be wider
  
  for (w in 1 : (N_weeks - 1)) 
    z_weekly_team_strength_innovation[w] ~ std_normal();
  
  // CHANGED: tighten/regularize SD prior (replaces heavy-tailed student_t)
  sigma_weekly_team_strength_innovation ~ normal(0, 1); // half-normal
  
  // CHANGED: prior near your Beta(9,1) mean (~0.9)
  phi_weekly_team_strength_innovation_raw ~ normal(logit(0.9), 0.5);
  
  for (s in 1 : (N_seasons - 1)) 
    z_season_team_strength_innovation[s] ~ std_normal();
  
  // CHANGED: tighten/regularize SD prior (replaces heavy-tailed student_t)
  sigma_season_team_strength_innovation ~ normal(0, 3); // half-normal; season jumps can be larger
  
  // CHANGED: prior near your Beta(6,4) mean (~0.6)
  phi_season_team_strength_innovation_raw ~ normal(logit(0.6), 0.8);
  
  // CHANGED: tighten/regularize observation SD prior (replaces heavy-tailed student_t)
  // Use a broad half-normal; adjust scale if your margin SD is known.
  sigma_obs ~ normal(0, 10); // half-normal due to <lower=0>
  
  // Likelihood
  {
    vector[N_games] mu = compute_mu(home_team, away_team, week_idx,
                                    season_idx, hfa, team_strength, team_hfa,
                                    N_games);
    result ~ normal(mu, sigma_obs);
  }
}
generated quantities {
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  vector[N_teams] filtered_team_strength = team_strength[last_w];
  vector[N_teams] filtered_team_hfa = team_hfa[last_s];
  real filtered_league_hfa = league_hfa[last_s];
  
  vector[N_teams] predicted_team_strength;
  vector[N_teams] predicted_team_hfa;
  real predicted_league_hfa;
  
  {
    int next_is_first = is_last_week[last_w];
    
    // Team strength predictions
    vector[N_teams - 1] z_raw;
    for (t in 1 : (N_teams - 1)) 
      z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    
    if (next_is_first == 1) {
      predicted_team_strength = phi_season_team_strength_innovation
                                * team_strength[last_w]
                                + z0 * sigma_season_team_strength_innovation;
    } else {
      predicted_team_strength = phi_weekly_team_strength_innovation
                                * team_strength[last_w]
                                + z0 * sigma_weekly_team_strength_innovation;
    }
    
    // HFA predictions
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
  
  // Log-likelihood
  vector[N_games] log_lik;
  {
    vector[N_games] mu = compute_mu(home_team, away_team, week_idx,
                                    season_idx, hfa, team_strength, team_hfa,
                                    N_games);
    for (g in 1 : N_games) {
      log_lik[g] = normal_lpdf(result[g] | mu[g], sigma_obs);
    }
  }
}
