# NFL Bayesian State-Space Model — Master Reference Document (v1.0)
## Comprehensive Review, Model Diagnostics, Upgrade Path, and Stan Best Practices

---

# Table of Contents
- [1. Overview](#1-overview)
- [2. Current Model Structure](#2-current-model-structure)
- [3. Identified Issues in Model Fit (`team_strength_fit.stan`)](#3-identified-issues-in-model-fit-mod5stan)
- [4. Improvements — Statistical Modeling](#4-improvements--statistical-modeling)
- [5. Improvements — Computational Performance](#5-improvements--computational-performance)
- [6. Improvements — Stan Coding Best Practices](#6-improvements--stan-coding-best-practices)
- [7. Improvements — Future Extensions](#7-improvements--future-extensions)
- [8. Standalone Generated Quantities Model (`team_strength_gq.stan`)](#8-standalone-generated-quantities-model-mod5_gqstan)
- [9. Summary of Critical Fixes](#9-summary-of-critical-fixes)
- [10. Modular Sections for Future Chats](#10-modular-sections-for-future-chats)

---


# 1. Overview
This reference document summarizes the full review and recommended improvements for your **Bayesian NFL team-strength state-space model**, including:

- **AR(1) weekly + AR(1) seasonal evolution**
- **Team-specific HFA on top of league-wide HFA**
- **Sum-to-zero identifiability constraints**
- **Standalone forecasting (`generate_quantities`) model**

This document will remain your **central blueprint** for refining and extending the model in focused, separate conversations.

---


# 2. Current Model Structure

### **Latent States**
- Weekly team strengths (`team_strength[w]`)
- Season-level team HFA (`team_hfa[s]`)
- AR(1) evolution for:
  - league-HFA
  - team-specific HFA
  - team strength (weekly + seasonal)

### **Observation Model**
```stan
result ~ normal(mu, sigma_obs);
```

### **Forecasting**
- Forward-simulation of latent states
- Multi-week and multi-season forecasting
- Out-of-sample predictions via `oos_*`

---


# 3. Identified Issues in Model Fit (`team_strength_fit.stan`)

### Structural Issues
- Models only score differential, losing information
- HFA fixed within season
- Bye weeks can break inferred week→season mapping

### Priors
- Heavy-tailed scale priors can cause funnels
- AR(1) priors overly persistent

### Computational
- `compute_mu` in transformed parameters heavily recomputed
- No parallelization
- Under-vectorized transitions

---


# 4. Improvements — Statistical Modeling

## 4.1 Joint Score Modeling
Model home and away scores jointly with a bivariate normal.

## 4.2 Improved Priors
Replace heavy-tailed priors with half-normal and milder Beta AR(1) priors.

## 4.3 Hierarchical HFA
Use hierarchical league-average HFA per season.

## 4.4 Explicit Season Transition Metadata
Fix bye week misalignment by passing correct mapping from R.

---


# 5. Improvements — Computational Performance

## 5.1 Parallelization
Use `reduce_sum` for likelihood parallelization.

## 5.2 Move mu Calculation
Compute mu inside the model block, not transformed parameters.

## 5.3 Vectorize Transitions
Rewrite AR(1) recursions to reduce loops.

---


# 6. Improvements — Stan Coding Best Practices

- Pass week/season metadata from R
- Remove unused variables
- Modularize evolution functions

---


# 7. Improvements — Future Extensions

## 7.1 Kalman Filter Version
Major speed improvements with exact Gaussian filtering.

## 7.2 Marginalize Latent States
Integrate out latent states for highest efficiency.

## 7.3 Dynamic HFA
Model weekly HFA variation.

## 7.4 ML + Bayesian Hybrid
Use XGBoost predictions as innovations for improved spread prediction.

---


# 8. Standalone Generated Quantities Model

The GQ model:

- Correctly forward-simulates latent states
- Supports multi-season forecasting
- Uses sum-to-zero innovations
- Correctly mirrors fitted model structure

Ensure week→season metadata is correct for alignment.

---


# 9. Summary of Critical Fixes

## Statistical
1. Joint score modeling  
2. Improved priors  
3. Fixed season transitions  
4. Optional hierarchical HFA  
5. Correct bye-week handling  

## Computational
1. Parallelization  
2. Move mu out of transformed parameters  
3. Vectorization  
4. Remove unused computation  
5. More modular code  

---


# 10. Modular Sections for Future Chats

Use these modules in new chats:

- **Module A — Joint Score Modeling**
- **Module B — Priors & AR Structure**
- **Module C — Parallelization**
- **Module D — Week/Season Mapping**
- **Module E — Kalman Filter Version**
- **Module F — Dynamic HFA**
- **Module G — Marginalization**

---

