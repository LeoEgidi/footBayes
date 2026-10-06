# footBayes 2.1.0

### New models

* Add the Dixon-Coles model (`"dixon_coles"`) to `stan_foot()`, static and dynamic, and to `mle_foot()`.
* Add the negative binomial model (`"neg_bin"`) to `mle_foot()`.

### Dynamic models in `stan_foot()`

* Add `dynamic_par` to choose the evolution variance: a common variance (`common_sd`, Owen, 2011) or a variance inflation after the summer break (`kl_variance` and `periods_per_season`, Koopman and Lit, 2015).
* Add `dynamic_weight` for the weighted dynamic models with commensurate priors (Macrì Demartino, Egidi and Torelli, 2026), with spike and slab hyperpriors set through `dynamic_par$spike` and `dynamic_par$slab`.

### Other changes

* `print.stanFoot()` with `teams` keeps all the global parameters and shows the team names in the team-indexed parameters.
* `foot_prob()` supports the `"dixon_coles"` and `"neg_bin"` MLE models.
* Add an identifiability constraint to the Bradley-Terry-Davidson Stan models.
* Extend the confidence intervals of `mle_foot()` to the model-specific parameters and move the MLE simulations to the internal `simulate_goals_mle()`.
* Rewrite the vignette.

### Bug fixes

* Fix the likelihood of the static bivariate Poisson model.
* Draw `y_rep` and `y_prev` of the bivariate Poisson models from the bivariate Poisson distribution.
* Rewrite the diagonal-inflated bivariate Poisson model as in Karlis and Ntzoufras (2003), with the new parameter `draw_dist` for the draws.
* Return `diff_y_prev` in the bivariate Poisson and diagonal-inflated bivariate Poisson models.
* Fix the AIC and BIC of `mle_foot()` to count only the free parameters of each model.
* Fix `foot_prob()` for MLE models, which used `object$home` instead of `object$home_effect`.
* Fix `compare_foot()` with `NA` rows in a probability matrix, which misaligned the outcomes of the following elements of `source`.
* Match `ranking` in `stan_foot()` to the teams by name instead of by position, with informative errors for missing or duplicated teams.
* Fix the observed frequencies in `pp_foot(type = "aggregated")` for models fitted with `predict > 0`.

### Documentation

* Update `foot_abilities()` for the six MLE models.
* Fix the documentation of `italy`, `priors`, `btd_foot()`, `compare_foot()`, `plot_logStrength()` and of `norm_method` and `rho` in `stan_foot()`.

# footBayes 2.0.1

* Updated vignette.
* Correct typo in foot_compare() description.
* Correct typo in pp_foot() description.

# footBayes 2.0.0

* Migration from `rstan` interface to `CmdStanR` interface.
* Add support for the `VI`, `pathfinder` and `laplace` algorithms.
* Add pre-compiled CmdStan models using the package `instantiate`.
* Add AIC and BIC output elements in `mle_foot()`.
* Add Bayesian static/dynamic Negative Binomial model in `stan_foot()`.
* Minor `ggplot2` edits on `foot_rank()`, `foot_abilities()` and `pp_foot()`.
* Updated vignette.

# footBayes 1.0.0

* Bradley-Terry model for abilities.
* Dynamic ranking in the models.
* Updated vignette.
* Probabilistic predictive performance (pseudo-R squared, Brier score, etc.).

# footBayes 0.2.0

* Inclusion of diagonal-inflated bivariate Poisson and zero-inflated Skellam models.
* Minor edits and drop engsoccerdata dependence.
* Updated vignette.

# footBayes 0.1.0

* First submission to CRAN.
