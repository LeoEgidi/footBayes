# footBayes 2.1.0

## New models

* Add the Dixon-Coles model (`"dixon_coles"`) in `stan_foot()`, both static and dynamic, with the low-score dependence parameter `rho` following Dixon and Coles (1997).
* Add the Dixon-Coles model (`"dixon_coles"`) in `mle_foot()` with low-score dependence adjustment (rho parameter).
* Add the static Negative Binomial model (`"neg_bin"`) in `mle_foot()` with NB2 parameterization and separate home/away overdispersion parameters.

## New dynamic specifications in `stan_foot()`

* Add the `dynamic_par` argument to choose the evolution variance of the dynamic models:
  `common_sd = TRUE` for a single evolution standard deviation shared by attack and defence (Owen, 2011);
  `kl_variance = TRUE` for the variance inflation in the periods following a summer break (Koopman and Lit, 2015), with the new option `periods_per_season` (default 2) declaring how many consecutive periods form one season.
* Add the `dynamic_weight` argument for the Bayesian weighted dynamic models with team- and period-specific commensurate priors and spike-and-slab hyperpriors (Macrì Demartino, Egidi and Torelli, 2026, JRSS-C). The spike and slab hyperparameters are set through `dynamic_par$spike` and `dynamic_par$slab`, with defaults `normal(9, 1.5)` and `normal(0, 3)` as in the paper.
* Incompatible combinations (`dynamic_weight`, `common_sd`, `kl_variance`, and the `student_t` model) now stop with an informative error. `kl_variance = TRUE` requires `dynamic_type = "seasonal"` and more than `periods_per_season` training periods.
* Dynamic Stan models rewritten with a non-centered parameterization for the commensurate prior.

## Other changes

* `print.stanFoot()` with the `teams` argument now keeps all the global parameters (e.g. `sigma_common`, `sigma_break`, `nu`, `phi`, `prob_spike`) instead of a fixed list, and replaces the team index with the team name in all the team-indexed parameters.
* Refactor MLE prediction logic: extract `simulate_goals_mle()` utility into `utils_foot.R`.
* Update `foot_prob()` to support `"dixon_coles"` and `"neg_bin"` MLE predictions.
* Update `foot_abilities()` documentation to reflect all six supported MLE models.
* Refactor profile likelihood and Wald confidence interval computation in `mle_foot()` to dynamically handle model-specific extra parameters.
* Fix AIC/BIC computation in `mle_foot()` to count only effective parameters per model.
* Fix `foot_prob()` referencing `object$home` instead of `object$home_effect` for MLE models.
* Identifiability constraint in the Bradley-Terry-Davidson Stan models.
* The experimental dynamic Conway-Maxwell-Poisson Stan model is kept in the sources but it is not exposed through `stan_foot()`.
* Vignette rewritten: single running example, new section on the dynamic specifications, model comparison with `compare_foot()` and `loo`, and a fix in the `compare_foot()` example (the test set now matches the fitted seasons).

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
