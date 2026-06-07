# footBayes 2.1.0

* Add Dixon-Coles model (`"dixon_coles"`) in `mle_foot()` with low-score dependence adjustment (rho parameter) following Dixon and Coles (1997).
* Add static Negative Binomial model (`"neg_bin"`) in `mle_foot()` with NB2 parameterization and separate home/away overdispersion parameters.
* Refactor MLE prediction logic: extract `simulate_goals_mle()` utility into `utils_foot.R`.
* Update `foot_prob()` to support `"dixon_coles"` and `"neg_bin"` MLE predictions.
* Update `foot_abilities()` documentation to reflect all six supported MLE models.
* Refactor profile likelihood and Wald confidence interval computation in `mle_foot()` to dynamically handle model-specific extra parameters.
* Fix AIC/BIC computation in `mle_foot()` to count only effective parameters per model.
* Fix `foot_prob()` referencing `object$home` instead of `object$home_effect` for MLE models.
* Updated vignette and documentation.

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
