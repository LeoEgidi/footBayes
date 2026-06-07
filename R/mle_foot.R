#' Fit football models with Maximum Likelihood
#'
#' Fits football goal-based models using maximum likelihood estimation.
#' Supported models include: double Poisson, bivariate Poisson, Dixon-Coles,
#' negative binomial, Skellam, and Student's t.
#'
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param model A character specifying the model to fit. Options are:
#'   \itemize{
#'     \item \code{"double_pois"}: Double Poisson model.
#'     \item \code{"biv_pois"}: Bivariate Poisson model.
#'     \item \code{"dixon_coles"}: Dixon-Coles model.
#'     \item \code{"neg_bin"}: Negative Binomial model.
#'     \item \code{"skellam"}: Skellam model.
#'     \item \code{"student_t"}: Student's t model.
#'     }
#' @param predict An integer specifying the number of out-of-sample matches for prediction. If missing, the function fits the model to the entire dataset without making predictions.
#'
#' @param maxit An integer specifying the maximum number of optimizer iterations default is 1000).
#' @param method A character specifying the optimization method. Options are
#'   \itemize{
#'     \item \code{"Nelder-Mead"}.
#'     \item \code{"BFGS"} (default).
#'     \item \code{"CG"}.
#'     \item \code{"L-BFGS-B"}.
#'     \item \code{"SANN"}.
#'     \item \code{"Brent"}.

#'   }
#' For further details see \code{{optim}} function in \code{\link[stats]{stats}} package.
#' @param interval A character specifying the interval type for confidence intervals. Options are
#'   \itemize{
#'     \item \code{"profile"} (default).
#'     \item \code{"Wald"}.
#'     }
#' @param hessian A logical value indicating to include the computation of the Hessian (default FALSE).
#' @param sigma_y A positive numeric value indicating the scale parameter for Student t likelihood (default 1).
#'
#' @return A named list containing:
#' \itemize{
#'   \item{\code{att}}: A matrix of attack ratings, with MLE and 95\% confidence intervals (for \code{"double_pois"}, \code{"biv_pois"}, \code{"dixon_coles"}, \code{"neg_bin"} and \code{"skellam"} models).
#'   \item{\code{def}}: A matrix of defence ratings, with MLE and 95\% confidence intervals (for \code{"double_pois"}, \code{"biv_pois"}, \code{"dixon_coles"}, \code{"neg_bin"} and \code{"skellam"} models).
#'   \item{\code{abilities}}: A matrix of combined ability, with MLE and 95\% confidence intervals (for \code{"student_t"} only).
#'   \item{\code{home_effect}}: A matrix with with MLE and 95\% confidence intervals for the home effect estimate.
#'   \item{\code{corr}}: A matrix with MLE and 95\% confidence intervals for the bivariate Poisson correlation parameter (for \code{"biv_pois"} only).
#'   \item{\code{rho}}: A matrix with MLE and 95\% confidence intervals for the Dixon-Coles dependence parameter (for \code{"dixon_coles"} only).
#'   \item{\code{overdispersion}}: A matrix with MLE and 95\% confidence intervals for the home and away overdispersion parameters (for \code{"neg_bin"} only).
#'   \item{\code{model}}: The name of the fitted model (character).
#'   \item{\code{predict}}: The number of out-of-sample matches used for prediction (integer).
#'   \item{\code{sigma_y}}: The scale parameter used in the Student t likelihood (for \code{"student_t"} only).
#'   \item{\code{team1_prev}}: Integer indices of home teams in the out-of-sample matches (if \code{predict > 0}).
#'   \item{\code{team2_prev}}: Integer indices of away teams in the out-of-sample matches (if \code{predict > 0}).
#'   \item{\code{logLik}}: The maximized log likelihood (numeric).
#'   \item{\code{aic}}: Akaike Information Criterion (numeric).
#'   \item{\code{bic}}: Bayesian Information Criterion (numeric).
#' }
#'
#' @details
#'
#' MLE can be obtained only for static models, with no time-dependence.
#' Likelihood optimization is performed via the \code{BFGS} method
#' of the \code{{optim}} function in \code{\link[stats]{stats}} package.
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}
#'
#' @references
#' Baio, G. and Blangiardo, M. (2010). Bayesian hierarchical model for the prediction of football
#' results. Journal of Applied Statistics 37(2), 253-264.
#'
#' Dixon, M. J. and Coles, S. G. (1997). Modelling association football scores and
#' inefficiencies in the football betting market. Journal of the Royal Statistical Society:
#' Series C (Applied Statistics) 46(2), 265-280.
#'
#' Egidi, L., Pauli, F., and Torelli, N. (2018). Combining historical data
#' and bookmakers' odds in modelling football scores. Statistical Modelling, 18(5-6), 436-459.
#'
#' Gelman, A. (2014). Stan goes to the World Cup. From
#' "Statistical Modeling, Causal Inference, and Social Science" blog.
#'
#' Karlis, D. and Ntzoufras, I. (2003). Analysis of sports data by using bivariate poisson models.
#' Journal of the Royal Statistical Society: Series D (The Statistician) 52(3), 381-393.
#'
#' Karlis, D. and Ntzoufras,I. (2009).  Bayesian modelling of football outcomes: Using
#' the Skellam's distribution for the goal difference. IMA Journal of Management Mathematics 20(2), 133-145.
#'
#' Owen, A. (2011). Dynamic Bayesian forecasting models
#' of football match outcomes with estimation of the
#' evolution variance parameter. IMA Journal of Management Mathematics, 22(2), 99-113.
#'
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data("italy")
#' italy <- as_tibble(italy)
#' italy_2000_2002 <- italy %>%
#'   dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'   dplyr::filter(Season == "2000" | Season == "2001" | Season == "2002")
#'
#' colnames(italy_2000_2002) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#' mle_fit <- mle_foot(
#'   data = italy_2000_2002,
#'   model = "double_pois"
#' )
#' }
#'
#' @importFrom stats optim setNames
#' @importFrom numDeriv hessian
#' @export
#'

mle_foot <- function(data,
                     model,
                     predict = 0,
                     maxit = 1000,
                     method = "BFGS",
                     interval = "profile",
                     hessian = FALSE,
                     sigma_y = 1) {
  #   ____________________________________________________________________________
  #   Data Checks                                                             ####

  if (!is.data.frame(data)) {
    stop("Input data must be a data.frame with columns: periods, home_team, away_team, home_goals, away_goals.")
  }


  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # checks about formats
  if (!is.numeric(data$home_goals) || !is.numeric(data$away_goals)) {
    stop("Goals are not numeric! Please, provide
         numeric values for the goals")
  }

  # check about columns
  if (dim(data)[2] > 5) {
    warning("Your dataset seems too large!
             The function will evaluate the first
             five columns as follows:
             periods, home_team, away_team, home_goals,
             away_goals")
  }


  #   ____________________________________________________________________________
  #   Tuning arguments checks                                               ####


  if (!(is.numeric(maxit) && length(maxit) == 1 && maxit >= 1 && maxit == as.integer(maxit))) {
    stop("`maxit` must be a single positive integer.")
  }

  method <- match.arg(method, c(
    "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
    "Brent"
  ))

  interval <- match.arg(interval, c("profile", "Wald"))

  if (!(is.logical(hessian) && length(hessian) == 1)) {
    stop("`hessian` must be a single TRUE or FALSE.")
  }

  if (!(is.numeric(sigma_y) && length(sigma_y) == 1 && sigma_y > 0)) {
    stop("`sigma_y` must be a single positive number.")
  }

  if (interval == "Wald") {
    hessian <- TRUE
    # stop("Select 'hessian=TRUE' to compute Wald intervals")
  }

  #   ____________________________________________________________________________
  #   Models' Name Checks                                                     ####

  model <- match.arg(model, c(
    "double_pois",
    "biv_pois",
    "skellam",
    "student_t",
    "dixon_coles",
    "neg_bin"
  ))



  #   ____________________________________________________________________________
  #   Predict checks                                                          ####

  if (!is.numeric(predict) || predict < 0 || predict %% 1 != 0) {
    stop("The argument 'predict' must be a non-negative integer.")
  }

  if (predict == 0) {
    predict <- 0
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  } else if (is.numeric(predict)) {
    N <- dim(data)[1] - predict
    N_prev <- predict
    type <- "prev"
  }

  if (predict >= dim(data)[1]) {
    stop("The training set size is zero!
            Please, select a lower value for the
            out-of-sample matches, through the
            argument predict.")
  }


  y1 <- data$home_goals[1:N]
  y2 <- data$away_goals[1:N]
  N <- length(y1)
  teams <- unique(data$home_team)
  nteams <- length(teams)
  team_home <- match(data$home_team, teams)
  team_away <- match(data$away_team, teams)
  team1 <- team_home[1:N]
  team2 <- team_away[1:N]
  # Out-of-sample team indices
  if (N_prev > 0) {
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  } else {
    team1_prev <- integer(0)
    team2_prev <- integer(0)
  }


  #   ____________________________________________________________________________
  #   Parameter specification                                                 ####

  # Teams are subject to a sum-to-zero identifiability constraint: the first
  # team is dropped from the optimisation and reconstructed afterwards as minus
  # the sum of the remaining ones
  free_teams <- teams[-1]

  # Team-level location parameters. Poisson-type models use separate attack and
  # defence ratings; the Student's t model is defined on the goal difference and
  # therefore admits a single ability rating per team
  if (model == "student_t") {
    loc_init <- setNames(rep(0, nteams - 1), paste0("ability.", free_teams))
  } else {
    loc_init <- c(
      setNames(rep(0, nteams - 1), paste0("att.", free_teams)),
      setNames(rep(0, nteams - 1), paste0("def.", free_teams))
    )
  }

  # Model-specific extra (scalar) parameters with their initial values. The
  # negative binomial dispersions are parameterised on the log scale
  extra_init <- switch(model,
    biv_pois    = c(const = 1),
    dixon_coles = c(rho = 0),
    neg_bin     = c(phi1 = log(10), phi2 = log(10)),
    numeric(0)
  )
  extra_par_names <- names(extra_init)

  # Full vector of free parameters passed to optim()
  init_par <- c(loc_init, home = 2, extra_init)


  #   ____________________________________________________________________________
  #   Model fitting                                                           ####

  # Bind the model-specific negative log-likelihood, capturing the team labels
  # (and the fixed scale for the Student's t model). The wrapper exposes the
  # uniform signature fn(parameters, y1, y2, team1, team2) expected by optim(),
  # numDeriv::hessian() and the profiling routine; the likelihood bodies and the
  # parameter-unpacking helpers live in utils_foot.R.
  fn <- switch(model,
    double_pois = function(parameters, y1, y2, team1, team2) mle_double_pois_lik(parameters, y1, y2, team1, team2, teams),
    biv_pois    = function(parameters, y1, y2, team1, team2) mle_biv_pois_lik(parameters, y1, y2, team1, team2, teams),
    skellam     = function(parameters, y1, y2, team1, team2) mle_skellam_lik(parameters, y1, y2, team1, team2, teams),
    student_t   = function(parameters, y1, y2, team1, team2) mle_student_t_lik(parameters, y1, y2, team1, team2, teams, sigma_y),
    dixon_coles = function(parameters, y1, y2, team1, team2) mle_dixon_coles_lik(parameters, y1, y2, team1, team2, teams),
    neg_bin     = function(parameters, y1, y2, team1, team2) mle_neg_bin_lik(parameters, y1, y2, team1, team2, teams)
  )
  mle_fit <- optim(
    par = init_par, fn = fn,
    team1 = team1, team2 = team2, y1 = y1, y2 = y2,
    method = method, hessian = hessian, control = list(maxit = maxit)
  )
  par_hat <- mle_fit$par
  par_names <- names(par_hat)
  mle_value <- -fn(par_hat, team1 = team1, team2 = team2, y1 = y1, y2 = y2)


  #   ____________________________________________________________________________
  #   Confidence intervals                                                    ####

  ci <- matrix(NA_real_, length(par_hat), 2,
    dimnames = list(par_names, c("lower", "upper"))
  )

  if (interval == "profile") {
    # "profile" option (default): conditional likelihood intervals — each
    # parameter is varied with the others held fixed at their MLE. This is
    # chosen for computational efficiency (no re-optimisation of the nuisance
    # parameters) and yields intervals narrower than a full profile likelihood;
    # use "Wald" when the correlation among parameters matters.
    conditional_interval <- function(j) {
      mle_conditional_interval(j, par_hat, fn, mle_value, team1, team2, y1, y2)
    }
    for (j in seq_along(par_hat)) {
      ci[j, ] <- conditional_interval(j)
    }
  } else if (interval == "Wald") {
    # Wald intervals from the inverse of the full observed information matrix.
    # Because the parameter vector contains only the parameters that the model
    # actually estimates, the information matrix is non-singular and retains the
    # cross-correlations among parameters; the standard errors are the square
    # roots of the diagonal of its inverse.
    H <- hessian(func = fn, x = par_hat, team1 = team1, team2 = team2, y1 = y1, y2 = y2)
    se <- sqrt(diag(solve(H)))
    ci[, 1] <- par_hat - 1.96 * se
    ci[, 2] <- par_hat + 1.96 * se
  }


  #   ____________________________________________________________________________
  #   Output tables                                                           ####

  # Thin wrappers that bind the local fit state (par_hat, par_names, ci, teams)
  # and delegate to the table builders defined in utils_foot.R.
  loc_table <- function(prefix, digits = 2) {
    mle_loc_table(prefix, par_hat, par_names, ci, teams, digits)
  }
  scalar_table <- function(name, transform = identity, digits = 2, rows = NULL) {
    mle_scalar_table(name, par_hat, ci, transform, digits, rows)
  }

  home_est <- scalar_table("home", digits = 2)


  #   ____________________________________________________________________________
  #   AIC and BIC                                                             ####

  logLik <- -mle_fit$value
  # The number of effective parameters is exactly the length of the estimated
  # parameter vector.
  k <- length(par_hat)
  AIC <- 2 * k - 2 * logLik
  BIC <- k * log(N) - 2 * logLik


  #   ____________________________________________________________________________
  #   Output                                                                  ####

  if (model == "student_t") {
    return(list(
      abilities = loc_table("ability"),
      home_effect = home_est,
      model = model,
      predict = predict,
      sigma_y = sigma_y,
      team1_prev = team1_prev,
      team2_prev = team2_prev,
      logLik = logLik,
      aic = AIC,
      bic = BIC
    ))
  } else if (model == "biv_pois") {
    corr_est <- scalar_table("const", transform = exp, digits = 2)
    if (corr_est[1, 2] == 0) corr_est[1, c(1, 3)] <- 0
    return(list(
      att = loc_table("att"),
      def = loc_table("def"),
      home_effect = home_est,
      corr = corr_est,
      model = model,
      predict = predict,
      team1_prev = team1_prev,
      team2_prev = team2_prev,
      logLik = logLik,
      aic = AIC,
      bic = BIC
    ))
  } else if (model == "dixon_coles") {
    return(list(
      att = loc_table("att"),
      def = loc_table("def"),
      home_effect = home_est,
      rho = scalar_table("rho", digits = 4),
      model = model,
      predict = predict,
      team1_prev = team1_prev,
      team2_prev = team2_prev,
      logLik = logLik,
      aic = AIC,
      bic = BIC
    ))
  } else if (model == "neg_bin") {
    overdispersion_est <- rbind(
      scalar_table("phi1", transform = exp, digits = 4),
      scalar_table("phi2", transform = exp, digits = 4)
    )
    rownames(overdispersion_est) <- c("phi1 (home)", "phi2 (away)")
    return(list(
      att = loc_table("att"),
      def = loc_table("def"),
      home_effect = home_est,
      overdispersion = overdispersion_est,
      model = model,
      predict = predict,
      team1_prev = team1_prev,
      team2_prev = team2_prev,
      logLik = logLik,
      aic = AIC,
      bic = BIC
    ))
  } else {
    return(list(
      att = loc_table("att"),
      def = loc_table("def"),
      home_effect = home_est,
      model = model,
      predict = predict,
      team1_prev = team1_prev,
      team2_prev = team2_prev,
      logLik = logLik,
      aic = AIC,
      bic = BIC
    ))
  }
}
