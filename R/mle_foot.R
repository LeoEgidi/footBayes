#' Fit football models with Maximum Likelihood
#'
#' Fits football goal-based models using maximum likelihood estimation.
#' Supported models include: double Poisson, bivariate Poisson, Skellam, and Student's t.
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
#'     \item \code{"skellam"}: Skellam model.
#'     \item \code{"student_t"}: Student's t model.
#'     }
#' @param predict An integer specifying the number of out-of-sample matches for prediction. If missing, the function fits the model to the entire dataset without making predictions.
#'
#' @param maxit An integer specifying the maximum number of optimizer iterations  default is 1000).
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
#'   \item{\code{att}}: A matrix of attack ratings, with MLE and 95\% confidence intervals (for \code{"double_pois"}, \code{"biv_pois"} and \code{"skellam"} models).
#'   \item{\code{def}}: A matrix of defence ratings, with MLE and 95\% confidence intervals (for \code{"double_pois"}, \code{"biv_pois"} and \code{"skellam"} models).
#'   \item{\code{abilities}}: A matrix of combined ability, with MLE and 95\% confidence intervals (for \code{"student_t"} only).
#'   \item{\code{home_effect}}: A matrix with with MLE and 95\% confidence intervals for the home effect estimate.
#'   \item{\code{corr}}: A matrix with MLE and 95\% confidence intervals for the bivariate Poisson correlation parameter (for \code{"biv_pois"} only).
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
#' @importFrom extraDistr dbvpois dskellam
#' @importFrom metRology dt.scaled
#' @importFrom numDeriv hessian
#' @importFrom magrittr "%>%"
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
    "student_t"
  ))



  #   ____________________________________________________________________________
  #   Predict checks                                                          ####

  # if (missing(predict)) { # check on predict
  #   predict <- 0
  #   N <- dim(data)[1]
  #   N_prev <- 0
  #   type <- "fit"
  # } else if (predict == 0) {
  #   predict <- 0
  #   N <- dim(data)[1]
  #   N_prev <- 0
  #   type <- "fit"
  # } else if (is.numeric(predict)) {
  #   if (predict %% 1 != 0) {
  #     warning("Please, use integer numbers for the argument 'predict'!
  #             The input has been rounded to the closes integer number.")
  #     predict <- round(predict)
  #   }
  #   N <- dim(data)[1] - predict
  #   N_prev <- predict
  #   type <- "prev"
  # } else if (!is.numeric(predict)) {
  #   stop("The number of out-of-sample matches is ill posed!
  #        Pick up an integer number.")
  # }

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
  team1_prev <- team_home[(N + 1):(N + N_prev)]
  team2_prev <- team_away[(N + 1):(N + N_prev)]

  # optim requires parameters to be supplied as a vector
  # we'll unlist the parameters then relist in the function
  relist_params <- function(parameters) {
    parameter_list <- list(
      # att = attack rating
      att = parameters %>%
        .[grepl("att", names(.))] %>%
        append(prod(sum(.), -1), .) %>% # sum-to-zero constraints
        `names<-`(teams),
      # def = defence rating
      def = parameters %>%
        .[grepl("def", names(.))] %>%
        append(prod(sum(.), -1), .) %>% # sum-to-zero constraints
        `names<-`(teams),
      # home = home field advantage
      home = parameters["home"],
      # const = correl. parameter (biv pois)
      const = parameters["const"],
      # ability = team abilities (student_t)
      # ability = parameters %>%
      #   .[grepl("ability", names(.))] %>%
      #   append(prod(sum(.), -1), .) %>%  # sum-to-zero constraints
      #   `names<-`(teams),
      # sigma_y = student_t sd
      sigma_y = parameters["sigma_y"]
    )

    return(parameter_list)
  }


  #   ____________________________________________________________________________
  #   Likelihood functions                                                    ####

  # double poisson
  double_pois_lik <- function(parameters, y1, y2, team1, team2) {
    param_list <- relist_params(parameters)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home

    theta_1 <- exp(home + att[team1] + def[team2])
    theta_2 <- exp(att[team2] + def[team1])
    home_log_lik <- dpois(y1, lambda = theta_1, log = TRUE)
    away_log_lik <- dpois(y2, lambda = theta_2, log = TRUE)
    return(-sum(home_log_lik + away_log_lik))
  }

  # bivariate poisson
  biv_pois_lik <- function(parameters, y1, y2, team1, team2) {
    param_list <- relist_params(parameters)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home
    const <- param_list$const

    theta_1 <- exp(home + att[team1] + def[team2])
    theta_2 <- exp(att[team2] + def[team1])
    theta_3 <- exp(const)
    log_lik <- dbvpois(y1, y2,
      a = theta_1,
      b = theta_2, c = theta_3,
      log = TRUE
    )

    return(-sum(log_lik))
  }

  # skellam
  skellam_lik <- function(parameters, y1, y2, team1, team2) {
    param_list <- relist_params(parameters)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home

    theta_1 <- exp(home + att[team1] + def[team2])
    theta_2 <- exp(att[team2] + def[team1])
    log_lik <- dskellam(y1 - y2,
      mu1 = theta_1,
      mu2 = theta_2, log = TRUE
    )

    return(-sum(log_lik))
  }

  # student t
  student_t_lik <- function(parameters, y1, y2, team1, team2) {
    param_list <- relist_params(parameters)
    ability <- param_list$att + param_list$def
    home <- param_list$home
    sigma_y <- as.numeric(param_list$sigma_y)

    log_lik <- dt.scaled(
      x = y1 - y2, df = 7,
      mean = home + ability[team1] - ability[team2],
      sd = sigma_y,
      log = TRUE
    )

    return(-sum(log_lik))
  }


  #   ____________________________________________________________________________
  #   Model fitting                                                           ####

  # parameters initialization
  # (remove the first team from the attack and defence ratings)
  equal_parameters <- list(
    att = rep(0, length(teams) - 1) %>% `names<-`(teams[2:length(teams)]),
    def = rep(0, length(teams) - 1) %>% `names<-`(teams[2:length(teams)]),
    home = 2,
    const = 1, # for bivariate poisson
    sigma_y = sigma_y # for student_t
  )


  # MLE fit

  likelihoods <- list(
    double_pois = double_pois_lik,
    biv_pois    = biv_pois_lik,
    skellam     = skellam_lik,
    student_t   = student_t_lik
  )

  mle_fit <- optim(
    par = unlist(equal_parameters),
    fn = likelihoods[[model]],
    team1 = team1, team2 = team2,
    y1 = y1, y2 = y2,
    method = method,
    hessian = hessian,
    control = list(maxit = maxit)
  )

  # Compute likelihood confidence intervals
  fn <- likelihoods[[model]]
  mle_value <- -fn(mle_fit$par,
    team1 = team1,
    team2 = team2,
    y1 = y1, y2 = y2
  )

  ci <- matrix(NA, (2 * nteams), 2)
  # Profile likelihood intervals (default)
  if (interval == "profile") {
    index <- function(j) {
      profile <- function(x) {
        parameters <- mle_fit$par
        parameters[j] <- x
        return(-fn(parameters,
          team1 = team1,
          team2 = team2,
          y1 = y1, y2 = y2
        ))
      }
      # defining likelihood inverse for the profile likelihood ci's
      profile <- Vectorize(profile, "x")
      h <- mle_value - pchisq(0.95, 1) / 2
      # curve(profile(x), -1,1)
      # abline(h = h , col="red")
      x <- seq(-5, 5, 0.01)
      f_v <- profile(x)
      return(c(min(x[f_v >= h]), max(x[f_v >= h])))
    }

    ci_out <- sapply(c(1:(2 * nteams)), index)
    ci <- t(ci_out)

    # Wald-type intervals (only if hessian = TRUE)
  } else if (interval == "Wald") {
    ci[1:(2 * nteams - 2), 1] <- round(mle_fit$par[1:(2 * nteams - 2)] - 1.96 * sqrt(diag(solve(mle_fit$hessian[1:(2 * nteams - 2), 1:(2 * nteams - 2)]))), 2)
    ci[1:(2 * nteams - 2), 2] <- round(mle_fit$par[1:(2 * nteams - 2)] + 1.96 * sqrt(diag(solve(mle_fit$hessian[1:(2 * nteams - 2), 1:(2 * nteams - 2)]))), 2)
    ci[2 * nteams - 1, 1] <- round(mle_fit$par[2 * nteams - 1] - 1.96 * sqrt(solve(mle_fit$hessian[2 * nteams - 1, 2 * nteams - 1])), 2)
    ci[2 * nteams - 1, 2] <- round(mle_fit$par[2 * nteams - 1] + 1.96 * sqrt(solve(mle_fit$hessian[2 * nteams - 1, 2 * nteams - 1])), 2)
    if (model == "biv_pois") {
      hessian <- hessian(
        func = fn, x = unlist(equal_parameters), team1 = team1, team2 = team2,
        y1 = y1, y2 = y2
      )
      # ci[2*nteams, 1] <- mle_fit$par[2*nteams]-1.96*sqrt(solve(mle_fit$hessian[2*nteams, 2*nteams]))
      # ci[2*nteams, 2] <- mle_fit$par[2*nteams]+1.96*sqrt(solve(mle_fit$hessian[2*nteams, 2*nteams]))
      ci[2 * nteams, 1] <- mle_fit$par[2 * nteams] - 1.96 * sqrt(solve(hessian[2 * nteams, 2 * nteams]))
      ci[2 * nteams, 2] <- mle_fit$par[2 * nteams] + 1.96 * sqrt(solve(hessian[2 * nteams, 2 * nteams]))
    }
  }

  # Extract parameters and reparametrization for the first team
  att <- c(
    -sum(as.vector(mle_fit$par %>%
      .[grepl("att", names(.))])),
    as.vector(mle_fit$par %>%
      .[grepl("att", names(.))])
  )
  def <- c(
    -sum(as.vector(mle_fit$par %>%
      .[grepl("def", names(.))])),
    as.vector(mle_fit$par %>%
      .[grepl("def", names(.))])
  )
  home <- as.numeric(mle_fit$par %>%
    .[grepl("home", names(.))])
  corr_par <- round(exp(as.numeric(mle_fit$par %>%
    .[grepl("const", names(.))])), 2)
  abilities <- c(
    -sum(as.vector(mle_fit$par %>%
      .[grepl("att", names(.))]) +
      as.vector(mle_fit$par %>%
        .[grepl("def", names(.))])),
    as.vector(mle_fit$par %>%
      .[grepl("att", names(.))]) +
      as.vector(mle_fit$par %>%
        .[grepl("def", names(.))])
  )
  # Final tables
  att_est <- def_est <- abilities_est <- matrix(NA, nteams, 3)
  home_est <- corr_est <- matrix(NA, 1, 3)

  att_est[1, 1] <- round(att[1], 2)
  att_est[1, 2] <- round(att[1], 2)
  att_est[1, 3] <- round(att[1], 2)
  def_est[1, 1] <- round(def[1], 2)
  def_est[1, 2] <- round(def[1], 2)
  def_est[1, 3] <- round(def[1], 2)
  abilities_est[1, 1] <- round(abilities[1], 2)
  abilities_est[1, 2] <- round(abilities[1], 2)
  abilities_est[1, 3] <- round(abilities[1], 2)
  att_est[2:nteams, 1] <- ci[1:(nteams - 1), 1]
  att_est[2:nteams, 2] <- round(att[2:nteams], 2)
  att_est[2:nteams, 3] <- ci[1:(nteams - 1), 2]
  def_est[2:nteams, 1] <- ci[(nteams):(2 * nteams - 2), 1]
  def_est[2:nteams, 2] <- round(def[2:nteams], 2)
  def_est[2:nteams, 3] <- ci[(nteams):(2 * nteams - 2), 2]
  abilities_est[2:nteams, 1] <- ci[1:(nteams - 1), 1] + ci[(nteams):(2 * nteams - 2), 1]
  abilities_est[2:nteams, 2] <- round(abilities[2:nteams], 2)
  abilities_est[2:nteams, 3] <- ci[1:(nteams - 1), 2] + ci[(nteams):(2 * nteams - 2), 2]
  home_est[1, 2] <- round(home, 2)
  home_est[1, 1] <- ci[2 * nteams - 1, 1]
  home_est[1, 3] <- ci[2 * nteams - 1, 2]
  corr_est[1, 2] <- round(corr_par, 2)
  corr_est[1, 1] <- round(exp(ci[2 * nteams, 1]), 2)
  corr_est[1, 3] <- round(exp(ci[2 * nteams, 2]), 2)
  if (corr_est[1, 2] == 0) {
    corr_est[1, 1] <- corr_est[1, 3] <- 0
  }
  rownames(att_est) <- teams
  colnames(att_est) <- c("2.5%", "mle", "97.5%")
  rownames(def_est) <- teams
  colnames(def_est) <- c("2.5%", "mle", "97.5%")
  rownames(abilities_est) <- teams
  colnames(abilities_est) <- c("2.5%", "mle", "97.5%")
  colnames(corr_est) <- c("2.5%", "mle", "97.5%")
  colnames(home_est) <- c("2.5%", "mle", "97.5%")



  #   ____________________________________________________________________________
  #   Compute AIC and BIC                                                     ####

  logLik <- -mle_fit$value
  k <- length(mle_fit$par)

  AIC <- 2 * k - 2 * logLik
  BIC <- k * log(N) - 2 * logLik


  #   ____________________________________________________________________________
  #   Output                                                                  ####


  if (model == "student_t") {
    return(list(
      abilities = abilities_est,
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
    return(list(
      att = att_est,
      def = def_est,
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
  } else {
    return(list(
      att = att_est,
      def = def_est,
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
