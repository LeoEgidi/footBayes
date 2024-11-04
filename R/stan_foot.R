#' Fit football models  with Stan
#'
#' Stan football modelling for the most famous models:
#' double Poisson, bivariate Poisson, Skellam, student t, diagonal-inflated bivariate Poisson and zero-inflated Skellam.
#'
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{season}: Season identifier (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{homegoals}: Goals scored by the home team (integer >= 0).
#'     \item \code{awaygoals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param model A character string specifying the Stan model to fit. Options are:
#'   \itemize{
#'     \item \code{"double_pois"}: Double Poisson model.
#'     \item \code{"biv_pois"}: Bivariate Poisson model.
#'     \item \code{"skellam"}: Skellam model.
#'     \item \code{"student_t"}: Student's t model.
#'     \item \code{"diag_infl_biv_pois"}: Diagonal-inflated bivariate Poisson model.
#'     \item \code{"zero_infl_skellam"}: Zero-inflated Skellam model.
#'   }
#' @param predict An integer specifying the number of out-of-sample matches for prediction. If missing, the function fits the model to the entire dataset without making predictions.
#' @param ranking An optional data frame containing ranking points for teams:
#'   \itemize{
#'     \item \code{periods}: Time periods corresponding to the rankings (integer >= 1).
#'     \item \code{team}: Team names matching those in \code{data} (character string).
#'     \item \code{rank_points}: Ranking points for each team (numeric).
#'   }
#' @param dynamic_type A character string specifying the type of dynamics in the model. Options are:
#'   \itemize{
#'     \item \code{"weekly"}: Weekly dynamic parameters.
#'     \item \code{"seasonal"}: Seasonal dynamic parameters.
#'   }
#' @param prior_par A list specifying the prior distributions for the parameters of interest:
#'   \itemize{
#'     \item \code{ability}: Prior distribution for team-specific abilities. Possible distributions are \code{normal}, \code{student_t}, \code{cauchy}, \code{laplace}. Default is \code{normal(0, NULL)}.
#'     \item \code{ability_sd}:  Prior distribution for the team-specific standard deviations. See the \code{prior} argument for more details. Default is \code{cauchy(0, 5)}.
#'     \item \code{home}: Prior distribution for the home effect (\code{home}). Applicable only if \code{home_effect = TRUE}. Only normal priors are allowed. Default is \code{normal(0, 5)}.
#'   }
#'
#'   See the \pkg{rstanarm} package for more details on specifying priors.
#' @param home_effect Logical indicating whether to include a home effect (default is \code{TRUE}).
#' @param norm_method A character string specifying the method used to normalize team-specific ranking points. Options are:
#'   \itemize{
#'     \item \code{"none"}: No normalization (default).
#'     \item \code{"standard"}: Standardization (mean 0, standard deviation 1).
#'     \item \code{"mad"}: Median Absolute Deviation normalization.
#'     \item \code{"min_max"}: Min-max scaling to [0,1].
#'   }
#' @param ranking_map An optional vector mapping ranking periods to data periods. If not provided and the number of ranking periods matches the number of data periods, a direct mapping is assumed.
#' @param ... Optional parameters passed to \code{\link[rstan]{stan}} (e.g., \code{iter}, \code{chains}, \code{cores}, \code{control}).
#'
#' @return A list of class \code{"stanFoot"} containing:
#'   \itemize{
#'     \item \code{fit}: The fitted \code{stanfit} object returned by \code{\link[rstan]{stan}}.
#'     \item \code{data}: The data prepared for Stan.
#'   }
#'
#'
#'@details
#'Let \eqn{(y^{H}_{n}, y^{A}_{n})} denote the
#'observed number of goals scored by the home
#'and the away team in the \eqn{n}-th game,
#'respectively. A general bivariate Poisson model
#'allowing for goals' correlation
#'(Karlis & Ntzoufras, 2003) is the following:
#'
#'\deqn{ Y^H_n, Y^A_n| \lambda_{1n}, \lambda_{2n}, \lambda_{3n}  \sim \mathsf{BivPoisson}(\lambda_{1n}, \lambda_{2n}, \lambda_{3n})}
#'\deqn{\log(\lambda_{1n})  = \mu+att_{h_n} + def_{a_n}}
#'\deqn{\log(\lambda_{2n})  = att_{a_n} + def_{h_n}}
#'\deqn{\log(\lambda_{3n})  =\beta_0,}
#'
#' where the case \eqn{\lambda_{3n}=0} reduces to
#' the double Poisson model (Baio & Blangiardo, 2010).
#'  \eqn{\lambda_{1n}, \lambda_{2n}} represent the
#'  scoring rates for the home and the away team,
#'  respectively, where: \eqn{\mu} is the home effect;
#'  the parameters \eqn{att_T} and
#'   \eqn{def_T} represent the attack and the
#'   defence abilities,
#' respectively, for each team \eqn{T}, \eqn{T=1,\ldots,N_T};
#' the nested indexes \eqn{h_{n}, a_{n}=1,\ldots,N_T}
#' denote the home and the away team playing in the \eqn{n}-th game,
#' respectively. Attack/defence parameters are imposed a
#' sum-to-zero constraint to achieve identifiability and
#' assigned some weakly-informative prior distributions:
#'
#' \deqn{att_T \sim \mathcal{N}(\mu_{att}, \sigma_{att})}
#' \deqn{def_T \sim \mathcal{N}(\mu_{def}, \sigma_{def}),}
#'
#' with hyperparameters \eqn{\mu_{att}, \sigma_{att}, \mu_{def}, \sigma_{def}}.
#'
#' Instead of using the marginal number of goals,
#' another alternative is to modelling directly
#' the score difference \eqn{(y^{H}_{n}- y^{A}_{n})}.
#' We can use the Poisson-difference distribution
#' (or Skellam distribution) to model goal
#' difference in the \eqn{n}-th match (Karlis & Ntzoufras, 2009):
#'
#' \deqn{y^{H}_{n}- y^{A}_{n}| \lambda_{1n}, \lambda_{2n} \sim PD(\lambda_{1n}, \lambda_{2n}),}
#'
#' and the scoring rates \eqn{\lambda_{1n}, \lambda_{2n}} are
#' unchanged with respect to the bivariate/double Poisson model.
#' If we want to use a continue distribution, we can
#' use a student t distribution with 7 degrees of
#' freedom (Gelman, 2014):
#'
#' \deqn{y^{H}_{n}- y^{A}_{n} \sim t(7, ab_{h_{n}}-ab_{a(n)}, \sigma_y)}
#' \deqn{ab_t \sim \mathcal{N}(\mu + b \times {prior\_score}_t, sigma_{ab}),}
#'
#' where \eqn{ab_t} is the overall ability for
#' the \eqn{t}-th team, whereas \eqn{prior\_score_t}
#' is a prior measure of team's strength (for instance a
#' ranking).
#'
#' These model rely on the assumption of static parameters.
#' However, we could assume dynamics in the attach/defence
#' abilities (Owen, 2011; Egidi et al., 2018) in terms of weeks or seasons through the argument
#' \code{dynamic_type}. In such a framework, for a given
#' number of times \eqn{1, \ldots, \mathcal{T}}, the models
#' above would be unchanged, but the priors for the abilities
#' parameters at each time \eqn{\tau, \tau=2,\ldots, \mathcal{T},} would be:
#'
#' \deqn{att_{T, \tau} \sim \mathcal{N}({att}_{T, \tau-1}, \sigma_{att})}
#' \deqn{def_{T, \tau} \sim \mathcal{N}({def}_{T, \tau-1}, \sigma_{def}),}
#'
#' whereas for \eqn{\tau=1} we have:
#'
#' \deqn{att_{T, 1} \sim \mathcal{N}(\mu_{att}, \sigma_{att})}
#' \deqn{def_{T, 1} \sim \mathcal{N}(\mu_{def}, \sigma_{def}).}
#'
#' Of course, the identifiability constraint must be imposed for
#' each time \eqn{\tau}.
#'
#' The current version of the package allows for the fit of a
#' diagonal-inflated bivariate Poisson and a zero-inflated Skellam model in the
#' spirit of (Karlis & Ntzoufras, 2003) to better capture draw occurrences. See the vignette for further details.
#'
#'@author Leonardo Egidi \email{legidi@units.it}, Vasilis Palaskas \email{vasilis.palaskas94@gmail.com}.
#'
#'@references
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
#'@examples
#'\dontrun{
#'require(tidyverse)
#'require(dplyr)
#'
#'
#'
#' # Example usage with ranking
#' data("italy")
#' italy <- as_tibble(italy)
#' italy_2021 <- italy %>%
#'   select(Season, home, visitor, hgoal, vgoal) %>%
#'   filter(Season == "2021")
#'
#' teams <- unique(italy_2021$home)
#' n_rows <- 20
#'
#' # Create fake ranking
#' ranking <- data.frame(
#'   periods = rep(1, n_rows),
#'   team = sample(teams, n_rows, replace = FALSE),
#'   rank_points = sample(0:60, n_rows, replace = FALSE)
#' )
#'
#' ranking <- ranking %>%
#'   arrange(periods, desc(rank_points))
#'
#' fit_with_ranking <- stan_foot(
#'   data = italy_2021,
#'   model = "diag_infl_biv_pois",
#'   ranking = ranking,
#'   home_effect = TRUE,
#'   prior_par = list(
#'     ability = student_t(4, 0, NULL),
#'     ability_sd = cauchy(0, 3),
#'     home = normal(1, 10)
#'   ),
#'   norm_method = "mad",
#'   iter = 1000,
#'   chains = 2,
#'   cores = 2,
#'   control = list(adapt_delta = 0.95, max_treedepth = 15)
#' )
#'
#' # Print a summary of the model fit
#' print(fit_with_ranking$fit)
#'
#'
#'
#'### Use Italian Serie A from 2000 to 2002
#'
#'data("italy")
#'italy <- as_tibble(italy)
#'italy_2000_2002<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  dplyr::filter(Season=="2000" |  Season=="2001"| Season=="2002")
#'
#'
#' ### Fit Stan models
#' ## no dynamics, no predictions
#'
#' fit1 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois") # double poisson
#' print(fit1, pars =c("home", "sigma_att",
#'                     "sigma_def"))
#'
#' fit2 <- stan_foot(data = italy_2000_2002,
#'                 model="biv_pois")    # bivariate poisson
#' print(fit2, pars =c("home", "rho",
#'                     "sigma_att", "sigma_def"))
#'
#' fit3 <- stan_foot(data = italy_2000_2002,
#'                 model="skellam")     # skellam
#' print(fit3, pars =c("home", "sigma_att",
#'                     "sigma_def"))
#'
#' fit4 <- stan_foot(data = italy_2000_2002,
#'                 model="student_t")   # student_t
#' print(fit4, pars =c("home", "beta"))
#'
#' ## seasonal dynamics, no prediction
#'
#' fit5 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois",
#'                 dynamic_type ="seasonal") # double poisson
#' print(fit5, pars =c("home", "Sigma_att",
#'                     "Sigma_def"))
#'
#' ## seasonal dynamics, prediction for the last season
#'
#' fit6 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois",
#'                 dynamic_type ="seasonal",
#'                 predict = 306) # double poisson
#' print(fit6, pars =c("home", "Sigma_att",
#'                     "Sigma_def"))
#'
#' ## other priors' options
#'
#' fit_p <- stan_foot(data = italy_2000_2002,
#'                    model="double_pois",
#'                    priors = student_t (4, 0, NULL),
#'                    prior_sd = laplace(0,1)) # double poisson with
#'                                             # student_t priors for teams abilities
#'                                             # and laplace prior for the hyper sds
#' print(fit_p,  pars = c("home", "sigma_att",
#'                     "sigma_def"))
#' }
#'@import bayesplot
#'@import reshape2
#'@import ggplot2
#' @importFrom dplyr mutate select arrange ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rstan stan extract
#' @import matrixStats
#'@export


stan_foot <- function(data,
                      model,
                      predict = 0,
                      ranking,
                      dynamic_type,
                      prior_par = list(
                        ability = normal(0, NULL),
                        ability_sd = cauchy(0, 5),
                        home = normal(0, 5)
                      ),
                      home_effect = TRUE,
                      norm_method = "none",
                      ranking_map = NULL,
                      ...){


#   ____________________________________________________________________________
#   Data Checks                                                             ####


  if (!is.matrix(data) & !is.data.frame(data)){
    stop("Data are not stored in matrix/data frame
         structure. Pleasy, provide data correctly.")
     }



  if (dim(data)[2]<5){
    stop("Data dimensions are wrong! Please,
         supply a matrix/data frame containing
         the following mandatory column items:
         season, home team, away team,
         home goals, away goals.")
  }


  #if (dim(data)[2]==5){
  colnames(data) <- c("season", "home_team", "away_team",
                      "homegoals", "awaygoals")
  #}

  # checks about formats
  if ( !is.numeric(data$homegoals) |!is.numeric(data$awaygoals)){
    stop("Goals are not numeric! Please, provide
         numeric values for the goals")
  }

  # check about columns
  if (dim(data)[2]>5){
    warning("Your dataset seems too large!
             The function will evaluate the first
             five columns as follows:
             season, home team, away team, home goals,
             away goals")
    #  stop("Wrong number of columns! Please,
    #       supply a matrix/data frame containing
    #       the following mandatory column items:
    #       season, home team, away team,
    #       home goals, away goals.")
  }


#   ____________________________________________________________________________
#   Models' Name Checks                                                     ####


  allowed_model_names <- c( "double_pois",
                            "biv_pois",
                            "skellam",
                            "student_t",
                            "diag_infl_biv_pois",
                            "zero_infl_skellam")

  model <- match.arg(model, allowed_model_names)


  nteams <- length(unique(data$home_team))

  # Default control parameters
  default_control <- list(adapt_delta = 0.8, max_treedepth = 10)

  # Initialize user_dots with default arguments, including the control list
  user_dots <- list(
    chains = 4,
    iter = 2000,
    # warmup = floor(iter / 2),
    thin = 1,
    init = "random",
    seed = sample.int(.Machine$integer.max, 1),
    algorithm = "NUTS",
    control = default_control,  # Default control parameters
    sample_file = NULL,
    diagnostic_file = NULL,
    save_dso = TRUE,
    verbose = FALSE,
    include = TRUE,
    cores = getOption("mc.cores", 1L),
    open_progress = interactive() && !isatty(stdout()) && !identical(Sys.getenv("RSTUDIO"), "1"),
    boost_lib = NULL,
    eigen_lib = NULL,
    nu = 7
  )



#   ____________________________________________________________________________
#   Optional Arguments Checks                                               ####

  user_dots_prel <- list(...)

  # Handle control argument separately
  if ("control" %in% names(user_dots_prel)) {
    # Extract user-supplied control parameters
    user_control <- user_dots_prel$control

    # Merge default control with user-supplied control
    user_dots$control <- utils::modifyList(default_control, user_control)

    user_dots_prel$control <- NULL
  }

  # Update 'user_dots'
  user_dots <- utils::modifyList(user_dots, user_dots_prel)





# if (missing(...)){
#   user_dots <- user_dots
# }else{
#   user_dots_prel <- list(...)
#   names_prel <- names(user_dots_prel)
#   names_dots <- names(user_dots)
#   for (u in 1:length(names_prel)){
#     user_dots[names_prel[u] == names_dots] <- user_dots_prel[u]
#   }
# }



#   ____________________________________________________________________________
#   Predict Checks                                                          ####


  #predict <- round(predict)

  if (!is.numeric(predict) || predict < 0 || predict %% 1 != 0) {
    stop("The argument 'predict' must be a non-negative integer.")
  }


  # if (missing(predict)){ # check on predict
  #   predict <- 0
  #   N <- dim(data)[1]# rows of the dataset
  #   N_prev <- 0
  #   type <- "fit"
  # }
  if(predict == 0){
    predict <- 0
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else if (is.numeric(predict)){
    if (predict %% 1 != 0){
      warning("Please, use integer numbers for the argument 'predict'!
              The input has been rounded to the closes integer number.")
      predict <- round(predict)
    }
    N <- dim(data)[1] - predict
    N_prev <- predict
    type <- "prev"

  }else if (!is.numeric(predict)){
    stop("The number of out-of-sample matches is ill posed!
         Pick up an integer number.")
  }

   if (predict >= dim(data)[1]){
    stop("The training set size is zero!
            Please, select a lower value for the
            out-of-sample matches, through the
            argument predict.")
     }



#   ____________________________________________________________________________
#   Dynamic Models Checks                                                   ####


    # names conditions
  if (!missing(dynamic_type)){
    dynamic_names <- c("weekly", "seasonal")
    dynamic_type <- match.arg(dynamic_type, dynamic_names)
    }

  if (missing(dynamic_type)){
    dyn <-""
    ntimes <- 1
    instants <- rep(1, N)
  }else if (dynamic_type == "weekly" ){
      dyn <- "dynamic_"
      if (length(unique(data$season))!=1){
        stop("When using weekly dynamics,
              please consider one season only.")
      }else{
      weak_count <- ((N+predict)*2)/(nteams)
      if ((N*2)%%(nteams)!=0){
        stop("The number of total matches is not
              the same for all the teams. Please,
              provide an adequate number of matches
              (hint: proportional to the number
              of matches for each match day).")
      }
      weak <- rep(seq(1, weak_count ), each = nteams/2)
      data <- data %>%
        mutate(weak)
      ntimes <- length(unique(weak))
      #time_tot <- c(1:length(unique(weak[1:(N+N_prev)])))
      time <- c(1:length(unique(weak)))
      instants <- weak[1:N]
      #ntimes_prev <- length(unique(weak[1:(N+N_prev)]))-length(unique(weak[1:N]))
      #time_prev <- setdiff(time_tot, time)
      instants_prev <- weak[(N+1):(N+N_prev)]
      }
    }else if(dynamic_type=="seasonal"){
      dyn <- "dynamic_"
      if (length(unique(data$season))==1){
        dyn <-""
        warning("When using seasonal dynamics,
              please consider more than one season.
              No dynamics is used to fit the model")
      }
      season_count <- length(unique(data$season))
      season <- match(data$season, unique(data$season))
      ntimes <- season_count
      #time_tot <- c(1:length(unique(data$season)))
      time <- c(1:season_count)
      instants <- season[1:N]
      #ntimes_prev <- length(unique(season[1:(N+N_prev)]))-length(unique(season[1:N]))
      #time_prev <- setdiff(time_tot, time)
      instants_prev <- season[(N+1):(N+N_prev)]
    }




#   ____________________________________________________________________________
#   Prior Checks                                                            ####

  # Validate prior_par names
  allowed_prior_names <- c("ability", "ability_sd", "home")

  # Check that prior_par contains only allowed elements
  if (!is.null(prior_par)) {
    if (!is.list(prior_par)) {
      stop("'prior_par' must be a list.")
    }
    unknown_prior_names <- setdiff(names(prior_par), allowed_prior_names)
    if (length(unknown_prior_names) > 0) {
      stop(
        paste(
          "Unknown elements in 'prior_par':",
          paste(unknown_prior_names, collapse = ", ")
        )
      )
    }
  }

  # Extract prior parameters from the priors list
  ability_prior <- prior_par$ability
  ability_prior_sd <- prior_par$ability_sd
  home_prior <- prior_par$home


  prior_dist <- ability_prior$dist
  hyper_df <- 1           # initialization
  if (missing(ability_prior)){    # Normal as default weakly-inf. prior
    prior_dist_num <- 1
    ability_prior <- normal(0,NULL)
    hyper_location <- 0    # location
    #hyper_sd_scale <- 5  # scale
  }else{
    prior_dist <- ability_prior$dist
    #good_prior_names <- c("normal", "student_t", "cauchy", "laplace")
    #prior_dist <- match.arg(prior_dist, good_prior_names)
    if (is.null(ability_prior$scale)==FALSE){
      warning("Group-level standard deviations cannot be fixed to
               numerical values, rather they need to be assigned
               a reasonable prior distribution. Thus, the 'scale'
               argument in the 'prior' argument will be omitted
               (by default, prior$scale=NULL).")
    }
    if (prior_dist == "normal"){
        prior_dist_num <- 1
        hyper_df <- 1
        hyper_location <- ability_prior$location
          # if (is.null(prior_sd$scale)){
          #   hyper_sd_scale <-1
          # }else{
          #   hyper_sd_scale <- prior_sd$scale
          # }
      }else if (prior_dist=="t" && ability_prior$df!=1){
        prior_dist_num <- 2   # student-t
        hyper_df <- ability_prior$df
        hyper_location <- ability_prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      }else if (prior_dist=="t"&& ability_prior$df==1){
        prior_dist_num <- 3
        hyper_df <- 1     # by default of Cauchy distribution
        hyper_location <- ability_prior$location
        # if (is.null(ability_prior$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- ability_prior_sd$scale
        # }
      } else if (prior_dist =="laplace"){
        prior_dist_num <- 4
        hyper_df <- 1
        hyper_location <- ability_prior$location
        # if (is.null(ability_prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- ability_prior_sd$scale
        # }
        }
    }


         hyper_sd_df <- 1        # initialization
      if (missing(ability_prior_sd)){    # Cauchy as default weakly-inf. prior
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1        # student_t with 1 df
         hyper_sd_location<- 0   # location
         hyper_sd_scale <- 5     # scale
      }else{
        prior_dist_sd <- ability_prior_sd$dist
       if (prior_dist_sd == "normal"){
         prior_dist_sd_num <- 1
         hyper_sd_df <- 1
         hyper_sd_location <- ability_prior_sd$location
         if (is.null(ability_prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- ability_prior_sd$scale
         }
      }else if (prior_dist_sd=="t" && ability_prior_sd$df!=1){
         prior_dist_sd_num <- 2   # student-t
         hyper_sd_df <- ability_prior_sd$df
         hyper_sd_location <- ability_prior_sd$location
         if (is.null(ability_prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- ability_prior_sd$scale
         }
      }else if (prior_dist_sd=="t"&& ability_prior_sd$df==1){
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1     # by default of Cauchy distribution
         hyper_sd_location <- ability_prior_sd$location
         if (is.null(ability_prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- ability_prior_sd$scale
         }
      } else if (prior_dist_sd =="laplace"){
        prior_dist_sd_num <- 4
        hyper_sd_df <- 1
        hyper_sd_location <- ability_prior_sd$location
        if (is.null(ability_prior_sd$scale)){
          hyper_sd_scale <-1
        }else{
          hyper_sd_scale <- ability_prior_sd$scale
        }
      }
    }



  teams <- unique(data$home_team)
  team_home <- match( data$home_team, teams)
  team_away <- match( data$away_team, teams)
  team1 <- team_home[1:N]
  team2 <- team_away[1:N]
  team1_prev <- team_home[(N+1):(N+N_prev)]
  team2_prev <- team_away[(N+1):(N+N_prev)]
  y <- matrix(NA, N, 2)
  y[,1] <- as.numeric(as.vector(data$homegoals)[1:N])
  y[,2] <- as.numeric(as.vector(data$awaygoals)[1:N])
  diff_y <- y[,1]-y[,2]



  # ____________________________________________________________________________
  # Ranking Checks ####

  # Define normalization function
  normalize_rank_points <- function(rank_points, method) {
    if (method == "none") {
      rank_points
    } else if (method == "standard") {
      s <- stats::sd(rank_points, na.rm = TRUE)
      m <- mean(rank_points, na.rm = TRUE)
      if (s == 0) {
        rep(0, length(rank_points))
      } else {
        (rank_points - m) / (2 * s)
      }
    } else if (method == "mad") {
      md <- stats::mad(rank_points, na.rm = TRUE)
      med <- stats::median(rank_points, na.rm = TRUE)
      if (md == 0) {
        rep(0, length(rank_points))
      } else {
        (rank_points - med) / md
      }
    } else if (method == "min_max") {
      min_rp <- min(rank_points, na.rm = TRUE)
      max_rp <- max(rank_points, na.rm = TRUE)
      if (max_rp == min_rp) {
        rep(0, length(rank_points))
      } else {
        (rank_points - min_rp) / (max_rp - min_rp)
      }
    }
  }


  norm_method <- match.arg(norm_method, choices = c("none", "standard", "mad", "min_max"))

  # Check if ranking is provided
  if (missing(ranking)) {
    warning("Ranking is missing, creating a default zero matrix.")
    ntimes_rank <- 1
    nteams <- length(unique(data$home_team))
    ranking_matrix <- matrix(0, nrow = ntimes_rank, ncol = nteams)
  } else {
    # Ensure ranking is either a matrix or a data frame
    if (!is.matrix(ranking) && !is.data.frame(ranking)) {
      stop("Ranking must be a matrix or a data frame with at least 5 columns: season, home team, away team, home goals, away goals.")
    }

    # Convert ranking to data frame if it's not already
    ranking <- as.data.frame(ranking)

    # Check if the ranking dataset has more than the expected number of columns
    if (ncol(ranking) > 3) {
      warning("Your ranking dataset seems too large! Only the first three columns will be used as: periods,
            team, rank_points")
      ranking <- ranking[, 1:3]
    }

    # Check if the required columns are present
    required_cols <- c("periods", "team", "rank_points")
    if (!all(required_cols %in% colnames(ranking))) {
      stop(paste("Ranking data frame must contain the following columns:", paste(required_cols, collapse = ", ")))
    }

    # Check for NAs in required columns
    if (any(is.na(ranking[, required_cols]))) {
      stop("Ranking data contains NAs in required columns. Please remove or impute NAs.")
    }

    # Check if the rank_point variable is an integer or numeric
    if (!is.numeric(ranking$rank_points) && !is.integer(ranking$rank_points)) {
      stop("Ranking points type must be numeric or integer. Please check that the column 'rank_points' contains numerical values.")
    }

    # Define the number of ranking periods for STAN
    ntimes_rank <- length(unique(ranking$periods))

    # Convert rank_points to numeric
    ranking <- ranking %>%
      mutate(rank_points = as.numeric(rank_points)) %>%
      group_by(periods) %>%
      mutate(rank_points = normalize_rank_points(rank_points, norm_method)) %>%
      ungroup()

    # Transform ranking to wide format
    ranking_transformed <- ranking %>%
      pivot_wider(
        names_from = team,
        values_from = rank_points
      ) %>%
      arrange(periods)

    # # Replace NA values with 0
    # if (any(is.na(ranking_transformed))) {
    #   warning("Some ranking points are NAs, they will be set to zero.")
    #   ranking_transformed[is.na(ranking_transformed)] <- 0
    # }

    # Update ranking to be the transformed version
    ranking_matrix <- as.matrix(ranking_transformed[, -1])
  }




##  ............................................................................
##  Ranking periods map with the data periods                               ####

  ntimes_fit <- length(unique(instants))

  if (ntimes_rank > 1) {
    if (is.null(ranking_map)) {
      if (ntimes_fit == ntimes_rank) {
        # Assume periods correspond directly
        instants_rank <- instants
      } else {
        stop("The length of 'ntimes' and 'ntimes_rank' must be equal to directly map the periods.")
      }
    } else {
      # Use user-provided mapping
      instants_rank <- ranking_map
      if (length(instants_rank) != N) {
        stop("Length of 'ranking_map' must equal the number of matches.")
      }
    }
  } else {
    # If ntimes_rank = 1, instants_rank are all equal to 1 and repeated the length of instants
    instants_rank <- rep(1, length(instants))
  }





#   ____________________________________________________________________________
#   Home Effect Check                                                       ####


  # Check that home_effect is logical
  if (!is.logical(home_effect) || length(home_effect) != 1) {
    stop("'home_effect' must be a single logical value (TRUE or FALSE).")
  }


  # Define default values for home priors if not provided
  default_mean_home <- 0
  default_sd_home <- 5


  if (home_effect) {
    ind_home <- 1
    if (home_prior$dist != "normal") {
      stop("Home effect prior must be 'normal'.")
    }
    mean_home <- home_prior$location
    sd_home <- home_prior$scale
  } else {
    ind_home <- 0
    mean_home <- default_mean_home
    sd_home <- default_sd_home
  }

  # #Check home_prior_par value
  # if (home_effect) {
  #   check_prior(mean_home, "mean_home")
  #   check_prior(sd_home, "sd_home", positive = TRUE)
  # }



  # home_names <- c("TRUE", "FALSE")
  # ind_home <- match.arg(ind_home, home_names)
  #
  #  if (missing(ind_home)){
  #    ind_home = "TRUE"
  #  }else{
  #    ind_home = ind_home
  #  }
  #
  # ind_home <- 0*(ind_home=="FALSE") + 1*(ind_home =="TRUE")





#   ____________________________________________________________________________
#   STAN Data                                                               ####


  data_stan <- list(y = y,
                    spi_std = rep(0, nteams),
                    diff_y = diff_y,
                    N = N,
                    N_prev = N_prev,
                    nteams = nteams,
                    ntimes_rank = ntimes_rank,
                    instants_rank = instants_rank,
                    team1 = team1,
                    team2 = team2,
                    team1_prev = team1_prev,
                    team2_prev = team2_prev,
                    prior_dist_num = prior_dist_num,
                    prior_dist_sd_num = prior_dist_sd_num,
                    hyper_df = hyper_df,
                    hyper_location = hyper_location,
                    hyper_sd_df = hyper_sd_df,
                    hyper_sd_location = hyper_sd_location,
                    hyper_sd_scale = hyper_sd_scale,
                    ranking = ranking_matrix,
                    nu = user_dots$nu,
                    ind_home = ind_home,
                    mean_home = mean_home,
                    sd_home = sd_home
  )

  if (!missing(dynamic_type)){
    data_stan$ntimes <- ntimes
    data_stan$instants <- instants
    data_stan$time <- time
    data_stan$instants_prev <- instants_prev
  }


#   ____________________________________________________________________________
#   STAN Models                                                             ####

  # Construct the dynamic part of the model name
  dyn <- if (!missing(dynamic_type)) "dynamic_" else ""

  # Determine the type of the model (fit or prev)
  type <- if (predict == 0) "fit" else "prev"

  # Build the file name of the Stan model
  stan_model_filename <- paste0(model, "_", dyn, type, ".stan")

  # Path to the Stan model file
  stan_model_path <- system.file("stan", stan_model_filename, package = "footBayes")

  # Check if the Stan model file exists
  if (!file.exists(stan_model_path)) {
    stop("The Stan model file does not exist: ", stan_model_path)
  }

  fit <- stan(file = stan_model_path,
              data= data_stan,
              iter = user_dots$iter,
              chains = user_dots$chains,
              thin = user_dots$thin,
              cores = user_dots$cores,
              control = user_dots$control)

  output <- list(
    fit = fit,
    data = data_stan
  )
  class(output) <- "stanFoot"
  return(output)
}

