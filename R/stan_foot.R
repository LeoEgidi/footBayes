#' Fit football models  with Stan
#'
#' Stan football modelling for the most famous models:
#' double Poisson, bivariate Poisson, Skellam, student t, diagonal-inflated bivariate Poisson and zero-inflated Skellam.
#'
#'@param data A data frame, or a matrix containing the following mandatory items: season, home team, away team,
#'home goals, away goals.
#'@param model The type of Stan model used to fit the data.
#'             One among the following: \code{"double_pois"},
#'             \code{"biv_pois"}, \code{"skellam"}, \code{"student_t"}, \code{"diag_infl_biv_pois"}, \code{"zero_infl_skellam"}.
#'@param predict The number of out-of-sample matches. If missing, the function returns
#'the fit for the training set only.
#'@param ranking Eventual matrix ranking points (relative strengths) provided for the teams in the dataset (e.g., the Coca-Cola Fifa ranking)
#'@param dynamic_type One among \code{"weekly"} or \code{"seasonal"} for weekly dynamic parameters or seasonal
#'dynamic parameters.
#'@param prior The prior distribution for the team-specific abilities.
#'Possible choices: \code{normal}, \code{student_t}, \code{cauchy}, \code{laplace}.
#'See the \pkg{rstanarm} for a deep overview and read the vignette \href{http://mc-stan.org/rstanarm/articles/priors.html}{\emph{Prior
#'   Distributions for rstanarm Models}}
#'@param prior_sd The prior distribution for the team-specific standard deviations. See the \code{prior} argument for more details.
#'@param ind_home Home effect (default is \code{TRUE}).
#'@param norm_method Method used to normalize team-specific ranking points.
#'                   One among \code{"none"}, \code{"standard"}, \code{"mad"}, \code{"min_max"}.
#'@param ranking_map An optional mapping argument to associate ranking periods with the corresponding data periods.
#'@param ... Optional parameters passed to the function
#' in the \bold{rstan} package. It is possibly to specify \code{iter}, \code{chains}, \code{cores}, \code{refresh}, etc.
#'@return
#'
#'An object of S4 class, \code{\link[rstan]{stanfit-class}}.
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
#'@import rstan
#'@import bayesplot
#'@import matrixStats
#'@import reshape2
#'@import ggplot2
#'@import dplyr
#'@export


stan_foot <- function(data,
                      model,
                      predict,
                      ranking,
                      dynamic_type,
                      prior,
                      prior_sd,
                      ind_home = "TRUE",
                      norm_method = c("none", "standard", "mad", "min_max"),
                      ranking_map = NULL, # Argument for mapping ranking periods with data periods
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
  colnames(data) <- c("season", "home", "away",
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
  

  good_names <- c("double_pois",
                  "biv_pois",
                  "skellam",
                  "student_t",
                  "diag_infl_biv_pois",
                  "zero_infl_skellam")
  model <- match.arg(model, good_names)


  nteams <- length(unique(data$home))

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
    user_dots$control <- modifyList(default_control, user_control)
    
    user_dots_prel$control <- NULL
  }
  
  # Update 'user_dots'
  user_dots <- modifyList(user_dots, user_dots_prel)
  
  
  
  

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

  if (missing(predict)){ # check on predict
    predict <- 0
    N <- dim(data)[1]# rows of the dataset
    N_prev <- 0
    type <- "fit"
  }else if(predict ==0){
    predict <- 0
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else if (is.numeric(predict)){
    if (predict%%1 !=0){
      warning("Please, use integer numbers for the argument 'predict'!
              The input has been rounded to the closes integer number.")
      predict <- round(predict)
    }
    N <- dim(data)[1]-predict
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
  
  hyper_df <- 1           # initialization
  if (missing(prior)){    # Normal as default weakly-inf. prior
    prior_dist_num <- 1
    prior <- normal(0,NULL)
    hyper_location<- 0    # location
    #hyper_sd_scale <- 5  # scale
  }else{
    prior_dist <- prior$dist
    #good_prior_names <- c("normal", "student_t", "cauchy", "laplace")
    #prior_dist <- match.arg(prior_dist, good_prior_names)
    if (is.null(prior$scale)==FALSE){
      warning("Group-level standard deviations cannot be fixed to
               numerical values, rather they need to be assigned
               a reasonable prior distribution. Thus, the 'scale'
               argument in the 'prior' argument will be omitted
               (by default, prior$scale=NULL).")
    }
      if (prior_dist == "normal"){
        prior_dist_num <- 1
        hyper_df <- 1
        hyper_location <- prior$location
          # if (is.null(prior_sd$scale)){
          #   hyper_sd_scale <-1
          # }else{
          #   hyper_sd_scale <- prior_sd$scale
          # }
      }else if (prior_dist=="t" && prior$df!=1){
        prior_dist_num <- 2   # student-t
        hyper_df <- prior$df
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      }else if (prior_dist=="t"&& prior$df==1){
        prior_dist_num <- 3
        hyper_df <- 1     # by default of Cauchy distribution
        hyper_location <- prior$location
        # if (is.null(prior$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      } else if (prior_dist =="laplace"){
        prior_dist_num <- 4
        hyper_df <- 1
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
        }
    }


         hyper_sd_df <- 1        # initialization
      if (missing(prior_sd)){    # Cauchy as default weakly-inf. prior
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1        # student_t with 1 df
         hyper_sd_location<- 0   # location
         hyper_sd_scale <- 5     # scale
      }else{
        prior_dist_sd <- prior_sd$dist
       if (prior_dist_sd == "normal"){
         prior_dist_sd_num <- 1
         hyper_sd_df <- 1
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t" && prior_sd$df!=1){
         prior_dist_sd_num <- 2   # student-t
         hyper_sd_df <- prior_sd$df
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t"&& prior_sd$df==1){
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1     # by default of Cauchy distribution
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      } else if (prior_dist_sd =="laplace"){
        prior_dist_sd_num <- 4
        hyper_sd_df <- 1
        hyper_sd_location <- prior_sd$location
        if (is.null(prior_sd$scale)){
          hyper_sd_scale <-1
        }else{
          hyper_sd_scale <- prior_sd$scale
        }
      }
    }



  teams <- unique(data$home)
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)
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
      s <- sd(rank_points, na.rm = TRUE)
      m <- mean(rank_points, na.rm = TRUE)
      if (s == 0) {
        rep(0, length(rank_points))
      } else {
        (rank_points - m) / (2 * s)
      }
    } else if (method == "mad") {
      md <- mad(rank_points, na.rm = TRUE)
      med <- median(rank_points, na.rm = TRUE)
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
    nteams <- length(unique(data$team))
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


  home_names <- c("TRUE", "FALSE")
  ind_home <- match.arg(ind_home, home_names)

   if (missing(ind_home)){
     ind_home = "TRUE"
   }else{
     ind_home = ind_home
   }

  ind_home <- 0*(ind_home=="FALSE") + 1*(ind_home =="TRUE")





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
                    ind_home = ind_home
  )

  if (!missing(dynamic_type)){
    data_stan$ntimes <- ntimes
    data_stan$instants <- instants
    data_stan$time <- time
    data_stan$instants_prev <- instants_prev
  }

  
#   ____________________________________________________________________________
#   STAN Models                                                             ####
  
  stanfoot_models <- function(model, dyn, type){
    right_name <- paste(model,"_", dyn, type, sep="")
    models_name <- c("biv_pois_dynamic_fit",
                     "biv_pois_dynamic_prev",
                      "biv_pois_fit",
                      "biv_pois_prev",
                     "diag_infl_biv_pois_dynamic_fit",
                     "diag_infl_biv_pois_dynamic_prev",
                     "diag_infl_biv_pois_fit",
                     "diag_infl_biv_pois_prev",
                     "double_pois_dynamic_fit",
                     "double_pois_dynamic_prev",
                     "double_pois_fit",
                     "double_pois_prev",
                     "skellam_dynamic_fit",
                     "skellam_dynamic_prev",
                     "skellam_fit",
                     "skellam_prev",
                     "zero_infl_skellam_dynamic_fit",
                     "zero_infl_skellam_dynamic_prev",
                     "zero_infl_skellam_fit",
                     "zero_infl_skellam_prev",
                     "student_t_dynamic_fit",
                     "student_t_dynamic_prev",
                     "student_t_fit",
                     "student_t_prev"
                     )

    
##  ............................................................................
##  Dynamic Bivariate Poisson                                               ####
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    biv_pois_dynamic_fit<-
      "functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }
    }
    data{
      int N;   // number of games
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes_rank;             // ranking periods
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants_rank[N];       // ranking instants
      int instants[N];
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real rho;
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;         // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;        // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                 // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location, nteams);
        mu_def[t]= def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta_corr[n] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);

      // likelihood

      for (n in 1:N){
        //target+=bipois_lpmf(y[n,]| theta_home[n],
        //                    theta_away[n], theta_corr[n]);
          target+=poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n]);
          target+=poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);

      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n])+
                     poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);
       //bipois_lpmf(y[n,]| theta_home[n],
       //                          theta_away[n], theta_corr[n]);
      }
    }"

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    
    biv_pois_dynamic_prev<-"
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }
    }
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes_rank;             // ranking periods
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants_rank[N];       // ranking instants
      int instants[N];
      int instants_prev[N_prev];
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real rho;
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      //cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      //cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta_corr[n] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);

      // likelihood

      for (n in 1:N){
        //target+=bipois_lpmf(y[n,]| theta_home[n],
        //                    theta_away[n], theta_corr[n]);
        target+=poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n]);
        target+=poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];
      int y_prev[N_prev,2];
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;
      vector[N_prev] theta_corr_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n]) +
                     poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);
        //bipois_lpmf(y[n,]| theta_home[n],
        //                        theta_away[n], theta_corr[n]);
      }

      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home*ind_home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N], team1_prev[n]]-ranking[instants_rank[N], team2_prev[n]]));
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N], team1_prev[n]]-ranking[instants_rank[N], team2_prev[n]]));
        theta_corr_prev[n] = exp(rho);
        y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
      }
    }"

    
##  ............................................................................
##  Static Bivariate Poisson                                                ####
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    biv_pois_fit<-"
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }

    }
    data{
      int N;   // number of games
      int y[N,2];
      int nteams;
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking
      int team1[N];
      int team2[N];
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real beta;
      real rho;
      real home;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      vector[3] theta[N];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta[n,3] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);

      // likelihood
      for (n in 1:N){
         target+=poisson_lpmf(y[n,1]|theta[n,1]+theta[n,3]);
         target+=poisson_lpmf(y[n,2]|theta[n,2]+theta[n,3]);
       //  target+=bipois_lpmf(y[n,]| theta[n,1],
          //                   theta[n,2], theta[n,3]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
        y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]|theta[n,1]+theta[n,3])+
                     poisson_lpmf(y[n,2]|theta[n,2]+theta[n,3]);
       //log_lik[n] =bipois_lpmf(y[n,]| theta[n,1],
                //                 theta[n,2], theta[n,3]);
      }
    }"

    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    
    biv_pois_prev<-"
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }

    }
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real rho;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      vector[3] theta[N];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
        theta[n,3] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);

      // likelihood
      for (n in 1:N){
        //target+=bipois_lpmf(y[n,]| theta[n,1],
        //                    theta[n,2], theta[n,3]);
        target+=poisson_lpmf(y[n,1]| theta[n,1]+theta[n,3]);
        target+=poisson_lpmf(y[n,2]| theta[n,2]+theta[n,3]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int y_prev[N_prev,2];
      vector[3] theta_prev[N_prev];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
        y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]| theta[n,1]+theta[n,3])+
                     poisson_lpmf(y[n,2]| theta[n,2]+theta[n,3]);
        //bipois_lpmf(y[n,]| theta[n,1],
        //                        theta[n,2], theta[n,3]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home*ind_home+att[team1_prev[n]]+
                                def[team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,3] = exp(rho);
        y_prev[n,1] = poisson_rng(theta_prev[n,1]+theta_prev[n,3]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]+theta_prev[n,3]);
      }
    }"

##  ............................................................................
##  Dynamic Diagonal Inflated Bivariate Poisson                             ####

  

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
 
diag_infl_biv_pois_dynamic_fit <- "
  functions{

    real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
      real ss;
      real log_s;
      real mus;
      int  miny;

      miny = min(r[1], r[2]);

      ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
        exp(mu3);
      if(miny > 0) {
        mus = -mu1-mu2+mu3;
        log_s = ss;

        for(k in 1:miny) {
          log_s = log_s + log(r[1] - k + 1) + mus
          + log(r[2] - k + 1)
          - log(k);
          ss = log_sum_exp(ss, log_s);
        }
      }
      return(ss);
    }
    real diag_infl_bipois_lpmf(int[] r , real mu1,real mu2,real mu3, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(bipois_lpmf(r| mu1, mu2,mu3));

      if (r[1] == r[2])
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }
}
    data{
      int N;   // number of games
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real rho;
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;         // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;        // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                 // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location, nteams);
        mu_def[t]= def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home*ind_home + att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_corr[n] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood

      for (n in 1:N){
         target+=diag_infl_bipois_lpmf(y[n,]| theta_home[n],
                    theta_away[n], theta_corr[n],prob_of_draws);

   // if (y[n,1] == y[n,2]){// Alternative way as proposed by Stan manual
  //      target += log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
   //                         bernoulli_lpmf(0 | prob_of_draws)
                    //          + bipois_lpmf(y[n,] | theta_home[n],
                  //   theta_away[n], theta_corr[n]) );
     // } else {
       //  target += bernoulli_lpmf(0 |prob_of_draws)
         //            + bipois_lpmf(y[n,] | theta_home[n],
               //      theta_away[n], theta_corr[n]);
    // }
 }
}

generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =diag_infl_bipois_lpmf(y[n,]| theta_home[n],
                    theta_away[n], theta_corr[n],prob_of_draws);
           //    if (y[n,1] == y[n,2]){// Alternative way proposed by Stan documentation
    //  log_lik[n] = log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
          //                  bernoulli_lpmf(0 |prob_of_draws)
           //                   + bipois_lpmf(y[n,] | theta_home[n],
          //        theta_away[n], theta_corr[n]));
   //} else {
   //   log_lik[n] = bernoulli_lpmf(0 |prob_of_draws)
              //    + bipois_lpmf(y[n,] | theta_home[n],
            //      theta_away[n], theta_corr[n]);
  //}
  }
}"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####

diag_infl_biv_pois_dynamic_prev<-"
functions{

  real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
    real ss;
    real log_s;
    real mus;
    int  miny;

    miny = min(r[1], r[2]);

    ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
      exp(mu3);
    if(miny > 0) {
      mus = -mu1-mu2+mu3;
      log_s = ss;

      for(k in 1:miny) {
        log_s = log_s + log(r[1] - k + 1) + mus
        + log(r[2] - k + 1)
        - log(k);
        ss = log_sum_exp(ss, log_s);
      }
    }
    return(ss);
  }
    real diag_infl_bipois_lpmf(int[] r , real mu1,real mu2,real mu3, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
    real base_prob;
    real prob;
    real log_prob;

    base_prob = exp(bipois_lpmf(r| mu1, mu2,mu3));

    if (r[1] == r[2])
      prob = p + (1 - p) * base_prob;
    else
      prob = (1 - p) * base_prob;

    log_prob = log(prob);

    return log_prob;
  }

}
data{
  int N;   // number of games
  int N_prev;
  int y[N,2];
  int nteams;
  int team1[N];
  int team2[N];
  int team1_prev[N_prev];
  int team2_prev[N_prev];
  int ntimes;                 // dynamic periods
  int time[ntimes];
  int instants[N];
  int instants_rank[N];
  int ntimes_rank;                 // dynamic periods for ranking      
  matrix[ntimes_rank,nteams] ranking;
  int instants_prev[N_prev];
  int<lower=0, upper=1> ind_home;

  // priors part
  int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
  int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

  real hyper_df;
  real hyper_location;

  real hyper_sd_df;
  real hyper_sd_location;
  real hyper_sd_scale;
}
parameters{
  matrix[ntimes, nteams] att_raw;        // raw attack ability
  matrix[ntimes, nteams] def_raw;        // raw defense ability
  real rho;
  real home;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real gamma;
  real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

}
transformed parameters{
  matrix[ntimes, nteams] att;            // attack abilities
  matrix[ntimes, nteams] def;            // defense abilities
  //cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
  //cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
  matrix[ntimes, nteams] mu_att;         // attack hyperparameter
  matrix[ntimes, nteams] mu_def;         // defense hyperparameter
  vector[N] theta_home;                    // exponentiated linear pred.
  vector[N] theta_away;
  vector[N] theta_corr;

  // Gaussian process covariance functions
  // for (i in 1:(ntimes)){
    //   for (j in 1:(ntimes)){
      //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
      //     + (i == j ? 0.1 : 0.0);
      //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
      //                 + (i == j ? 0.1 : 0.0);
      //   }}

  // Sum-to-zero constraint for attack/defense parameters
  att[1]=att_raw[1]-mean(att_raw[1]);
  def[1]=def_raw[1]-mean(def_raw[1]);
  for (t in 2:ntimes){
    att[t]=att_raw[t]-mean(att_raw[t]);
    def[t]=def_raw[t]-mean(def_raw[t]);
  }

  // Lagged prior mean for attack/defense parameters
  for (t in 2:(ntimes)){
    mu_att[1]=rep_row_vector(hyper_location,nteams);
    mu_att[t]=att[t-1];
    //rep_row_vector(0,nteams);

    mu_def[1]=rep_row_vector(hyper_location,nteams);
    mu_def[t]=def[t-1];
    //rep_row_vector(0,nteams);

  }


  for (n in 1:N){
    theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                          (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
    theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                          (gamma/2)*(ranking[instants_rank[n], team1[n]]-ranking[instants_rank[n], team2[n]]));
    theta_corr[n] = exp(rho);
  }
}
model{
  // log-priors for team-specific abilities
  for (h in 1:(nteams)){
    if (prior_dist_num == 1 ){
      att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
    else if (prior_dist_num == 2 ){
      att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
    else if (prior_dist_num == 3 ){
      att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
  }

  // log-hyperpriors for sd parameters
  if (prior_dist_sd_num == 1 ){
    target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 2){
    target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 3){
    target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 4){
    target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }

  // log-priors fixed effects
  target+=normal_lpdf(home|0,5);
  target+=normal_lpdf(rho|0,1);
  target+=normal_lpdf(gamma|0,1);
  target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood

      for (n in 1:N){
         target+=diag_infl_bipois_lpmf(y[n,]| theta_home[n],
                    theta_away[n], theta_corr[n],prob_of_draws);

   // if (y[n,1] == y[n,2]){// Alternative way as proposed by Stan manual
  //      target += log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
   //                         bernoulli_lpmf(0 | prob_of_draws)
     //                    + bipois_lpmf(y[n,] | theta_home[n],
                  //   theta_away[n], theta_corr[n]) );
     // } else {
       //  target += bernoulli_lpmf(0 |prob_of_draws)
         //            + bipois_lpmf(y[n,] | theta_home[n],
               //      theta_away[n], theta_corr[n]);
    // }
 }
}
generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int diff_y_rep[N];
  int y_prev[N_prev,2];
  vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
  vector[N_prev] theta_away_prev;
  vector[N_prev] theta_corr_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =diag_infl_bipois_lpmf(y[n,]| theta_home[n],
                    theta_away[n], theta_corr[n],prob_of_draws);
           //    if (y[n,1] == y[n,2]){// Alternative way proposed by Stan documentation
    //  log_lik[n] = log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
          //                  bernoulli_lpmf(0 |prob_of_draws)
           //                   + bipois_lpmf(y[n,] | theta_home[n],
          //        theta_away[n], theta_corr[n]));
   //} else {
   //   log_lik[n] = bernoulli_lpmf(0 |prob_of_draws)
              //    + bipois_lpmf(y[n,] | theta_home[n],
            //      theta_away[n], theta_corr[n]);
  //}
  }
  // out-of-sample predictions
  for (n in 1:N_prev){
    theta_home_prev[n] = exp(home*ind_home+att[instants_prev[n], team1_prev[n]]+
                               def[instants_prev[n], team2_prev[n]]+
                               (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
    theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                               def[instants_prev[n], team1_prev[n]]-
                               (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
    theta_corr_prev[n] = exp(rho);
    y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
    y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
  }
}//"



##  ............................................................................
##  Static Diagonal Inflated Bivariate Poisson                              ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####


diag_infl_biv_pois_fit<-"
functions{

  real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
    real ss;
    real log_s;
    real mus;
    int  miny;

    miny = min(r[1], r[2]);

    ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
      exp(mu3);
    if(miny > 0) {
      mus = -mu1-mu2+mu3;
      log_s = ss;

      for(k in 1:miny) {
        log_s = log_s + log(r[1] - k + 1) + mus
        + log(r[2] - k + 1)
        - log(k);
        ss = log_sum_exp(ss, log_s);
      }
    }
    return(ss);
  }
  real diag_infl_bipois_lpmf(int[] r , real mu1,real mu2,real mu3, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
    real base_prob;
    real prob;
    real log_prob;

    base_prob = exp(bipois_lpmf(r| mu1, mu2,mu3));

    if (r[1] == r[2])
      prob = p + (1 - p) * base_prob;
    else
      prob = (1 - p) * base_prob;

    log_prob = log(prob);

    return log_prob;
  }

}
data{
  int N;   // number of games
  int y[N,2];
  int nteams;
  int team1[N];
  int team2[N];
  int instants_rank[N];
  int ntimes_rank;                 // dynamic periods for ranking      
  matrix[ntimes_rank,nteams] ranking;
  int<lower=0, upper=1> ind_home;

  // priors part
  int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
  int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

  real hyper_df;
  real hyper_location;

  real hyper_sd_df;
  real hyper_sd_location;
  real hyper_sd_scale;
}
parameters{
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real beta;
  real rho;
  real home;
  real gamma;
  real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

}
transformed parameters{
  vector[nteams] att;
  vector[nteams] def;
  vector[3] theta[N];

  for (t in 1:nteams){
    att[t] = att_raw[t]-mean(att_raw);
    def[t] = def_raw[t]-mean(def_raw);
  }

  for (n in 1:N){
    theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                       (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
    theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                       (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
    theta[n,3] = exp(rho);
  }
}
model{
  // log-priors for team-specific abilities
  for (t in 1:(nteams)){
    if (prior_dist_num == 1){
      target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
      target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
    }
    else if (prior_dist_num == 2){
      target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
      target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
    }
    else if (prior_dist_num == 3){
      target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
      target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
    }
    else if (prior_dist_num == 4){
      target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
      target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
    }
  }


  // log-hyperpriors for sd parameters
  if (prior_dist_sd_num == 1 ){
    target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 2){
    target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 3){
    target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 4){
    target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }

  // log-priors fixed effects
  target+=normal_lpdf(rho|0,1);
  target+=normal_lpdf(home|0,5);
  target+=normal_lpdf(gamma|0,1);
  target+=uniform_lpdf(prob_of_draws|0,1);

  // likelihood

  for (n in 1:N){
    target+=diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                                  theta[n,2], theta[n,3],prob_of_draws);

    // if (y[n,1] == y[n,2]){// Alternative way as proposed by Stan manual
      //      target += log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
                                    //                         bernoulli_lpmf(0 | prob_of_draws)
                                    //                    + bipois_lpmf(y[n,] | theta[n,1],
                                                                        //  theta[n,2], theta[n,3]) );
      // } else {
        //  target += bernoulli_lpmf(0 |prob_of_draws)
        //            + bipois_lpmf(y[n,] |theta[n,1],
                                    //  theta[n,2], theta[n,3]);
        // }
  }
}


generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int diff_y_rep[N];

  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
    y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];

    log_lik[n] =diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                                      theta[n,2], theta[n,3],prob_of_draws);
    //    if (y[n,1] == y[n,2]){// Alternative way proposed by Stan documentation
      //  log_lik[n] = log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
                                   //                  bernoulli_lpmf(0 |prob_of_draws)
                                   //                   + bipois_lpmf(y[n,] | theta[n,1],
                                                                      //  theta[n,2], theta[n,3]));
      //} else {
        //   log_lik[n] = bernoulli_lpmf(0 |prob_of_draws)
        //    + bipois_lpmf(y[n,] | theta[n,1],
                            //  theta[n,2], theta[n,3]);
        //}
  }
}"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####


diag_infl_biv_pois_prev<-"
   functions{

    real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
      real ss;
      real log_s;
      real mus;
      int  miny;

      miny = min(r[1], r[2]);

      ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
        exp(mu3);
      if(miny > 0) {
        mus = -mu1-mu2+mu3;
        log_s = ss;

        for(k in 1:miny) {
          log_s = log_s + log(r[1] - k + 1) + mus
          + log(r[2] - k + 1)
          - log(k);
          ss = log_sum_exp(ss, log_s);
        }
      }
      return(ss);
    }
    real diag_infl_bipois_lpmf(int[] r , real mu1,real mu2,real mu3, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(bipois_lpmf(r| mu1, mu2,mu3));

      if (r[1] == r[2])
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

  }
data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int instants[N];
      int ntimes;    \\ Dynamic periods for ranking
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real rho;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      vector[3] theta[N];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,3] = exp(rho);
      }
}
model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood

      for (n in 1:N){
         target+=diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                   theta[n,2], theta[n,3],prob_of_draws);

   // if (y[n,1] == y[n,2]){// Alternative way as proposed by Stan manual
  //      target += log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
   //                         bernoulli_lpmf(0 | prob_of_draws)
     //                    + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]) );
     // } else {
       //  target += bernoulli_lpmf(0 |prob_of_draws)
         //            + bipois_lpmf(y[n,] |theta[n,1],
                 //  theta[n,2], theta[n,3]);
    // }
 }
}

generated quantities{
      int y_rep[N,2];
      int y_prev[N_prev,2];
      vector[3] theta_prev[N_prev];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
        y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                   theta[n,2], theta[n,3],prob_of_draws);
           //    if (y[n,1] == y[n,2]){// Alternative way proposed by Stan documentation
    //  log_lik[n] = log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
          //                  bernoulli_lpmf(0 |prob_of_draws)
           //                   + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]));
   //} else {
   //   log_lik[n] = bernoulli_lpmf(0 |prob_of_draws)
              //    + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]);
  //}
}

      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home*ind_home+att[team1_prev[n]]+
                                def[team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,3] = exp(rho);
        y_prev[n,1] = poisson_rng(theta_prev[n,1]+theta_prev[n,3]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]+theta_prev[n,3]);
      }
}"

##  ............................................................................
##  Dynamic Double Poisson                                                  ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####

double_pois_dynamic_fit<-"
    data{
      int N;                      // number of games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);

      // likelihood

      target+=poisson_lpmf(y[,1]| theta_home);
      target+=poisson_lpmf(y[,2]| theta_away);

    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta_home[n])+
          poisson_lpmf(y[n,2]| theta_away[n]);
      }
    }"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####

    double_pois_dynamic_prev<-"
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);


      // likelihood
      target+=poisson_lpmf(y[,1]| theta_home);
      target+=poisson_lpmf(y[,2]| theta_away);

    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];
      int y_prev[N_prev,2];
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta_home[n])+
          poisson_lpmf(y[n,2]| theta_away[n]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home*ind_home+att[instants_prev[n],team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_away_prev[n] = exp(att[instants_prev[n],team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));

        y_prev[n,1] = poisson_rng(theta_home_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]);
      }
    }"


##  ............................................................................
##  Static Double Poisson                                                   ####
    
      
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    
    double_pois_fit<-"
    data{
      int N;                      // number of games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      //real mu;
      real home;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;        // attack parameters
      vector[nteams] def;        // defence parameters
      vector[2] theta[N];        // exponentiated linear pred.


      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp( home*ind_home+att[team1[n]]+def[team2[n]] +
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp( att[team2[n]]+def[team1[n]] -
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      //target+=normal_lpdf(mu|0,5);
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);


      // likelihood
      //for (n in 1:N){
        target+=poisson_lpmf(y[,1]| theta[,1]);
        target+=poisson_lpmf(y[,2]| theta[,2]);
      //}
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta[n,1])+
          poisson_lpmf(y[n,2]| theta[n,2]);
      }
    }"

    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    

    double_pois_prev<-"
    data{
      int N;                      // number of games
      int N_prev;                 // number of predicted games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
      int team1_prev[N_prev];     // home team for pred.
      int team2_prev[N_prev];     // away team for pred.
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;        // attack parameters
      vector[nteams] def;        // defence parameters
      vector[2] theta[N];        // exponentiated linear pred.

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);

      // likelihood

      target+=poisson_lpmf(y[,1]| theta[,1]);
      target+=poisson_lpmf(y[,2]| theta[,2]);

    }
    generated quantities{
      int y_rep[N,2];
      int y_prev[N_prev,2];
      vector[2] theta_prev[N_prev];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta[n,1])+
          poisson_lpmf(y[n,2]| theta[n,2]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home*ind_home+att[team1_prev[n]]+def[team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,2] = exp(att[team2_prev[n]]+def[team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        y_prev[n,1] = poisson_rng(theta_prev[n,1]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]);
      }
    }"

    
##  ............................................................................
##  Dynamic Skellam                                                         ####
    
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    skellam_dynamic_fit<-"
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);

      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
      }
    }"


    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    
    skellam_dynamic_prev <- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);


      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta_home[n],
                             theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      vector[N_prev] theta_home_prev;
      vector[N_prev] theta_away_prev;
      int y_prev[N_prev,2];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
      }
      //out-of-sample predictions

      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home*ind_home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        y_prev[n,1] = poisson_rng(theta_home_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
    }"


##  ............................................................................
##  Static Skellam                                                          ####
    
        
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    
    skellam_fit <- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);

      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta[n,1], theta[n,2]);
      }
    }"

    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    

    skellam_prev<- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real gamma;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);


      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      int y_prev[N_prev,2];
      vector[2] theta_prev[N_prev];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta[n,1], theta[n,2]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home*ind_home+att[team1_prev[n]]+
                                def[team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        y_prev[n,1] = poisson_rng(theta_prev[n,1]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
    }"

    
##  ............................................................................
##  Dynamic Zero-Inflated Skellam                                           ####
    

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
zero_infl_skellam_dynamic_fit <- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }

       real zero_infl_skellam_lpmf(int k, real lambda1, real lambda2, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(skellam_lpmf(k| lambda1,lambda2));

      if (k== 0)
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

}

data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood
      for (n in 1:N){
        target+=zero_infl_skellam_lpmf(diff_y[n]| theta_home[n],
        theta_away[n],prob_of_draws);
      }
}
generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =zero_infl_skellam_lpmf(diff_y[n]| theta_home[n],
        theta_away[n],prob_of_draws);
      }
}"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####


zero_infl_skellam_dynamic_prev <- "functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }

       real zero_infl_skellam_lpmf(int k, real lambda1, real lambda2, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(skellam_lpmf(k| lambda1,lambda2));

      if (k== 0)
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

}
data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

}

transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home*ind_home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);


      // likelihood
      for (n in 1:N){

        target+=zero_infl_skellam_lpmf(diff_y[n]| theta_home[n],
        theta_away[n],prob_of_draws);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      vector[N_prev] theta_home_prev;
      vector[N_prev] theta_away_prev;
      int y_prev[N_prev,2];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =zero_infl_skellam_lpmf(diff_y[n]| theta_home[n],
        theta_away[n],prob_of_draws);
        }
      //out-of-sample predictions

      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home*ind_home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        y_prev[n,1] = poisson_rng(theta_home_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
    }"


##  ............................................................................
##  Static Zero-Inflated Skellam                                            ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####

  zero_infl_skellam_fit<-"
  functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }

       real zero_infl_skellam_lpmf(int k, real lambda1, real lambda2, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(skellam_lpmf(k| lambda1,lambda2));

      if (k== 0)
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

}
    data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
    model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood
      for (n in 1:N){
        target+=zero_infl_skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2],
        prob_of_draws);
      }
}
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =zero_infl_skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2],
        prob_of_draws);
      }
}"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####


  zero_infl_skellam_prev<-"
  functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }

       real zero_infl_skellam_lpmf(int k, real lambda1, real lambda2, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(skellam_lpmf(k| lambda1,lambda2));

      if (k== 0)
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

}
    data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int instants_rank[N];
      int ntimes_rank;                 // dynamic periods for ranking      
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home*ind_home+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }
    }
model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);


      // likelihood
      for (n in 1:N){
        target+=zero_infl_skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2],
        prob_of_draws);
      }
}

generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      int y_prev[N_prev,2];
      vector[2] theta_prev[N_prev];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =zero_infl_skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2],
        prob_of_draws);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home*ind_home+att[team1_prev[n]]+
                                def[team2_prev[n]]+
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]-
                         (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
        y_prev[n,1] = poisson_rng(theta_prev[n,1]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
}"



##  ............................................................................
##  Dynamic t-Student                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####

    student_t_dynamic_fit<-"
    data {
      int N;                       // number of matches
      int nteams;                  // number of teams
      int team1[N];                // team 1 indices
      int team2[N];                // team 2 indices
      matrix[N, 2] y;              // scores: column 1 is team1, column 2 is team2
      int ntimes;                  // number of dynamic periods for abilities
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants[N];             // time indices for abilities
      int instants_rank[N];        // time indices for rankings
      matrix[ntimes_rank, nteams] ranking; // rankings over time
      real nu;                     // degrees of freedom for the Student's t-distribution
    

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    transformed data {
      vector[N] diff_y = y[,1] - y[,2];  // modeled data
    }
    parameters {
      real beta;                           // Coefficient for the ranking
      matrix[ntimes, nteams] alpha;        // Team-specific abilities over time
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in the model
      real<lower=0> sigma_alpha;           // Standard deviation for alpha's prior
    }
    transformed parameters {
      //cov_matrix[ntimes] Sigma_alpha;
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;
      matrix[ntimes, nteams] mu_alpha;
      
      // Compute abilities for each match at the data point level
        for (n in 1:N) {
          ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[instants[n], team1[n]] * sigma_a;
          ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[instants[n], team2[n]] * sigma_a;
        }

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //   }}

      // Lagged prior mean for attack/defense parameters
      mu_alpha[1, ] = rep_row_vector(0, nteams);
      for (t in 2:ntimes) {
        mu_alpha[t, ] = alpha[t - 1, ];
      }

    }
    model {

      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1){
          alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 2){
          alpha[,h]~multi_student_t(hyper_df, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 3){
          alpha[,h]~multi_student_t(1, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_a|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_alpha|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }

      // Priors for beta and sigma_y
      beta ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);
    
      // Likelihood
      for (n in 1:N) {
        diff_y[n] ~ student_t(nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }
    generated quantities {
        // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
        // based on the current estimate of our parameters
        
        vector[N] diff_y_rep;
        vector[N] log_lik;
      
        for (n in 1:N) {
          diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
          log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability_team1[n] - ability_team2[n], sigma_y);
        }

}"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    
    student_t_dynamic_prev<- "
    data {
      int N;                       // number of matches
      int N_prev;                  // number of previous matches for prediction
      int nteams;                  // number of teams
      int team1[N];                // team 1 indices for N matches
      int team2[N];                // team 2 indices for N matches
      int team1_prev[N_prev];      // team 1 indices for N_prev matches
      int team2_prev[N_prev];      // team 2 indices for N_prev matches
      matrix[N, 2] y;              // observed scores: column 1 is team1, column 2 is team2
      real nu;                     // degrees of freedom for the Student's t-distribution
      int ntimes;                  // number of dynamic periods for abilities
      int time[ntimes];            // time periods for abilities
      int instants[N];             // time indices for abilities for N matches
      int instants_prev[N_prev];   // time indices for abilities for N_prev matches
      int instants_rank[N];        // time indices for rankings for N matches
      int ntimes_rank;             // number of dynamic periods for rankings
      matrix[ntimes_rank, nteams] ranking; // rankings over time
      
      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    
    transformed data {
      vector[N] diff_y = y[, 1] - y[, 2]; // Difference in scores
    }
    
    parameters {
      real beta;                           // Coefficient for the ranking
      matrix[ntimes, nteams] alpha;        // Team-specific abilities over time
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in the model
      real<lower=0> sigma_alpha;           // Standard deviation for alpha's prior
    }
    
    transformed parameters {
      //cov_matrix[ntimes] Sigma_alpha;
      // mixed effects model - common intercept + random effects
        vector[N] ability_team1;
        vector[N] ability_team2;
        vector[N_prev] ability_team1_prev;
        vector[N_prev] ability_team2_prev;
        matrix[ntimes, nteams] mu_alpha;
        
      // Compute abilities for each match in N
        for (n in 1:N) {
          ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[instants[n], team1[n]] * sigma_a;
          ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[instants[n], team2[n]] * sigma_a;
        }
        
     // Compute abilities for each match in N_prev
        for (n in 1:N_prev) {
          ability_team1_prev[n] = beta * ranking[instants_rank[N], team1_prev[n]] + alpha[instants_prev[n], team1_prev[n]] * sigma_a;
          ability_team2_prev[n] = beta * ranking[instants_rank[N], team2_prev[n]] + alpha[instants_prev[n], team2_prev[n]] * sigma_a;
        }
        
     // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //   }}

      // Lagged prior mean for attack/defense parameters
        mu_alpha[1, ] = rep_row_vector(0, nteams);
        for (t in 2:ntimes) {
          mu_alpha[t, ] = alpha[t - 1, ];
        }

    }
    model {
        // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1){
          alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 2){
          alpha[,h]~multi_student_t(hyper_df, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 3){
          alpha[,h]~multi_student_t(1, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_a|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_alpha|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }

     // Priors for beta and sigma_y
      beta ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);
    
     // Likelihood for observed data
      for (n in 1:N) {
        diff_y[n] ~ student_t(nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
  
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  vector[N_prev] diff_y_prev;


  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability_team1[n] - ability_team2[n], sigma_y);
  }

  for (n in 1:N_prev) {
    diff_y_prev[n] = student_t_rng(nu, ability_team1_prev[n] - ability_team2_prev[n], sigma_y);
  }

}"

    
##  ............................................................................
##  Static t-Student                                                        ####
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Fit                                                                     ####
    
    student_t_fit<-"
    data {
      int N;                       // number of matches
      int nteams;                  // number of teams
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants_rank[N];        // time indices for rankings for each match
      matrix[ntimes_rank, nteams] ranking; // rankings over time
    
      int team1[N];                // team 1 indices for N matches
      int team2[N];                // team 2 indices for N matches
      matrix[N, 2] y;              // observed scores: column 1 is team1, column 2 is team2
      real nu;                     // degrees of freedom for the Student's t-distribution
    
      // Priors part
      int<lower=1, upper=4> prior_dist_num;    // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace
      int<lower=1, upper=4> prior_dist_sd_num; // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace
    
      real hyper_df;
      real hyper_location;
    
      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    
    transformed data {
      vector[N] diff_y = y[, 1] - y[, 2]; // Difference in scores
    }
    
    parameters {
      real beta;                           // Coefficient for the ranking
      vector[nteams] alpha;                // Team-specific abilities
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in the model
      real<lower=0> sigma_alpha;           // Standard deviation for alpha's prior
    }
    
    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;

      for (n in 1:N) {
        ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[team1[n]] * sigma_a;
        ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[team2[n]] * sigma_a;
      }
    }
    
    model {
    // log-priors for team-specific abilities
        for (t in 1:nteams) {
          if (prior_dist_num == 1) {
            alpha[t] ~ normal(hyper_location, sigma_alpha);
          } else if (prior_dist_num == 2) {
            alpha[t] ~ student_t(hyper_df, hyper_location, sigma_alpha);
          } else if (prior_dist_num == 3) {
            alpha[t] ~ cauchy(hyper_location, sigma_alpha);
          } else if (prior_dist_num == 4) {
            alpha[t] ~ double_exponential(hyper_location, sigma_alpha);
          }
        }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_a|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_alpha|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
     // Priors for beta and sigma_y
        beta ~ normal(0, 2.5);
        sigma_y ~ normal(0, 2.5);

     // Likelihood
      for (n in 1:N) {
        diff_y[n] ~ student_t(nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }
    
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
      // now estimate a whole season's worth of games
      // based on the current estimate of our parameters
      vector[N] diff_y_rep; // Replicated differences for posterior predictive checks
      vector[N] log_lik;    // Log-likelihood for each observation
    
      for (n in 1:N) {
        diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
        log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }"


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Previsions                                                              ####
    
    student_t_prev<-"
    data {
      int N;                       // number of matches
      int N_prev;                  // number of predicted matches
      int nteams;                  // number of teams
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants_rank[N];        // time indices for rankings for each match
      int instants_rank_prev[N_prev]; // time indices for rankings for predicted matches
      matrix[ntimes_rank, nteams] ranking; // rankings over time
    
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      matrix[N, 2] y;
      real nu;
    
      // Priors part
      int<lower=1, upper=4> prior_dist_num;    // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace
      int<lower=1, upper=4> prior_dist_sd_num; // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace
    
      real hyper_df;
      real hyper_location;
    
      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    
    transformed data {
      vector[N] diff_y = y[, 1] - y[, 2];  // Modeled data
    }
    
    parameters {
      real beta;                           // Coefficient for the ranking
      vector[nteams] alpha;                // Team-specific abilities
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in our estimate
      real<lower=0> sigma_alpha;
    }
    
    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;
      vector[N_prev] ability_team1_prev;
      vector[N_prev] ability_team2_prev;

      // Compute abilities for each match at the data point level
      for (n in 1:N) {
        ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[team1[n]] * sigma_a;
        ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[team2[n]] * sigma_a;
      }

      // Compute abilities for predicted matches
      for (n in 1:N_prev) {
        ability_team1_prev[n] = beta * ranking[instants_rank[N], team1_prev[n]] + alpha[team1_prev[n]] * sigma_a;
        ability_team2_prev[n] = beta * ranking[instants_rank[N], team2_prev[n]] + alpha[team2_prev[n]] * sigma_a;
      }
  }
    
    model {
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(alpha[t]|hyper_location, sigma_alpha);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(alpha[t]|hyper_df, hyper_location, sigma_alpha);

        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(alpha[t]|hyper_location, sigma_alpha);

        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(alpha[t]|hyper_location, sigma_alpha);

        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_a|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_alpha|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }

      beta ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);
    
      // Likelihood
      for (n in 1:N) {
        diff_y[n] ~ student_t(nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }
    
    generated quantities {
     // posterior predictive check - carry along uncertainty!!!
     // now estimate a whole season's worth of games
     // based on the current estimate of our parameters
    vector[N] diff_y_rep;
    vector[N] log_lik;
    vector[N_prev] diff_y_prev;
  
    for (n in 1:N) {
      diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
      log_lik[n] = student_t_lpdf(diff_y[n]| nu, ability[team1[n]] - ability[team2[n]], sigma_y);
    }
  
    for (n in 1:N_prev) {
      diff_y_prev[n] = student_t_rng(nu, ability_team1_prev[n] - ability_team2_prev[n], sigma_y);
    }
}"

    models <- list(biv_pois_dynamic_fit,
                     biv_pois_dynamic_prev,
                     biv_pois_fit,
                     biv_pois_prev,
                     diag_infl_biv_pois_dynamic_fit,
                     diag_infl_biv_pois_dynamic_prev,
                     diag_infl_biv_pois_fit,
                     diag_infl_biv_pois_prev,
                     double_pois_dynamic_fit,
                     double_pois_dynamic_prev,
                     double_pois_fit,
                     double_pois_prev,
                     skellam_dynamic_fit,
                     skellam_dynamic_prev,
                     skellam_fit,
                     skellam_prev,
                     zero_infl_skellam_dynamic_fit,
                     zero_infl_skellam_dynamic_prev,
                     zero_infl_skellam_fit,
                     zero_infl_skellam_prev,
                     student_t_dynamic_fit,
                     student_t_dynamic_prev,
                     student_t_fit,
                     student_t_prev)

    right_number <- (1:length(models_name))[right_name==models_name]
    return(models[[right_number]])

}

  fit <- stan(model_code = stanfoot_models(model, dyn, type),
                       data= data_stan,
                       iter = user_dots$iter,
                       chains = user_dots$chains,
                       thin = user_dots$thin,
                       cores = user_dots$cores,
                       control = user_dots$control
                       )
  
  stanFoot_output <- list(fit = fit,
                          data = data_stan)
  return(stanFoot_output)

}

