#' Fit football models with Maximum Likelihood
#'
#' ML football modelling for the most famous models:
#' double Poisson, bivariate Poisson, Skellam and student t.
#'
#'@param data A data frame, or a matrix containing the following mandatory items: season, home team, away team,
#'home goals, away goals.
#'@param model The type of model used to fit the data.
#'             One among the following: \code{"double_pois"},
#'             \code{"biv_pois"}, \code{"skellam"}, \code{"student_t"}.
#'@param predict The number of out-of-sample matches. If missing, the function returns
#'the fit for the training set only.
#'@param ... Optional arguments for MLE fit algorithms.
#'
#'@return
#'
#' MLE and 95\% profile likelihood deviance confidence intervals for the
#' model's parameters: attack, defence, home effect and goals' correlation.
#'
#'@details
#'
#'See documentation of \code{stan_foot} function for model details.
#'MLE can be obtained only for static models, with no time-dependence.
#'Likelihood optimization is performed via the \code{BFGS} method
#'of the \code{\link{optim}} function.
#'
#'@author Leonardo Egidi \email{legidi@units.it}
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
#'\donttest{
#'if(requireNamespace("engsoccerdata")){
#'require(engsoccerdata)
#'require(tidyverse)
#'require(dplyr)
#'
#'italy <- as_tibble(italy)
#'italy_2008<- italy %>%
#'    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'    dplyr::filter( Season=="2008")
#'
#'mle_fit <- mle_foot(data  = italy_2008,
#'                    model = "double_pois")
#' }
#'}
#'
#'
#' @importFrom extraDistr dbvpois
#' @importFrom extraDistr rbvpois
#' @importFrom extraDistr dskellam
#' @importFrom extraDistr rskellam
#' @importFrom metRology dt.scaled
#' @importFrom metRology rt.scaled
#' @importFrom parallel clusterExport
#' @importFrom parallel makeCluster
#' @importFrom numDeriv hessian
#' @importFrom magrittr "%>%"
#' @export
#'

mle_foot <- function(data, model, predict, ...){

  ## DATA CHECKS
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


  ## OPTIONAL ARGUMENTS CHECKS

  user_dots <- list(maxit = 1000,
                    method = "BFGS",
                    interval = "profile",
                    hessian = FALSE,
                    n.iter = 200,
                    sigma_y = 1 # for student-t
                    )

  if (missing(...)){
    user_dots <- user_dots
  }else{
    user_dots_prel <- list(...)
    names_prel <- names(user_dots_prel)
    names_dots<- names(user_dots)
    for (u in 1:length(names_prel)){
      user_dots[names_prel[u] == names_dots]<- user_dots_prel[u]
    }
  }
  if (user_dots$interval == "Wald" ){
    user_dots$hessian <- TRUE
    #stop("Select 'hessian=TRUE' to compute Wald intervals")
  }



  good_names <- c("double_pois",
                "biv_pois",
                "skellam",
                "student_t")
  model <- match.arg(model, good_names)

  colnames(data) <- c("season", "home", "away",
                    "homegoals", "awaygoals")

  # checks sui formati
  if ( !is.numeric(data$homegoals) |!is.numeric(data$awaygoals)){
    stop("Goals are not numeric! Please, provide
         numeric values for the goals")
  }

  # conditions about dimensions
  if (dim(data)[2]>5){
    warning("Your dataset seems too large!
            The function will evaluate the first
            five columns as follows:
            season, home team, away team, home goals,
            away goals")
  }

  ## PREDICT CHECKS

  if (missing(predict)){ # check on predict
    predict <- 0
    N <- dim(data)[1]
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


  y1 <- data$homegoals[1:N]
  y2 <- data$awaygoals[1:N]
  N <- length(y1)
  teams <- unique(data$home)
  nteams <- length(teams)
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)
  team1 <- team_home[1:N]
  team2 <- team_away[1:N]
  team1_prev <- team_home[(N+1):(N+N_prev)]
  team2_prev <- team_away[(N+1):(N+N_prev)]

  # optim requires parameters to be supplied as a vector
  # we'll unlist the parameters then relist in the function
  relist_params <- function(parameters) {
    parameter_list <- list(
      # att = attack rating
      att = parameters %>%
        .[grepl("att", names(.))] %>%
        append(prod(sum(.), -1), .) %>%  # sum-to-zero constraints
        `names<-`(teams),
      # def = defence rating
      def = parameters %>%
        .[grepl("def", names(.))] %>%
        append(prod(sum(.), -1), .) %>%  # sum-to-zero constraints
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

######################
# Likelihood functions
######################

  # double poisson
  double_pois_lik <- function(parameters, y1, y2, team1, team2){

    param_list <- relist_params(parameters)
    home_log_lik = away_log_lik = c()
    theta <- matrix(NA, N, 2)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home

    for (n in 1:N){
      theta[n,1] <- exp(home + att[team1[n]] + def[team2[n]])
      theta[n,2] <- exp(att[team2[n]] + def[team1[n]])
      home_log_lik[n] <- dpois(y1[n], lambda = theta[n,1], log = TRUE)
      away_log_lik[n] <- dpois(y2[n], lambda = theta[n,2], log = TRUE )
    }
  return(-sum(home_log_lik + away_log_lik))
  }

  # bivariate poisson
  biv_pois_lik <- function(parameters, y1, y2, team1, team2){

    param_list <- relist_params(parameters)
    log_lik <- c()
    theta <- matrix(NA, N, 3)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home
    const <- param_list$const

    for (n in 1:N){
      theta[n,1] <- exp(home + att[team1[n]] + def[team2[n]])
      theta[n,2] <- exp(att[team2[n]] + def[team1[n]])
      theta[n,3] <- exp(const)
      log_lik[n] <- dbvpois(y1[n], y2[n], a = theta[n,1],
                          b=theta[n,2], c = theta[n,3],
                          log = TRUE)

    }
    return(-sum(log_lik))
  }

  # skellam
  skellam_lik <- function(parameters, y1, y2, team1, team2){

    param_list <- relist_params(parameters)
    log_lik <- c()
    theta <- matrix(NA, N, 2)
    att <- param_list$att
    def <- param_list$def
    home <- param_list$home

    for (n in 1:N){
      theta[n,1] <- exp(home + att[team1[n]] + def[team2[n]])
      theta[n,2] <- exp(att[team2[n]] + def[team1[n]])
      log_lik[n] <- dskellam(y1[n]- y2[n],
                            mu1 = theta[n,1],
                            mu2 = theta[n,2], log = TRUE)
    }
    return(-sum(log_lik))
  }

  # student t
  student_t_lik <- function(parameters, y1, y2, team1, team2){

    param_list <- relist_params(parameters)
    log_lik <- c()
    ability <- param_list$att + param_list$def
    home <- param_list$home
    sigma_y <- as.numeric(param_list$sigma_y)

    for (n in 1:N){
      log_lik[n] <- dt.scaled(x = y1[n]- y2[n], df = 7,
                       mean = home + ability[team1[n]] - ability[team2[n]],
                       sd = sigma_y,
                       log = TRUE)
    }
    return(-sum(log_lik))
  }

  ## parameters initialization
  ## (remove the first team from the attack and defence ratings)
  equal_parameters <- list(
    att = rep(0, length(teams)-1) %>% `names<-`(teams[2:length(teams)]),
    def = rep(0, length(teams)-1) %>% `names<-`(teams[2:length(teams)]),
    home = 2,
    const = 1, # for bivariate poisson
    sigma_y = user_dots$sigma_y # for student_t
    )


  ## mle fit
  mle_fit <- optim(par = unlist(equal_parameters),
                     fn = eval(parse(text=paste(model, "_lik", sep=""))),
                     team1 = team1, team2=team2,
                     y1=y1, y2=y2,
                     method = user_dots$method,
                     hessian = user_dots$hessian,
                     control = list(maxit = user_dots$maxit))

  # compute likelihood confidence intervals


    fn <- eval(parse(text=paste(model, "_lik", sep="")))
    mle_value <- -fn(mle_fit$par, team1 = team1,
                     team2=team2,
                     y1=y1, y2=y2)

    ci <- matrix(NA,(2*nteams),2)
    # profile likelihood intervals (default)
    if (user_dots$interval == "profile"){

      index <- function(j){
        profile <- function(x){
        parameters <- mle_fit$par
        parameters[j] <- x
        return(-fn(parameters, team1 = team1,
                   team2=team2,
                   y1=y1, y2=y2))
      }
    # defining likelihood inverse for the profile likelihood ci's
    profile <- Vectorize(profile, "x")
    h <- mle_value - pchisq(0.95, 1)/2
    #curve(profile(x), -1,1)
    #abline(h = h , col="red")
    x <- seq(-5,5, 0.01)
    f_v <- profile(x)
    return(c(min(x[f_v>=h]), max(x[f_v>=h])))
      }

      ci_out <- sapply(c(1:(2*nteams)), index)
      ci <- t(ci_out)

    # Wald-type intervals (only if hessian = TRUE)
    }else if(user_dots$interval == "Wald"){

      ci[1:(2*nteams-2),1] <- round(mle_fit$par[1:(2*nteams-2)] -1.96*sqrt( diag(solve(mle_fit$hessian[1:(2*nteams-2), 1:(2*nteams-2)])) ),2)
      ci[1:(2*nteams-2),2] <- round(mle_fit$par[1:(2*nteams-2)] +1.96*sqrt( diag(solve(mle_fit$hessian[1:(2*nteams-2), 1:(2*nteams-2)])) ),2)
      ci[2*nteams-1, 1] <- round(mle_fit$par[2*nteams-1]-1.96*sqrt(solve(mle_fit$hessian[2*nteams-1, 2*nteams-1 ])),2)
      ci[2*nteams-1, 2] <- round(mle_fit$par[2*nteams-1]+1.96*sqrt(solve(mle_fit$hessian[2*nteams-1, 2*nteams-1 ])),2)
      if (model == "biv_pois"){
        hessian <-  hessian(func = fn, x  = unlist(equal_parameters), team1 = team1, team2=team2,
                            y1=y1, y2=y2)
      #ci[2*nteams, 1] <- mle_fit$par[2*nteams]-1.96*sqrt(solve(mle_fit$hessian[2*nteams, 2*nteams]))
      #ci[2*nteams, 2] <- mle_fit$par[2*nteams]+1.96*sqrt(solve(mle_fit$hessian[2*nteams, 2*nteams]))
      ci[2*nteams, 1] <- mle_fit$par[2*nteams]-1.96*sqrt(solve(hessian[2*nteams, 2*nteams]))
      ci[2*nteams, 2] <- mle_fit$par[2*nteams]+1.96*sqrt(solve(hessian[2*nteams, 2*nteams]))
      }
      }

  # extract parameters and reparametrization for
  #    the first team
  att <- c(- sum(as.vector(mle_fit$par%>%
                          .[grepl("att", names(.))])),
             as.vector(mle_fit$par%>%
              .[grepl("att", names(.))]))
  def <- c(-sum(as.vector(mle_fit$par%>%
                          .[grepl("def", names(.))])),
            as.vector(mle_fit$par%>%
                     .[grepl("def", names(.))]))
  home <- as.numeric(mle_fit$par%>%
                     .[grepl("home", names(.))])
  corr_par <- round(exp(as.numeric(mle_fit$par%>%
                            .[grepl("const", names(.))])),2)
  abilities <- c(- sum(as.vector(mle_fit$par%>%
                                 .[grepl("att", names(.))])+
                         as.vector(mle_fit$par%>%
                                   .[grepl("def", names(.))])),
                 as.vector(mle_fit$par%>%
                             .[grepl("att", names(.))])+
                   as.vector(mle_fit$par%>%
                               .[grepl("def", names(.))]))
  ## Final tables
  att_est = def_est = abilities_est =  matrix(NA, nteams, 3)
  home_est = corr_est = matrix(NA,1,3)

  att_est[1,1] <- round(att[1],2) # da aggiustare...
  att_est[1,2] <- round(att[1],2)
  att_est[1,3] <- round(att[1],2) # da aggiustare...
  def_est[1,1] <- round(def[1],2) # da aggiustare...
  def_est[1,2] <- round(def[1],2)
  def_est[1,3] <- round(def[1],2) # da aggiustare...
  abilities_est[1,1] <- round(abilities[1],2) # da aggiustare...
  abilities_est[1,2] <- round(abilities[1],2)
  abilities_est[1,3] <- round(abilities[1],2) # da aggiustare...
  att_est[2:nteams,1] <- ci[1:(nteams-1),1]
  att_est[2:nteams,2] <- round(att[2:nteams],2)
  att_est[2:nteams,3] <- ci[1:(nteams-1),2]
  def_est[2:nteams,1] <- ci[(nteams):(2*nteams-2),1]
  def_est[2:nteams,2] <- round(def[2:nteams],2)
  def_est[2:nteams,3] <- ci[(nteams):(2*nteams-2),2]
  abilities_est[2:nteams,1] <- ci[1:(nteams-1),1] + ci[(nteams):(2*nteams-2),1]
  abilities_est[2:nteams,2] <- round(abilities[2:nteams],2)
  abilities_est[2:nteams,3] <- ci[1:(nteams-1),2] + ci[(nteams):(2*nteams-2),2]
  home_est[1,2] <- round(home,2)
  home_est[1,1] <- ci[2*nteams-1,1]
  home_est[1,3] <- ci[2*nteams-1,2]
  corr_est[1,2] <- round(corr_par,2)
  corr_est[1,1] <- round(exp(ci[2*nteams,1]),2)
  corr_est[1,3] <- round(exp(ci[2*nteams,2]),2)
  if (corr_est[1,2] ==0){
    corr_est[1,1] = corr_est[1,3] = 0
  }
  rownames(att_est) <- teams
  colnames(att_est) <- c("2.5%", "mle", "97.5%")
  rownames(def_est) <- teams
  colnames(def_est) <- c("2.5%", "mle", "97.5%")
  rownames(abilities_est) <- teams
  colnames(abilities_est) <- c("2.5%", "mle", "97.5%")
  colnames(corr_est) <- c("2.5%", "mle", "97.5%")
  colnames(home_est) <- c("2.5%", "mle", "97.5%")


  if (model=="student_t"){
    return(list(abilities = abilities_est,
                home = home_est,
                model = model,
                predict = predict,
                n.iter = user_dots$n.iter,
                sigma_y = user_dots$sigma_y,
                team1_prev = team1_prev,
                team2_prev = team2_prev))

  }else if (model=="biv_pois"){
    return(list(att = att_est,
                def = def_est,
                home = home_est,
                corr = corr_est,
                model = model,
                predict = predict,
                n.iter = user_dots$n.iter,
                team1_prev = team1_prev,
                team2_prev = team2_prev))

  }else{
    return(list(att = att_est,
                def = def_est,
                home = home_est,
                model = model,
                predict = predict,
                n.iter = user_dots$n.iter,
                team1_prev = team1_prev,
                team2_prev = team2_prev))

  }


}

