#' Fit any football model
#'
#'@param data a data frame, or a matrix containing the following mandatory items: home team, away team,
#'home goals, away goals.
#'@param model
#'@param predict
#'
#'
#'@examples
#'library(engsoccerdata)
#'library(tidyverse)
#'
#'### Use Italian Serie A from 2000 to 2002
#'
#'italy <- as_tibble(italy)
#'italy_2000_2002<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000" |  Season=="2001"| Season=="2002")
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
#' print(fit2, pars =c("home", "rho"
#'                     "sigma_att", "sigma_def"))
#'
#' fit3 <- stan_foot(data = italy_2000_2002,
#'                 model="skellam")     # skellam
#' print(fit3, pars =c("home", "sigma_att",
#'                     "sigma_def"))
#'
#' fit4 <- stan_foot(data = italy_2000_2002,
#'                 model="student_t")   # student_t
#' print(fit4, pars =c("home", "beta")
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
#'@export


stan_foot <- function(data,
                      model,
                      predict,
                      n.iter = 200,
                      chains =4,
                      dynamic_type = FALSE){


  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))

  if (missing(predict)){ # check on predict
    predict <- 0
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else if (is.numeric(predict)){
    N <- dim(data)[1]-predict
    N_prev <- predict
    type <- "prev"
  }else{
    stop("The number of out-of-sample matches is ill posed!
         Pick up an integer number.")
  }

  if (missing(dynamic_type)){
    dyn <-""
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
              please consider more than one season.")
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
    }else if (dynamic_type =="FALSE"){
      dyn <-""
    }else{
      stop("The type of dynamics is not correct.
           Choose one among 'weekly' or 'seasonal'.")
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

  # Stan data
  data_stan <- list( y=y,
                spi_std = rep(0, nteams),
                diff_y = diff_y,
                N=N,
                N_prev = N_prev,
                nteams=nteams,
                team1 = team1,
                team2=team2,
                team1_prev= team1_prev,
                team2_prev=team2_prev)

  if (!missing(dynamic_type)){
    data_stan$ntimes <- ntimes
    data_stan$instants <- instants
    data_stan$time <- time
    data_stan$instants_prev <- instants_prev
  }
  fit <- stan(file=paste(model,"_", dyn, type, ".stan", sep=""),
                       data= data_stan,
                       iter=n.iter,
                       chains=4)
  return(fit)

}


# =fit,
# nteams = nteams,
# teams = teams,
# team1 = team1,
# team2= team2,
# team1_prev = team1_prev,
# team2_prev = team2_prev,
# instants = instants,
# instants_prev = instants_prev

