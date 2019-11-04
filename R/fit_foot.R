#' Fit any football model
#'
#'@param data a data frame, or a matrix containing the following mandatory items: home team, away team,
#'home goals, away goals.
#'@param
#'@param predict
#'
#'
#'@examples
#'library(engsoccerdata)
#'library(tidyverse)
#'
#'ristr_italy <- as_data_frame(italy)
#'ristr_italy<- ristr_italy %>%
#'  select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000")
#'


fit_foot <- function(data, model, predict, n.iter = 2000){

  if (missing(predict)){
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else{
    N <- dim(data)[1]-predict
    N_prev <- predict
    type <- "prev"
  }
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))
  teams <- unique(data$home)
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)
  team1 <- team_home[1:N]
  team2 <- team_away[1:N]
  team1_prev <- team_home[(N+1):(N+N_prev)]
  team2_prev <- team_away[(N+1):(N+N_prev)]
  y <- matrix(NA, N, 2)
  y[,1] <- as.numeric(as.vector(data$homegoals))
  y[,2] <- as.numeric(as.vector(data$awaygoals))

  # Stan data
  data_poisson <- list( y=y,
                        N=N,
                        N_prev = N_prev,
                        nteams=nteams,
                        team1 = team1,
                        team2=team2,
                        team1_prev= team1_prev,
                        team2_prev=team2_prev)
  stan_poisson <- stan(file=paste(model,"_", type, ".stan", sep=""),
                       data= data_poisson,
                       iter=n.iter,
                       chains=4)

}
