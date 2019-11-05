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


stan_foot <- function(data, model, predict, n.iter = 200,
                     chains =4, trend = FALSE ){

  if (missing(predict)){
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else{
    N <- dim(data)[1]-predict
    N_prev <- predict
    type <- "prev"
  }

  if (trend ==TRUE){
    dyn <- "dynamic_"
  }else{
    dyn <-""
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
  y[,1] <- as.numeric(as.vector(data$homegoals)[1:N])
  y[,2] <- as.numeric(as.vector(data$awaygoals)[1:N])
  diff_y <- y[,1]-y[,2]

  # Stan data
  data <- list( y=y,
                spi_std = rep(0, nteams),
                diff_y = diff_y,
                N=N,
                N_prev = N_prev,
                nteams=nteams,
                team1 = team1,
                team2=team2,
                team1_prev= team1_prev,
                team2_prev=team2_prev,
                ntimes = 2,
                time =c(1,2),
                instants = c(rep(1, N/2), rep(2, N/2)))
  fit <- stan(file=paste(model,"_", dyn, type, ".stan", sep=""),
                       data= data,
                       iter=n.iter,
                       chains=4)
  return(fit)

}

