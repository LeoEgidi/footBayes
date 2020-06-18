colnames(data) <- c("season", "home", "away",
                    "homegoals", "awaygoals")
y1 <- data$homegoals
y2 <- data$awaygoals
N <- length(y1)
teams <- unique(data$home)
nteams <- length(teams)
team_home <- match( data$home, teams)
team_away <- match( data$away, teams)
team1 <- team_home[1:N]
team2 <- team_away[1:N]

# optim requires parameters to be supplied as a vector
# we'll unlist the parameters then relist in the function
relist_params <- function(parameters) {
  parameter_list <- list(
    # alpha = attack rating
    att = parameters %>%
      .[grepl("att", names(.))] %>%
      `names<-`(teams),
    # beta = defence rating
    def = parameters %>%
      .[grepl("def", names(.))] %>%
      `names<-`(teams),
    # gamma = home field advantage
    home = parameters["home"]
  )

  return(parameter_list)
}

double_pois_lik <- function(parameters, y1, y2, team1, team2){

  param_list <- relist_params(parameters)
  home_log_lik = away_log_lik = c()
  theta <- matrix(NA, N, 2)
  att <- param_list$att
  def <- param_list$def
  home <- param_list$home

  # sum-to-zero constraints

  for (n in 1:N){
    theta[n,1] <- exp(home + att[team1[n]] + def[team2[n]])
    theta[n,2] <- exp(att[team2[n]] + def[team1[n]])
    home_log_lik[n] <- dpois(y1[n], lambda = theta[n,1], log = TRUE)
    away_log_lik[n] <- dpois(y2[n], lambda = theta[n,2], log = TRUE )
  }
  return(-sum(home_log_lik + away_log_lik))
}



equal_parameters <- list(
  att = rep(0, length(teams)) %>% `names<-`(teams),
  def = rep(0, length(teams)) %>% `names<-`(teams),
  home = 2
)


#double_pois_lik <- Vectorize(double_pois_lik)
double_pois_fit <- optim(par = unlist(equal_parameters),
                     fn = double_pois_lik,
                     team1 = team1, team2=team2,
                     y1=y1, y2=y2)

# extract parameters

att_raw <- as.vector(double_pois_fit$par%>%
                  .[grepl("att", names(.))])
def_raw <- as.vector(double_pois_fit$par%>%
                   .[grepl("def", names(.))])



att <- att_raw - mean(att_raw)
def <- def_raw - mean(def_raw)

