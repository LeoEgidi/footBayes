#' Round-robin for football leagues
#'
#' Posterior predictive probabilities for a football season in a round-robin format
#'
#' @param object An object of class \code{\link[rstan]{stanfit}} or \code{stanFoot} as given by \code{stan_foot} function.
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param teams An optional character vector specifying team names to include. If \code{NULL}, all teams are included.
#'
#'@details
#'
#'For Bayesian models fitted via \code{stan_foot} the round-robin table is computed according to the
#'simulation from the posterior predictive distribution of future (out-of-sample) matches.
#'The dataset should refer to one or more seasons from a given national football league (Premier League, Serie A, La Liga, etc.).
#'
#'@return
#'
#'Round-robin plot with the home-win posterior probabilities computed from the ppd of the fitted model via the \code{stan_foot} function.
#'
#'
#'@author Leonardo Egidi \email{legidi@units.it}
#'
#'@examples
#'
#'\dontrun{
#'require(dplyr)
#'
#'data("italy")
#'italy_1999_2000<- italy %>%
#' dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#' dplyr::filter(Season == "1999"|Season=="2000")
#'
#'colnames(italy_1999_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#'fit <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
#'
#'foot_round_robin(fit, italy_1999_2000)
#'foot_round_robin(fit, italy_1999_2000, c("Parma AC", "AS Roma"))
#'
#'}
#' @import ggplot2
#' @importFrom dplyr as_tibble
#' @export


foot_round_robin <- function(object, data, teams = NULL){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  # # plot for the torunament "box"
  # colnames(data) <- c("season", "home", "away",
  #                     "homegoals", "awaygoals")

  if (inherits(object, "stanFoot")) {
    stan_fit <- object$fit
  } else if (inherits(object, "stanfit")) {
    stan_fit <- object
  } else {
    stop("Please provide an object of class 'stanfit' or 'stanFoot'.")
  }

  sims <- rstan::extract(stan_fit)
  y <- as.matrix(data[,4:5])
  teams_all <- unique(data$home_team)
  team_home <- match(data$home_team, teams_all)
  team_away <- match(data$away_team, teams_all)

  if (is.null(sims$diff_y_prev) & is.null(sims$y_prev)){
    stop("There is not any test set!
         Please, use this function only for
         out-of-samples predictions.")
  }

  if (!is.null(sims$diff_y_prev) & is.null(sims$y_prev)){
    # caso t-student
    N_prev <- dim(sims$diff_y_prev)[2]
    N <- dim(sims$diff_y_rep)[2]
    y_rep1 <- round(sims$diff_y_prev*(sims$diff_y_prev>0)+0*(sims$diff_y_prev<=0))
    y_rep2 <- round(abs(sims$diff_y_prev)*(sims$diff_y_prev<0)+0*(sims$diff_y_prev>=0))
    team1_prev <- team_home[(N+1):(N+N_prev)]
    team2_prev <- team_away[(N+1):(N+N_prev)]
  }

  if (!is.null(sims$diff_y_prev) & !is.null(sims$y_prev)){
    # caso skellam
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[,,1]
    y_rep2 <- sims$y_prev[,,2]
    team1_prev <- team_home[(N+1):(N+N_prev)]
    team2_prev <- team_away[(N+1):(N+N_prev)]
  }

  if (is.null(sims$diff_y_prev) & !is.null(sims$y_prev)){
    # caso double Poisson e biv Poisson
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[,,1]
    y_rep2 <- sims$y_prev[,,2]
    team1_prev <- team_home[(N+1):(N+N_prev)]
    team2_prev <- team_away[(N+1):(N+N_prev)]
  }

  # condizione per fare si che quando si prevede
  # solo l'ultima giornata, non venga considerata
  # solo la metà delle squadre
  if (length(unique(team1_prev)) !=
      length(unique(c(team1_prev, team2_prev)))  ){
    team1_prev <- c(team1_prev, team2_prev)
    team2_prev <- c(team2_prev, team1_prev)
  }

  if (is.null(teams)){
    teams <- teams_all[unique(team1_prev)]
  }
  team_index <- match(teams, teams_all)


  if (is.na(sum(team_index))){
    warning(paste(teams[is.na(team_index)],
    "is not in the test set. Pleasy provide a valid team name. "))
    team_index <- team_index[!is.na(team_index)]
  }

  team_names <- teams_all[team_index]
  nteams<- length(unique(team_home))
  nteams_new <- length(team_index)
  M <-dim(sims$diff_y_rep)[1]
  counts_mix <- matrix(0, nteams, nteams)
  number_match_days <- length(unique(team1_prev))*2-2
  punt <- matrix("-", nteams, nteams)

  #defaultW <- getOption("warn")
  #options(warn = -1)
  suppressWarnings(
  # questa condizione significa che siamo "dentro" alla #     # stagione e che il training ha le stesse squadre del      # test
  cond_1 <-   all(sort(unique(team_home))== sort(unique(team1_prev))) & N < length(unique(team1_prev))*( length(unique(team1_prev))-1)
  )
  # questa condizione significa che il training NON ha
  # le stesse squadre del test, e che stiamo considerando
  # dati di training di più stagioni
  suppressWarnings(
  cond_2 <- N > length(unique(team1_prev))*( length(unique(team1_prev))-1) &
    all(sort(unique(team_home))== sort(unique(team1_prev)))==FALSE &
    N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0
  )

  suppressWarnings(
  # questa condizione significa che siamo alla fine di una   # stagione
  cond_3 <-  N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))==0
  )
  #options(warn = defaultW)



  if (cond_1 == TRUE){
    for (n in 1:N){
      punt[team_home[n], team_away[n]] <-
          paste(y[n,1], "-", y[n,2], sep="")
    }
  }else if(cond_2 == TRUE){

    mod <- floor((N/ (length(unique(team1_prev))/2))/number_match_days)
    old_matches <- number_match_days*mod*length(unique(team1_prev))/2
    new_N <- seq(1+old_matches, N)

    for (n in new_N){
      punt[team_home[n], team_away[n]] <-
        paste(y[n,1], "-", y[n,2], sep="")
      }
    }

  for (n in 1: N_prev){
    prob<- sum(y_rep1[,n]> y_rep2[,n])/M
    counts_mix[unique(team1_prev[n]),
             unique(team2_prev[n])] <- prob
  }

  x1 = seq(0.5, nteams_new-1+0.5)
  x2 = seq(1.5, nteams_new-1+1.5)
  x1_x2 <- matrix(0, nteams_new, nteams_new)
  x2_x1 <- matrix(0, nteams_new, nteams_new)
  y1_y2 <- matrix(0, nteams_new, nteams_new)
  y2_y1 <- matrix(0, nteams_new, nteams_new)
  for (j in 1:nteams_new){
    x1_x2[j,j] = x1[j]
    x2_x1[j,j] = x2[j]
    y1_y2[j,j] = x1[j]
    y2_y1[j,j] = x2[j]
  }
  x_ex <- seq(1,nteams_new, length.out=nteams_new)
  y_ex <- seq(1,nteams_new, length.out=nteams_new)
   data_ex <- expand.grid(Home=x_ex, Away=y_ex)
   data_ex$prob=as.double(counts_mix[1:nteams, 1:nteams][team_index, team_index])

   p <- ggplot(data_ex, aes(Home, Away, z = prob)) +
     geom_tile(aes(fill = prob)) +
     theme_bw() +
     labs(x = x_ex, xaxis_text(size = rel(1.2))) +
     labs(y = y_ex, yaxis_text(size = rel(1.2))) +
     scale_fill_gradient(low = "white", high = "red3", name = "Prob") +  # Change legend title here
     scale_x_discrete(limits = team_names) +
     scale_y_discrete(limits = team_names) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     geom_text(aes(label = as.vector(punt[team_index, team_index])), size = 4.1) +
     geom_rect(aes(xmin = as.vector(x1_x2),
                   xmax = as.vector(x2_x1),
                   ymin = as.vector(x1_x2),
                   ymax = as.vector(x2_x1)),
               fill = "black", color = "black",
               size = 1) +
     ggtitle("Home win posterior probabilities")

   if (sum(data_ex$prob)==0){
      tbl <- cbind(teams[data_ex$Home], teams[data_ex$Away], as.vector(punt[team_index, team_index]))
      colnames(tbl) <- c("Home", "Away", "Observed")
      tbl <- dplyr::as_tibble(tbl) %>% dplyr::filter(Home!=Away)
      }else{
      tbl <- cbind(teams[data_ex$Home], teams[data_ex$Away], round(data_ex$prob,3),
                   as.vector(punt[team_index, team_index]))
      colnames(tbl) <- c("Home", "Away", "Home_prob", "Observed")
      tbl <- dplyr::as_tibble(tbl) %>% dplyr::filter(Home!=Away & Home_prob!=0 )
    }
   return(list(round_plot = p, round_table = tbl))

}
