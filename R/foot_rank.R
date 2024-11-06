#' Rank and points predictions
#'
#' Posterior predictive plots and final rank table for football seasons.
#'
#' @param object An object of class \code{\link[rstan]{stanfit}} or \code{stanFoot} as given by \code{stan_foot} function.
#' @param data A data frame, or a matrix containing the following mandatory items: home team, away team,
#'home goals, away goals.
#' @param team_sel Selected team(s). By default, all the teams are selected.
#' @param visualize Type of plot, default is \code{"aggregated"}.
#'
#' @return
#'
#' Final rank tables and plots with the predicted points for the selected teams as given by the models fitted via the \code{stan_foot}
#' function.
#'
#' @details
#'
#'For Bayesian models fitted via \code{stan_foot} the final rank tables are computed according to the
#'simulation from the posterior predictive distribution of future (out-of-sample) matches.
#'The dataset should refer to one or more seasons from a given national football league (Premier League, Serie A, La Liga, etc.).
#'
#'
#'
#' @author Leonardo Egidi \email{legidi@units.it}
#'
#' @examples
#'
#' \dontrun{
#' require(tidyverse)
#' require(dplyr)
#'
#' data("italy")
#' italy_1999_2000<- italy %>%
#' dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#' dplyr::filter(Season == "1999"|Season=="2000")
#'
#' fit <- stan_foot(italy_1999_2000, "double_pois", iter = 200)
#' foot_rank(italy_1999_2000, fit)
#' foot_rank(italy_1999_2000, fit, visualize =  "individual")
#'  }
#'
#' @importFrom reshape2 melt
#' @importFrom bayesplot color_scheme_get
#' @export

foot_rank <- function(data, object,
                      team_sel,
                      visualize = c("aggregated","individual"))
  {
  #checks
  if (inherits(object, "stanFoot")) {
    stan_fit <- object$fit
  } else if (inherits(object, "stanfit")) {
    stan_fit <- object
  } else {
    stop("Please provide an object of class 'stanfit' or 'stanFoot'.")
  }

  good_names <- c("aggregated","individual")
  check_vis <- match.arg(visualize, good_names)
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))
  sims <- rstan::extract(stan_fit)
  y <- as.matrix(data[,4:5])
  teams <- unique(data$home)
  team_home <- match(data$home, teams)
  team_away <- match(data$away, teams)
  seasons_levels <-unique(data$season)
  team_seasons <- list()
  for (j in 1:length(seasons_levels))
    team_seasons[[j]] <-
    unique(team_home[data$season == seasons_levels[j]])

  # caso in-sample
  if (is.null(sims$diff_y_prev) & is.null(sims$y_prev)){
    if (!is.null(sims$y_rep)){
    N <- dim(data)[1]
    N_prev <- 0
    y_rep1 <- sims$y_rep[,,1]
    y_rep2 <- sims$y_rep[,,2]
    team1_prev <- team_home[1:N]
    team2_prev <- team_away[1:N]
    }else{
      # caso t di student
      N <- dim(data)[1]
      N_prev <- 0
      y_rep1 <- round(sims$diff_y_rep*(sims$diff_y_rep>0)+0*(sims$diff_y_rep<=0))
      y_rep2 <- round(abs(sims$diff_y_rep)*(sims$diff_y_rep<0)+0*(sims$diff_y_rep>=0))
      team1_prev <- team_home[1:N]
      team2_prev <- team_away[1:N]
    }
  }


  # caso out-of-sample
  if (!is.null(sims$diff_y_prev) & is.null(sims$y_prev)){
    # caso t-student
    N_prev <- dim(sims$diff_y_prev)[2]
    N <- dim(sims$diff_y_rep)[2]
    y_rep1 <- round(sims$diff_y_prev*(sims$diff_y_prev>0)+0*(sims$diff_y_prev<=0))
    y_rep2 <- round(abs(sims$diff_y_prev)*(sims$diff_y_prev<0)+0*(sims$diff_y_prev>=0))
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

  if (!is.null(sims$diff_y_prev) & !is.null(sims$y_prev)){
    # caso skellam
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[,,1]
    y_rep2 <- sims$y_prev[,,2]
    team1_prev <- team_home[(N+1):(N+N_prev)]
    team2_prev <- team_away[(N+1):(N+N_prev)]
  }

  # identifica la stagione all'interno del quale prevedere
  season_prev <- unique(data$season[(N+1):(N+N_prev)])
  season_prev <- season_prev[!is.na(season_prev)]

  # condizione per far si che non si possano prevedere
  # dati di stagioni diverse
  if (length(unique(data$season[(N+1):(N+N_prev)][!is.na(data$season[(N+1):(N+N_prev)])])) !=1){
    stop("Please, to use this function,
          do not provide out-of-sample
          matches belonging to different seasons, provide
          only out-of samples matches from one season.
          Consider to refit the model.")

  # warning("Please, do not provide out-of-sample
  #     matches belonging to different seasons, provide
  #     only matches from one season.")
  #
  #   prev_indexes <- (N+1):(N+N_prev)
  #   prev_values <- unique(data$season[prev_indexes])
  #   useful_indexes <- prev_indexes[
  #           data$season[prev_indexes]== prev_values[1]]
  #   N_prev <- length(useful_indexes)
  #   season_prev <- prev_values[1]
  #
  #
  #   if (!is.null(sims$diff_y_prev) &
  #         is.null(sims$y_prev)){
  #     # t di student
  #     y_rep1 <- round(sims$diff_y_prev[,1:N_prev]*
  #         (sims$diff_y_prev[,1:N_prev]>0)+
  #           0*(sims$diff_y_prev[, 1:N_prev]<=0))
  #     y_rep2 <- round(abs(sims$diff_y_prev[, 1:N_prev])*
  #         (sims$diff_y_prev[, 1:N_prev]<0)+
  #           0*(sims$diff_y_prev[,1:N_prev]>=0))
  #   }
  #
  #   if (is.null(sims$diff_y_prev) &
  #         !is.null(sims$y_prev)){
  #     # caso double Poisson e biv Poisson
  #     y_rep1 <- sims$y_prev[,1:N_prev,1]
  #     y_rep2 <- sims$y_prev[,1:N_prev,2]
  #   }
  #
  #   if (!is.null(sims$diff_y_prev) &
  #         !is.null(sims$y_prev)){
  #     # skellam
  #     y_rep1 <- sims$y_prev[,,1]
  #     y_rep2 <- sims$y_prev[,,2]
  #   }
  #
  #   team1_prev <- team_home[useful_indexes]
  #   team2_prev <- team_away[useful_indexes]
  }

  if(missing(visualize)){
      visualize <- "aggregated"
    }

  # condizione per fare si che quando si prevede
  # solo l'ultima giornata, vengano considrate tutte le
  # squadre

  ind_season_prev <-
    (1:length(seasons_levels))[season_prev ==
        seasons_levels]

  if (N_prev < length(team_seasons[[ind_season_prev]])/2 & N_prev!=0){
      stop(paste("The number of out-of-samples matches
                  is too small,  then is forced to be zero.
                  Please, to allow for out-of-samples matches, consider to
                  refit the model with the argument predict greater
                  or equal than",
        length(team_seasons[[ind_season_prev]])/2 ))


    sims$diff_y_prev <- as.null(sims$diff_y_prev)
    sims$y_prev <- as.null(sims$y_prev)

    # consider in-sample case
    if (!is.null(sims$y_rep)){
      N <- dim(data)[1] - N_prev
      #N_prev <- N
      y_rep1 <- sims$y_rep[,,1]
      y_rep2 <- sims$y_rep[,,2]
      team1_prev <- team_home[1:N]
      team2_prev <- team_away[1:N]
    }else{
      # caso t di student
      N <- dim(data)[1]-N_prev
      #N_prev <- N
      y_rep1 <- round(sims$diff_y_rep*(sims$diff_y_rep>0)+0*(sims$diff_y_rep<=0))
      y_rep2 <- round(abs(sims$diff_y_rep)*(sims$diff_y_rep<0)+0*(sims$diff_y_rep>=0))
      team1_prev <- team_home[1:N]
      team2_prev <- team_away[1:N]
    }

  }

  # questa condizione è sbagliata? si, per le ultime giornate è sbagliata!
  if (length(unique(team1_prev)) !=
      length(unique(c(team1_prev, team2_prev)))  ){
     stop("Please, select more out-of-sample matches through
           the argument 'predict' of the 'stan_foot' function
           (hint: select at least two complete match-days for
            out-of-sample predictions)")
     # team1_prev_temp <- c(team1_prev, team2_prev)
     # team2_prev_temp <- c(team2_prev, team1_prev)
     # team1_prev <- team1_prev_temp
     # team2_prev <- team2_prev_temp
   }

  ## condizione fondamentale per in-sample o out-of-sample
  in_sample_cond <- is.null(sims$diff_y_prev) & is.null(sims$y_prev)

  if (in_sample_cond){
    set <- (1:N)[data$season==season_prev]
  }else{
    set <- (1:N_prev)
  }

  if (missing(team_sel)){
    team_sel <- teams[unique(team1_prev[set])]
  }else if (all(team_sel =="all")){
    team_sel <- teams[unique(team1_prev[set])]
  }
  team_index <- match(team_sel, teams)
  if (is.na(sum(team_index))){
    warning(paste(team_sel[is.na(team_index)],
                  "is not in the test set. Pleasy provide a valid team name. "))
    team_index <- team_index[!is.na(team_index)]
  }
  team_names <- teams[team_index]

  M <-dim(sims$diff_y_rep)[1]
  ngames_train <- dim(sims$y_rep)[2]
  conta_punti <- matrix(0, M, length(teams))
  conta_punti_veri <- rep(0, length(teams))
  number_match_days <- length(unique(team1_prev))*2-2

  fill_test <- c("yellow", "yellow")[c(!in_sample_cond, in_sample_cond)]

  #defaultW <- getOption("warn")
  #options(warn = -1)

  # questa condizione significa che siamo "dentro" alla #     # stagione e che il training ha le stesse squadre del      # test
  suppressWarnings(
  cond_1 <-   all(sort(unique(team_home))== sort(unique(team1_prev)))
  #& N < length(unique(team1_prev[(1:N)[data$season==season_prev]]))*( length(unique(team1_prev[(1:N)[data$season==season_prev]]))-1)
  )

  # questa condizione significa che il training NON ha
  # le stesse squadre del test, e che stiamo considerando
  # dati di training di più stagioni
  suppressWarnings(
  cond_2 <- N > length(unique(team1_prev))*( length(unique(team1_prev))-1) &
    all(sort(unique(team_home))== sort(unique(team1_prev)))==FALSE &
    N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0
  )

  # questa condizione significa che siamo alla fine di una   # stagione
  suppressWarnings(
  cond_3 <-  N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))==0
  )
  #options(warn = defaultW)



  if (visualize =="aggregated"){

    # in-sample
    if (in_sample_cond == TRUE)
      {
      #cond_1 <- FALSE
      conta_punti_veri <- rep(0, length(unique(team_home)))
      for (n in (1:N)[data$season==season_prev]){
        if (y[(n),1]>y[(n),2]){
          conta_punti_veri[team_home[n]]=conta_punti_veri[team_home[n]]+3
          conta_punti_veri[team_away[n]]=conta_punti_veri[team_away[n]]
        }else if(y[(n),1]==y[(n),2]){

          conta_punti_veri[team_home[n]]=conta_punti_veri[team_home[n]]+1
          conta_punti_veri[team_away[n]]=conta_punti_veri[team_away[n]]+1

        }else if(y[(n),1]<y[(n),2]){

          conta_punti_veri[team_home[n]]=conta_punti_veri[team_home[n]]
          conta_punti_veri[team_away[n]]=conta_punti_veri[team_away[n]]+3

        }

      }

    }else{
  # compute the true points on the test set
      conta_punti_veri <- rep(0, length(unique(team_home)))
  for (n in 1:N_prev){

      if (y[(N+n),1]>y[(N+n),2]){
        conta_punti_veri[team1_prev[n]]=conta_punti_veri[team1_prev[n]]+3
        conta_punti_veri[team2_prev[n]]=conta_punti_veri[team2_prev[n]]
      }else if(y[(N+n),1]==y[(N+n),2]){

        conta_punti_veri[team1_prev[n]]=conta_punti_veri[team1_prev[n]]+1
        conta_punti_veri[team2_prev[n]]=conta_punti_veri[team2_prev[n]]+1

      }else if(y[(N+n),1]<y[(N+n),2]){

        conta_punti_veri[team1_prev[n]]=conta_punti_veri[team1_prev[n]]
        conta_punti_veri[team2_prev[n]]=conta_punti_veri[team2_prev[n]]+3

      }
  }
    }
  obs <- sort.int(conta_punti_veri, index.return = TRUE, decreasing = TRUE)$x
  obs_names <- sort.int(conta_punti_veri, index.return = TRUE, decreasing = TRUE)$ix
  teams_rank_names <- teams[obs_names]
  teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]

  if (cond_2 == TRUE){

    number_match_days <- length(unique(team1_prev))*2-2
    mod <- floor((N/ (length(unique(team1_prev))/2))/number_match_days)
    old_matches <- number_match_days*mod*length(unique(team1_prev))/2
    new_N <- seq(1+old_matches, N)

    # compute the true points on the training set
    conta_punti_veri_pre <- rep(0, length(unique(team_home)))
    for (n in new_N){
      if (y[(n),1]>y[(n),2]){
        conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]+3
        conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]
      }else if(y[(n),1]==y[(n),2]){

        conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]+1
        conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]+1

      }else if(y[(n),1]<y[(n),2]){

        conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]
        conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]+3

      }

    }


  }else{

  # compute the true points on the training set
  conta_punti_veri_pre <- rep(0, length(unique(team_home)))
  if (in_sample_cond==FALSE){
  for (n in 1:N){
    if (y[(n),1]>y[(n),2]){
      conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]+3
      conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]
    }else if(y[(n),1]==y[(n),2]){

      conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]+1
      conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]+1

    }else if(y[(n),1]<y[(n),2]){

      conta_punti_veri_pre[team_home[n]]=conta_punti_veri_pre[team_home[n]]
      conta_punti_veri_pre[team_away[n]]=conta_punti_veri_pre[team_away[n]]+3

    }

  }
  }
}

  # compute the points on the MCMC
  for (t in 1:M){
    if (  cond_1 == TRUE | cond_2 == TRUE ){
        conta_punti[t,] <- conta_punti_veri_pre
    }

    if (in_sample_cond){
       set <- (1:N)[data$season==season_prev]
    }else{
      set <- (1:N_prev)
    }

    for (n in set){
      if (y_rep1[t,n]>y_rep2[t,n]){
        conta_punti[t,team1_prev[n]]=conta_punti[t,team1_prev[n]]+3
        conta_punti[t,team2_prev[n]]=conta_punti[t,team2_prev[n]]
      }else if(y_rep1[t,n]==y_rep2[t,n]){

        conta_punti[t,team1_prev[n]]=conta_punti[t,team1_prev[n]]+1
        conta_punti[t,team2_prev[n]]=conta_punti[t,team2_prev[n]]+1

      }else if(y_rep1[t,n]<y_rep2[t,n]){

        conta_punti[t,team1_prev[n]]=conta_punti[t,team1_prev[n]]
        conta_punti[t,team2_prev[n]]=conta_punti[t,team2_prev[n]]+3

      }

    }

  }


  # assumption for games "within" the season
  if (  cond_1 == TRUE | cond_2 == TRUE
    # all(sort(unique(team_home))== sort(unique(team1_prev))) &
    #     N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0
    ){

    obs <- sort.int(conta_punti_veri + conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$x
    obs_names <- sort.int(conta_punti_veri+ conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$ix
    teams_rank_names <- teams[obs_names]
    teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]
  }

  if (is.matrix(conta_punti[,team_index])){
  expected_point=apply(conta_punti[,team_index],2,median)
  points_25=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.25))
  points_75=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.75))
  points_025=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.025))
  points_975=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.975))
  sd_expected=apply(conta_punti[,team_index],2,sd)
  }else{
    expected_point=median(conta_punti[,team_index])
    points_25=quantile(conta_punti[,team_index],  0.25)
    points_75=quantile(conta_punti[,team_index],0.75)
    points_025=quantile(conta_punti[,team_index],0.025)
    points_975=quantile(conta_punti[,team_index],0.975)
    sd_expected=sd(conta_punti[,team_index])
   }

  class=sort.int(expected_point, index.return=TRUE,
                 decreasing=TRUE)

  rank_bar=cbind(teams[team_index][class$ix], class$x,
                 points_25[class$ix],
                 points_75[class$ix],
                 points_025[class$ix],
                 points_975[class$ix])

  rank_frame=data.frame(
    teams=rank_bar[,1],
    mid=as.numeric(as.vector(rank_bar[,2])),
    obs=obs[  match(  rank_bar[,1], teams_rank_names)],
    lo=as.numeric(as.vector(rank_bar[,3])),
    hi=as.numeric(as.vector(rank_bar[,4])),
    lo2=as.numeric(as.vector(rank_bar[,5])),
    hi2=as.numeric(as.vector(rank_bar[,6]))
  )

  rank_frame$teams=factor(rank_frame$teams,
                            levels=rank_bar[,1])
  p <- ggplot() +
    geom_ribbon(aes(x = teams, ymin = lo2, ymax = hi2, group = 1),
                data = rank_frame,
                fill = color_scheme_get(fill_test)[[4]]) +
    geom_ribbon(aes(x = teams, ymin = lo, ymax = hi, group = 1),
                data = rank_frame,
                fill = color_scheme_get(fill_test)[[5]]) +
    geom_line(aes(x = teams, y = mid, group = 1, color = "Simulated"),
              data = rank_frame) +
    geom_point(aes(x = teams, y = obs, color = "Observed"),
               data = rank_frame) +
    scale_colour_manual(name = "",
                        values = c(Observed = "blue", Simulated = color_scheme_get(fill_test)[[4]]),
                        labels = c("Observed", "Simulated")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Posterior predicted points and ranks") +
    labs(x = "Teams", y = "Points") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 15)) +
    theme_bw()


    tbl <- rank_frame[,1:5]
    tbl$lo <- round(tbl$lo)
    tbl$hi <- round(tbl$hi)
    tbl$mid <- round(tbl$mid)

    if (length(team_sel)==1){
      rownames(tbl) <- "1"
      return(list(rank_table = tbl))
    }else{

  return(list(rank_table = tbl, rank_plot = p))
    }

  }else if(visualize == "individual"){

    if ( cond_1 == TRUE ){

      if (in_sample_cond==TRUE){
      day_index <- floor( (length(set)/ (length(unique(team1_prev[set]))/2))  )
      day_index_rep <- rep(rep(seq(1, day_index),
                               each = length(unique(team1_prev[set]))/2), length(unique(seasons_levels)))
      day_index_prev <- rep(seq( (day_index+1),
                                 (length(set)+N_prev)/(length(unique(team1_prev[set]))/2)
      ), each = length(unique(team1_prev[set]))/2)
      day_index_prev <- day_index_rep
      set2 <- set
      }else{
        day_index <- floor( N/ (length(unique(team1_prev[set]))/2))
        day_index_rep <- rep(rep(seq(1, day_index),
                                 each = length(unique(team1_prev[set]))/2), length(unique(seasons_levels)))
        day_index_prev <- rep(seq( (day_index+1),
                                   (N+N_prev)/(length(unique(team1_prev[set]))/2)
        ), each = length(unique(team1_prev[set]))/2)
      set2<-(1:N)
    }


    # compute the true point for the training sample, dynamically
    conta_punti_veri_pre_dyn <- matrix(0, length(unique(team_home)), day_index )
    for (n in set2){
      if (y[(n),1]>y[(n),2]){
        conta_punti_veri_pre_dyn[team_home[n], day_index_rep[n]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n]]+3
        conta_punti_veri_pre_dyn[team_away[n], day_index_rep[n]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n]]
      }else if(y[(n),1]==y[(n),2]){

        conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n]]+1
        conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n]]+1

      }else if(y[(n),1]<y[(n),2]){

        conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n]]
        conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n]]+3

      }

    }




    cumsum_punti_pre <- t(apply(conta_punti_veri_pre_dyn,1,cumsum))
  }else if (cond_2 == TRUE ){

    mod <- floor((N/ (length(unique(team1_prev))/2))/number_match_days)


    day_index <- max(1, floor( (N/ (length(unique(team1_prev))/2))  )-mod*number_match_days)
    if (day_index==1){
      stop("Please, provide more training set matches in the model fit!")
    }

    day_index_rep <- rep(seq(1, day_index) ,
                         each = length(unique(team1_prev))/2)
    day_index_prev <- rep(seq( (day_index+1),
                               day_index + (N+N_prev)/(length(unique(team1_prev))/2)-floor( (N/ (length(unique(team1_prev))/2))  )
                                ),
                          each = length(unique(team1_prev))/2)

    if (in_sample_cond==TRUE){
      day_index_prev <- day_index_rep
    }

    conta_punti_veri_pre_dyn <- matrix(0, length(unique(team_home)), day_index )

    # qui è un casino: non sempre le stagioni hanno lo stesso numero di squadre...
    # per esempio la serie A 2004-2005 aveva 20 squadre, quella prima 18.
    # questo new_N funziona quindi solo nell'ipotesi in cui il campionato da prevedere
    # e quelli prima abbiano lo stesso numero di squadre
    old_matches <- number_match_days*mod*length(unique(team1_prev))/2
    new_N <- seq(1+old_matches, N)
                 #floor( (N/ (length(unique(team1_prev))/2))  ))


    # compute the true point for the training sample, dynamically
    for (n in new_N){
      if (y[(n),1]>y[(n),2]){
        conta_punti_veri_pre_dyn[team_home[n], day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n-old_matches]]+3
        conta_punti_veri_pre_dyn[team_away[n], day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n-old_matches]]
      }else if(y[(n),1]==y[(n),2]){

        conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n-old_matches]]+1
        conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n-old_matches]]+1

      }else if(y[(n),1]<y[(n),2]){

        conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_home[n],day_index_rep[n-old_matches]]
        conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n-old_matches]]=conta_punti_veri_pre_dyn[team_away[n],day_index_rep[n-old_matches]]+3

      }

    }
    cumsum_punti_pre <- t(apply(conta_punti_veri_pre_dyn,1,cumsum))

  }else if (cond_3 == TRUE)
    {
      day_index <- 0
      day_index_rep <- rep(seq(1, day_index) ,
                         each = length(unique(team1_prev))/2)
      day_index_prev <- rep(seq( (day_index+1), (N_prev)/(length(unique(team1_prev))/2) ),
                          each = length(unique(team1_prev))/2)

      if (in_sample_cond==TRUE){
        day_index_prev <- day_index_rep
      }
  }




  # compute the true points for the test set sample, dynamically
  conta_punti_veri_post_dyn <- matrix(NA, length(unique(team_home)), max(unique(day_index_prev)) )

  # per levare le linee nere nel test set...
  # if (in_sample_cond == TRUE)
  # {
  #   for (n in 1:N){
  #     if (y[(n),1]>y[(n),2]){
  #       conta_punti_veri_post_dyn[team1_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+3
  #       conta_punti_veri_post_dyn[team2_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]
  #     }else if(y[(n),1]==y[(n),2]){
  #
  #       conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+1
  #       conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+1
  #
  #     }else if(y[(n),1]<y[(n),2]){
  #
  #       conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]
  #       conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+3
  #
  #     }
  #   }
  # }else{
  # for (n in 1:N_prev){
  #   if (y[(N+n),1]>y[(N+n),2]){
  #     conta_punti_veri_post_dyn[team1_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+3
  #     conta_punti_veri_post_dyn[team2_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]
  #   }else if(y[(N+n),1]==y[(N+n),2]){
  #
  #     conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+1
  #     conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+1
  #
  #   }else if(y[(N+n),1]<y[(N+n),2]){
  #
  #     conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]
  #     conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+3
  #
  #     }
  #   }
  # }

  # compute the points on the MCMC, dynamically
  conta_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  cumsum_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  for (t in 1:M){
    if (cond_3 == FALSE){
      if (in_sample_cond==FALSE){
      conta_punti_dyn[t,,1:day_index] <- conta_punti_veri_pre_dyn
      }
    }

    if (in_sample_cond){
      set <- (1:N)[data$season==season_prev]
    }else{
      set <- (1:N_prev)
    }

    for (n in set ){
      if (y_rep1[t,n]>y_rep2[t,n]){
        conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]+3
        conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]
      }else if(y_rep1[t,n]==y_rep2[t,n]){

        conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]+1
        conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]+1

      }else if(y_rep1[t,n]<y_rep2[t,n]){

        conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team1_prev[n],day_index_prev[n]]
        conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]=conta_punti_dyn[t,team2_prev[n],day_index_prev[n]]+3

      }

    }

    cumsum_punti_dyn[t,,] <- t(apply(conta_punti_dyn[t,,],1, cumsum))

  }




cumsum_punti_post <- t(apply(conta_punti_veri_post_dyn,1,cumsum))
cumsum_punti_post <- cumsum_punti_post[, unique(day_index_prev)]
  # se cumsum_punti_post è un vettore, significa che stiamo   prevedendo solo l'ultima giornata. Per il codice che
  # segue, bisogna convertirlo in matrice
   if(is.vector(cumsum_punti_post)){
     cumsum_punti_post <- as.matrix(cumsum_punti_post)
   }


# compute quantiles for MCMC point
punti_dyn_med <- apply(cumsum_punti_dyn, c(2,3), median)
punti_dyn_025 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.025)))
punti_dyn_25 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.25)))
punti_dyn_75 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.75)))
punti_dyn_975 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.975)))

  if (cond_1 == TRUE)
    {
    if (in_sample_cond==FALSE){
      if (length(team_sel)==1){
        mt_obs <- melt(c(cumsum_punti_pre[team_index, ],
                             cumsum_punti_pre[team_index,day_index]+
                               cumsum_punti_post[team_index,]))$value
        mt_50 <- melt(c(rep(NA, day_index),
                            punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value

      }else{

    mt_obs <- melt(cbind(cumsum_punti_pre[team_index, ],
                     cumsum_punti_pre[team_index,day_index]+
                      cumsum_punti_post[team_index,]))$value

    mt_50 <- melt(cbind(matrix(NA,
      length(team_names),
      #length(unique(team_home)),
      day_index),
      punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value
      }

    }else{
      mt_obs <- melt(cumsum_punti_pre[team_index, ])$value
      mt_50 <- melt(punti_dyn_med[team_index, ])$value
    }

  }else if ( cond_2 == TRUE ){
    if (length(team_sel)==1){
      mt_obs <- melt(c(cumsum_punti_pre[team_index, ],
                       cumsum_punti_pre[team_index,day_index]+
                         cumsum_punti_post[team_index,]))$value
      mt_50 <- melt(c(rep(NA, day_index),
                      punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value

    }else{

    mt_obs <- melt(cbind(cumsum_punti_pre[team_index, ],
                         cumsum_punti_pre[team_index,day_index]+
                         cumsum_punti_post[team_index,] ))$value
    mt_50 <- melt(cbind(matrix(NA,
                               length(team_names),
                               #length(unique(team_home)),
                               day_index),
                        punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value
    }

  }else if (cond_3 == TRUE){
    mt_obs <- melt( cumsum_punti_post[team_index,])$value
    mt_50 <- melt(punti_dyn_med[team_index, (day_index+1):max(day_index_prev)])$value
    }
mt_025 <- melt((punti_dyn_025)[team_index, ])$value
mt_25 <- melt((punti_dyn_25)[team_index, ])$value
mt_75 <- melt((punti_dyn_75)[team_index, ])$value
mt_975 <- melt((punti_dyn_975)[team_index, ])$value


df_team_sel <- data.frame(obs = mt_obs,
                              day = rep(seq(1, max(day_index_prev)), each=length(team_index)),
                              q_50 = mt_50,
                              q_025 = mt_025,
                              q_25 = mt_25,
                              q_75 = mt_75,
                              q_975 = mt_975,
                              teams = rep(teams[team_index], max(day_index_prev)))



p <- ggplot(df_team_sel, aes(day, obs)) +
  geom_ribbon(aes(x = day, ymin = q_025, ymax = q_975, group = 1),
              fill = color_scheme_get(fill_test)[[4]],
              data = df_team_sel) +
  geom_ribbon(aes(x = day, ymin = q_25, ymax = q_75, group = 1),
              data = df_team_sel,
              fill = color_scheme_get(fill_test)[[5]]) +
  geom_line(aes(x = day, y = q_50, color = "Simulated"),
            data = df_team_sel,
            size = 1.1, na.rm = TRUE) +
  geom_line(size = 0.8, linetype = "solid", aes(color = "Observed"), na.rm = TRUE) +
  xlab("Match day") +
  ylab("Cumulated Points") +
  ylim(0, max(mt_975) + 2) +
  scale_colour_manual(name = "",
                      values = c(Observed = "blue", Simulated = color_scheme_get(fill_test)[[4]]),
                      labels = c("Observed", "Simulated")) +
  facet_wrap("teams", scales = "free") +
  ggtitle("Posterior predicted points") +
  theme(plot.title = element_text(size = 22),
        legend.position = "bottom",
        legend.text = element_text(size = 15)) +
  annotate("rect", xmin = -Inf, xmax = day_index, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "white") +
  annotate("rect", xmin = day_index, xmax = max(day_index_prev), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "white") +
  theme_bw()

      #suppressWarnings(print(p))

     return(list(rank_plot = p))
  }
}



