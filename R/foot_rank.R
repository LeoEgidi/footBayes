#' Predicted rank positions
#'
#' @importFrom reshape2 melt
#' @importFrom bayesplot color_scheme_get
#' @export

foot_rank <- function(data, object,
                      team_sel,
                      visualize = c(1,2))
  {
  #checks
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))
  sims <- rstan::extract(object)
  y <- as.matrix(data[,4:5])
  teams <- unique(data$home)
  team_home <- match(data$home, teams)
  team_away <- match(data$away, teams)
  # if (missing(type)){
  #   type = "out-of-sample"
  # }

  # caso in-sample
  if (is.null(sims$diff_y_prev) & is.null(sims$y_prev)){
    if (!is.null(sims$y_rep)){
    N <- dim(data)[1]
    N_prev <- N
    y_rep1 <- sims$y_rep[,,1]
    y_rep2 <- sims$y_rep[,,2]
    team1_prev <- team_home[1:N]
    team2_prev <- team_away[1:N]
    }else{
      # caso t di student
      N <- dim(data)[1]
      N_prev <- N
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

  if(missing(visualize)){
      visualize <- 1
    }

  # condizione per fare si che quando si prevede
  # solo l'ultima giornata, non venga considerata
  # solo la metà delle squadre
  if (length(unique(team1_prev)) !=
      length(unique(c(team1_prev, team2_prev)))  ){
    team1_prev <- c(team1_prev, team2_prev)
    team2_prev <- c(team2_prev, team1_prev)
  }

  if (missing(team_sel)){
    team_sel <- teams[unique(team1_prev)]
  }else if (team_sel =="all"){
    team_sel <- teams[unique(team1_prev)]
  }
  team_index <- match(team_sel, teams)
  team_names <- teams[team_index]

  M <-dim(sims$diff_y_rep)[1]
  ngames_train <- dim(sims$y_rep)[2]
  conta_punti <- matrix(0, M, length(teams))
  conta_punti_veri <- rep(0, length(teams))
  number_match_days <- length(unique(team1_prev))*2-2
  in_sample_cond <- is.null(sims$diff_y_prev) & is.null(sims$y_prev)
  fill_test <- c("red", "gray")[c(!in_sample_cond, in_sample_cond)]

  if (visualize ==1){

    # in-sample
    if (is.null(sims$diff_y_prev) & is.null(sims$y_prev))
      {
      conta_punti_veri <- rep(0, length(unique(team_home)))
      for (n in 1:N){
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


  # compute the true points on the training set
  conta_punti_veri_pre <- rep(0, length(unique(team_home)))
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

  # compute the points on the MCMC
  for (t in 1:M){
    if (  N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0
          &
          all(sort(unique(team_home))== sort(unique(team1_prev)))
          #N <= length(unique(team_home))*(length(unique(team_home))-1 )
          ){

      conta_punti[t,] <- conta_punti_veri_pre
    }

    for (n in 1:N_prev){
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


  # assumption for games coming from the same seasons
  # (training set and test set belong to the same season)
  if (  all(sort(unique(team_home))== sort(unique(team1_prev))) &
        N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0  ){

    obs <- sort.int(conta_punti_veri + conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$x
    obs_names <- sort.int(conta_punti_veri+ conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$ix
    teams_rank_names <- teams[obs_names]
    teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]
  }

  expected_point=apply(conta_punti[,team_index],2,median)
  points_25=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.25))
  points_75=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.75))
  points_025=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.025))
  points_975=apply(conta_punti[,team_index],2,function(x) quantile(x, 0.975))
  sd_expected=apply(conta_punti[,team_index],2,sd)
  class=sort.int(expected_point, index.return=TRUE,
                 decreasing=TRUE)

  rank_bar=cbind(teams[team_index][class$ix], class$x,
                 points_25[class$ix],
                 points_75[class$ix],
                 points_025[class$ix],
                 points_975[class$ix])

  rank_frame=data.frame(
    squadre=rank_bar[,1],
    mid=as.numeric(as.vector(rank_bar[,2])),
    lo=as.numeric(as.vector(rank_bar[,3])),
    hi=as.numeric(as.vector(rank_bar[,4])),
    obs=obs[  match(  rank_bar[,1], teams_rank_names)],
    lo2=as.numeric(as.vector(rank_bar[,5])),
    hi2=as.numeric(as.vector(rank_bar[,6]))
  )

  rank_frame$squadre=factor(rank_frame$squadre,
                            levels=rank_bar[,1])
  ggplot()+
    geom_ribbon(aes(x=squadre, ymin=lo2, ymax=hi2, group=1),
                data=rank_frame,
                fill = color_scheme_get(fill_test)[[1]]
    )+
    geom_ribbon(aes(x=squadre, ymin=lo, ymax=hi, group=1),
                data=rank_frame,
                fill = color_scheme_get(fill_test)[[2]]
    )+
    geom_line(aes(x=squadre, y= mid, group=1),
              data=rank_frame,
              color = color_scheme_get(fill_test)[[4]]
    )+
    geom_point(aes(x=squadre, y=obs),
              data=rank_frame)+
    scale_color_manual(values = c(color_scheme_get("blue")[[2]],
                                  color_scheme_get("red")[[2]]))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x="Teams", y="Points")

  }else if(visualize == 2){

  if (#N %% (length(unique(team1_prev))/2)!=0 &
        # questa condizione significa che siamo "dentro" alla stagione
        all(sort(unique(team_home))== sort(unique(team1_prev))) &
      N < length(unique(team1_prev))*( length(unique(team1_prev))-1   )
        # quest'altra significa che il training ha le stesse squadre del test
        ){

    day_index <- floor( (N/ (length(unique(team1_prev))/2))  )
    day_index_rep <- rep(seq(1, day_index) ,
                       each = length(unique(team1_prev))/2)
    day_index_prev <- rep(seq( (day_index+1),
                               (N+N_prev)/(length(unique(team1_prev))/2)
                               ),
                        each = length(unique(team1_prev))/2)
    conta_punti_veri_pre_dyn <- matrix(0, length(unique(team_home)), day_index )

    # compute the true point for the training sample, dynamically
    for (n in 1:N){
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
  }else if (
             N > length(unique(team1_prev))*( length(unique(team1_prev))-1) &
             # questa condizione significa che siamo "dentro" alla stagione
             all(sort(unique(team_home))== sort(unique(team1_prev)))==FALSE &
             N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0 ){
             # quest'altra significa che il training NON ha le stesse squadre del test){
    mod <- floor((N/ (length(unique(team1_prev))/2))/number_match_days)
    day_index <- floor( (N/ (length(unique(team1_prev))/2))  )-mod*number_match_days
    day_index_rep <- rep(seq(1, day_index) ,
                         each = length(unique(team1_prev))/2)
    day_index_prev <- rep(seq( (day_index+1),
                               (N+N_prev)/(length(unique(team1_prev))/2)-floor( (N/ (length(unique(team1_prev))/2))  )
                                ),
                          each = length(unique(team1_prev))/2)
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

  }else if ( N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))==0)# questa condizione significa che siamo alla fine di una stagione
    {
      day_index <- 0
      day_index_rep <- rep(seq(1, day_index) ,
                         each = length(unique(team1_prev))/2)
      day_index_prev <- rep(seq( (day_index+1), (N_prev)/(length(unique(team1_prev))/2) ),
                          each = length(unique(team1_prev))/2)
  }




  # compute the true points for the test set sample, dynamically
  conta_punti_veri_post_dyn <- matrix(0, length(unique(team_home)), max(unique(day_index_prev)) )
  if (is.null(sims$diff_y_prev) & is.null(sims$y_prev))
  {
    for (n in 1:N){
      if (y[(n),1]>y[(n),2]){
        conta_punti_veri_post_dyn[team1_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+3
        conta_punti_veri_post_dyn[team2_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]
      }else if(y[(n),1]==y[(n),2]){

        conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+1
        conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+1

      }else if(y[(n),1]<y[(n),2]){

        conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]
        conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+3

      }
    }
  }else{
  for (n in 1:N_prev){
    if (y[(N+n),1]>y[(N+n),2]){
      conta_punti_veri_post_dyn[team1_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+3
      conta_punti_veri_post_dyn[team2_prev[n], day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]
    }else if(y[(N+n),1]==y[(N+n),2]){

      conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]+1
      conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+1

    }else if(y[(N+n),1]<y[(N+n),2]){

      conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team1_prev[n],day_index_prev[n]]
      conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]=conta_punti_veri_post_dyn[team2_prev[n],day_index_prev[n]]+3

      }
    }
  }

  # compute the points on the MCMC, dynamically
  conta_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  cumsum_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  for (t in 1:M){
    if (  N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0
      #all(sort(unique(team_home))== sort(unique(team1_prev)))
          # & N <= length(unique(team_home))*(length(unique(team_home))-1 )

      ){

      conta_punti_dyn[t,,1:day_index] <- conta_punti_veri_pre_dyn
    }

    for (n in 1:N_prev){
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

# compute quantiles for MCMC point
punti_dyn_med <- apply(cumsum_punti_dyn, c(2,3), median)
punti_dyn_025 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.025)))
punti_dyn_25 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.25)))
punti_dyn_75 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.75)))
punti_dyn_975 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.975)))

  if (all(sort(unique(team_home))== sort(unique(team1_prev))) &
         N < length(unique(team1_prev))*( length(unique(team1_prev))-1  ))
    {
    mt_obs <- melt(cbind(cumsum_punti_pre[team_index, ],
                     cumsum_punti_pre[team_index,day_index]+
                       cumsum_punti_post[team_index,]))$value
    mt_50 <- melt(cbind(matrix(NA,
                           length(team_names),
                           #length(unique(team_home)),
                           day_index),
                    punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value
  }else if (N > length(unique(team1_prev))*( length(unique(team1_prev))-1) &
            # questa condizione significa che siamo "dentro" alla stagione
            all(sort(unique(team_home))== sort(unique(team1_prev)))==FALSE &
            N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))!=0   ){
    mt_obs <- melt(cbind(cumsum_punti_pre[team_index, ],
                         cumsum_punti_pre[team_index,day_index]+
                           cumsum_punti_post[team_index,]))$value
    mt_50 <- melt(cbind(matrix(NA,
                               length(team_names),
                               #length(unique(team_home)),
                               day_index),
                        punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value

  }else if (N %% (length(unique(team1_prev))*( length(unique(team1_prev))-1))==0){
    mt_obs <- melt(cumsum_punti_post[team_index,])$value
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

    ggplot(df_team_sel,aes(day, obs))+
      geom_ribbon(aes(x=day, ymin=q_025, ymax=q_975, group=1),
                  data=df_team_sel,
                  fill = color_scheme_get("blue")[[1]]
      )+
      geom_ribbon(aes(x=day, ymin=q_25, ymax=q_75, group=1),
                  data=df_team_sel,
                  fill = color_scheme_get("blue")[[2]]
      )+
      geom_line(aes(x= day, y= q_50),
                data=df_team_sel,
                color = color_scheme_get("blue")[[4]],
                #fill = color_scheme_get("red")[[2]],
                size =1.1
      )+
      geom_line(#aes(x=day, y = obs),
                size=1.2, linetype="dashed")+
      geom_vline(
                  xintercept =day_index,
                  linetype="dashed",
                  color=fill_test, size=1)+

      xlab("Match day")+
      ylab("Cumulated Points")+
      facet_wrap("teams", scales ="free")+
      ggtitle("Ranks")+
      theme(plot.title = element_text(size=22))+
      annotate("rect",xmin=-Inf,xmax=day_index,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
      annotate("rect",xmin=day_index ,xmax= max(day_index_prev),ymin=-Inf,ymax=Inf, alpha=0.1, fill=fill_test)

  }
}

