#' Predicted rank positions
#'
#'

foot_rank <- function(data, object,
                      type = c("in-sample", "out-of-sample"),
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
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)

  if(missing(visualize)){
    visualize <- 1
  }
  if (missing(team_sel)){
    team_sel <- teams
  }
  if (missing(type)){
    type = "out-of-sample"
  }

  if (type =="in-sample"){
    N <- dim(data)[1]
    N_prev <- N
    y_rep1 <- sims$y_rep[,,1]
    y_rep2 <- sims$y_rep[,,2]
    team1_prev <- team_home[1:N]
    team2_prev <- team_away[1:N]
  }else if (type=="out-of-sample"){
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[,,1]
    y_rep2 <- sims$y_prev[,,2]
    team1_prev <- team_home[(N+1):(N+N_prev)]
    team2_prev <- team_away[(N+1):(N+N_prev)]
  }

  M <-dim(sims$diff_y_rep)[1]
  ngames_train <- dim(sims$y_rep)[2]
  conta_punti <- matrix(0, M, length(teams))
  conta_punti_veri <- rep(0, length(teams))


  if (visualize ==1){

  # compute the final true point on the test set
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
  obs <- sort.int(conta_punti_veri, index.return = TRUE, decreasing = TRUE)$x
  obs_names <- sort.int(conta_punti_veri, index.return = TRUE, decreasing = TRUE)$ix
  teams_rank_names <- teams[obs_names]
  teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]


  # compute the true point on the training set
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
    if (  all(sort(unique(team_home))== sort(unique(team_home))) &
          N <= length(unique(team_home))*(length(unique(team_home))-1 )  ){

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
  if (  all(sort(unique(team_home))== sort(unique(team_home))) &
        N <= length(unique(team_home))*(length(unique(team_home))-1 )  ){

    obs <- sort.int(conta_punti_veri + conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$x
    obs_names <- sort.int(conta_punti_veri+ conta_punti_veri_pre, index.return = TRUE, decreasing = TRUE)$ix
    teams_rank_names <- teams[obs_names]
    teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]
  }

  expected_point=apply(conta_punti,2,median)
  points_25=apply(conta_punti,2,function(x) quantile(x, 0.25))
  points_75=apply(conta_punti,2,function(x) quantile(x, 0.75))
  sd_expected=apply(conta_punti,2,sd)
  cbind(teams[unique(team1_prev)], expected_point[unique(team1_prev)],
        points_25[unique(team1_prev)], points_75[unique(team1_prev)] )
  class=sort.int(expected_point[unique(team1_prev)], index.return=TRUE,
                 decreasing=TRUE)

  rank_bar=cbind(teams[unique(team1_prev)][class$ix], class$x,
                 points_25[unique(team1_prev)][class$ix],
                 points_75[unique(team1_prev)][class$ix]  )

  rank_frame=data.frame(
    squadre=rank_bar[,1],
    mid=as.numeric(as.vector(rank_bar[,2])),
    lo=as.numeric(as.vector(rank_bar[,3])),
    hi=as.numeric(as.vector(rank_bar[,4])),
    obs=obs[  match(  rank_bar[,1], teams_rank_names) ]
  )

  rank_frame$squadre=factor(rank_frame$squadre, levels=rank_bar[,1])



  ggplot()+
    geom_ribbon(aes(x=squadre, ymin=lo, ymax=hi, group=1),
                data=rank_frame,
                fill = color_scheme_get("gray")[[2]]
    )+
    geom_line(aes(x=squadre, y= mid, group=1),
              data=rank_frame,
              fill = color_scheme_get("blue")[[2]]
    )+
    geom_point(aes(x=squadre, y=obs),
              data=rank_frame)+
    scale_color_manual(values = c(color_scheme_get("blue")[[2]],
                                  color_scheme_get("red")[[2]]))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x="Teams", y="Points")

  }else if(visualize == 2){


  team_index <- match(team_sel, teams)
  team_names <- teams[team_index]
  if (  all(sort(unique(team_home))== sort(unique(team_home))) &
        N <= length(unique(team_home))*(length(unique(team_home))-1 )  ){
  day_index <- floor(N/ (length(unique(team1_prev))/2))
  day_index_rep <- rep(seq(1, day_index) ,
                       each = length(unique(team1_prev))/2)
  day_index_prev <- rep(seq( (day_index+1), (N+N_prev)/(length(unique(team1_prev))/2) ),
                        each = length(unique(team1_prev))/2)
  conta_punti_veri_pre_dyn <- matrix(0, length(unique(team_home)), day_index )

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
  }else{
    day_index <- 0
    day_index_rep <- rep(seq(1, day_index) ,
                         each = length(unique(team1_prev))/2)
    day_index_prev <- rep(seq( (day_index+1), (N_prev)/(length(unique(team1_prev))/2) ),
                          each = length(unique(team1_prev))/2)
    conta_punti_veri_pre_dyn <- matrix(0, length(unique(team_home)), day_index )
  }

  # compute the true point for the training sample, dynamically


  # compute the true points for the test set sample, dynamically
  conta_punti_veri_post_dyn <- matrix(0, length(unique(team_home)), max(unique(day_index_prev)) )
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

  # compute the points on the MCMC, dynamically
  conta_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  cumsum_punti_dyn <- array(0, c( M, length(unique(team_home)), max(day_index_prev)))
  for (t in 1:M){
    if (  all(sort(unique(team_home))== sort(unique(team_home))) &
          N <= length(unique(team_home))*(length(unique(team_home))-1 )  ){

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



cumsum_punti_pre <- t(apply(conta_punti_veri_pre_dyn,1,cumsum))
cumsum_punti_post <- t(apply(conta_punti_veri_post_dyn,1,cumsum))
cumsum_punti_post <- cumsum_punti_post[, unique(day_index_prev)]

# compute quantiles for MCMC point
punti_dyn_med <- apply(cumsum_punti_dyn, c(2,3), median)
punti_dyn_025 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.025)))
punti_dyn_25 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.25)))
punti_dyn_75 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.75)))
punti_dyn_975 <- apply(cumsum_punti_dyn, c(2,3), function(x) quantile(x, c(0.975)))

mt_obs <- melt(cbind(cumsum_punti_pre[team_index,], cumsum_punti_pre[team_index,day_index]+cumsum_punti_post[team_index,]))$value
mt_50 <- melt(cbind(matrix(NA,
                           length(team_names),
                           #length(unique(team_home)),
                           day_index),
                    punti_dyn_med[team_index, (day_index+1):max(day_index_prev)]))$value

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
                  color="red", size=1)+

      xlab("Match day")+
      ylab("Cumulated Points")+
      facet_wrap("teams", scales ="free")+
      ggtitle("Ranks")+
      theme(plot.title = element_text(size=22))+
      annotate("rect",xmin=-Inf,xmax=day_index,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
      annotate("rect",xmin=day_index ,xmax= max(day_index_prev),ymin=-Inf,ymax=Inf, alpha=0.1, fill="red")

  }
}



# foot_rank(data = italy_2002, object= fit1,
#           team_sel = c("AS Roma", "Inter"),
#           type="out-of-sample",visualize = 2)

foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 2)
  # qui non vengono contati bene i punti!

foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 1)
