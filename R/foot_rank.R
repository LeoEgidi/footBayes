#' Predicted rank positions
#'
#'

foot_rank <- function(data, object,
                      type = c("in-sample", "out-of-sample"))
  {
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))
  sims <- rstan::extract(object)
  y <- as.matrix(data[,4:5])
  teams <- unique(data$home)
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)

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
  conta_punti=matrix(0, length(y_rep1[,1]), length(teams))

  for (t in 1:M){

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
    hi=as.numeric(as.vector(rank_bar[,4]))
   # ,obs=obs[  match(  rank_bar[,1], class_names) ]
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
    # geom_point(aes(x=squadre, y=obs),
    #            data=rank_frame)+
    scale_color_manual(values = c(color_scheme_get("blue")[[2]],
                                  color_scheme_get("red")[[2]]))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x="Teams", y="Points")


}
