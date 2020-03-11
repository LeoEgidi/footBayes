#' Round-robin
#'
#'
#' @export

# checks su stagioni differenti, come in foot_rank
# checks su student t,skellam, etc.
# team_sel con selezione teams
# check su in-sample e out-of-sample

foot_round_robin <- function(data, object, team_sel){
  # # plot for the torunament "box"
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")

  sims <- rstan::extract(object)
  y <- as.matrix(data[,4:5])
  teams <- unique(data$home)
  team_home <- match(data$home, teams)
  team_away <- match(data$away, teams)
  N_prev <- dim(sims$y_prev)[2]
  N <- dim(sims$y_rep)[2]
  y_rep1 <- sims$y_prev[,,1]
  y_rep2 <- sims$y_prev[,,2]
  team1_prev <- team_home[(N+1):(N+N_prev)]
  team2_prev <- team_away[(N+1):(N+N_prev)]
  nteams<- length(unique(team1_prev))
  M <-dim(sims$diff_y_rep)[1]
  counts_mix <- matrix(0, nteams, nteams)
  new_teams <- teams[unique(team1_prev)]

  punt <- matrix("-", nteams, nteams)
  for (n in 1:N){
    punt[team_home[n], team_away[n]] <-
      paste(y[n,1], "-", y[n,2], sep="")
  }

  for (n in 1: N_prev){
    prob<- sum(y_rep1[,n]> y_rep2[,n])/M
    counts_mix[unique(team1_prev[n]),
             unique(team2_prev[n])] <- prob
  }

  x1 = seq(0.5, nteams-1+0.5)
  x2 = seq(1.5, nteams-1+1.5)
  x1_x2 <- matrix(0, nteams, nteams)
  x2_x1 <- matrix(0, nteams, nteams)
  y1_y2 <- matrix(0, nteams, nteams)
  y2_y1 <- matrix(0, nteams, nteams)
  for (j in 1:nteams){
    x1_x2[j,j] = x1[j]
    x2_x1[j,j] = x2[j]
    y1_y2[j,j] = x1[j]
    y2_y1[j,j] = x2[j]
  }
  x_ex <- seq(1,nteams, length.out=nteams)
  y_ex <- seq(1,nteams, length.out=nteams)
   data_ex <- expand.grid(Home=x_ex, Away=y_ex)
   data_ex$prob=as.double(counts_mix[1:nteams, 1:nteams])
  ggplot(data_ex, aes(Home, Away, z= prob)) +
    geom_tile(aes(fill = prob)) +
    theme_bw() +
    labs(x_ex, xaxis_text( size = rel(1.2)))+
    labs(y_ex, yaxis_text( size = rel(1.2)))+
    scale_fill_gradient(low="white", high="red")+
    scale_x_discrete( limits= teams  ) +
    scale_y_discrete( limits= teams  ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_text(aes(label=as.vector((punt))), size =1.9)+
    geom_rect(aes(xmin =as.vector(x1_x2),
      xmax = as.vector(x2_x1),
      ymin =as.vector(x1_x2),
      ymax =as.vector(x2_x1)),
      fill = "black", color = "black", size = 1)



    #+
    # geom_rect(aes(xmin =2.5,
    #   xmax = 3.5,
    #   ymin =2.5,
    #   ymax =3.5),
    #   fill = "gray", color = "black", size = 1)+

}
