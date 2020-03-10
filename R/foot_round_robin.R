#' Round-robin
#'
#'
#'

foot_round_robin <- function(){
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

  #
  counts_mix <- matrix(1, nteams, nteams)

  #
  #
  # punteggio_esatto <- matrix(NA, 132,2)
  # punteggio_esatto[,1] <- punt_home
  # punteggio_esatto[,2] <- punt_away
  # punti_esatti <- matrix(NA, 132, 2)
  # punti_esatti[,1] <- point_home
  # punti_esatti[,2] <- point_away
  #
  # ord_fin<-sort(class_fin_mean, decreasing =TRUE, index.return=TRUE)$ix
  #
  #
  # punt <- c()
  #
  for (n in 1: N_prev){
    prob<- sum(y_rep1[,n]> y_rep2[,n])/M
      #as.double(table(paste(congiunto[,1]," ",congiunto[,2], sep="")))[ which.max(table(paste(congiunto[,1]," ",congiunto[,2], sep=""))  )   ]/M
    counts_mix[unique(team1_prev[n]),
             unique(team2_prev[n])] <- prob
  }

  # write.table(punt, file="punteggio.txt")
  # punt_mat<-read.table("punteggio.txt", sep=" ")
  # dim(punt_mat)
  #
  #
  x <- seq(1,nteams, length.out=nteams)
  y <- seq(1,nteams, length.out=nteams)
   data <- expand.grid(Home=x, Away=y)
   data$prob=as.double(counts_mix[1:nteams, 1:nteams])
  ggplot(data, aes(Home, Away, z= RMSE)) + geom_tile(aes(fill = RMSE)) +
    theme_bw() +
    labs(x, xaxis_text( size = rel(1.2)))+
    labs(y, yaxis_text( size = rel(1.2)))+
    scale_fill_gradient(low="white", high="red")+
    scale_x_discrete( limits= teams_trad[ord_fin]  ) +
    scale_y_discrete( limits= teams_trad[ord_fin]  ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_rect(aes(xmin =0.5,
      xmax = 1.5,
      ymin =0.5,
      ymax =1.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =1.5,
      xmax = 2.5,
      ymin =1.5,
      ymax =2.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =2.5,
      xmax = 3.5,
      ymin =2.5,
      ymax =3.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =3.5,
      xmax = 4.5,
      ymin =3.5,
      ymax =4.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =4.5,
      xmax = 5.5,
      ymin =4.5,
      ymax =5.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =5.5,
      xmax = 6.5,
      ymin =5.5,
      ymax =6.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =6.5,
      xmax = 7.5,
      ymin =6.5,
      ymax =7.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =7.5,
      xmax = 8.5,
      ymin =7.5,
      ymax =8.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =8.5,
      xmax = 9.5,
      ymin =8.5,
      ymax =9.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =9.5,
      xmax = 10.5,
      ymin =9.5,
      ymax =10.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =10.5,
      xmax = 11.5,
      ymin =10.5,
      ymax =11.5),
      fill = "gray", color = "black", size = 1)+
    geom_rect(aes(xmin =11.5,
      xmax = 12.5,
      ymin =11.5,
      ymax =12.5),
      fill = "gray", color = "black", size = 1)
}
