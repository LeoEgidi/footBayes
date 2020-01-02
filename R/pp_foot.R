pp_foot <- function(data, object,
                    type = c("aggregated", "matches")){

  sims <- rstan::extract(object)
  y <- data[,4:5]
  diff_gol <- as.vector(y[,1] - y[,2])
  diff_gol_rep <- sims$diff_y_rep
  esiti_short <- seq(-3,3,1)
  M <-dim(sims$y_rep)[1]
  freq_rel_matrix <- matrix(NA, M, length(esiti_short))
  ngames_train <- dim(sims$y_rep)[2]

  if (type =="aggregated"){

  for (j in 1:M){
    for (u in 1:length(esiti_short)){
       freq_rel_matrix[j,u] <-  (as.double(table(diff_gol_rep[j,]))[as.double(names(table(diff_gol_rep[j,])))==esiti_short[u]])/ngames_train
      }}

  freq_rel_frame_add <- matrix(NA, M*length(esiti_short),2)

  for(j in 1:M){
    freq_rel_frame <- data.frame(valori=esiti_short,  rel=freq_rel_matrix[j,])
    freq_rel_frame_add[( (7*j)-6):(7*j),] <- as.matrix(freq_rel_frame)
  }

  freq_rel_obs=c()

  for (u in 1:length(esiti_short)){
    freq_rel_obs[u]=(as.double(table(diff_gol))[ as.double(names(table(diff_gol)))==esiti_short[u]])/ngames_train
    }

  frame <- data.frame(valori=esiti_short, rel=freq_rel_frame_add[,2] )

  ggplot(frame, aes(x=valori, y=rel))+
    geom_point(position = "jitter", alpha = 0.2, col="gray") +
    geom_segment(mapping=aes( x=-3-0.5, y=freq_rel_obs[1],
                              xend=-3+0.5, yend=freq_rel_obs[1]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=-2-0.5, y=freq_rel_obs[2],
                              xend=-2+0.5, yend=freq_rel_obs[2]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=-1-0.5, y=freq_rel_obs[3],
                              xend=-1+0.5, yend=freq_rel_obs[3]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=0-0.5, y=freq_rel_obs[4],
                              xend=0+0.5, yend=freq_rel_obs[4]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=1-0.5, y=freq_rel_obs[5],
                              xend=1+0.5, yend=freq_rel_obs[5]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=2-0.5, y=freq_rel_obs[6],
                              xend=2+0.5, yend=freq_rel_obs[6]) , size=2, color = "blue")+
    geom_segment(mapping=aes( x=3-0.5, y=freq_rel_obs[7],
                              xend=3+0.5, yend=freq_rel_obs[7]) , size=2, color = "blue")+
    labs(x="Goal difference", y="Posterior probabilities")+
    yaxis_text(size=rel(1.2))+
    xaxis_text( size = rel(1.2))+
    scale_x_discrete(limits=esiti_short, labels=c("-3", "-2", "-1", "0","1", "2", "3"))+
    theme(axis.title=element_text(size=19),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15))
  }else if (type=="matches"){

  }

}
