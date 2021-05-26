#' Posterior predictive checks for football  models
#'
#' @importFrom bayesplot yaxis_text
#' @importFrom bayesplot xaxis_text
#' @importFrom matrixStats colMedians
#' @importFrom matrixStats colVars
#' @importFrom matrixStats colQuantiles
#' @export


pp_foot <- function(data, object,
                    type = c("aggregated", "matches")){

  sims <- rstan::extract(object)
  y <- as.matrix(data[,4:5])
  diff_gol <- as.vector(y[,1] - y[,2])
  diff_gol_rep <- sims$diff_y_rep
  esiti_short <- seq(-3,3,1)
  M <-dim(sims$diff_y_rep)[1]
  freq_rel_matrix <- matrix(NA, M, length(esiti_short))
  ngames_train <- dim(sims$y_rep)[2]

  if (missing(type)){
    type <- "aggregated"
  }
  if (type =="aggregated"){

    check.integer <- function(x) {
      x == round(x)
    }

    if (check.integer(median(diff_gol_rep))==TRUE){

  for (j in 1:M){
    for (u in 1:length(esiti_short)){

      if (length((as.double(table(diff_gol_rep[j,]))[as.double(names(table(diff_gol_rep[j,])))==esiti_short[u]]))==0){
        freq_rel_matrix[j,u] <- 0 # correzione quando non ricorre il risultato nel MCMC
      }else{
       freq_rel_matrix[j,u] <-  (as.double(table(diff_gol_rep[j,]))[as.double(names(table(diff_gol_rep[j,])))==esiti_short[u]])/ngames_train
      }}}

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
    labs(x="Goal difference", y="Posterior pred. distrib.")+
    yaxis_text(size=rel(1.2))+
    xaxis_text( size = rel(1.2))+
    scale_x_discrete(limits=esiti_short, labels=c("-3", "-2", "-1", "0","1", "2", "3"))+
    theme(axis.title=element_text(size=19),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15))
    }else{
      #ppc_dens_overlay(diff_gol, diff_gol_rep)
      plot(density(diff_gol, bw =0.5),
           xlab = "Goal difference",
           ylab = "Posterior pred. distrib.",
           main = "",
           lwd =3, col = "blue", cex.lab=1.3)
      for (i in 1:M){
        lines(density(diff_gol_rep[i,], bw =0.5),
              col = "lightgray")
      }
      lines(density(diff_gol, bw =0.5), col = "blue",
            lwd=3)
    }


  }else if (type=="matches"){
    scd <- as.numeric(as.vector(diff_gol))
    scd_sims <- diff_gol_rep
    scd_hat <- colMedians(scd_sims)
    scd_se <- sqrt(colVars(scd_sims))
    alpha <- 0.95;
    scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2)
    scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2)
    ci95 <- sum(scd < scd_ub & scd_lb<scd)/ngames_train
    ngames_train_draw <- sum(scd ==0)
    scd_draw <- scd[scd==0]
    ci95_draw <- sum(scd_draw < scd_ub[scd==0] & scd_lb[scd==0]<scd_draw)/ngames_train_draw
    alpha <- 0.5;
    scd_ub2 <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2)
    scd_lb2 <- colQuantiles(scd_sims, probs = (1-alpha)/2)
    ci50 <- sum(scd < scd_ub2 & scd_lb2<scd)/ngames_train
    ci50_draw <- sum(scd_draw < scd_ub2[scd==0] & scd_lb2[scd==0]<scd_draw)/ngames_train_draw

    sort_scd <- scd[order(scd)]
    sort_scd_hat <- scd_hat[order(scd)]
    sort_scd_se <- scd_se[order(scd)]
    sort_scd_ub <- scd_ub[order(scd)]
    sort_scd_lb <- scd_lb[order(scd)]
    sort_scd_ub2 <- scd_ub2[order(scd)]
    sort_scd_lb2 <- scd_lb2[order(scd)]

    df <- data.frame(list(scd = sort_scd, scd_hat = sort_scd_hat, scd_se = sort_scd_se,
                          scd_ub = sort_scd_ub, scd_lb = sort_scd_lb,
                          scd_ub2 = sort_scd_ub2, scd_lb2 = sort_scd_lb2))

ggplot(df, aes(x = c(1:ngames_train))) +
      geom_ribbon(aes(ymin = scd_lb, ymax = scd_ub),
                  fill="#F0E442") +
      geom_ribbon(aes(ymin = scd_lb2, ymax = scd_ub2),
                  fill="khaki3") +
      #geom_line(aes(y=scd_hat),colour="darkred") +
      #geom_point(aes(y=scd_hat),colour="darkred",shape=4) +
      geom_point(aes(y=scd), size = 0.5, col="blue") +
      scale_x_continuous(name="games") +
      #scale_y_discrete(name="score difference", limits=seq(-8,8)) +
      scale_y_continuous(name="Goal difference",
                  breaks = c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8),
                  sec.axis = dup_axis()) +
      yaxis_text(size=rel(1.4))+
      xaxis_text( size = rel(1.4))+
      theme(axis.title=element_text(size=19),
      axis.text.x = element_text(size=15),
      axis.text.y = element_text(size=15))
  }

}
