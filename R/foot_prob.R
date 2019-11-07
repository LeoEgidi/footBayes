#' Probabilities
#'
#' @param object
#'
#' @examples
#'
#' ### weekly dynamics, predict the last four weeks

#' italy_2000<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000")
#'
#' fit <- stan_foot(data = italy_2000,
#'                  model="student_t", predict =18,
#'                  dynamic_type = "weekly")  # double poisson
#'
#' teams <- unique(italy_2000$home)
#' foot_prob(fit, teams, italy_2000, "Inter",
#'           "Bologna FC", predict = 18)
#' @export


foot_prob <- function(object, teams, data, home_team, away_team,
                      predict,
                      true_gol_home = 0, true_gol_away = 0){

  data_prev <- data[(dim(data)[1]-predict +1):(dim(data)[1]),]
  find_match <- which(data_prev$home==home_team & data_prev$visitor == away_team )
  sims <- extract(object)
  M <- dim(sims$y_prev)[1]
  previsioni1<-sims$y_prev[, find_match ,1]
  previsioni2<-sims$y_prev[, find_match,2]
  posterior_prop1<-table(subset(previsioni1, previsioni1<15))
  posterior_prop2<-table(subset(previsioni2, previsioni2<15))

  teamaa=home_team
  teamab=away_team

  x_min=y_min=min(length(posterior_prop1),
                  length(posterior_prop2))

  counts_mix<-matrix(0, min(length(posterior_prop1), length(posterior_prop2)),
                     min(length(posterior_prop1), length(posterior_prop2)))

  for (j in 1: min(length(posterior_prop1), length(posterior_prop2))){
    for (t in 1: min(length(posterior_prop1), length(posterior_prop2))){
      counts_mix[j,t]<-posterior_prop1[j]*posterior_prop2[t]
    }}

  x <- seq(0,5, length.out=dim(counts_mix)[1])
  y <- seq(0,5, length.out=dim(counts_mix)[2])
  data <- expand.grid(Home=x, Away=y)
  data$Prob <- as.double(counts_mix/(M*M))


  # To change the color of the gradation :

  ggplot(data, aes(Home, Away, z= Prob)) + geom_tile(aes(fill = Prob)) +
    theme_bw() +
    scale_fill_gradient(low="white", high="black") +
    geom_rect(aes(xmin = as.numeric(as.vector(true_gol_home))[1]-0.5,
                  xmax = as.numeric(as.vector(true_gol_home))[1]+0.5,
                  ymin = as.numeric(as.vector(true_gol_away))[1]-0.5,
                  ymax =as.numeric(as.vector(true_gol_away))[1]+0.5),
              fill = "transparent", color = "red", size = 1.5)+
    labs(title=paste(  teamaa,"-", teamab))+
    yaxis_text(size=12)+
    xaxis_text( size = rel(12))+
    theme(plot.title = element_text(size = 22),
          strip.text = element_text(size = 18),
          axis.text.x = element_text(size=22),
          axis.text.y = element_text(size=22),
          plot.subtitle=element_text(size=13),
          axis.title=element_text(size=18,face="bold"),
          legend.text=element_text(size=14))
  #ggsave(file=paste(teams[team1_prev[1]],"-", teams[team2_prev[1]], "Heatmap_pois.pdf", sep=""), width=6, height=6)



}