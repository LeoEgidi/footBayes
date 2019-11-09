#' Plot football matches probabilities from Stan model
#'
#' Depicts probabilities from out-of-sample football matches.
#'
#' @param object An object of class \code{stanfit} as given by \code{stan_foot} function.
#' @param data A data frame, or a matrix containing the following mandatory items: home team, away team,
#'home goals, away goals.
#' @param home_team The home team for the predicted match.
#' @param away_team The away team for the predicted match.
#'
#' @examples
#' \dontrun{
#' ### weekly dynamics, predict the last four weeks

#' italy_2000<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000")
#'
#' fit <- stan_foot(data = italy_2000,
#'                  model="double_pois", predict =18,
#'                  dynamic_type = "weekly")  # double pois
#'
#' foot_prob(fit, italy_2000, "Inter",
#'           "Bologna FC")
#'
#' foot_prob(fit, italy_2000, "Reggina Calcio",
#'            "AC Milan")
#'}
#' @export


foot_prob <- function(object, data, home_team, away_team){

  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  teams <- unique(data$home)
  sims <- rstan::extract(object)
  predict <- dim(sims$y_prev)[2]
  data_prev <- data[(dim(data)[1]-predict +1):(dim(data)[1]),]
  find_match <- which(data_prev$home==home_team & data_prev$away == away_team )
  true_gol_home <- data$homegoals[dim(sims$y_rep)[2]+predict]
  true_gol_away <- data$awaygoals[dim(sims$y_rep)[2]+predict]

  if (length(find_match)==0){
    stop(paste("There is not any out-of-sample match:",
               home_team,"-", away_team, sep=""))
  }

  sims <- rstan::extract(object)
  M <- dim(sims$y_prev)[1]
  if (is.null(sims$y_prev)){
    stop("foot_prob function can not be used with the student_t model")
  }
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
  dim1 <- dim(counts_mix)[1]
  dim2 <- dim(counts_mix)[2]

  x <- seq(0,dim1-1, length.out=dim1)
  y <- seq(0,dim2-1, length.out=dim2)
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
