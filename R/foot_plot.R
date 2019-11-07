#' Plots
#'
#'
#' @param object An object of class \code{stanfit} as given by \code{stan_foot} function.
#' @param teams Character vector with the list of teams as extracted from the dataset.
#'
#' @examples
#'library(engsoccerdata)
#'library(tidyverse)
#'
#'ristr_italy <- as_data_frame(italy)
#'ristr_italy<- ristr_italy %>%
#'  select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000" |  Season=="2001"| Season =="2002)
#'  fit<-stan_foot(data = ristr_italy,
#'                        model="biv_pois", predict =306,
#'                        dynamic_type = "seasonal")
#'  teams <- unique(ristr_italy$home)
#'  foot_abilities(fit, teams)


foot_abilities <- function(object, teams){

  sims <- extract(object)
  att <- sims$att
  def <- sims$def

  if (length(dim(att))==3){
  T <- dim(att)[2]
  nteams <- dim(att)[3]
  att_med=apply(att,c(2,3), median)
  def_med=apply(def,c(2,3), median)
  att_025=apply(att, c(2,3), function(x) quantile(x, 0.025))
  att_25=apply(att, c(2,3), function(x) quantile(x, 0.25))
  att_75=apply(att, c(2,3), function(x) quantile(x, 0.75))
  att_975=apply(att, c(2,3), function(x) quantile(x, 0.975))
  def_025=apply(def, c(2,3), function(x) quantile(x, 0.025))
  def_25=apply(def, c(2,3), function(x) quantile(x, 0.25))
  def_75=apply(def, c(2,3), function(x)  quantile(x, 0.75))
  def_975=apply(def, c(2,3), function(x)  quantile(x, 0.975))

  mt_att_025 <- melt(att_025)
  mt_att_25 <- melt(att_25)
  mt_att_50 <- melt(att_med)
  mt_att_75 <- melt(att_75)
  mt_att_975 <- melt(att_975)

  mt_def_025 <- melt(def_025)
  mt_def_25 <- melt(def_25)
  mt_def_50 <- melt(def_med)
  mt_def_75 <- melt(def_75)
  mt_def_975 <- melt(def_975)

  teams_fac_rep <- rep(teams, each = T)
  times_rep <- rep(1:T, length(teams))

  att_data=data.frame(
    teams=teams_fac_rep,
    times=times_rep,
    mid=mt_att_50$value,
    lo=mt_att_25$value,
    hi=mt_att_75$value
  )

  def_data=data.frame(
    teams=teams_fac_rep,
    times=times_rep,
    mid=mt_def_50$value,
    lo=mt_def_25$value,
    hi=mt_def_75$value
  )

  position_lookup <-
    att_data %>%
    group_by(teams) %>%
    summarise(pos=first(teams))
  label_w_position <- function(team_name) {
    paste0(team_name, " (", with(position_lookup, pos[teams == player_name]),")")
  }
  ggplot() +
    geom_ribbon(
      aes(x = times, ymin = lo, ymax = hi),
      data = att_data,
      fill = color_scheme_get("gray")[[2]]
      ) +
    geom_ribbon(
      aes(x = times, ymin = lo, ymax = hi),
      data = def_data,
      fill = color_scheme_get("gray")[[2]]
      )+
    geom_line(
      aes(x = times, y = mid),
      data = att_data,
      size = 1,
      color = color_scheme_get("red")[[4]]
      )+
    geom_line(
      aes(x = times, y = mid),
      data = def_data,
      size = 1,
      color = color_scheme_get("blue")[[4]]
      )+
    scale_color_manual(values = c(color_scheme_get("blue")[[4]],
                                  color_scheme_get("red")[[4]]))+
    facet_wrap("teams", scales = "free")+
    lims(y = c( min(att_25-0.2), max(att_75+0.2))) +
    #scale_x_discrete( limits=c("07/08","","","", "11/12","", "","","", "16/17")  ) +
    labs(x = "Times", y = "Teams' effects",
         title = "Attack and defense effects (50% posterior bars)"
         #,
         #subtitle = "for teams of Premier League 2016/2017"
         ) +
    yaxis_text(size=rel(1.2))+
    xaxis_text( size = rel(1.2))+
    theme(plot.title = element_text(size = 16),
          strip.text = element_text(size = 8),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.subtitle=element_text(size=12))


  }else if (length(dim(att))==2){
    att_mean <- apply(att, 2, mean)
    att_sd <- apply(att, 2, sd)
    att_025 <- apply(att, 2, function(x) quantile(x, 0.025))
    att_975 <- apply(att, 2, function(x) quantile(x, 0.975))
    def_mean <- apply(def, 2, mean)
    def_sd <- apply(def, 2, sd)
    def_025 <- apply(def, 2, function(x) quantile(x, 0.025))
    def_975 <- apply(def, 2, function(x) quantile(x, 0.975))

    par(mfrow=c(1,1), oma =c(1,1,1,1))
    par(mfrow=c(1,1), oma =c(1,1,1,1))

    posterior <- as.array(object)
    #mcmc_intervals(posterior, regex_pars=c("att"))

    coefplot(rev(att_mean), rev(att_sd), CI=2,
             varnames=rev(teams), main="Attack/Defense abilities (95% post. intervals)\n",
             cex.var=1, mar=c(1,7,4,2), lwd=2,
             cex.main=0.9,pch=16, col="red")
    coefplot(rev(def_mean), rev(def_sd), CI=2,
             varnames=rev(teams), main="Defense abilities (95% post. intervals)\n",
             cex.var=1, mar=c(1,7,4,2), lwd=2,
             cex.main=0.9,pch=16, col="blue", add=TRUE)

  }

}


# foot_rank <- function(x, teams){
#   # Fra-Cro
#
#   previsioni1_pois<-sims_poisson$y_prev[, 1,1]
#   previsioni2_pois<-sims_poisson$y_prev[, 1,2]
#
#   previsioni1_bivpois<-sims_bivpois$y_prev[, 1,1]
#   previsioni2_bivpois<-sims_bivpois$y_prev[, 1,2]
#
#
#   posterior_prop1_pois<-table(subset(previsioni1_pois, previsioni1_pois<15))
#   posterior_prop2_pois<-table(subset(previsioni2_pois, previsioni2_pois<15))
#
#   posterior_prop1_bivpois<-table(subset(previsioni1_bivpois, previsioni1_bivpois<15))
#   posterior_prop2_bivpois<-table(subset(previsioni2_bivpois, previsioni2_bivpois<15))
#
#
#   teamaa=teams[team1_prev[1]]
#   teamab=teams[team2_prev[1]]
#
#   x_min=y_min=min(length(posterior_prop1_pois), length(posterior_prop2_pois))
#
#   counts_mix_pois<-matrix(0, min(length(posterior_prop1_pois), length(posterior_prop2_pois)),
#                           min(length(posterior_prop1_pois), length(posterior_prop2_pois)))
#
#   counts_mix_bivpois<-matrix(0, min(length(posterior_prop1_bivpois), length(posterior_prop2_bivpois)),
#                              min(length(posterior_prop1_bivpois), length(posterior_prop2_bivpois)))
#
#
#   for (j in 1: min(length(posterior_prop1_pois), length(posterior_prop2_pois))){
#     for (t in 1: min(length(posterior_prop1_pois), length(posterior_prop2_pois))){
#       counts_mix_pois[j,t]<-posterior_prop1_pois[j]*posterior_prop2_pois[t]
#     }}
#
#   for (j in 1: min(length(posterior_prop1_bivpois), length(posterior_prop2_bivpois))){
#     for (t in 1: min(length(posterior_prop1_bivpois), length(posterior_prop2_bivpois))){
#       counts_mix_bivpois[j,t]<-posterior_prop1_bivpois[j]*posterior_prop2_bivpois[t]
#     }}
#
#
#   x <- seq(0,5, length.out=6)
#   y <- seq(0,5, length.out=6)
#   data <- expand.grid(Home=x, Away=y)
#   data$Prob=as.double(counts_mix_pois[1:6, 1:6]/(M*M))
#
#
#   # To change the color of the gradation :
#
#   ggplot(data, aes(Home, Away, z= Prob)) + geom_tile(aes(fill = Prob)) +
#     theme_bw() +
#     scale_fill_gradient(low="white", high="black") +
#     geom_rect(aes(xmin = as.numeric(as.vector(gol_vero1))[1]-0.5,
#                   xmax = as.numeric(as.vector(gol_vero1))[1]+0.5,
#                   ymin = as.numeric(as.vector(gol_vero2))[1]-0.5,
#                   ymax =as.numeric(as.vector(gol_vero2))[1]+0.5),
#               fill = "transparent", color = "red", size = 1.5)+
#     labs(title=paste(  teams[team1_prev[1]],"-", teams[team2_prev[1]]))+
#     yaxis_text(size=12)+
#     xaxis_text( size = rel(12))+
#     theme(plot.title = element_text(size = 22),
#           strip.text = element_text(size = 18),
#           axis.text.x = element_text(size=22),
#           axis.text.y = element_text(size=22),
#           plot.subtitle=element_text(size=13),
#           axis.title=element_text(size=18,face="bold"),
#           legend.text=element_text(size=14))
#   ggsave(file=paste(teams[team1_prev[1]],"-", teams[team2_prev[1]], "Heatmap_pois.pdf", sep=""), width=6, height=6)
#
#
#   x <- seq(0,5, length.out=6)
#   y <- seq(0,5, length.out=6)
#   data <- expand.grid(Home=x, Away=y)
#   data$Prob=as.double(counts_mix_bivpois[1:6, 1:6]/(M*M))
#
#
#   # To change the color of the gradation :
#
#   ggplot(data, aes(Home, Away, z= Prob)) + geom_tile(aes(fill = Prob)) +
#     theme_bw() +
#     scale_fill_gradient(low="white", high="black") +
#     geom_rect(aes(xmin = as.numeric(as.vector(gol_vero1))[1]-0.5,
#                   xmax = as.numeric(as.vector(gol_vero1))[1]+0.5,
#                   ymin = as.numeric(as.vector(gol_vero2))[1]-0.5,
#                   ymax =as.numeric(as.vector(gol_vero2))[1]+0.5),
#               fill = "transparent", color = "red", size = 1.5)+
#     labs(title=paste(  teams[team1_prev[1]],"-", teams[team2_prev[1]]))+
#     yaxis_text(size=12)+
#     xaxis_text( size = rel(12))+
#     theme(plot.title = element_text(size = 22),
#           strip.text = element_text(size = 18),
#           axis.text.x = element_text(size=22),
#           axis.text.y = element_text(size=22),
#           plot.subtitle=element_text(size=13),
#           axis.title=element_text(size=18,face="bold"),
#           legend.text=element_text(size=14))
#   ggsave(file=paste(teams[team1_prev[1]],"-", teams[team2_prev[1]], "Heatmap_bivpois.pdf", sep=""), width=6, height=6)
#
# }
#
# foot_prob <- function()
