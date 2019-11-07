#' Plots
#'
#'
#' @param object An object of class \code{stanfit} as given by \code{stan_foot} function.
#' @param teams Character vector with the list of teams as extracted from the dataset.
#'
#' @examples
#' library(engsoccerdata)
#' library(tidyverse)
#' italy <- as_tibble(italy)
#'
#' ### no dynamics, no prediction
#'
#' italy_2000_2002<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'  filter(Season=="2000" |  Season=="2001" | Season =="2002")
#'
#' fit1 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois") # double poisson
#'
#' fit2 <- stan_foot(data = italy_2000_2002,
#'                 model="biv_pois")    # bivariate poisson
#'
#' fit3 <- stan_foot(data = italy_2000_2002,
#'                 model="skellam")     # skellam
#'
#' fit4 <- stan_foot(data = italy_2000_2002,
#'                 model="student_t")   # student_t
#'
#' teams <- unique(italy_2000_2002$home)
#' foot_abilities(fit1, teams)
#' foot_abilities(fit2, teams)
#' foot_abilities(fit3, teams)
#' foot_abilities(fit4, teams)
#'
#' ### seasonal dynamics, predict the last season
#'
#' fit5 <-stan_foot(data = italy_2000_2002,
#'                        model="biv_pois", predict =306,
#'                        dynamic_type = "seasonal")   # bivariate poisson
#' teams <- unique(italy_2000_2002)
#' foot_abilities(fit5, teams)
#'
#' ### weekly dynamics, predict the last four weeks
#'
#' italy_2000<- italy %>%
#'   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'   filter(Season=="2000")
#'
#' fit6 <- stan_foot(data = italy_2000,
#'                 model="double_pois", predict =36,
#'                 dynamic_type = "weekly")  # double poisson
#'
#' fit7 <- stan_foot(data = italy_2000,
#'                 model="student_t", predict =36,
#'                 dynamic_type = "weekly")  # student_t
#'
#' teams <- unique(italy_2000$home)
#' foot_abilities(fit6, teams)
#' foot_abilities(fit7, teams)
#'
#'@export


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
    ord <- sort.int(att_mean, decreasing =TRUE,
                    index.return = TRUE)$ix

    coefplot(rev(att_mean[ord]), rev(att_sd[ord]), CI=2,
             varnames=rev(teams[ord]), main="Attack/Defense abilities (95% post. intervals)\n",
             cex.var=1, mar=c(1,7,4,2), lwd=2,
             cex.main=0.9,pch=16, col="red")
    coefplot(rev(def_mean[ord]), rev(def_sd[ord]), CI=2,
             varnames=rev(teams[ord]), main="Defense abilities (95% post. intervals)\n",
             cex.var=1, mar=c(1,7,4,2), lwd=2,
             cex.main=0.9,pch=16, col="blue", add=TRUE)

  }else if (length(dim(att))==0){ # student_t case

    ability <- sims$ability

    if (length(dim(ability))==3){

      T <- dim(ability)[2]
      nteams <- dim(ability)[3]
      ability_med=apply(ability,c(2,3), median)
      ability_025=apply(ability, c(2,3), function(x) quantile(x, 0.025))
      ability_25=apply(ability, c(2,3), function(x) quantile(x, 0.25))
      ability_75=apply(ability, c(2,3), function(x) quantile(x, 0.75))
      ability_975=apply(ability, c(2,3), function(x) quantile(x, 0.975))

      mt_ability_025 <- melt(ability_025)
      mt_ability_25 <- melt(ability_25)
      mt_ability_50 <- melt(ability_med)
      mt_ability_75 <- melt(ability_75)
      mt_ability_975 <- melt(ability_975)

      teams_fac_rep <- rep(teams, each = T)
      times_rep <- rep(1:T, length(teams))

      ability_data=data.frame(
        teams=teams_fac_rep,
        times=times_rep,
        mid=mt_ability_50$value,
        lo=mt_ability_25$value,
        hi=mt_ability_75$value
      )

      position_lookup <-
        ability_data %>%
        group_by(teams) %>%
        summarise(pos=first(teams))
      label_w_position <- function(team_name) {
        paste0(team_name, " (", with(position_lookup, pos[teams == player_name]),")")
      }
      ggplot() +
        geom_ribbon(
          aes(x = times, ymin = lo, ymax = hi),
          data = ability_data,
          fill = color_scheme_get("red")[[2]]
        ) +
        geom_line(
          aes(x = times, y = mid),
          data = ability_data,
          size = 1,
          color = color_scheme_get("orange")[[4]]
        )+
        scale_color_manual(values = c(color_scheme_get("blue")[[4]],
                                      color_scheme_get("red")[[4]]))+
        facet_wrap("teams", scales = "free")+
        lims(y = c( min(ability_25-0.2), max(ability_75+0.2))) +
        #scale_x_discrete( limits=c("07/08","","","", "11/12","", "","","", "16/17")  ) +
        labs(x = "Times", y = "Teams' effects",
             title = "Global abilities effects (50% posterior bars)"
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


    }else if (length(dim(ability))==2){
      ability_mean <- apply(ability, 2, mean)
      ability_sd <- apply(ability, 2, sd)
      ability_025 <- apply(ability, 2, function(x) quantile(x, 0.025))
      ability_975 <- apply(ability, 2, function(x) quantile(x, 0.975))

      par(mfrow=c(1,1), oma =c(1,1,1,1))
      par(mfrow=c(1,1), oma =c(1,1,1,1))

      posterior <- as.array(object)
      #mcmc_intervals(posterior, regex_pars=c("ability"))
      ord <- sort.int(ability_mean, decreasing =TRUE,
                      index.return = TRUE)$ix

      coefplot(rev(ability_mean[ord]), rev(ability_sd[ord]), CI=2,
               varnames=rev(teams[ord]), main="Global abilities (95% post. intervals)\n",
               cex.var=1, mar=c(1,7,4,2), lwd=2,
               cex.main=0.9,pch=16, col="orange")


    }


  }

}


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
#
# foot_prob <- function()
