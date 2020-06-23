#' Plot football abilities from Stan models
#'
#' Depicts teams' abilities from the Stan models fitted via the \code{stan_foot} function.
#'
#'
#' @param object An object of class \code{stanfit} as given by \code{stan_foot} function.
#' @param data A data frame, or a matrix containing the following mandatory items: home team, away team,
#'            home goals, away goals.
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
#' foot_abilities(fit1, italy_2000_2002)
#' foot_abilities(fit2, italy_2000_2002)
#' foot_abilities(fit3, italy_2000_2002)
#' foot_abilities(fit4, italy_2000_2002)
#' ggsave(file="student_t_ab.pdf", width =12, height =7)
#'
#' ### seasonal dynamics, predict the last season
#'
#' fit5 <-stan_foot(data = italy_2000_2002,
#'                        model="biv_pois", predict =306,
#'                        dynamic_type = "seasonal")   # bivariate poisson
#' foot_abilities(fit5, italy_2000_2002)
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
#'
#' foot_abilities(fit6, italy_2000)
#' foot_abilities(fit7, italy_2000)
#'
#'@export


foot_abilities <- function(object, data){

  teams <- unique(data$home)
  if (class(object)=="stanfit"){

  sims <- rstan::extract(object)
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
  }else{
    if (!is.null(dim(object$att))){
    att <- object$att
    def <- object$def
    par(mfrow=c(1,1), oma =c(1,1,1,1))
    par(mfrow=c(1,1), oma =c(1,1,1,1))

    ord <- sort.int(att[,2], decreasing =TRUE,
                    index.return = TRUE)$ix

    arm::coefplot(as.vector(rev(att[ord,2])),
                  as.vector(rev(att[ord,2])),
             #sds = rev(att[ord,2]),
             CI=2,
             lower.conf.bounds = as.vector(rev(att[ord,1])),
             upper.conf.bounds = as.vector(rev(att[ord,3])),
             varnames=rev(teams[ord]), main="Attack/Defense abilities (95% conf. intervals)\n",
             cex.var=1, mar=c(5,4,2,1), lwd=2,
             cex.main=0.9,pch=16, col="red")
    arm::coefplot(as.vector(rev(def[ord,2])),
                  as.vector(rev(def[ord,2])),
                  CI=2,
                  lower.conf.bounds = as.vector(rev(def[ord,1])),
                  upper.conf.bounds = as.vector(rev(def[ord,3])),
                  varnames=rev(teams[ord]), main="Defense abilities (95% post. intervals)\n",
                  cex.var=1, mar=c(5,4,2,1), lwd=2,
                  cex.main=0.9,pch=16, col="blue", add=TRUE)

    }else{  # student_t case
      ability <- object$abilities
      par(mfrow=c(1,1), oma =c(1,1,1,1))
      par(mfrow=c(1,1), oma =c(1,1,1,1))

      #mcmc_intervals(posterior, regex_pars=c("ability"))
      ord <- sort.int(ability[,2], decreasing =TRUE,
                      index.return = TRUE)$ix

      coefplot(as.vector(rev(ability[ord,2])),
               as.vector(rev(ability[ord,2])), CI=2,
               lower.conf.bounds = as.vector(rev(ability[ord,1])),
               upper.conf.bounds = as.vector(rev(ability[ord,3])),
               varnames=rev(teams[ord]), main="Global abilities (95% conf. intervals)\n",
               cex.var=1, mar=c(1,7,4,2), lwd=2,
               cex.main=0.9,pch=16, col="orange")

    }
  }




}



