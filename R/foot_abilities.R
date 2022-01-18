#' Plot football abilities from Stan and MLE models
#'
#' Depicts teams' abilities either from the Stan models fitted via the \code{stan_foot} function
#' or from MLE models fitted via the \code{mle_foot} function.
#'
#'
#' @param object An object either of class \code{stanfit} as given by \code{stan_foot} function or class
#'               \code{list} containing the Maximum Likelihood Estimates (MLE) for the model parameters fitted
#'                with \code{mle_foot}.
#' @param data A data frame, or a matrix containing the following mandatory items: home team, away team,
#'            home goals, away goals.
#' @param type Type of ability in Poisson models: one among \code{"defense"}, \code{"attack"} or \code{"both"}.
#' @param team  Valid team names.
#'
#' @return
#'
#' Abilities plots for the selected teams: for Poisson models only, red denotes the attack,
#' blue the defense.
#'
#' @author Leonardo Egidi \email{legidi@units.it}
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


foot_abilities <- function(object, data,
                           type = c("attack", "defense", "both"),
                           team,...){


  ## Check for 'type'
  if (missing(type)){
    type <- "both"
  }
  match.arg(type, c("attack", "defense", "both") )

  ## Further arguments for coefplot
  user_dots <- list(CI=2,
                    vertical=TRUE,
                    v.axis=TRUE, h.axis=TRUE,
                    cex.var=0.7, cex.pts=0.9,
                    var.las=2, main=NULL, xlab=NULL, ylab=NULL, mar=c(1,5,4.1,1.8),
                    plot=TRUE,  offset=0.1,
                    cex.main =0.9, pch = 16, lwd = 2,
                    col = 1)

  if (missing(...)){
    user_dots <- user_dots
  }else{
    user_dots_prel <- list(...)
    names_prel <- names(user_dots_prel)
    names_dots<- names(user_dots)
    for (u in 1:length(names_prel)){
      user_dots[names_prel[u] == names_dots]<- user_dots_prel[u]
    }
  }

  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")

  teams <- unique(c(data$home, data$away))

  if (!is.character(teams)){
    teams <- as.character(teams)
  }



  if (class(object)=="stanfit"){
    sims <- rstan::extract(object)
    # if (is.null(sims$y_prev)){
    #   teams <- unique(c(data$home, data$away))
    # }else{
    #   teams <- unique(c(data$home[(dim(sims$y_rep)[2]+1):
    #                                 (dim(sims$y_rep)[2] +
    #                                    dim(sims$y_prev)[2])],
    #                     data$away[(dim(sims$y_rep)[2]+1):
    #                                 (dim(sims$y_rep)[2] +
    #                                    dim(sims$y_prev)[2])]))
    # }

    # check on selected team

    if (missing(team)){
      sel_teams <- teams
    # }else if(team==c("all")){
    #   sel_teams <- teams
    }else{
      sel_teams<-teams[match(team, teams)]
    }
    sel_teams_index <- match(sel_teams, teams)

    if (is.na(sum(sel_teams_index))){
      stop("Select only valid teams' names!")
    }

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

  squadre_valide <- match(sel_teams,unique(c(data$home, data$away)))

  mt_att_025 <- melt(att_025[, squadre_valide])
  mt_att_25 <- melt(att_25[, squadre_valide])
  mt_att_50 <- melt(att_med[, squadre_valide])
  mt_att_75 <- melt(att_75[, squadre_valide])
  mt_att_975 <- melt(att_975[, squadre_valide])

  mt_def_025 <- melt(def_025[, squadre_valide])
  mt_def_25 <- melt(def_25[, squadre_valide])
  mt_def_50 <- melt(def_med[, squadre_valide])
  mt_def_75 <- melt(def_75[, squadre_valide])
  mt_def_975 <- melt(def_975[, squadre_valide])

  teams_fac_rep <- rep(sel_teams, each = T)
  times_rep <- rep(1:T, length(sel_teams))

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

  if (length(unique(data$season))==1){
    timings <- 1:dim(sims$att)[2]
    sp <- length(timings)%/%5
    timings_breaks <- timings[sp*c(1:5)]
  }else{
    timings <- unique(data$season)
  }
  if (type =="both"){
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
    lims(y = c( min(att_25-0.3), max(att_75+0.3))) +
    scale_x_discrete( limits=factor(timings), breaks = timings_breaks  ) +
    labs(x = "Times", y = "Teams' effects",
         title = "Attack and defense effects (50% posterior bars)"
         #,
         #subtitle = "for teams of Premier League 2016/2017"
         ) +
    yaxis_text(size=rel(1.2))+
    xaxis_text( size = rel(1.2))+
    theme(plot.title = element_text(size = 16),
          strip.text = element_text(size = 8),
          axis.text.x =  element_text(face="bold",
                                                   color="black",
                                                   angle=45, size =9),
          axis.text.y = element_text(size=11),
          plot.subtitle=element_text(size=12))
  }else if (type =="attack"){
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
      geom_line(
        aes(x = times, y = mid),
        data = att_data,
        size = 1,
        color = color_scheme_get("red")[[4]]
      )+
      scale_color_manual(values = c(color_scheme_get("red")[[4]]))+
      facet_wrap("teams", scales = "free")+
      lims(y = c( min(att_25-0.3), max(att_75+0.3))) +
      scale_x_discrete( limits=factor(timings), breaks = timings_breaks  ) +
      labs(x = "Times", y = "Teams' effects",
           title = "Attack effects (50% posterior bars)"
      ) +
      yaxis_text(size=rel(1.2))+
      xaxis_text( size = rel(1.2))+
      theme(plot.title = element_text(size = 16),
            strip.text = element_text(size = 8),
            axis.text.x =  element_text(face="bold",
                                        color="black",
                                        angle=45, size =9),
            axis.text.y = element_text(size=11),
            plot.subtitle=element_text(size=12))

  }else if (type =="defense"){
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
        data = def_data,
        fill = color_scheme_get("gray")[[2]]
      )+
      geom_line(
        aes(x = times, y = mid),
        data = def_data,
        size = 1,
        color = color_scheme_get("blue")[[4]]
      )+
      scale_color_manual(values = c(color_scheme_get("blue")[[4]]
      ))+
      facet_wrap("teams", scales = "free")+
      lims(y = c( min(def_25-0.3), max(def_75+0.3))) +
      scale_x_discrete( limits=factor(timings), breaks = timings_breaks  ) +
      labs(x = "Times", y = "Teams' effects",
           title = "Defense effects (50% posterior bars)"
      ) +
      yaxis_text(size=rel(1.2))+
      xaxis_text( size = rel(1.2))+
      theme(plot.title = element_text(size = 16),
            strip.text = element_text(size = 8),
            axis.text.x =  element_text(face="bold",
                                        color="black",
                                        angle=45, size =9),
            axis.text.y = element_text(size=11),
            plot.subtitle=element_text(size=12))
  }


  }else if (length(dim(att))==2){
    att_mean <- apply(att, 2, mean)[sel_teams_index]
    att_sd <- apply(att, 2, sd)[sel_teams_index]
    att_025 <- apply(att, 2, function(x) quantile(x, 0.025))[sel_teams_index]
    att_975 <- apply(att, 2, function(x) quantile(x, 0.975))[sel_teams_index]
    def_mean <- apply(def, 2, mean)[sel_teams_index]
    def_sd <- apply(def, 2, sd)[sel_teams_index]
    def_025 <- apply(def, 2, function(x) quantile(x, 0.025))[sel_teams_index]
    def_975 <- apply(def, 2, function(x) quantile(x, 0.975))[sel_teams_index]

    par(mfrow=c(1,1), oma =c(1,1,1,1))
    par(mfrow=c(1,1), oma =c(1,1,1,1))

    posterior <- as.array(object)
    #mcmc_intervals(posterior, regex_pars=c("att"))
    ord <- sort.int(att_mean, decreasing =TRUE,
                    index.return = TRUE)$ix
    ord2 <- sort.int(def_mean, decreasing =FALSE,
                    index.return = TRUE)$ix

    if (type == "both"){
    arm::coefplot(rev(att_mean[ord]), rev(att_sd[ord]), CI=user_dots$CI,
             varnames=rev(sel_teams[ord]),
             main="Attack/Defense abilities (95% post. intervals)\n",
             cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
             cex.main=user_dots$cex.main, pch=user_dots$pch,
             h.axis=user_dots$h.axis,
             cex.pts=user_dots$cex.pts,
             vertical= user_dots$vertical,
             v.axis=user_dots$v.axis,
             xlab=user_dots$xlab, ylab=user_dots$ylab,
             plot = user_dots$plot, offset = user_dots$offset,
             col="red", xlim= c(-1.5, 1.5))
    arm::coefplot(rev(def_mean[ord]), rev(def_sd[ord]), CI=user_dots$CI,
             varnames=rev(sel_teams[ord]),
             main="Defense abilities (95% post. intervals)\n",
             cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
             cex.main=user_dots$cex.main, pch=user_dots$pch,
             h.axis=user_dots$h.axis,
             cex.pts=user_dots$cex.pts,
             vertical= user_dots$vertical,
             v.axis=user_dots$v.axis,
             xlab=user_dots$xlab, ylab=user_dots$ylab,
             plot = user_dots$plot, offset = user_dots$offset,
             col="blue", add=TRUE)
    }else if (type == "attack"){
      arm::coefplot(rev(att_mean[ord]), rev(att_sd[ord]), CI=user_dots$CI,
                    varnames=rev(sel_teams[ord]),
                    main="Attack abilities (95% post. intervals)\n",
                    cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
                    cex.main=user_dots$cex.main, pch=user_dots$pch,
                    h.axis=user_dots$h.axis,
                    cex.pts=user_dots$cex.pts,
                    vertical= user_dots$vertical,
                    v.axis=user_dots$v.axis,
                    xlab=user_dots$xlab, ylab=user_dots$ylab,
                    plot = user_dots$plot, offset = user_dots$offset,
                    col="red", xlim= c(-1.5, 1.5))

    }else if (type =="defense"){
      arm::coefplot(rev(def_mean[ord2]), rev(def_sd[ord2]), CI=user_dots$CI,
                    varnames=rev(sel_teams[ord2]),
                    main="Defense abilities (95% post. intervals)\n",
                    cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
                    cex.main=user_dots$cex.main, pch=user_dots$pch,
                    h.axis=user_dots$h.axis,
                    cex.pts=user_dots$cex.pts,
                    vertical= user_dots$vertical,
                    v.axis=user_dots$v.axis,
                    xlab=user_dots$xlab, ylab=user_dots$ylab,
                    plot = user_dots$plot, offset = user_dots$offset,
                    col="blue")
    }
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

      squadre_valide <- match(sel_teams,unique(c(data$home, data$away)))

      mt_ability_025 <- melt(ability_025[, squadre_valide])
      mt_ability_25 <- melt(ability_25[, squadre_valide])
      mt_ability_50 <- melt(ability_med[, squadre_valide])
      mt_ability_75 <- melt(ability_75[, squadre_valide])
      mt_ability_975 <- melt(ability_975[, squadre_valide])

      teams_fac_rep <- rep(sel_teams, each = T)
      times_rep <- rep(1:T, length(sel_teams))

      ability_data=data.frame(
        teams=teams_fac_rep,
        times=times_rep,
        mid=mt_ability_50$value,
        lo=mt_ability_25$value,
        hi=mt_ability_75$value
      )

      if (length(unique(data$season))==1){
        timings <- 1:dim(sims$ability)[2]
        sp <- length(timings)%/%5
        timings_breaks <- timings[sp*c(1:5)]

      }else{
        timings <- unique(data$season)
      }

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
          color = color_scheme_get("red")[[4]]
        )+
        scale_color_manual(values = c(color_scheme_get("blue")[[4]],
                                      color_scheme_get("red")[[4]]))+
        facet_wrap("teams", scales = "free")+
        lims(y = c( min(ability_25-0.2), max(ability_75+0.2))) +
        scale_x_discrete( limits=factor(timings), breaks = timings_breaks  ) +
        labs(x = "Times", y = "Teams' effects",
             title = "Global abilities effects (50% posterior bars)"
             #,
             #subtitle = "for teams of Premier League 2016/2017"
        ) +
        yaxis_text(size=rel(1.2))+
        xaxis_text( size = rel(1.2))+
        theme(plot.title = element_text(size = 16),
              strip.text = element_text(size = 8),
              axis.text.x =  element_text(face="bold",
                                          color="black",
                                          angle=45, size =9),
              axis.text.y = element_text(size=11),
              plot.subtitle=element_text(size=12))


    }else if (length(dim(ability))==2){
      ability_mean <- apply(ability, 2, mean)[sel_teams_index]
      ability_sd <- apply(ability, 2, sd)[sel_teams_index]
      ability_025 <- apply(ability, 2, function(x) quantile(x, 0.025))[sel_teams_index]
      ability_975 <- apply(ability, 2, function(x) quantile(x, 0.975))[sel_teams_index]

      par(mfrow=c(1,1), oma =c(1,1,1,1))
      par(mfrow=c(1,1), oma =c(1,1,1,1))

      posterior <- as.array(object)
      #mcmc_intervals(posterior, regex_pars=c("ability"))
      ord <- sort.int(ability_mean, decreasing =TRUE,
                      index.return = TRUE)$ix

      arm::coefplot(rev(ability_mean[ord]), rev(ability_sd[ord]), CI=user_dots$CI,
               varnames=rev(sel_teams[ord]), main="Global abilities (95% post. intervals)\n",
               cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
               cex.main=user_dots$cex.main, pch=user_dots$pch,
               h.axis=user_dots$h.axis,
               cex.pts=user_dots$cex.pts,
               vertical= user_dots$vertical,
               v.axis=user_dots$v.axis,
               xlab=user_dots$xlab, ylab=user_dots$ylab,
               plot = user_dots$plot, offset = user_dots$offset,
               col= user_dots$col)


    }
  }
  }else if (class(object)=="list"){

    # check on selected team

    if (missing(team)){
      sel_teams <- teams
    # }else if(team==c("all")){
    #   sel_teams <- teams
    }else{
      sel_teams<-teams[match(team, unique(c(data$home, data$away)))]
    }
    sel_teams_index <- match(sel_teams, unique(c(data$home, data$away)))

    if (is.na(sum(sel_teams_index))){
      stop("Select only valid teams' names!")
    }

    if (!is.null(dim(object$att))){
    att <- object$att[sel_teams_index,]
    def <- object$def[sel_teams_index,]
    par(mfrow=c(1,1), oma =c(1,1,1,1))
    par(mfrow=c(1,1), oma =c(1,1,1,1), mar = c(5,4,2,1))



    if (is.vector(att)&is.vector(def)){
      stop("Please, select at least two teams")

    }else{
      ord <- sort.int(att[,2], decreasing =TRUE,
                      index.return = TRUE)$ix
      ord2 <- sort.int(def[,2], decreasing =FALSE,
                       index.return = TRUE)$ix
    }


   if (type =="both"){
    arm::coefplot(as.vector(rev(att[ord,2])),
                  as.vector(rev(att[ord,2])),
             CI=user_dots$CI,
             lower.conf.bounds = as.vector(rev(att[ord,1])),
             upper.conf.bounds = as.vector(rev(att[ord,3])),
             varnames=rev(sel_teams[ord]), main="Attack/Defense abilities (95% conf. intervals)\n",
             cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
             cex.main=user_dots$cex.main, pch=user_dots$pch,
             h.axis=user_dots$h.axis,
             cex.pts=user_dots$cex.pts,
             vertical= user_dots$vertical,
             v.axis=user_dots$v.axis,
             xlab=user_dots$xlab, ylab=user_dots$ylab,
             plot = user_dots$plot, offset = user_dots$offset, col="red",
             xlim= c(-1.5, 1.5))
    arm::coefplot(as.vector(rev(def[ord,2])),
                  as.vector(rev(def[ord,2])),
                  CI=user_dots$CI,
                  lower.conf.bounds = as.vector(rev(def[ord,1])),
                  upper.conf.bounds = as.vector(rev(def[ord,3])),
                  varnames=rev(sel_teams[ord]), main="Defense abilities (95% post. intervals)\n",
                  cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
                  cex.main=user_dots$cex.main, pch=user_dots$pch,
                  h.axis=user_dots$h.axis,
                  cex.pts=user_dots$cex.pts,
                  vertical= user_dots$vertical,
                  v.axis=user_dots$v.axis,
                  xlab=user_dots$xlab, ylab=user_dots$ylab,
                  plot = user_dots$plot, offset = user_dots$offset,
                  col="blue", add=TRUE)
      }else if (type =="attack"){
        arm::coefplot(as.vector(rev(att[ord,2])),
                      as.vector(rev(att[ord,2])),
                      CI=user_dots$CI,
                      lower.conf.bounds = as.vector(rev(att[ord,1])),
                      upper.conf.bounds = as.vector(rev(att[ord,3])),
                      varnames=rev(sel_teams[ord]), main="Attack abilities (95% conf. intervals)\n",
                      cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
                      cex.main=user_dots$cex.main, pch=user_dots$pch,
                      h.axis=user_dots$h.axis,
                      cex.pts=user_dots$cex.pts,
                      vertical= user_dots$vertical,
                      v.axis=user_dots$v.axis,
                      xlab=user_dots$xlab, ylab=user_dots$ylab,
                      plot = user_dots$plot, offset = user_dots$offset, col="red",
                      xlim= c(-1.5, 1.5))
      }else if (type =="defense"){
        arm::coefplot(as.vector(rev(def[ord2,2])),
                      as.vector(rev(def[ord2,2])),
                      CI=user_dots$CI,
                      lower.conf.bounds = as.vector(rev(def[ord2,1])),
                      upper.conf.bounds = as.vector(rev(def[ord2,3])),
                      varnames=rev(sel_teams[ord2]), main="Defense abilities (95% conf. intervals)\n",
                      cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
                      cex.main=user_dots$cex.main, pch=user_dots$pch,
                      h.axis=user_dots$h.axis,
                      cex.pts=user_dots$cex.pts,
                      vertical= user_dots$vertical,
                      v.axis=user_dots$v.axis,
                      xlab=user_dots$xlab, ylab=user_dots$ylab,
                      plot = user_dots$plot, offset = user_dots$offset,
                      col="blue")
      }
    }else{  # student_t case



      ability <- object$abilities[sel_teams_index,]
      par(mfrow=c(1,1), oma =c(1,1,1,1))
      par(mfrow=c(1,1), oma =c(1,1,1,1))

      if (is.vector(ability)){
        stop("Please, select at least two teams")

      }else{
      #mcmc_intervals(posterior, regex_pars=c("ability"))
      ord <- sort.int(ability[,2], decreasing =TRUE,
                      index.return = TRUE)$ix
      }

      arm::coefplot(as.vector(rev(ability[ord,2])),
               as.vector(rev(ability[ord,2])), CI=user_dots$CI,
               lower.conf.bounds = as.vector(rev(ability[ord,1])),
               upper.conf.bounds = as.vector(rev(ability[ord,3])),
               varnames=rev(sel_teams[ord]), main="Global abilities (95% conf. intervals)\n",
               cex.var= user_dots$cex.var, mar=user_dots$mar, lwd=user_dots$lwd,
               cex.main=user_dots$cex.main, pch=user_dots$pch,
               h.axis=user_dots$h.axis,
               cex.pts=user_dots$cex.pts,
               vertical= user_dots$vertical,
               v.axis=user_dots$v.axis,
               xlab=user_dots$xlab, ylab=user_dots$ylab,
               plot = user_dots$plot, offset = user_dots$offset,
               col=user_dots$col)

    }
  }else{
    stop("Please, provide one of the following
         classes for the 'object' argument: 'stanfit',
         'list'.")
  }




}



