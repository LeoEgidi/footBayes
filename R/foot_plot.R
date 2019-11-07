#' Plots
#'
#'
#' @#param
#'
#'


foot_plot <- function(fit, type){

  if (type=="abilities"){

  sims <- extract(fit$fit)
  teams <- fit$teams
  att <- sims$att
  def <- sims$def
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

  att_025_mat = att_25_mat = att_75_mat = att_975_mat = def_025_mat = def_25_mat = def_75_mat=def_975_mat=att_50_mat=def_50_mat=matrix(NA, nteams, T)
  for (t in 1:T){
    att_025_mat[,t]=as.vector(att_025[  (t*nteams-((nteams-1))):(t*nteams)])
    att_25_mat[,t]=as.vector(att_25[  (t*nteams-((nteams-1))):(t*nteams)])
    att_50_mat[,t]=as.vector(att_med[  (t*nteams-((nteams-1))):(t*nteams)])
    att_75_mat[,t]=as.vector(att_75[  (t*nteams-((nteams-1))):(t*nteams)])
    att_975_mat[,t]=as.vector(att_975[  (t*nteams-((nteams-1))):(t*nteams)])
    def_025_mat[,t]=as.vector(def_025[  (t*nteams-((nteams-1))):(t*nteams)])
    def_25_mat[,t]=as.vector(def_25[  (t*nteams-((nteams-1))):(t*nteams)])
    def_50_mat[,t]=as.vector(def_med[  (t*nteams-((nteams-1))):(t*nteams)])
    def_75_mat[,t]=as.vector(def_75[  (t*nteams-((nteams-1))):(t*nteams)])
    def_975_mat[,t]=as.vector(def_975[  (t*nteams-((nteams-1))):(t*nteams)])
  }

  mt_att_025 <- melt(att_025_mat)
  mt_att_25 <- melt(att_25_mat)
  mt_att_50 <- melt(att_50_mat)
  mt_att_75 <- melt(att_75_mat)
  mt_att_975 <- melt(att_975_mat)

  mt_def_025 <- melt(def_025_mat)
  mt_def_25 <- melt(def_25_mat)
  mt_def_50 <- melt(def_50_mat)
  mt_def_75 <- melt(def_75_mat)
  mt_def_975 <- melt(def_975_mat)

  teams_fac_rep <- rep(teams, T)
  times_rep <- rep(1:T, each=length(teams))

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
    lims(y = c(-0.8, 0.8)) +
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
          plot.subtitle=element_text(size=7))


  }

}
