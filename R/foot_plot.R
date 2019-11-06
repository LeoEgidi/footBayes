#' Plots
#'
#'
#' @#param
#'
#'


foot_plot <- function(fit, type){

  if (type=="abilities")

  sims <- extract(fit)
  att_med=apply(att,2, median)
  def_med=apply(def,2, median)
  att_25=apply(att, 2, function(x) quantile(x, 0.25))
  att_75=apply(att, 2, function(x)  quantile(x, 0.75))
  def_25=apply(def, 2, function(x) quantile(x, 0.25))
  def_75=apply(def, 2, function(x)  quantile(x, 0.75))



  att_25_mat=att_75_mat=def_25_mat=def_75_mat=att_50_mat=def_50_mat=matrix(NA, nteams, T)
  for (t in 1:T){

    att_25_mat[,t]=as.vector(att_25[  (t*nteams-((nteams-1))):(t*nteams)])

    att_50_mat[,t]=as.vector(att_med[  (t*nteams-((nteams-1))):(t*nteams)])

    att_75_mat[,t]=as.vector(att_75[  (t*nteams-((nteams-1))):(t*nteams)])

    def_25_mat[,t]=as.vector(def_25[  (t*nteams-((nteams-1))):(t*nteams)])

    def_50_mat[,t]=as.vector(def_med[  (t*nteams-((nteams-1))):(t*nteams)])

    def_75_mat[,t]=as.vector(def_75[  (t*nteams-((nteams-1))):(t*nteams)])

  }

}
