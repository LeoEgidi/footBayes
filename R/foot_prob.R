#' Plot football matches probabilities for out-of-sample football matches.
#'
#' The function provides a table containing the home win, draw and away win probabilities for a bunch of
#' out-of-sample matches as specified by \code{stan_foot} or \code{mle_foot}.
#'
#' @param object An object either of class \code{\link[rstan]{stanfit}} as given by \code{stan_foot} function or
#' \code{\link{list}} as given by \code{mle_foot}.
#' @param data A data frame, or a matrix containing the following mandatory items: home team, away team,
#'home goals, away goals.
#' @param home_team The home team(s) for the predicted matches.
#' @param away_team The away team(s) for the predicted matches.
#'
#'@return
#'
#'A \code{\link{data.frame}} containing the number of out-of-sample matches specified through the
#'argument \code{predict}  passed either in the \code{mle_foot} or in the \code{stan_foot} function.
#'For Bayesian Poisson models the function returns also the most likely outcome (mlo) and a posterior
#' probability plot for the exact results.
#'
#'@details
#'
#'For Bayesian models fitted via \code{stan_foot} the results probabilities are computed according to the
#'simulation from the posterior predictive distribution of future (out-of-sample) matches. For MLE models
#'fitted via the \code{mle_foot} the probabilities are computed by simulating from the MLE estimates.
#'
#'
#'@author Leonardo Egidi \email{legidi@units.it}
#'
#'
#' @examples
#' \donttest{
#' ### weekly dynamics, predict the last four weeks
#' require(tidyverse)
#' require(dplyr)
#'
#' data("italy")
#' italy_2000<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  dplyr::filter(Season=="2000")
#'
#' fit <- stan_foot(data = italy_2000,
#'                  model="double_pois", predict =18,
#'                  dynamic_type = "weekly")  # double pois
#'
#' foot_prob(fit, italy_2000, "Inter",
#'           "Bologna FC")
#'
#' foot_prob(fit, italy_2000) # all the out-of-sample matches
#'}
#' @export


foot_prob <- function(object, data, home_team, away_team){

  # rename

  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  teams <- unique(data$home)

  # predict check: se 0 o nullo, no probabilities

  if (class(object)=="stanfit"){
    sims <- rstan::extract(object)
    predict <- c(dim(sims$y_prev)[2], dim(sims$diff_y_prev)[2])
  }else if (class(object)=="list"){
    predict <- object$predict
  }else{
    stop("Provide one among these two model fit classes: 'stanfit' or 'list'.")
  }

  if (is.null(predict)){
    stop("foot_prob cannot be used if the 'predict' argument is set to zero.")
  }else if (predict ==0){
    stop("foot_prob cannot be used if the 'predict' argument is set to zero.")
  }

  # define data

  data_prev <- data[(dim(data)[1]-predict +1):(dim(data)[1]),]

  # checks su home_team/away_team

  if (missing(home_team) & missing(away_team)){
    home_team <- data_prev$home
    away_team <- data_prev$away
  }

  if (length(home_team)!= length(away_team)){
    stop("Please, include the same number for home and away teams.")
  }

  find_match <- c()
  for (i in 1:length(home_team))
    find_match[i] <- which( data_prev$home %in% home_team[i] & data_prev$away %in% away_team[i])


  true_gol_home <- data_prev$homegoals[find_match]
  true_gol_away <- data_prev$awaygoals[find_match]

  if (length(find_match)==0){
    stop(paste("There is not any out-of-sample match:",
               home_team,"-", away_team, sep=""))
  }

  # calcola probabilità con stan/mle

  if (class(object)=="stanfit"){

    if (is.null(sims$y_prev)){  # student_t model
      M <- dim(sims$diff_y_prev)[1]
      prob_h =  prob_d = prob_a = c()

      x <- round(sims$diff_y_prev,0)
      prob_h <- round(apply(x, 2, function(x) sum(x>0))/M,3)
      prob_d <- round(apply(x, 2, function(x) sum(x==0))/M,3)
      prob_a <- round(apply(x, 2, function(x) sum(x<0))/M,3)

      # only table

      tbl <- data.frame(home_team = home_team,
                        away_team = away_team,
                        prob_h = prob_h[find_match],
                        prob_d = prob_d[find_match],
                        prob_a = prob_a[find_match])

      return(list(prob_table = tbl))

    }else{  # poisson models
    M <- dim(sims$y_prev)[1]
    previsioni1<-sims$y_prev[, find_match ,1]
    previsioni2<-sims$y_prev[, find_match,2]

    prob_h =  prob_d = prob_a = row_pos = col_pos = mlo = c()
    data_exp_tot <- rep(1,4)

    if (length(find_match)==1){
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
      data_exp <- expand.grid(Home=x, Away=y)
      data_exp$Prob <- as.double(counts_mix/(M*M))
      data_exp$matches <- paste(  teamaa,"-", teamab)
      data_exp$true_gol_home <- true_gol_home
      data_exp$true_gol_away <- true_gol_away


      #rep(i, length(data_exp$Prob))

      # overall "adjusted" probabilities
      prob_h <- sum(counts_mix[lower.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)
      prob_d <- sum(diag(counts_mix/(M*M)))/sum(data_exp$Prob)
      prob_a <- sum(counts_mix[upper.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)

      # MLO (most likely outcome)

      row_pos <- row(counts_mix)[counts_mix==max(counts_mix)]
      col_pos <- col(counts_mix)[counts_mix==max(counts_mix)]


      mlo <- paste(row_pos-1, "-", col_pos-1, " (",
                   round(max(counts_mix/(M*M)),3), ")" , sep="")

      data_exp_tot <- rbind(data_exp_tot, data_exp)
      data_exp_tot <- data_exp_tot[-c(1), ]



      tbl <- data.frame(home_team = home_team,
                        away_team = away_team,
                        prob_h = round(prob_h,3),
                        prob_d = round(prob_d,3),
                        prob_a = round(prob_a,3),
                        mlo = mlo)


      # To change the color of the gradation :

      p <- ggplot(data_exp_tot, aes(Home, Away, z= Prob)) + geom_tile(aes(fill = Prob)) +
        theme_bw() +
        scale_fill_gradient(low="white", high="black") +

        geom_rect(aes(xmin = as.numeric(as.vector(true_gol_home))-0.5,
                      xmax = as.numeric(as.vector(true_gol_home))+0.5,
                      ymin = as.numeric(as.vector(true_gol_away))-0.5,
                      ymax =as.numeric(as.vector(true_gol_away))+0.5),
                  fill = "transparent", color = "red", size = 1.5)+
        labs(title= "Posterior match probabilities")+
        yaxis_text(size=12)+
        xaxis_text( size = rel(12))+
        theme(plot.title = element_text(size = 22),
              strip.text = element_text(size = 12),
              axis.text.x = element_text(size=22),
              axis.text.y = element_text(size=22),
              plot.subtitle=element_text(size=13),
              axis.title=element_text(size=18,face="bold"),
              legend.text=element_text(size=14))

    }else{

        teamaa = teamab = c()
      for (i in 1:length(find_match)){
        posterior_prop1<-table(previsioni1[,i])
        posterior_prop2<-table(previsioni2[,i])

        teamaa[i]=home_team[i]
        teamab[i]=away_team[i]

        x_min=y_min= 5
          #min(length(posterior_prop1),              ## OLD CODE
          #              length(posterior_prop2))

        counts_mix<- matrix(0, x_min, y_min)
          #matrix(0, min(length(posterior_prop1), length(posterior_prop2)),          ## OLD CODE
          #                 min(length(posterior_prop1), length(posterior_prop2)))

        for (j in 1: x_min ){
          for (t in 1: y_min ){
            counts_mix[j,t]<-posterior_prop1[j]*posterior_prop2[t]
          }}

            # qq<-posterior_prop1[as.double(names(posterior_prop1))>=x_min]
            # rr <- posterior_prop2
            # qq_rr <- matrix(rep(rr, length(qq)), length(rr), length(qq))
            # counts_mix[x_min, 1:y_min ] <- qq%*%t(qq_rr)[,1:y_min] # arrivato qui: 23/11
            # counts_mix[x_min, y_min] <- counts_mix[x_min, y_min] + sum(qq%*%t(qq_rr)[,(y_min+1):length(rr)])
            # qq2<-posterior_prop2[as.double(names(posterior_prop2))>=y_min]
            # rr2 <- posterior_prop1
            # qq_rr2 <- matrix(rep(rr2, length(qq2)), length(rr2), length(qq2))
            # counts_mix[1:x_min, y_min] <- counts_mix[1:x_min, y_min] +  qq2%*%t(qq_rr2)[,1:x_min]
            # counts_mix[x_min, y_min] <-counts_mix[x_min, y_min] + sum(qq2%*%t(qq_rr2)[,(x_min+1):length(rr2)])
        dim1 <- dim(counts_mix)[1]
        dim2 <- dim(counts_mix)[2]

        x <- seq(0,dim1-1, length.out=dim1)
        y <- seq(0,dim2-1, length.out=dim2)
        data_exp <- expand.grid(Home=x, Away=y)
        data_exp$Prob <- as.double(counts_mix/(M*M))
        data_exp$matches <- paste(  teamaa[i],"-", teamab[i])
        data_exp$true_gol_home <- true_gol_home[i]
        data_exp$true_gol_away <- true_gol_away[i]


        #rep(i, length(data_exp$Prob))

        # overall "adjusted" probabilities
        prob_h[i] <- sum(counts_mix[lower.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)
        prob_d[i] <- sum(diag(counts_mix/(M*M)))/sum(data_exp$Prob)
        prob_a[i] <- sum(counts_mix[upper.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)

        # MLO (most likely outcome)

        row_pos[i] <- row(counts_mix)[counts_mix==max(counts_mix)]
        col_pos[i] <- col(counts_mix)[counts_mix==max(counts_mix)]


        mlo[i] <- paste(row_pos[i]-1, "-", col_pos[i]-1, " (",
                        round(max(counts_mix/(M*M)),3), ")" , sep="")

        data_exp_tot <- rbind(data_exp_tot, data_exp)
      }
      data_exp_tot <- data_exp_tot[-c(1), ]



      tbl <- data.frame(home_team = home_team,
                        away_team = away_team,
                        prob_h = round(prob_h,3),
                        prob_d = round(prob_d,3),
                        prob_a = round(prob_a,3),
                        mlo = mlo)

      data_exp_tot <- data_exp_tot %>%
        dplyr::group_by(matches)%>%
        dplyr::mutate(prob_h = sum(Prob[Home > Away]),
                      prob_d = sum(Prob[Home == Away]),
                      prob_a = sum(Prob[Home < Away]))

      data_exp_tot$favorite <- rep(teamaa, each = x_min*y_min)
      data_exp_tot$underdog <- rep(teamab, each = x_min*y_min)

      # re-order in terms of favorite and underdog
      indexes <- (1:dim(data_exp_tot)[1])[data_exp_tot$prob_h < data_exp_tot$prob_a]
      temp1 <- data_exp_tot$prob_h[indexes]
      temp2 <- data_exp_tot$prob_a[indexes]
      data_exp_tot$prob_h[indexes] <- temp2
      data_exp_tot$prob_a[indexes] <- temp1

      temp_name1 <- data_exp_tot$favorite[indexes]
      temp_name2 <- data_exp_tot$underdog[indexes]
      data_exp_tot$favorite[indexes] <- temp_name2
      data_exp_tot$underdog[indexes] <- temp_name1

      temp_coord1 <- data_exp_tot$Home[indexes]
      temp_coord2 <- data_exp_tot$Away[indexes]
      data_exp_tot$Home[indexes] <- temp_coord2
      data_exp_tot$Away[indexes] <- temp_coord1

      temp_tg1 <- data_exp_tot$true_gol_home[indexes]
      temp_tg2 <- data_exp_tot$true_gol_away[indexes]
      data_exp_tot$true_gol_home[indexes] <- temp_tg2
      data_exp_tot$true_gol_away[indexes] <- temp_tg1

      data_exp_tot <- dplyr::arrange(data_exp_tot, prob_h)
      fav_teams <- data_exp_tot%>%distinct(favorite)
      und_teams <- data_exp_tot%>%distinct(underdog)
      axes_titles <- data.frame(matches = unique(data_exp_tot$matches),
                                axis_title_x = fav_teams[,2],
                                axis_title_y = und_teams[,2])
      data_exp_tot$new_matches <- paste(data_exp_tot$favorite, "-", data_exp_tot$underdog)
      #axes_titles$favorite <- as.character(as.vector(axes_titles$favorite))
      # To change the color of the gradation :

      p <- ggplot(data_exp_tot, aes(Home, Away, z= Prob)) + geom_tile(aes(fill = Prob)) +
        theme_bw() +
        scale_fill_gradient(low="white", high="black") +
        facet_wrap(facets = ~reorder(new_matches, prob_h),
                   scales = "fixed"
                   #labeller = as_labeller(c(axes_titles$underdog)),
                   #strip.position = "left"
                   )+
        geom_rect(aes(xmin = as.numeric(as.vector(true_gol_home))-0.5,
                      xmax = as.numeric(as.vector(true_gol_home))+0.5,
                      ymin = as.numeric(as.vector(true_gol_away))-0.5,
                      ymax =as.numeric(as.vector(true_gol_away))+0.5),
                  fill = "transparent", color = "red", size = 1.5)+
        labs(title= "Posterior match probabilities")+
        yaxis_text(size=12)+
        xaxis_text( size = rel(12))+
        ylab("Underdog")+
        xlab("Favorite")+
        theme(plot.title = element_text(size = 22),
              strip.text = element_text(size = 11),
              #strip.placement = "outside",   # format to look like title
              strip.background = element_blank(),
              axis.text.x = element_text(size=22),
              axis.text.y = element_text(size=22),
              plot.subtitle=element_text(size=8.5),
              axis.title=element_text(size=18,face="bold"),
              legend.text=element_text(size=14),
              panel.spacing = unit(0.2, "lines"))


    }
    return(list(prob_table = tbl, prob_plot = p))
    }

  }else if (class(object)=="list"){
    model <- object$model
    predict <- object$predict
    n.iter <- object$n.iter
    team1_prev <- object$team1_prev
    team2_prev <- object$team2_prev
    N_prev <- predict



      # routine prediction if predict is not 0
      prediction_routine <- function(team1_prev, team2_prev, att, def, home,
                                     corr, ability, model, predict, n.iter){

        mean_home <- exp(home[1,2] + att[team1_prev,2] + def[team2_prev,2])
        mean_away <- exp(att[team2_prev,2] + def[team1_prev,2])


        if (model=="double_pois"){

          x = y = x_q1 = y_q1 = x_q2 = y_q2 = matrix(NA, n.iter, predict)
          for (n in 1: N_prev){
            x[,n] <- rpois(n.iter, mean_home[n])
            y[,n] <- rpois(n.iter, mean_away[n])
            # x_q1[,n] <- rpois(n.iter, mean_home_q1[n])
            # y_q1[,n] <- rpois(n.iter, mean_away_q1[n])
            # x_q2[,n] <- rpois(n.iter, mean_home_q2[n])
            # y_q2[,n] <- rpois(n.iter, mean_away_q2[n])

          }


        }else if (model == "biv_pois"){
          couple <- array(NA, c(n.iter, predict, 2))
          x = y = x_q1 = y_q1 = x_q2 = y_q2 = matrix(NA, n.iter, predict)
          for (n in 1: N_prev){
            couple[,n,] <- rbvpois(n.iter,  a = mean_home[n],
                                   b= mean_away[n],
                                   c = corr[1,2])

          }
          x <- couple[,,1]
          y <- couple[,,2]


        }else if (model == "skellam"){
          diff_y <- matrix(NA, n.iter, predict)
          for (n in 1:N_prev){
            diff_y[,n] <- rskellam(n.iter,
                                   mu1 = mean_home[n],
                                   mu2 = mean_away[n])
          }
          x <- diff_y
          y <- matrix(0, n.iter, predict)

        }else if (model == "student_t"){
          sigma_y <- object$sigma_y
          diff_y <- matrix(NA, n.iter, predict)
          for (n in 1:N_prev){
            diff_y[,n] <- rt.scaled(n.iter, df = 7,
                                    mean = home[1,2] + ability[team1_prev[n],2] - ability[team2_prev[n],2],
                                    sd = sigma_y)
          }
          x <- round(diff_y)   # rounded to the closest integer, as Gelman does
          y <- matrix(0, n.iter, predict)


        }

        prob_func <- function(mat_x, mat_y){
          res <- mat_x-mat_y
          prob_h <- apply(res, 2, function(x) sum(x > 0) )/n.iter
          prob_d <- apply(res, 2, function(x) sum(x == 0) )/n.iter
          prob_a <- apply(res, 2, function(x) sum(x < 0) )/n.iter
          return(list(prob_h = prob_h,
                      prob_d = prob_d,
                      prob_a = prob_a))
        }

        conf <- prob_func(x, y)
        # conf_q1 <- prob_func(x_q1, y_q1)
        # conf_q2 <- prob_func(x_q2, y_q2)

        tbl <- data.frame(home_team = teams[team1_prev[find_match]],
                          away_team = teams[team2_prev[find_match]],
                          prob_h = conf$prob_h[find_match],
                          prob_d = conf$prob_d[find_match],
                          prob_a = conf$prob_a[find_match]
        )
        return(tbl)

      }


      # lancia prediction_routine se e solo se predict è non missing

      if (predict!=0){
        prob_matrix <- prediction_routine(team1_prev, team2_prev, object$att,
                                          object$def, object$home,
                                          object$corr, object$abilities, model, predict,
                                          n.iter)


      }

      return(list(prob_table = prob_matrix))
  }









}
