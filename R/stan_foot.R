#' Fit football models  with Stan
#'
#' Stan football modelling for the most famous models:
#' double Poisson, bivariate Poisson, Skellam and student t.
#'
#'@param data A data frame, or a matrix containing the following mandatory items: season, home team, away team,
#'home goals, away goals.
#'@param model The type of Stan model used to fit the data.
#'             One among the following: \code{"double_pois"},
#'             \code{"biv_pois"}, \code{"skellam"}, \code{"student_t"}.
#'@param predict The number of out-of-sample matches. If missing, the function returns
#'the fit for the training set only.
#'@param dynamic_type One among \code{"weekly"} or \code{"seasonal"} for weekly dynamic parameters or seasonal
#'dynamic parameters.
#'@param ... Optional parameters passed to the function
#'in the \code{rstan} package. It is possibly to specify \code{iter}, \code{chains}, \code{cores}, \code{refresh}, etc.
#'@return
#'
#'An object of S4 class, \code{\link[rstan]{stanfit-class}}.
#'
#'@details
#'Let \eqn{(y^{H}_{n}, y^{A}_{n})} denote the
#'observed number of goals scored by the home
#'and the away team in the \eqn{n}-th game,
#'respectively. A general bivariate Poisson model
#'allowing for goals' correlation
#'(Karlis & Ntzoufras, 2003) is the following:
#'
#'\deqn{ Y^H_n, Y^A_n| \lambda_{1n}, \lambda_{2n}, \lambda_{3n}  \sim \mathsf{BivPoisson}(\lambda_{1n}, \lambda_{2n}, \lambda_{3n})}
#'\deqn{\log(\lambda_{1n})  = \mu+\text{att}_{h_n}+\text{def}_{a_n}}
#'\deqn{\log(\lambda_{2n})  = \text{att}_{a_n}+\text{def}_{h_n}}
#'\deqn{\log(\lambda_{3n})  =\beta_0,}
#'
#' where the case \eqn{\lambda_{3n}=0} reduces to
#' the double Poisson model (Baio & Blangiardo, 2010).
#'  \eqn{\lambda_{1n}, \lambda_{2n}} represent the
#'  scoring rates for the home and the away team,
#'  respectively, where: \eqn{\mu} is the home effect;
#'  the parameters \eqn{\text{att}_T} and
#'   \eqn{\text{def}_T} represent the attack and the
#'   defence abilities,
#' respectively, for each team \eqn{T}, \eqn{T=1,\ldots,N_T};
#' the nested indexes \eqn{h_{n}, a_{n}=1,\ldots,N_T}
#' denote the home and the away team playing in the \eqn{n}-th game,
#' respectively. Attack/defence parameters are imposed a
#' sum-to-zero constraint to achieve identifiability and
#' assigned some weakly-informative prior distributions:
#'
#' \deqn{\text{att}_T \sim \mathcal{N}(\mu_{\text{att}}, \sigma_{\text{att}})}
#' \deqn{\text{def}_T \sim \mathcal{N}(\mu_{\text{def}}, \sigma_{\text{def}}),}
#'
#' with hyperparameters \eqn{\mu_{\text{att}}, \sigma_{\text{att}}, \mu_{\text{def}}, \sigma_{\text{def}}}.
#'
#' Instead of using the marginal number of goals,
#' another alternative is to modelling directly
#' the score difference \eqn{(y^{H}_{n}- y^{A}_{n})}.
#' We can use the Poisson-difference distribution
#' (or Skellam distribution) to model goal
#' difference in the \eqn{n}-th match (Karlis & Ntzoufras, 2009):
#'
#' \deqn{y^{H}_{n}- y^{A}_{n}| \lambda_{1n}, \lambda_{2n} \sim PD(\lambda_{1n}, \lambda_{2n}),}
#'
#' and the scoring rates \eqn{\lambda_{1n}, \lambda_{2n}} are
#' unchanged with respect to the bivariate/double Poisson model.
#' If we want to use a continue distribution, we can
#' use a student t distribution with 7 degrees of
#' freedom (Gelman, 2014):
#'
#' \deqn{y^{H}_{n}- y^{A}_{n} \sim t(7, \text{ab}_{h_{n}}-\text{ab}_{a(n)}, \sigma_y)}
#' \deqn{\text{ab}_t \sim \mathcal{N}(\mu + b \times \text{prior\_score}_t, sigma_{\text{ab}}),}
#'
#' where \eqn{\text{ab}_t} is the overall ability for
#' the \eqn{t}-th team, whereas \eqn{\text{prior\_score}_t}
#' is a prior measure of team's strength (for instance a
#' ranking).
#'
#' These model rely on the assumption of static parameters.
#' However, we could assume dynamics in the attach/defence
#' abilities (Owen, 2011; Egidi et al., 2018) in terms of weeks or seasons through the argument
#' \code{dynamic_type}. In such a framework, for a given
#' number of times \eqn{1, \ldots, \mathcal{T}}, the models
#' above would be unchanged, but the priors for the abilities
#' parameters at each time \eqn{\tau, \tau=2,\ldots, \mathcal{T},} would be:
#'
#' \deqn{\text{att}_{T, \tau} \sim \mathcal{N}({\text{att}}_{T, \tau-1}, \sigma_{\text{att}})}
#' \deqn{\text{def}_{T, \tau} \sim \mathcal{N}({\text{def}}_{T, \tau-1}, \sigma_{\text{def}}),}
#'
#' whereas for \eqn{\tau=1} we have:
#'
#' \deqn{\text{att}_{T, 1} \sim \mathcal{N}(\mu_{\text{att}}, \sigma_{\text{att}})}
#' \deqn{\text{def}_{T, 1} \sim \mathcal{N}(\mu_{\text{def}}, \sigma_{\text{def}}).}
#'
#' Of course, the identifiability constraint must be impoed for
#' each time \eqn{\tau}.
#'
#'@author Leonardo Egidi \email{legidi@units.it}
#'
#'@references
#' Baio, G. and Blangiardo, M. (2010). Bayesian hierarchical model for the prediction of football
#' results. Journal of Applied Statistics 37(2), 253-264.
#'
#' Egidi, L., Pauli, F., and Torelli, N. (2018). Combining historical data
#' and bookmakers' odds in modelling football scores. Statistical Modelling, 18(5-6), 436-459.
#'
#' Gelman, A. (2014). Stan goes to the World Cup. From
#' "Statistical Modeling, Causal Inference, and Social Science" blog.
#'
#' Karlis, D. and Ntzoufras, I. (2003). Analysis of sports data by using bivariate poisson models.
#' Journal of the Royal Statistical Society: Series D (The Statistician) 52(3), 381-393.
#'
#' Karlis, D. and Ntzoufras,I. (2009).  Bayesian modelling of football outcomes: Using
#' the Skellam's distribution for the goal difference. IMA Journal of Management Mathematics 20(2), 133-145.
#'
#' Owen, A. (2011). Dynamic Bayesian forecasting models
#' of football match outcomes with estimation of the
#' evolution variance parameter. IMA Journal of Management Mathematics, 22(2), 99-113.
#'
#'
#'@examples
#'\dontrun{
#'library(engsoccerdata)
#'library(tidyverse)
#'
#'### Use Italian Serie A from 2000 to 2002
#'
#'italy <- as_tibble(italy)
#'italy_2000_2002<- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  filter(Season=="2000" |  Season=="2001"| Season=="2002")
#'
#' ### Fit Stan models
#' ## no dynamics, no predictions
#'
#' fit1 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois") # double poisson
#' print(fit1, pars =c("home", "sigma_att",
#'                     "sigma_def"))
#'
#' fit2 <- stan_foot(data = italy_2000_2002,
#'                 model="biv_pois")    # bivariate poisson
#' print(fit2, pars =c("home", "rho"
#'                     "sigma_att", "sigma_def"))
#'
#' fit3 <- stan_foot(data = italy_2000_2002,
#'                 model="skellam")     # skellam
#' print(fit3, pars =c("home", "sigma_att",
#'                     "sigma_def"))
#'
#' fit4 <- stan_foot(data = italy_2000_2002,
#'                 model="student_t")   # student_t
#' print(fit4, pars =c("home", "beta")
#'
#' ## seasonal dynamics, no prediction
#'
#' fit5 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois",
#'                 dynamic_type ="seasonal") # double poisson
#' print(fit5, pars =c("home", "Sigma_att",
#'                     "Sigma_def"))
#'
#' ## seasonal dynamics, prediction for the last season
#'
#' fit6 <- stan_foot(data = italy_2000_2002,
#'                 model="double_pois",
#'                 dynamic_type ="seasonal",
#'                 predict = 306) # double poisson
#' print(fit6, pars =c("home", "Sigma_att",
#'                     "Sigma_def"))
#'}
#'@import rstan
#'@import engsoccerdata
#'@import bayesplot
#'@import matrixStats
#'@import arm
#'@import reshape2
#'@import ggplot2
#'@importFrom arm coefplot
#'@export


stan_foot <- function(data,
                      model,
                      predict,
                      dynamic_type = FALSE,
                      ...){

  if (dim(data)[2]<5){
    stop("Data dimensions are wrong! Please,
         supply a matrix/data frame containing
         the following mandatory column items:
         season, home team, away team,
         home goals, away goals.")
  }
  if (!is.matrix(data) & !is.data.frame(data)){
    stop("Data are not stored in matrix/data frame
         structure. Pleasy, provide data correctly.")
  }
  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  nteams<- length(unique(data$home))
  user_dots <- list(chains = 4, iter = 2000,
                    #warmup = floor(iter/2),
                    thin = 1,
                    init = "random", seed = sample.int(.Machine$integer.max, 1),
                    algorithm = c("NUTS", "HMC", "Fixed_param"),
                    control = NULL, sample_file = NULL, diagnostic_file = NULL,
                    save_dso = TRUE, verbose = FALSE, include = TRUE,
                    cores = getOption("mc.cores", 1L),
                    open_progress = interactive() && !isatty(stdout()) &&
                      !identical(Sys.getenv("RSTUDIO"), "1"),
                    boost_lib = NULL, eigen_lib = NULL)
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


  if (missing(predict) | predict ==0){ # check on predict
    predict <- 0
    N <- dim(data)[1]
    N_prev <- 0
    type <- "fit"
  }else if (is.numeric(predict)){
    N <- dim(data)[1]-predict
    N_prev <- predict
    type <- "prev"
  }else if (is.numeric(predict)){
    stop("The number of out-of-sample matches is ill posed!
         Pick up an integer number.")
  }else if (predict >= dim(data)[1]){
    warning("The training set size is zero!
            Please, select a lower value for the
            out-of-sample matches, through the
            argument predict.")
  }

  if (missing(dynamic_type)){
    dyn <-""
  }else if (dynamic_type == "weekly" ){
      dyn <- "dynamic_"
      if (length(unique(data$season))!=1){
        stop("When using weekly dynamics,
              please consider one season only.")
      }else{
      weak_count <- ((N+predict)*2)/(nteams)
      if ((N*2)%%(nteams)!=0){
        stop("The number of total matches is not
              the same for all the teams. Please,
              provide an adequate number of matches
              (hint: proportional to the number
              of matches for each match day).")
      }
      weak <- rep(seq(1, weak_count ), each = nteams/2)
      data <- data %>%
        mutate(weak)
      ntimes <- length(unique(weak))
      #time_tot <- c(1:length(unique(weak[1:(N+N_prev)])))
      time <- c(1:length(unique(weak)))
      instants <- weak[1:N]
      #ntimes_prev <- length(unique(weak[1:(N+N_prev)]))-length(unique(weak[1:N]))
      #time_prev <- setdiff(time_tot, time)
      instants_prev <- weak[(N+1):(N+N_prev)]
      }
    }else if(dynamic_type=="seasonal"){
      dyn <- "dynamic_"
      if (length(unique(data$season))==1){
        dyn <-""
        warning("When using seasonal dynamics,
              please consider more than one season.")
      }
      season_count <- length(unique(data$season))
      season <- match(data$season, unique(data$season))
      ntimes <- season_count
      #time_tot <- c(1:length(unique(data$season)))
      time <- c(1:season_count)
      instants <- season[1:N]
      #ntimes_prev <- length(unique(season[1:(N+N_prev)]))-length(unique(season[1:N]))
      #time_prev <- setdiff(time_tot, time)
      instants_prev <- season[(N+1):(N+N_prev)]
    }else if (dynamic_type =="FALSE"){
      dyn <-""
    }else{
      stop("The type of dynamics is not correct.
           Choose one among 'weekly' or 'seasonal'.")
    }


  teams <- unique(data$home)
  team_home <- match( data$home, teams)
  team_away <- match( data$away, teams)
  team1 <- team_home[1:N]
  team2 <- team_away[1:N]
  team1_prev <- team_home[(N+1):(N+N_prev)]
  team2_prev <- team_away[(N+1):(N+N_prev)]
  y <- matrix(NA, N, 2)
  y[,1] <- as.numeric(as.vector(data$homegoals)[1:N])
  y[,2] <- as.numeric(as.vector(data$awaygoals)[1:N])
  diff_y <- y[,1]-y[,2]

  # Stan data
  data_stan <- list( y=y,
                spi_std = rep(0, nteams),
                diff_y = diff_y,
                N=N,
                N_prev = N_prev,
                nteams=nteams,
                team1 = team1,
                team2=team2,
                team1_prev= team1_prev,
                team2_prev=team2_prev)

  if (!missing(dynamic_type)){
    data_stan$ntimes <- ntimes
    data_stan$instants <- instants
    data_stan$time <- time
    data_stan$instants_prev <- instants_prev
  }

  stanfoot_models <- function(model, dyn, type){
    right_name <- paste(model,"_", dyn, type, sep="")
    models_name <- c("biv_pois_dynamic_fit",
                     "biv_pois_dynamic_prev",
                      "biv_pois_fit",
                      "biv_pois_prev",
                     "double_pois_dynamic_fit",
                     "double_pois_dynamic_prev",
                     "double_pois_fit",
                     "double_pois_prev",
                     "skellam_dynamic_fit",
                     "skellam_dynamic_prev",
                     "skellam_fit",
                     "skellam_prev",
                     "student_t_dynamic_fit",
                     "student_t_dynamic_prev",
                     "student_t_fit",
                     "student_t_prev"
                     )


    biv_pois_dynamic_fit<-
      "functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }
    }
    data{
      int N;   // number of games
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real<lower=0> rho;
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]= def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
        theta_corr[n] = rho;
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));

      }
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood

      for (n in 1:N){
        target+=bipois_lpmf(y[n,]| theta_home[n],
                            theta_away[n], theta_corr[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =bipois_lpmf(y[n,]| theta_home[n],
                                theta_away[n], theta_corr[n]);
      }
    }"

    biv_pois_dynamic_prev<-"
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }
    }
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real<lower=0> rho;
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      //cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      //cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
        theta_corr[n] = rho;
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
      }
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood

      for (n in 1:N){
        target+=bipois_lpmf(y[n,]| theta_home[n],
                            theta_away[n], theta_corr[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];
      int y_prev[N_prev,2];
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;
      vector[N_prev] theta_corr_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =bipois_lpmf(y[n,]| theta_home[n],
                                theta_away[n], theta_corr[n]);
      }

      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]);
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]);
        theta_corr_prev[n] = rho;
        y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
      }
    }"

    biv_pois_fit <- "
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }

    }
    data{
      int N;   // number of games
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real beta;
      real<lower=0> rho;
      real home;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      vector[3] theta[N];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
        theta[n,3] = rho;
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(rho|0,5);
      target+=normal_lpdf(home|0,5);
      // likelihood
      for (n in 1:N){
        target+=bipois_lpmf(y[n,]| theta[n,1],
                            theta[n,2], theta[n,3]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
        y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =bipois_lpmf(y[n,]| theta[n,1],
                                theta[n,2], theta[n,3]);
      }
    }"

    biv_pois_prev<-"
    functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }

    }
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real<lower=0> rho;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      vector[3] theta[N];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
        theta[n,3] = rho;
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,5);
      // likelihood
      for (n in 1:N){
        target+=bipois_lpmf(y[n,]| theta[n,1],
                            theta[n,2], theta[n,3]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int y_prev[N_prev,2];
      vector[3] theta_prev[N_prev];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =bipois_lpmf(y[n,]| theta[n,1],
                                theta[n,2], theta[n,3]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home+att[team1_prev[n]]+
                                def[team2_prev[n]]);
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]);
        theta_prev[n,3] = rho;
        y_prev[n,1] = poisson_rng(theta_prev[n,1]+theta_prev[n,3]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]+theta_prev[n,3]);
      }
    }"


    double_pois_dynamic_fit<-"
    data{
      int N;                      // number of games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
      }
      target+=normal_lpdf(home|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood
      for (n in 1:N){
        target+=poisson_lpmf(y[n,1]| theta_home[n]);
        target+=poisson_lpmf(y[n,2]| theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta_home[n])+
          poisson_lpmf(y[n,2]| theta_away[n]);
      }
    }"


    double_pois_dynamic_prev<-"
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                 + (i == j ? 0.1 : 0.0);
          //   }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
      }
      target+=normal_lpdf(home|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood
      for (n in 1:N){
        target+=poisson_lpmf(y[n,1]| theta_home[n]);
        target+=poisson_lpmf(y[n,2]| theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];
      int y_prev[N_prev,2];
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta_home[n])+
          poisson_lpmf(y[n,2]| theta_away[n]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home+att[instants_prev[n],team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]);
        theta_away_prev[n] = exp(att[instants_prev[n],team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]);

        y_prev[n,1] = poisson_rng(theta_home_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]);
      }
    }"

    double_pois_fit<-"
    data{
      int N;                      // number of games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
    }
    transformed parameters{
      vector[nteams] att;        // attack parameters
      vector[nteams] def;        // defence parameters
      vector[2] theta[N];        // exponentiated linear pred.

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(home|0,5);
      // likelihood
      for (n in 1:N){
        target+=poisson_lpmf(y[n,1]| theta[n,1]);
        target+=poisson_lpmf(y[n,2]| theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta[n,1])+
          poisson_lpmf(y[n,2]| theta[n,2]);
      }
    }"


    double_pois_prev<-"
    data{
      int N;                      // number of games
      int N_prev;                 // number of predicted games
      int y[N,2];                 // scores
      int nteams;                 // number of teams
      int team1[N];               // home team index
      int team2[N];               // away team index
      int team1_prev[N_prev];     // home team for pred.
      int team2_prev[N_prev];     // away team for pred.
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
    }
    transformed parameters{
      vector[nteams] att;        // attack parameters
      vector[nteams] def;        // defence parameters
      vector[2] theta[N];        // exponentiated linear pred.

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(home|0,5);
      // likelihood
      for (n in 1:N){
        target+=poisson_lpmf(y[n,1]| theta[n,1]);
        target+=poisson_lpmf(y[n,2]| theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int y_prev[N_prev,2];
      vector[2] theta_prev[N_prev];
      vector[N] log_lik;
      int diff_y_rep[N];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =poisson_lpmf(y[n,1]| theta[n,1])+
          poisson_lpmf(y[n,2]| theta[n,2]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home+att[team1_prev[n]]+def[team2_prev[n]]);
        theta_prev[n,2] = exp(att[team2_prev[n]]+def[team1_prev[n]]);
        y_prev[n,1] = poisson_rng(theta_prev[n,1]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]);
      }
    }"

    skellam_dynamic_fit<-"
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]);
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
      }
      target+=normal_lpdf(home|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta_home[n],
                             theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
      }
    }"


    skellam_dynamic_prev <- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real home;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

      // for (i in 1:(ntimes)){
        //     for (j in 1:(ntimes)){
          //       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
          //       + (i == j ? 0.1 : 0.0);
          //       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
          //                   + (i == j ? 0.1 : 0.0);
          //     }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(0,nteams);
        mu_att[t]=att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(0,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
        theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]);
      }
    }
    model{
      // priors
      for (h in 1:(nteams)){
        att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
        def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
      }
      target+=normal_lpdf(home|0,5);
      target+=cauchy_lpdf(sigma_att| 0,2.5);
      target+=cauchy_lpdf(sigma_def| 0,2.5);
      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta_home[n],
                             theta_away[n]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      vector[N_prev] theta_home_prev;
      vector[N_prev] theta_away_prev;
      int y_prev[N_prev,2];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
      }

      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]);
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]);
        y_prev[n,1] = poisson_rng(theta_home_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
    }"

    skellam_fit <- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(home|0,5);
      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta[n,1], theta[n,2]);
      }
    }"

    skellam_prev<- "
    functions{
      real skellam_lpmf(int k, real lambda1, real lambda2) {
        real r = k;
        return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
          log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
      }
    }
    data{
      int N;
      int N_prev;
      int diff_y[N];
      int nteams;
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real<lower=0> rho;
      real home;
    }
    transformed parameters{
      vector[nteams] att;
      vector[nteams] def;
      real theta[N,2];

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      for (n in 1:N){
        theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
      }
    }
    model{
      // priors
      for (t in 1:(nteams)){
        target+=normal_lpdf(att_raw[t]|0, sigma_att);
        target+=normal_lpdf(def_raw[t]|0, sigma_def);
      }
      target+=cauchy_lpdf(sigma_att|0, 5);
      target+=cauchy_lpdf(sigma_def|0, 5);
      target+=normal_lpdf(rho|0,5);
      target+=normal_lpdf(home|0,5);
      // likelihood
      for (n in 1:N){
        target+=skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2]);
      }
    }
    generated quantities{
      int y_rep[N,2];
      int diff_y_rep[N];
      vector[N] log_lik;
      int y_prev[N_prev,2];
      vector[2] theta_prev[N_prev];
      int diff_y_prev[N_prev];

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]);
        y_rep[n,2] = poisson_rng(theta[n,2]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =skellam_lpmf(diff_y[n]| theta[n,1], theta[n,2]);
      }
      //out-of-sample predictions
      for (n in 1:N_prev){
        theta_prev[n,1] = exp(home+att[team1_prev[n]]+
                                def[team2_prev[n]]);
        theta_prev[n,2] = exp(att[team2_prev[n]]+
                                def[team1_prev[n]]);
        y_prev[n,1] = poisson_rng(theta_prev[n,1]);
        y_prev[n,2] = poisson_rng(theta_prev[n,2]);
        diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
      }
    }"

    student_t_dynamic_fit<-"
    data {
      int N;   // number of matches
      int nteams;   // number of teams
      vector[nteams] spi_std;  // per-team ranking
      // this is a 4-column data table of per-game outcomes
      int team1[N];
      int team2[N];
      matrix[N,2] y;
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
    }
    transformed data {
      vector[N] diff_y = y[,1] - y[,2];  // modeled data
    }
    parameters {
      real beta;            // common intercept
      matrix[ntimes, nteams] alpha;    // vector of per-team weights
      real<lower=0> sigma_a;   // common variance
      real<lower=0> sigma_y;   // noise term in our estimate
      real<lower=0> sigma_alpha;
    }
    transformed parameters {
      //cov_matrix[ntimes] Sigma_alpha;
      // mixed effects model - common intercept + random effects
      matrix[ntimes, nteams] ability;
      matrix[ntimes, nteams] mu_alpha;

      for (t in 1: ntimes){
        ability[t]= to_row_vector(beta*spi_std) + alpha[t]*sigma_a;
      }

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //   }}

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_alpha[1]=rep_row_vector(0,nteams);
        mu_alpha[t]=alpha[t-1];
        //rep_row_vector(0,nteams);
      }

    }
    model {
      for (h in 1:(nteams)){
        alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
      }
      beta ~ normal(0, 2.5);
      sigma_a ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);
      sigma_alpha ~ normal(0, 2.5);

      for (n in 1:N)
        diff_y[n] ~ student_t(7, ability[instants[n], team1[n]] - ability[instants[n], team2[n]], sigma_y);
    }
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
  }

}"


    student_t_dynamic_prev<- "
    data {
      int N;   // number of matches
      int N_prev;
      int nteams;   // number of teams
      vector[nteams] spi_std;  // per-team ranking
      // this is a 4-column data table of per-game outcomes
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      matrix[N,2] y;
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int instants_prev[N_prev];
    }
    transformed data {
      vector[N] diff_y = y[,1] - y[,2];  // modeled data
    }
    parameters {
      real beta;            // common intercept
      matrix[ntimes, nteams] alpha;    // vector of per-team weights
      real<lower=0> sigma_a;   // common variance
      real<lower=0> sigma_y;   // noise term in our estimate
      real<lower=0> sigma_alpha;
    }
    transformed parameters {
      //cov_matrix[ntimes] Sigma_alpha;
      // mixed effects model - common intercept + random effects
      matrix[ntimes, nteams] ability;
      matrix[ntimes, nteams] mu_alpha;

      for (t in 1: ntimes){
        ability[t]= to_row_vector(beta*spi_std) + alpha[t]*sigma_a;
      }

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //   }}

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_alpha[1]=rep_row_vector(0,nteams);
        mu_alpha[t]=alpha[t-1];
        //rep_row_vector(0,nteams);
      }

    }
    model {
      for (h in 1:(nteams)){
        alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
      }
      beta ~ normal(0, 2.5);
      sigma_a ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);

      for (n in 1:N)
        diff_y[n] ~ student_t(7, ability[instants[n], team1[n]] - ability[instants[n], team2[n]], sigma_y);
    }
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  vector[N_prev] diff_y_prev;


  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
  }

  for (n in 1:N_prev) {
    diff_y_prev[n] = student_t_rng(7, ability[instants_prev[n], team1_prev[n]] - ability[instants_prev[n], team2_prev[n]], sigma_y);
  }

}"

    student_t_fit<-"
    data {
      int N;   // number of matches
      int nteams;   // number of teams
      vector[nteams] spi_std;  // per-team ranking
      // this is a 4-column data table of per-game outcomes
      int team1[N];
      int team2[N];
      matrix[N,2] y;
    }
    transformed data {
      vector[N] diff_y = y[,1] - y[,2];  // modeled data
    }
    parameters {
      real beta;            // common intercept
      vector[nteams] alpha;    // vector of per-team weights
      real<lower=0> sigma_a;   // common variance
      real<lower=0> sigma_y;   // noise term in our estimate
    }
    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[nteams] ability = beta * spi_std + alpha * sigma_a;
    }
    model {
      alpha ~ normal(0, 1); // priors on all parameters
      beta ~ normal(0, 2.5);
      sigma_a ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);

      diff_y ~ student_t(7, ability[team1] - ability[team2], sigma_y);
    }
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[team1[n]] - ability[team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[team1] - ability[team2], sigma_y);
  }
    }"

    student_t_prev<-"
    data {
      int N;   // number of matches
      int N_prev; // number of predictedmatched
      int nteams;   // number of teams
      vector[nteams] spi_std;  // per-team ranking
      // this is a 4-column data table of per-game outcomes
      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      matrix[N,2] y;
    }
    transformed data {
      vector[N] diff_y = y[,1] - y[,2];  // modeled data
    }
    parameters {
      real beta;            // common intercept
      vector[nteams] alpha;    // vector of per-team weights
      real<lower=0> sigma_a;   // common variance
      real<lower=0> sigma_y;   // noise term in our estimate
    }
    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[nteams] ability = beta * spi_std + alpha * sigma_a;
    }
    model {
      alpha ~ normal(0, 1); // priors on all parameters
      beta ~ normal(0, 2.5);
      sigma_a ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);

      diff_y ~ student_t(7, ability[team1] - ability[team2], sigma_y);
    }
    generated quantities {
      // posterior predictive check - carry along uncertainty!!!
        // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  vector[N_prev] diff_y_prev;

  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[team1[n]] - ability[team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[team1] - ability[team2], sigma_y);
  }

  for (n in 1:N_prev) {
    diff_y_prev[n] = student_t_rng(7, ability[team1_prev[n]] - ability[team2_prev[n]], sigma_y);
  }

}"

    models <- list(biv_pois_dynamic_fit,
                     biv_pois_dynamic_prev,
                     biv_pois_fit,
                     biv_pois_prev,
                     double_pois_dynamic_fit,
                     double_pois_dynamic_prev,
                     double_pois_fit,
                     double_pois_prev,
                     skellam_dynamic_fit,
                     skellam_dynamic_prev,
                     skellam_fit,
                     skellam_prev,
                     student_t_dynamic_fit,
                     student_t_dynamic_prev,
                     student_t_fit,
                     student_t_prev)

    right_number <- (1:length(models_name))[right_name==models_name]
    return(models[[right_number]])

}

  fit <- stan(model_code = stanfoot_models(model, dyn, type),
                       data= data_stan,
                       iter = user_dots$iter,
                       chains = user_dots$chains,
                       thin = user_dots$thin,
                       cores = user_dots$cores
                       )
  return(fit)

}



# =fit,
# nteams = nteams,
# teams = teams,
# team1 = team1,
# team2= team2,
# team1_prev = team1_prev,
# team2_prev = team2_prev,
# instants = instants,
# instants_prev = instants_prev

