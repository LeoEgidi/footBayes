#' Global variables
#'
#' Global variables
#'
#' @export

utils::globalVariables(c('median', 'quantile', 'group_by', 'summarise',
                       'first','times', 'lo', 'hi',  'mid', 'sd', 'par',
                       'Home', 'Away', 'Prob', 'rpois', 'lo2', 'hi2',
                       'q_025', 'q_975', 'q_25', 'q_75', 'q_50',
                       'filter', 'Home_prob', '.','dpois', 'optim', 'pchisq',
                       'as_nlist', 'valori', 'mutate', 'times', '%>%',
                       'day', 'distinct', 'matches', 'favorite', 'underdog',
                       'periods', 'rank_points', 'team'))

#LinkingTo: StanHeaders (>= 2.18.0), rstan (>= 2.18.1), BH (>= 1.66.0),
#Rcpp (>= 0.12.0), RcppEigen (>= 0.3.3.3.0)
