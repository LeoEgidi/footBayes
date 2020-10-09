library(tidyverse)
library(engsoccerdata)

##########################
## DATA
#########################

england <- as_tibble(england)
# one season only
england_2004 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(  Season=="2004")

# more seasons
england_1999_2001 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001" | Season == "2000" | Season =="1999")

#------------------------------------------------------
context("stan data reading")

test_that("stan models recognize errors/warnings" , {
  england <- as_tibble(england)
  england_2004_six <- england %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal,
                  FT) %>%
    filter(  Season=="2004")

  ## six arguments
  expect_warning(stan_foot(data = england_2004_six,
                           model ="double_pois"))

  ## three arguments
  expect_error(stan_foot(data = england_2004[,1:3],
                         model ="double_pois"))

  ## datasets different than matrix and dataframe
  expect_error(stan_foot(data = rnorm(20),
                        model = "skellam"))

  ## wrong model names
  expect_error(stan_foot(data = england_2004,
                        model = "neg_binomial"))

  ## two or more names
  expect_error(stan_foot(england_2004,
            model = c("double_pois", "biv_pois")))

  ### from a .csv (contained in data)
  bundes_2008 <- read.csv2(file="BundesLiga07-08.csv",
                            sep =",",dec=".")
  ## with no adjustment
  expect_error(stan_foot(data = bundes_2008,
                         model = "biv_pois"))

  ## with adjustment, but six columns, the last three as numeric
  bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
                                       'AwayTeam',
                                       'FTHG', 'FTAG',
                                       'HTHG')]
  expect_warning(stan_foot(data = bundes_2008_ristr,
                         model = "double_pois"))
})

#-----------------------------------------------------
context("stan static models")
test_that("prediction causes  warnings/errors", {

  ## predict > N
  expect_error(stan_foot(england_2004,
            model ="student_t",
            predict = dim(england_2004)[1]+1))

  ## predict not a number
  expect_error(stan_foot(england_2004,
            model ="student_t",
            predict = "a"))

  ## predict decimal number
  # expect_warning(stan_foot(england_2004,
  #           model ="student_t",
  #           predict = 30.5))
})

#----------------------------------------------------
context("stan dynamic models")
test_that("dymanics cause warnings/errors",{

  ### one season type
  ##  wrong dynamic
  expect_error(stan_foot(england_2004,
                         model ="student_t",
                         dynamic_type = "annual",
                         predict = 2))

  ## seasonal dynamic with only one season
  expect_warning(stan_foot(england_2004,
                         model ="double_pois",
                         dynamic_type = "seasonal"))

  ## weekly dynamics with unequal matches
  expect_error(stan_foot(england_2004,
                         model ="skellam",
                         dynamic_type = "weekly",
                         predict = 2))

  ### more seasons
  ## weekly dynamics with more seasons
  expect_error(stan_foot(england_1999_2001,
                         model ="double_pois",
                         dynamic_type = "weekly"))
})



#
#
#
#   ## I take six arguments, but within the five
#
#
#
# stan_foot(data = england_2001,
#           model ="biv_pois")
#
#   ## I take twelve arguments, no rename the columns
# england <- as_tibble(england)
# england_2001 <- england %>%
#   #dplyr::select(Season, home, FT, visitor, hgoal,vgoal) %>%
#   filter(  Season=="2001")
#
# stan_foot(data = england_2001,
#           model ="biv_pois")
#
#   ## I take twelve arguments, I rename the columns
# england <- as_tibble(england)
# england_2001 <- england %>%
#   #dplyr::select(Season, home, FT, visitor, hgoal,vgoal) %>%
#   filter(  Season=="2001")
# colnames(england_2001) <- c("season", "home",
#                             "away", "homegoals",
#                             "awaygoals")
#
# stan_foot(data = england_2001,
#           model ="biv_pois")
#
#
#   ## I take four arguments
# england <- as_tibble(england)
# england_2001 <- england %>%
#   dplyr::select(Season, visitor, hgoal,vgoal) %>%
#   filter(  Season=="2001")
#
# stan_foot(data = england_2001,
#           model ="biv_pois")
#
# ### csv file
#
#   ## five arguments
# bundes_2008 <- read.csv2(file="BundesLiga07-08.csv",
#                          sep =",",dec=".")
# is.data.frame(bundes_2008)
# bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
#                                'AwayTeam',
#                                'FTHG', 'FTAG')]
#
# stan_foot(data = bundes_2008_ristr,
#           model = "biv_pois")
#
#   ## more than five arguments
# bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
#                                      'AwayTeam',
#                                      'FTHG', 'FTAG',
#                                      'HTHG')]
# stan_foot(data = bundes_2008_ristr,
#           model = "biv_pois")
#
#   ## four or less arguments
# bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
#                                      'HTHG')]
# stan_foot(data = bundes_2008_ristr,
#           model = "biv_pois")
#
#
# ######################
# ## MODELS
# ######################
#
# england_2001 <- england %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter(  Season=="2001")
#
#   ## wrong name
# stan_foot(england_2001,
#           model ="normal")
#
#   ## two or more names
# stan_foot(england_2001,
#           model = c("double_pois", "biv_pois"))
#
#
#
#
#
# ####################
# ## DYNAMICS
# ####################
#
#   ## dynamics different than seasonal and weekly
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           dynamic_type = "monthly",
#           predict = 310)
#
#   ## dynamic as TRUE
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           dynamic_type = TRUE,
#           predict = 310)
#
#   ## dynamic as FALSE
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           dynamic_type = FALSE,
#           predict = 310)
#
#   ## dynamic as NULL
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           dynamic_type = NULL,
#           predict = 310)
#     # da risolvere...
#
#   ## seasonal dynamics for one season only
# stan_foot(germany_2001,
#           model ="student_t",
#           dynamic_type = "seasonal",
#           predict = 10)
#
#
# ###########################
# ## OPTIONAL ARGUMENTS
# ###########################
#
#   ## iter, chains, cores in a list...
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           predict = 310,
#           ...= list(iter =200, chains=3, cores = 4))
#
#   ## iter, chains, cores not in a list...
# stan_foot(germany_1999_2001,
#           model ="student_t",
#           predict = 310,
#           iter =200, chains=3, cores = 4)

