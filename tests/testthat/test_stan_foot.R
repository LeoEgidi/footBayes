## all the test PASSED (also the skipped ones!)

#------------------------------------------------------

context("stan data reading")

test_that("stan models recognize errors/warnings" , {

  library(engsoccerdata)
  library(dplyr)

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


  england <- as_tibble(england)
  england_2004_six <- england %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal,
                  FT) %>%
    filter(  Season=="2004")

  ## six arguments
  #expect_warning(stan_foot(data = england_2004_six,
  #                         model ="double_pois"))    # skipped, too long

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
  #expect_warning(stan_foot(data = bundes_2008_ristr,
   #                      model = "double_pois"))     # skipped, too long
})

#-----------------------------------------------------
context("stan static models")
test_that("prediction causes  warnings/errors", {

  require(engsoccerdata)
  require(dplyr)

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

  require(engsoccerdata)
  require(dplyr)

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


  ### one season type
  ##  wrong dynamic
  expect_error(stan_foot(england_2004,
                         model ="student_t",
                         dynamic_type = "annual",
                         predict = 2))

  ## seasonal dynamic with only one season
  #expect_warning(stan_foot(england_2004,
  #                       model ="double_pois",
  #                       dynamic_type = "seasonal"))  # skipped, too long

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

context("optional arguments")

test_that("no prior errors occur",{

  # expect_error(stan_foot(england_2004, "biv_pois",
  #         prior = normal(0,10), iter = 200), NA)   # skipped, too long
  #
  # expect_error(stan_foot(england_2004, "biv_pois",
  #         prior = cauchy(0,1), iter = 200), NA)    # skipped, too long
  #
  # expect_error(stan_foot(england_2004, "biv_pois",
  #         prior = student_t(4, 0,1), iter = 200), NA) # skipped, too long
  #
  # expect_error(stan_foot(england_2004, "biv_pois",
  #   prior = student_t(4, 0,1),
  #   prior_sd = laplace(0,1), iter = 200), NA)      # skipped, too long
  #
  # expect_error(stan_foot(england_2004, "biv_pois",
  #         prior = laplace(0,1), iter = 200), NA)   # skipped, too long

  # wrong input prior

  expect_error(stan_foot(england_2004, "biv_pois",
          prior = dirichlet(4, 0,1), iter = 200))

  # wrong scale argument
  a <- "d"
  expect_error(stan_foot(england_2004, "biv_pois",
          prior = normal(0, a), iter = 200))
  })
