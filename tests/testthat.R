# library(testthat)
# library(footBayes)
#
# test_check("footBayes")


#
#
#
# context("Testing btd_foot function")
#
# test_that("btd_foot runs without errors on static data", {
#   data_static <- data.frame(
#     periods = rep(1, 6),
#     team1 = c("AC Milan", "Roma", "Juventus", "Inter", "Roma", "AC Milan"),
#     team2 = c("Juventus", "Inter", "AC Milan", "Roma", "AC Milan", "Juventus"),
#     match_outcome = c(1, 2, 3, 1, 2, 1) # 1 = team1 wins, 2 = draw, 3 = team2 wins
#   )
#
#   expect_silent({
#     result_static <- btd_foot(
#       data = data_static,
#       dynamic_rank = FALSE,
#       home_effect = TRUE,
#       priors = list(
#         mean_psi = 0,
#         std_psi = 1,
#         mean_gamma = 0,
#         std_gamma = 1,
#         mean_home = 0,
#         std_home = 2
#       ),
#       rank_measure = "median",
#       iter = 100,    # Reduced iterations for testing speed
#       chains = 1
#     )
#   })
#
#   expect_s3_class(result_static, "btdFoot")
#   expect_true("fit" %in% names(result_static))
#   expect_true("rank" %in% names(result_static))
# })
#
# test_that("btd_foot handles missing columns gracefully", {
#   data_incomplete <- data.frame(
#     periods = rep(1, 3),
#     team1 = c("Team A", "Team B", "Team C"),
#     # Missing team2 and match_outcome
#     stringsAsFactors = FALSE
#   )
#
#   expect_error(
#     btd_foot(data = data_incomplete),
#     "Data is missing required columns: team2, match_outcome"
#   )
# })
#
# test_that("compute_MAP returns correct mode", {
#   samples <- c(1, 2, 2, 2, 3, 4, 5)
#   map_estimate <- compute_MAP(samples)
#   expect_equal(map_estimate, 2)
# })
#
# test_that("check_prior validates parameters correctly", {
#   expect_error(check_prior("a", "test_param"), "'test_param' must be a single numeric value.")
#   expect_error(check_prior(-1, "test_param", positive = TRUE), "'test_param' must be a positive numeric value.")
#   expect_silent(check_prior(1, "test_param"))
#   expect_silent(check_prior(1, "test_param", positive = TRUE))
# })
