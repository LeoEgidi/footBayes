## all the test PASSED (also the skipped ones!)

# require(dplyr)
#
# data("italy")
# italy_2000<- italy %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter(Season=="2000")
#
#
# test_that("expect error in input",{
#
#   # other than stanfit class
#   fit <- mle_foot(italy_2000, "double_pois", iter = 200)
#   expect_error(pp_foot(italy_2000, fit))
#
#   # wrong type
#   expect_error(pp_foot(italy_2000, fit, "rank"))
#
#   # scale_x_discrete
#   expect_warning(pp_foot(italy_2000, fit, "matches"))
#
#
# })
