#   ____________________________________________________________________________
#   Unit tests for the internal helpers used by mle_foot()                  ####

# These functions live in R/utils_foot.R and are not exported; within the
# package test suite they are reached directly by name.


##  ............................................................................
##  mle_team_block                                                          ####

test_that("mle_team_block reconstructs the dropped team and sums to zero", {
  par <- c("att.B" = 0.3, "att.C" = -0.1)
  teams <- c("A", "B", "C")
  res <- mle_team_block(par, "att", teams)

  expect_named(res, teams)
  expect_equal(unname(res[["A"]]), -(0.3 + -0.1)) # reference = -sum
  expect_equal(sum(res), 0)
  expect_equal(unname(res[c("B", "C")]), c(0.3, -0.1))
})

test_that("mle_team_block returns NULL when the block is absent", {
  par <- c("att.B" = 0.3, home = 0.2)
  expect_null(mle_team_block(par, "ability", c("A", "B")))
})

test_that("mle_team_block matches on the '<prefix>.' stem only", {
  # a 'def.' parameter must not be captured by prefix 'att'
  par <- c("att.B" = 0.3, "def.B" = -0.2)
  res <- mle_team_block(par, "att", c("A", "B"))
  expect_equal(unname(res), c(-0.3, 0.3)) # only att.B is used
})


##  ............................................................................
##  mle_relist_params                                                       ####

test_that("mle_relist_params unpacks team blocks and scalars", {
  teams <- c("A", "B", "C")
  par <- c(
    "att.B" = 0.2, "att.C" = -0.1,
    "def.B" = 0.05, "def.C" = 0.05,
    home = 0.3, const = -1
  )
  p <- mle_relist_params(par, teams)

  expect_length(p$att, 3)
  expect_length(p$def, 3)
  expect_null(p$ability)
  expect_equal(sum(p$att), 0)
  expect_equal(sum(p$def), 0)
  expect_equal(unname(p$home), 0.3)
  expect_equal(unname(p$const), -1)
})


##  ............................................................................
##  Likelihood helpers                                                      ####

test_that("mle_double_pois_lik equals the manual Poisson negative log-likelihood", {
  teams <- c("A", "B")
  par <- c("att.B" = 0.1, "def.B" = -0.2, home = 0.25)
  y1 <- c(2, 1, 0)
  y2 <- c(1, 1, 2)
  t1 <- c(1, 2, 1)
  t2 <- c(2, 1, 2)

  att <- c(A = -0.1, B = 0.1)
  def <- c(A = 0.2, B = -0.2)
  th1 <- exp(0.25 + att[t1] + def[t2])
  th2 <- exp(att[t2] + def[t1])
  manual <- -sum(stats::dpois(y1, th1, log = TRUE) + stats::dpois(y2, th2, log = TRUE))

  expect_equal(mle_double_pois_lik(par, y1, y2, t1, t2, teams), manual)
})

test_that("Dixon-Coles reduces to double Poisson when rho = 0", {
  teams <- c("A", "B", "C")
  par_dc <- c(
    "att.B" = 0.1, "att.C" = -0.05,
    "def.B" = -0.1, "def.C" = 0.2,
    home = 0.3, rho = 0
  )
  par_dp <- par_dc[c("att.B", "att.C", "def.B", "def.C", "home")]
  y1 <- c(0, 1, 2, 1)
  y2 <- c(0, 1, 1, 0)
  t1 <- c(1, 2, 3, 1)
  t2 <- c(2, 3, 1, 3)

  expect_equal(
    mle_dixon_coles_lik(par_dc, y1, y2, t1, t2, teams),
    mle_double_pois_lik(par_dp, y1, y2, t1, t2, teams)
  )
})

test_that("all MLE likelihoods return a single finite value", {
  teams <- c("A", "B", "C")
  base <- c("att.B" = 0.1, "att.C" = -0.05, "def.B" = -0.1, "def.C" = 0.2, home = 0.3)
  y1 <- c(1, 2, 0, 1)
  y2 <- c(0, 1, 1, 2)
  t1 <- c(1, 2, 3, 1)
  t2 <- c(2, 3, 1, 3)

  expect_true(is.finite(mle_double_pois_lik(base, y1, y2, t1, t2, teams)))
  expect_true(is.finite(mle_biv_pois_lik(c(base, const = -1), y1, y2, t1, t2, teams)))
  expect_true(is.finite(mle_skellam_lik(base, y1, y2, t1, t2, teams)))
  expect_true(is.finite(mle_neg_bin_lik(c(base, phi1 = log(5), phi2 = log(8)), y1, y2, t1, t2, teams)))

  ab <- c("ability.B" = 0.1, "ability.C" = -0.2, home = 0.3)
  expect_true(is.finite(mle_student_t_lik(ab, y1, y2, t1, t2, teams, sigma_y = 1)))
})


##  ............................................................................
##  mle_conditional_interval                                                ####

test_that("mle_conditional_interval matches the analytic chi-square interval", {
  # negative log-likelihood = 0.5 (theta - 2)^2  =>  MLE at 2,
  # 95% interval = 2 +/- sqrt(qchisq(0.95, 1))
  fn <- function(parameters, team1, team2, y1, y2) 0.5 * (parameters[["theta"]] - 2)^2
  par_hat <- c(theta = 2)
  mle_value <- -fn(par_hat)

  ci <- mle_conditional_interval(1, par_hat, fn, mle_value, NULL, NULL, NULL, NULL)
  half <- sqrt(stats::qchisq(0.95, 1))

  expect_length(ci, 2)
  # endpoints are grid points (step 0.01), so allow up to one grid step
  expect_lte(abs(ci[1] - (2 - half)), 0.011)
  expect_lte(abs(ci[2] - (2 + half)), 0.011)
  # interval brackets the MLE
  expect_lt(ci[1], 2)
  expect_gt(ci[2], 2)
})


##  ............................................................................
##  Output tables                                                           ####

test_that("mle_loc_table builds an nteams x 3 table with sum-to-zero MLEs", {
  teams <- c("A", "B", "C")
  par_hat <- c("att.B" = 0.30, "att.C" = -0.10, home = 0.2)
  par_names <- names(par_hat)
  ci <- matrix(
    c(0.20, -0.20, NA, 0.40, 0.00, NA),
    nrow = 3,
    dimnames = list(par_names, c("lower", "upper"))
  )

  tab <- mle_loc_table("att", par_hat, par_names, ci, teams)

  expect_equal(dim(tab), c(3, 3))
  expect_equal(colnames(tab), c("2.5%", "mle", "97.5%"))
  expect_equal(rownames(tab), teams)
  # mle column: reference = -(0.30 - 0.10) = -0.20
  expect_equal(unname(tab[, 2]), c(-0.20, 0.30, -0.10))
  # reference team carries no interval: all three columns equal the estimate
  expect_equal(unname(tab[1, ]), rep(-0.20, 3))
})

test_that("mle_scalar_table builds a 1 x 3 table and applies the transform", {
  par_hat <- c(const = log(2))
  ci <- matrix(c(log(1.5), log(3)), nrow = 1, dimnames = list("const", c("lower", "upper")))

  tab <- mle_scalar_table("const", par_hat, ci, transform = exp, digits = 4)

  expect_equal(dim(tab), c(1, 3))
  expect_equal(colnames(tab), c("2.5%", "mle", "97.5%"))
  expect_equal(unname(tab[1, ]), round(c(1.5, 2, 3), 4))
})
