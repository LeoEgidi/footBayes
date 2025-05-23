#' Football priors distributions and options
#'
#' @name priors
#' @description This prior specification is just a duplicate
#' of some of the priors used by the \pkg{rstanarm} package.
#'
#' These prior distributions can be passed to the
#' \code{stan_foot} function, through the arguments \code{prior} and \code{prior_sd}.
#' See  the vignette \href{http://mc-stan.org/rstanarm/articles/priors.html}{\emph{Prior
#'   Distributions for rstanarm Models}} for further details (to view the priors used for an existing model see
#'   \href{https://mc-stan.org/rstanarm/reference/prior_summary.stanreg.html}{prior_summary}).
#'   The default priors used in the \pkg{stan_foot} modeling function
#'   are intended to be \emph{weakly informative} in that they provide moderate
#'   regularlization and help stabilize computation.
#'
#' You can choose between: \code{normal}, \code{cauchy}, \code{laplace}, \code{student_t}.
#'
#' @param location Prior location. In most cases, this is the prior mean, but
#'   for \code{cauchy} (which is equivalent to \code{student_t} with
#'   \code{df=1}), the mean does not exist and \code{location} is the prior
#'   median. The default value is \eqn{0}.
#' @param scale Prior scale. The default depends on the family (see
#'   \strong{Details}).
#' @param df Prior degrees of freedom. The default is \eqn{1} for
#'   \code{student_t}, in which case it is equivalent to \code{cauchy}.
#' @param autoscale A logical scalar, defaulting to \code{TRUE}.
#'
#' @details The details depend on the family of the prior being used:
#' \subsection{Student t family}{
#'   Family members:
#'   \itemize{
#'   \item \code{normal(location, scale)}
#'   \item \code{student_t(df, location, scale)}
#'   \item \code{cauchy(location, scale)}
#'   }
#'   Each of these functions also takes an argument \code{autoscale}.
#'
#'   For the prior distribution for the intercept, \code{location},
#'   \code{scale}, and \code{df} should be scalars. For the prior for the other
#'   coefficients they can either be vectors of length equal to the number of
#'   coefficients (not including the intercept), or they can be scalars, in
#'   which case they will be recycled to the appropriate length. As the
#'   degrees of freedom approaches infinity, the Student t distribution
#'   approaches the normal distribution and if the degrees of freedom are one,
#'   then the Student t distribution is the Cauchy distribution.
#'
#'   If \code{scale} is not specified it will default to \eqn{10} for the
#'   intercept and \eqn{2.5} for the other coefficients.
#'
#'   If the \code{autoscale} argument is \code{TRUE} (the default), then the
#'   scales will be further adjusted as described above in the documentation of
#'   the \code{autoscale} argument in the \strong{Arguments} section.
#' }
#'
#' \subsection{Laplace family}{
#'   Family members:
#'   \itemize{
#'   \item \code{laplace(location, scale)}
#'   }
#'   Each of these functions also takes an argument \code{autoscale}.
#'
#'   The Laplace distribution is also known as the double-exponential
#'   distribution. It is a symmetric distribution with a sharp peak at its mean
#'   / median / mode and fairly long tails. This distribution can be motivated
#'   as a scale mixture of normal distributions and the remarks above about the
#'   normal distribution apply here as well.
#'
#' }
#'
#' @return A named list to be used internally by the
#'  \code{stan_foot} model fitting function.
#'
#' @seealso The various vignettes for the \pkg{rstanarm} package also discuss
#'   and demonstrate the use of some of the supported prior distributions.
#'
#' @author Leonardo Egidi \email{legidi@units.it}
#'
#' @references
#' Gelman, A., Jakulin, A., Pittau, M. G., and Su, Y. (2008). A weakly
#' informative default prior distribution for logistic and other regression
#' models. \emph{Annals of Applied Statistics}. 2(4), 1360--1383.
#'
#'
#' @rdname priors
#' @export
#'
#'

normal <- function(location = 0, scale = NULL, autoscale = TRUE) {
  validate_parameter_value(scale)
  list(
    dist = "normal", df = NA,
    location = location,
    scale = scale,
    autoscale = autoscale
  )
}

#' @rdname priors
#' @export
student_t <- function(df = 1, location = 0, scale = NULL, autoscale = TRUE) {
  validate_parameter_value(scale)
  validate_parameter_value(df)
  list(
    dist = "t", df = df,
    location = location,
    scale = scale,
    autoscale = autoscale
  )
}

#' @rdname priors
#' @export
cauchy <- function(location = 0, scale = NULL, autoscale = TRUE) {
  student_t(
    df = 1, location = location,
    scale = scale,
    autoscale = autoscale
  )
}

#' @rdname priors
#' @export
laplace <- function(location = 0, scale = NULL, autoscale = TRUE) {
  list(
    dist = "laplace", df = NA,
    location = location,
    scale = scale,
    autoscale = autoscale
  )
}


# internal ----------------------------------------------------------------

# Check for positive scale or df parameter (NULL ok)
#
# @param x The value to check.
# @return Either an error is thrown or \code{TRUE} is returned invisibly.
validate_parameter_value <- function(x) {
  nm <- deparse(substitute(x))
  if (!is.null(x)) {
    if (!is.numeric(x)) {
      stop(nm, " should be NULL or numeric", call. = FALSE)
    }
    if (any(x <= 0)) {
      stop(nm, " should be positive", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# An nlist object is an S3 class list of uniquely
# named  numeric elements.

nlist <- function(...) {
  args <- list(...)
  if (length(args)) {
    return(as_nlist(args))
  }
  structure(list(), .Names = character(0), class = "nlist")
}
