#' Football priors
#'
#' This prior specification is just a duplicate
#' of some of the priors used by the \pkg{rstanarm} package. They can be passed to the
#' \code{stan_foot} function, through the arguments \code{prior} and \code{prior_sd}.
#' See  the vignette \href{http://mc-stan.org/rstanarm/articles/priors.html}{\emph{Prior
#'   Distributions for rstanarm Models}} for further details.
#' You can choose between: code{normal}, \code{cauchy}, \code{laplace}, \code{student_t}.
#'
#' @export
#'
#'

normal <- function(location = 0, scale = NULL, autoscale = TRUE) {
  validate_parameter_value(scale)
  list(dist = "normal", df = NA, location = location,
       scale = scale,
       autoscale = autoscale)
}


#' @export
student_t <- function(df = 1, location = 0, scale = NULL, autoscale = TRUE) {
  validate_parameter_value(scale)
  validate_parameter_value(df)
  list(dist = "t", df = df,
       location = location,
       scale = scale,
       autoscale = autoscale)
}

#' @export
cauchy <- function(location = 0, scale = NULL, autoscale = TRUE) {
  student_t(df = 1, location = location, scale = scale, autoscale)
}

#' @export
laplace <- function(location = 0, scale = NULL, autoscale = TRUE) {
  list(dist = "laplace", df = NA, location, scale, autoscale)
}


# internal ----------------------------------------------------------------

# Check for positive scale or df parameter (NULL ok)
#
# @param x The value to check.
# @return Either an error is thrown or \code{TRUE} is returned invisibly.
validate_parameter_value <- function(x) {
  nm <- deparse(substitute(x))
  if (!is.null(x)) {
    if (!is.numeric(x))
      stop(nm, " should be NULL or numeric", call. = FALSE)
    if (any(x <= 0))
      stop(nm, " should be positive", call. = FALSE)
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
