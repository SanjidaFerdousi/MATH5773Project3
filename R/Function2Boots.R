#' Bootstrapping estimates for linear regression models
#'
#' @description
#' This function performs bootstrap estimation for linear regression models.
#' It calculates bootstrapped estimates of model coefficients, providing point estimates
#' and confidence intervals.
#'
#' @param df A data frame containing the data-set.
#' @param dep_var A string specifying the dependent variable.
#' @param ind_vars A vector of strings specifying the independent variables.
#' @param iter An integer specifying the number of bootstrap iterations.
#' @param alpha The significance level for confidence intervals.
#'
#' @return A list containing:
#'   \item{betas}{A matrix of bootstrapped estimates for model coefficients.}
#'   \item{point_estimates}{A vector of point estimates for model coefficients.}
#'   \item{confidence_intervals}{A data frame with lower and upper bounds of confidence intervals for model coefficients.}
#' @export
#' @import graphics
#' @import stats
#' @examples
#'
#' set.seed(123)  # For reproducibility
#' sample_data <- data.frame(
#'   Diameter = rnorm(100, mean = 50, sd = 10),
#'   Density = rnorm(100, mean = 10, sd = 2),
#'   MassFlux = runif(100, min = 1, max = 5),
#'   HeatFlux = runif(100, min = 0, max = 10)
#' )
#' out <- myboot(df = sample_data, dep_var = "Diameter",
#'   ind_vars = c("MassFlux", "HeatFlux"), iter = 2000, alpha = 0.05)
#' str(out)
#' out2 <- myboot(df = sample_data, dep_var = "Density",
#'   ind_vars = c("MassFlux", "HeatFlux"), iter = 2000, alpha = 0.05)
#' str(out2)
#'
myboot <- function(df, dep_var, ind_vars, iter, alpha) {

  if (!dep_var %in% names(df)) {
    stop("Dependent variable not found in the data frame.")
  }
  if (any(!ind_vars %in% names(df))) {
    stop("One or more independent variables not found in the data frame.")
  }
  if (!is.numeric(iter) || iter <= 0) {
    stop("'iter' should be a positive integer.")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a number between 0 and 1.")
  }


  formula_str <- as.formula(paste(dep_var, "~", paste(ind_vars, collapse = " + ")))
  X <- model.matrix(formula_str, data = df)


  hbetas <- matrix(NA, nrow = iter, ncol = ncol(X))
  bootstrap_estimates <- function() {
    ind <- sample(1:nrow(df), nrow(df), replace = TRUE)
    y <- df[ind, dep_var]
    X_boot <- X[ind, ]
    beta_hat <- solve(t(X_boot) %*% X_boot) %*% t(X_boot) %*% y
    return(beta_hat)
  }

  for (j in 1:iter) {
    hbetas[j, ] <- bootstrap_estimates()
  }


  point_estimates <- colMeans(hbetas)
  quantiles <- apply(hbetas, 2, quantile, probs = c((1 - alpha) / 2, 1 - (1 - alpha) / 2))
  ci <- data.frame(Lower = quantiles[1, ], Upper = quantiles[2, ])


  layout(matrix(1:ncol(X), nrow = 1, ncol = ncol(X)))
  for (i in 1:ncol(X)) {
    hist(hbetas[, i], main = paste("Histogram of Coefficient", i), xlab = paste("Coefficient", i))
  }


  result <- list(
    betas = hbetas,
    point_estimates = point_estimates,
    confidence_intervals = ci
  )

  return(result)
}
