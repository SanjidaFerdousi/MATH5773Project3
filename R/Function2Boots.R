#' Bootstrapping estimates
#'
#' @param df A data frame containing the data-set.
#' @param model A character string specifying the model ("model1" or "model2").
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
#' out_model1 <- myboot(df = sample_data, model = "model1", iter = 2000, alpha = 0.05)
#' str(out_model1)
#'
#' out_model2 <- myboot(df = sample_data, model = "model2", iter = 2000, alpha = 0.05)
#' str(out_model2)
#'
#'

myboot <- function(df, model, iter, alpha) {
  # Define the design matrices for both models
  if (model == "model1") {
    X <- model.matrix(Diameter ~ MassFlux + HeatFlux, data = df)
    y_var <- "Diameter"
  } else if (model == "model2") {
    X <- model.matrix(Density ~ MassFlux + HeatFlux, data = df)
    y_var <- "Density"
  } else {
    stop("Invalid model specified.")
  }

  # Initialize matrix to store bootstrapped estimates
  hbetas <- matrix(NA, nrow = iter, ncol = ncol(X))

  # Function to calculate estimates using bootstrapped samples
  bootstrap_estimates <- function() {
    ind <- sample(1:nrow(df), nrow(df), replace = TRUE)
    y <- df[ind, y_var]
    X_boot <- X[ind, ]
    beta_hat <- solve(t(X_boot) %*% X_boot) %*% t(X_boot) %*% y
    return(beta_hat)
  }

  # Bootstrap to estimate beta coefficients
  for (j in 1:iter) {
    hbetas[j, ] <- bootstrap_estimates()
  }

  # Calculate point estimates
  point_estimates <- colMeans(hbetas)

  # Calculate confidence intervals
  lower_quantile <- (1 - alpha) / 2
  upper_quantile <- 1 - lower_quantile
  quantiles <- apply(hbetas, 2, quantile, probs = c(lower_quantile, upper_quantile))
  ci <- data.frame(Lower = quantiles[1, ], Upper = quantiles[2, ])


  # Create histograms
  layout(matrix(1:ncol(X), nrow = 1,ncol = ncol(X)))

  lab = c("intercept estimate","slope 1 estimate","slope 2 estimate")
  xax = c(expression(widehat(beta)[0]),expression(widehat(beta)[1]),expression(widehat(beta)[2]))
  for (i in 1:ncol(X)) {
    hist(hbetas[, i], main = paste("Histogram of", lab[i]), xlab = xax[i])
  }

  # Return results as a list
  result <- list(
    betas = hbetas,
    point_estimates = point_estimates,
    confidence_intervals = ci
  )
  invisible(list(hatbetas = hbetas, pointest = bootstrap_estimates))
  return(result)
}


