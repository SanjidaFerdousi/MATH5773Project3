#' Bayesian analysis
#'
#' @description
#' This function provides an approach for conducting both Bayesian and classical
#' linear regression analyses. It provides detailed output summaries, point and interval
#' estimates, comparative statistics, and diagnostic plots.
#'
#' @param data A data frame containing the variables for the regression analysis.
#' @param formula An object of class `formula` specifying the regression model.
#' @param priorList A list of prior settings for the Bayesian regression analysis.
#' @param interval The confidence level for the interval estimates in the classical analysis.
#'
#' @return A list containing:
#'   \item{classical_point_estimates}{Point estimates from the classical regression analysis.}
#'   \item{classical_interval_estimates}{Interval estimates from the classical regression analysis.}
#'   \item{bayesian_summary}{Summary of the Bayesian regression analysis.}
#'   \item{comparison}{Comparison of classical and Bayesian estimates.}
#'   \item{geweke_diag}{Geweke diagnostic for the Bayesian analysis.}
#'   \item{heidel_diag}{Heidelberger-Welch diagnostic for the Bayesian analysis.}
#'
#'
#' @export
#' @import MCMCpack
#' @import coda
#' @import ggplot2
#'
#' @examples
#' df <- data.frame(temperature = c(16.8, 15.0, 16.5, 17.7, 20.6, 22.6, 23.3, 18.2, 18.6),
#'   catch_ratio = c(0.66, 0.30, 0.46, 0.44, 0.67, 0.99, 0.75, 0.24, 0.51))
#' a <- df$catch_ratio
#' b <- df$temperature
#' results <- performAnalyses(df, a ~ b + I(b^2) + I(b^3))
#' results2 <- performAnalyses(df, a ~ b + I(b^2))
performAnalyses <- function(data, formula, priorList = list(), interval = 0.95) {

  if (length(priorList) == 0) {
    priorList <- list(
      beta.mean=rep(0, length(coef(lm(formula, data)))),
      beta.var=diag(2, length(coef(lm(formula, data)))),
      sigma.mu=0,
      sigma.var=10^2,
      nu=0.01,
      delta=0.01
    )
  }


  bayesian_model <- do.call(MCMCregress, c(list(formula = formula, data = data), priorList))
  bayesian_summary <- summary(bayesian_model)

  classical_model <- lm(formula, data = data)
  classical_point_estimates <- coef(classical_model)
  classical_interval_estimates <- confint(classical_model, level = interval)


  coef_names <- names(classical_point_estimates)
  comparison <- data.frame(
    Classical = classical_point_estimates,
    BayesianMean = bayesian_summary$statistics[coef_names, "Mean"],
    Lower95CI_B = bayesian_summary$quantiles[coef_names, "2.5%"],
    Upper95CI_B = bayesian_summary$quantiles[coef_names, "97.5%"],
    classical_interval_estimates
  )


  cat("Classical point estimates:\n")
  print(classical_point_estimates)
  cat("\nClassical interval estimates (95% CI):\n")
  print(classical_interval_estimates)
  cat("\nBayesian mean estimates:\n")
  print(comparison$BayesianMean)
  cat("\nBayesian 95% CI estimates:\n")
  print(comparison[, c("Lower95CI_B", "Upper95CI_B")])

  cat("\nComparison of Classical and Bayesian Estimates:\n")
  print(comparison)


  #par(mfrow=c(2, 2))
  bayesian_mcmc <- as.mcmc(bayesian_model)
  for(i in 1:ncol(bayesian_mcmc)) {
    plot(density(bayesian_mcmc[,i]), main = colnames(bayesian_mcmc)[i])
  }

  traceplot(bayesian_mcmc)

  geweke_diag <- geweke.diag(bayesian_mcmc)
  heidel_diag <- heidel.diag(bayesian_mcmc)

  return(list(
    classical_point_estimates = classical_point_estimates,
    classical_interval_estimates = classical_interval_estimates,
    bayesian_summary = bayesian_summary,
    comparison = comparison,
    geweke_diag = geweke_diag,
    heidel_diag = heidel_diag
  ))
}


