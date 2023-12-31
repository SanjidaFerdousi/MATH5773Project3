#' MLR
#'
#' @param df A data frame.
#' @param dep_var A string of the dependent variable.
#' @param ind_vars A vector of strings of the independent variables names.
#'
#' @description
#' To use the function, call it with a data frame, the name of the dependent variable,
#' and a vector of independent variable names. The function then performs the specified
#' analyses and returns a comprehensive list of results and plots.
#'
#'
#' @return A list containing the following components:
#'   \itemize{
#'     \item{reduced}{
#'       \itemize{
#'         \item{model_summary}{Summary of the reduced model.}
#'         \item{AIC}{Akaike Information Criterion for the reduced model.}
#'         \item{cooks_distance}{Cooks distance for the reduced model.}
#'         \item{summary_reduced_anova}{ANOVA summary for the reduced model.}
#'       }
#'     }
#'     \item{full}{
#'       \itemize{
#'         \item{model_summary}{Summary of the full model.}
#'         \item{AIC}{Akaike Information Criterion for the full model.}
#'         \item{cooks_distance}{Cooks distance for the full model.}
#'         \item{summary_full_anova}{ANOVA summary for the full model.}
#'       }
#'     }
#'
#'
#'   }
#'
#'
#' @export
#'
#' @import stats
#' @import graphics
#' @import grDevices
#'
#' @examples
#' data <- data.frame(Y = rnorm(50, mean = 50, sd = 10),
#' X1 = rnorm(50, mean = 5, sd = 2),
#' X2 = sample(c("A", "B", "C"), 50, replace = TRUE))
#' results <- analyze_mlr_model(data, "Y", c("X1", "X2"))
#' data2 <- data.frame(
#' Y2 = rnorm(100, mean = 30, sd = 20),
#' X12 = rep(rnorm(10, mean = 2, sd = 1), each = 10),
#' X22 = rep(sample(c("A", "B", "C", "D"), 25, replace = TRUE), each = 4))
#' results <- analyze_mlr_model(data2, "Y2", c("X12", "X22"))
#'
analyze_mlr_model <- function(df, dep_var, ind_vars) {

  if (!dep_var %in% names(df)) {
    stop("Dependent variable not found in the data frame.")
  }

  if (any(!ind_vars %in% names(df))) {
    stop("One or more independent variables not found in the data frame.")
  }

  df <- df[, c(dep_var, ind_vars)]

  # Reduced model (without interactions)
  formula_reduced <- as.formula(paste(dep_var, "~ ."))
  model_reduced <- lm(formula_reduced, data = df)
  model_reduced_summary <- summary(model_reduced)
  reduced_AIC <- AIC(model_reduced)

  # Full model with two-way interactions
  formula_full <- as.formula(paste(dep_var, "~ .^2"))
  model_full <- lm(formula_full, data = df)
  model_full_summary <- summary(model_full)
  full_AIC <- AIC(model_full)

  # Reduced model
  plot(model_reduced$fitted.values, model_reduced$residuals,
       main="Residuals vs Fitted for reduced model",
       xlab="Fitted values", ylab="Residuals", pch=19)
  abline(h=0, col="red")

  qqnorm(model_reduced$residuals, main="Normal Q-Q")
  qqline(model_reduced$residuals, col="red")

  hist(model_reduced$residuals,
       main="Histogram of Residuals for reduced model",
       xlab="Residuals", col=rainbow(4), border="black")


  #Full model
  plot(model_full$fitted.values, model_full$residuals,
       main="Residuals vs Fitted for full model",
       xlab="Fitted values", ylab="Residuals", pch=19)
  abline(h=0, col="orange")

  qqnorm(model_full$residuals, main="Normal Q-Q")
  qqline(model_full$residuals, col="orange")

  hist(model_full$residuals,
       main="Histogram of Residuals for full model",
       xlab="Residuals", col=rainbow(5), border="black")



  # Summary
  summary_full_anova <- anova(model_full)
  summary_reduced_anova<- anova(model_reduced)

  cooks_dist_full <- cooks.distance(model_full)
  cooks_dist_reduced <- cooks.distance(model_reduced)


  result <- list(
    reduced = list(
      model_summary = model_reduced_summary,
      AIC = reduced_AIC,
      cooks_distance = cooks_dist_reduced,
      summary_reduced_anova = summary_reduced_anova
    ),
    full = list(
      model_summary = model_full_summary,
      AIC = full_AIC,
      cooks_distance = cooks_dist_full,
      summary_full_anova = summary_full_anova
    )


  )

  return(result)
}
