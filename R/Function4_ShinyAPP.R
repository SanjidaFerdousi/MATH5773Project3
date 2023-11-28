#' Shiny Interactive MLR Analysis
#'
#' @description
#' Function launches an interactive Shiny app for performing MLR analysis on a provided data set.
#' Users can select response and predictor variables, choose the model type (Linear or Quadratic),
#' and optionally include interaction terms. The app dynamically generates regression models based
#' on user input, displaying both the regression plot and a summary of the model.
#'
#'
#' @param df A data frame containing the variables to be used in the MLR analysis.
#'
#' @return A Shiny app for interactive MLR analysis is launched.
#' @export
#' @import shiny
#'
#' @examples
#' \dontrun{df <- data.frame(temperature = c(16.8, 15.0, 16.5, 17.7, 20.6, 22.6, 23.3, 18.2, 18.6),
#'   catch_ratio = c(0.66, 0.30, 0.46, 0.44, 0.67, 0.99, 0.75, 0.24, 0.51))
#' runMLRShinyApp() }
#' \dontrun{
#' df <- data.frame(
#'   temperature = rnorm(100, mean = 20, sd = 5),
#'   humidity = rnorm(100, mean = 50, sd = 10),
#'   energy_usage = rnorm(100, mean = 200, sd = 20))}

runMLRShinyApp <- function(df) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Please provide a valid data frame.")
  }


  ui <- fluidPage(
    titlePanel("Interactive MLR Analysis"),
    sidebarLayout(
      sidebarPanel(
        selectInput("responseVar", "Select the response variable:", names(df)),
        selectInput("predictorVars", "Select predictor variables:", names(df), multiple = TRUE),
        selectInput("interactionVars", "Select interaction variables:", names(df), multiple = TRUE),
        selectInput("modelType", "Select the model type:", c("Linear", "Quadratic")),
        actionButton("runAnalysis", "Run Analysis")
      ),
      mainPanel(
        plotOutput("regPlot"),
        plotOutput("residualPlot"),
        plotOutput("qqPlot"),
        verbatimTextOutput("modelSummary")
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive event when 'runAnalysis' button is clicked
    observeEvent(input$runAnalysis, {
      formulaString <- reactive({
        base_formula <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        if (input$modelType == "Quadratic") {
          quad_terms <- paste("I(", input$predictorVars, "^2)", collapse = " + ")
          base_formula <- paste(base_formula, "+", quad_terms)
        }
        if (length(input$interactionVars) > 1) {
          interaction_terms <- paste(input$interactionVars, collapse = "*")
          base_formula <- paste(base_formula, "+", interaction_terms)
        }
        as.formula(base_formula)
      })

      model <- reactive({
        lm(formulaString(), data = df)
      })

      output$regPlot <- renderPlot({
        plot(df[[input$predictorVars[1]]], df[[input$responseVar]], main = "MLR Plot", xlab = input$predictorVars[1], ylab = input$responseVar)
        abline(model(), col = "blue")
      })

      output$residualPlot <- renderPlot({
        plot(model()$fitted.values, model()$residuals, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
        abline(h = 0, col = "red")
      })

      output$qqPlot <- renderPlot({
        qqnorm(model()$residuals, main = "Normal Q-Q Plot")
        qqline(model()$residuals, col = "red")
      })

      output$modelSummary <- renderPrint({
        summary(model())
      })
    })
  }


  shinyApp(ui = ui, server = server)
}
