library(shiny)
library(lme4)
library(MASS)

growth_model <- readRDS("growth_model.rds")
logit_model <- readRDS("logit_model.rds")

ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f4f6f9; /* Softer light grey background */
      font-family: 'Helvetica', Arial, sans-serif;
    }
    h1 {
      font-size: 24px;
      color: #2c3e50; /* Dark blue-grey */
      text-align: center;
      font-weight: bold;
      margin-bottom: 20px;
    }
    .sidebar {
      background-color: #ffffff; /* White background */
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
    }
    .btn-primary {
      background-color: #007bff; /* Bootstrap blue */
      color: white;
      font-weight: bold;
      border: none;
    }
    .btn-primary:hover {
      background-color: #0056b3; /* Darker blue */
    }
    .result-text {
      font-size: 18px;
      font-weight: bold;
      margin-top: 20px;
      text-align: center
    }
    .result-text.at-risk {
      color: #e74c3c; /* Red for at risk */
      text-align: center
    }
    .result-text.not-at-risk {
      color: #27ae60; /* Green for not at risk */
      text-align: center
    }
    .main-content {
      background-color: #ffffff;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
    }
  ")),
  titlePanel(
    HTML("
      <div style='text-align: center;'>
        <span style='font-size: 32px; font-weight: bold;'>VTRACT</span><br>
        <span style='font-size: 24px;'>VSITE TRacker & VSCE Probability CalculaTor</span>
      </div>
    ")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = c("Male" = 1, "Female" = 0)),
      selectInput("usmle_step_1", "USMLE Step 1", choices = c("Pass" = 1, "Fail" = 0)),
      numericInput("usmle_step_2", "USMLE Step 2 Score", value = NA),
      numericInput("usmle_step_3", "USMLE Step 3 Score", value = NA),
      numericInput("vsite_1", "VSITE Score 1", value = NA),
      numericInput("vsite_2", "VSITE Score 2", value = NA),
      numericInput("vsite_3", "VSITE Score 3", value = NA),
      numericInput("vsite_4", "VSITE Score 4", value = NA),
      numericInput("vsite_5", "VSITE Score 5", value = NA),
      actionButton("analyze", "Run Analysis")
    ),
    mainPanel(
      h1("Analysis Results"),
      uiOutput("at_risk_result"),
      uiOutput("vsce_probability")
    )
  )
)

server <- function(input, output) {
  user_data <- reactive({
    data.frame(
      USMLE_Step_1 = as.numeric(input$usmle_step_1),
      USMLE_Step_2 = input$usmle_step_2,
      USMLE_Step_3 = input$usmle_step_3,
      Gender = as.numeric(input$gender),
      VSITE_Scores = c(
        input$vsite_1, input$vsite_2, input$vsite_3, input$vsite_4, input$vsite_5
      ),
      PGYs = 1:5
    )
  })
  
  calculate_intervals <- function(user_input, pgy_level) {
    fixed_effects <- fixef(growth_model)
    fixed_effects_cov <- vcov(growth_model)
    n_bootstrap <- 1000
    bootstrap_predictions <- replicate(n_bootstrap, {
      sampled_fixed_effects <- MASS::mvrnorm(1, mu = fixed_effects, Sigma = fixed_effects_cov)
      sampled_fixed_effects["(Intercept)"] +
        sampled_fixed_effects["PGY"] * pgy_level +
        sampled_fixed_effects["USMLE_Step_1"] * user_input$USMLE_Step_1 +
        sampled_fixed_effects["USMLE_Step_2"] * user_input$USMLE_Step_2 +
        sampled_fixed_effects["USMLE_Step_3"] * user_input$USMLE_Step_3 +
        sampled_fixed_effects["Gender"] * user_input$Gender
    })
    list(
      Lower_Bound = quantile(bootstrap_predictions, probs = 0.025),
      Upper_Bound = quantile(bootstrap_predictions, probs = 0.975)
    )
  }
  
  observeEvent(input$analyze, {
    user_input <- user_data()
    at_risk_flag <- FALSE
    
    # at-risk analysis
    for (i in 1:nrow(user_input)) {
      row <- user_input[i, ]
      if (is.na(row$VSITE_Score)) next
      
      intervals <- calculate_intervals(row, row$PGY)
      
      # check if the score is below the lower bound
      if (row$VSITE_Score < intervals$Lower_Bound) {
        at_risk_flag <- TRUE
        break
      }
    }
    
    output$at_risk_result <- renderUI({
      if (at_risk_flag) {
        tags$div("You are at risk. Please consult your program director.", class = "result-text at-risk")
      } else {
        tags$div("You are not at risk. Keep up the good work!", class = "result-text not-at-risk")
      }
    })
    
    # check if all VSITE scores are provided
    if (any(is.na(user_input$VSITE_Scores))) {
      output$vsce_probability <- renderUI({
        tags$div(
          "Probability analysis requires all VSITE scores to be provided. Please fill in all scores.",
          class = "result-text at-risk"
        )
      })
    } else {
      # probability analysis (if all scores are provided)
      prob_data <- data.frame(
        USMLE_Step_1 = as.numeric(input$usmle_step_1),
        USMLE_Step_2 = input$usmle_step_2,
        USMLE_Step_3 = input$usmle_step_3,
        Gender = as.numeric(input$gender),
        VSITE_Score_1 = input$vsite_1,
        VSITE_Score_2 = input$vsite_2,
        VSITE_Score_3 = input$vsite_3,
        VSITE_Score_4 = input$vsite_4,
        VSITE_Score_5 = input$vsite_5
      )
      
      probability <- as.numeric(predict(logit_model, newdata = prob_data, type = "response")[1])
      
      output$vsce_probability <- renderUI({
        tags$div(
          paste0("Your probability of passing is ", round(probability * 100, 2), "%."),
          class = "result-text"
        )
      })
    }
  })
}

shinyApp(ui = ui, server = server)
