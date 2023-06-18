# Load required libraries
library(tidyverse)
library(shiny)

# UI code
ui <- fluidPage(
  titlePanel("Comparison Results"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("test_nr", "Test Number:", min = 1, max = 4, value = 1),
      actionButton("run_button", "Run")
    ),
    
    mainPanel(
      plotOutput("average_plot"),
      plotOutput("histogram_plot")
    )
  )
)

# Server code
server <- function(input, output) {
  # Function to run the comparison and generate plots
  run_comparison <- function() {
    # Read the Evaluating_results file to access the compare_answers function
    eval(parse(text = readLines("scripts/Evaluating_results.R", warn = FALSE)))
    
    test_nr <- input$test_nr
    results <- compare_answers(config, test_nr)
    
    results$TotalScore <- rowSums(results[, grep("^Task", colnames(results), value = TRUE)])

    results$TotalScore <- as.numeric(results$TotalScore)

    task_cols <- grep("^Task", colnames(results), value = TRUE)
    avg_points <- results %>%
      select(all_of(task_cols)) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      gather(key = "Task", value = "AvgPoints")
    
    # Calculate overall average points
    total_points <- sum(results[, task_cols])
    total_tasks <- length(task_cols)
    overall_avg_points <- total_points / (nrow(results) * total_tasks)
    
    # Generate average plot
    output$average_plot <- renderPlot({
      barplot(avg_points$AvgPoints, names.arg = avg_points$Task, ylim = c(0, 1),
              main = "Average Points per Task",
              xlab = "Task", ylab = "Average Points")
      abline(h = overall_avg_points, col = "red", lty = 2)
    })
    
    # Generate histogram plot
    output$histogram_plot <- renderPlot({
      hist(results$TotalScore, main = "Histogram of Points",
           xlab = "Points", ylab = "Frequency", col = "lightblue")
    })
  }
  
  
  # Event handler for the run button
  observeEvent(input$run_button, {
    run_comparison()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
