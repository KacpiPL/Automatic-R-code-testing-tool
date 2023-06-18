# Load required libraries
library(tidyverse)
library(shiny)
rm(list = ls())

# Code for overall UI
ui <- fluidPage(
  titlePanel("Comparison Results"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("test_nr", "Test Number:", min = 1, max = 4, value = 1),
      textInput("student_id", "Student ID:"),
      actionButton("run_button", "Run")
    ),
    
    mainPanel(
      plotOutput("average_plot"),
      plotOutput("histogram_plot"),
      plotOutput("execution_time_plot")
    )
  )
)

# Code for Server
server <- function(input, output) {
  # Function to run the comparison and generate plots
  run_comparison <- function() {
    eval(parse(text = readLines("scripts/Evaluating_results.R", warn = FALSE)))
    
    test_nr <- input$test_nr
    results <- compare_answers(config, test_nr)
    
    # Calculate TotalScore by summing scores for each task
    results$TotalScore <- rowSums(results[, grep("^Task", colnames(results), value = TRUE)])
    
    # Convert TotalScore column to numeric
    results$TotalScore <- as.numeric(results$TotalScore)
    
    # Calculate average points per task
    task_cols <- grep("^Task", colnames(results), value = TRUE)
    avg_points <- results %>%
      select(all_of(task_cols)) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      gather(key = "Task", value = "AvgPoints")
    
    # Calculate overall average points
    total_points <- sum(results[, task_cols])
    total_tasks <- length(task_cols)
    overall_avg_points <- total_points / (nrow(results) * total_tasks)
    
    # Generate plot for averages
    output$average_plot <- renderPlot({
      ggplot(data = avg_points, aes(x = Task, y = AvgPoints)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        geom_hline(yintercept = overall_avg_points, color = "red", linetype = "dashed") +
        labs(title = "Average Points per Task", x = "Task", y = "Average Points") +
        theme_minimal()
    })
    
    # Generate histogram plot for points
    output$histogram_plot <- renderPlot({
      student_id <- input$student_id
      student_points <- results$TotalScore[results$Student_ID == student_id]
      
      if (is.na(student_id) || student_id == "") {
        # If no student ID is entered, show the histogram without the vertical line
        ggplot(data = results, aes(x = TotalScore)) +
          geom_histogram(fill = "lightblue", color = "black") +
          labs(title = "Histogram of Points", x = "Points", y = "Frequency") +
          theme_minimal()
      } else {
        # If a student ID is entered, show the histogram with the vertical line
        student_points <- results$TotalScore[results$Student_ID == student_id]
        
        ggplot(data = results, aes(x = TotalScore)) +
          geom_histogram(fill = "lightblue", color = "black") +
          geom_vline(xintercept = student_points, color = "red", linetype = "dashed") +
          labs(title = "Histogram of Points", x = "Points", y = "Frequency") +
          theme_minimal()
      }
    })
    
    # Generate histogram plot for execution time
    output$execution_time_plot <- renderPlot({
      ggplot(data = results, aes(x = Execution_Time)) +
        geom_histogram(fill = "lightblue", color = "black") +
        labs(title = "Histogram of Execution Time", x = "Execution Time", y = "Frequency") +
        theme_minimal()
    })
  }
  
  # Event handler for the run button
  observeEvent(input$run_button, {
    run_comparison()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
