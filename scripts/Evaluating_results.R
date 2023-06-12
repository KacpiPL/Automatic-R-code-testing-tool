# install.packages("googlesheets4")
# install.packages("gargle")
# install.packages("yaml")
# install.packages("testthat")

library(googlesheets4)
library(dplyr)
library(yaml)
library(testthat)

rm(list=ls())

# Load values from config
config <- yaml.load_file("config.yaml")

data <- read_sheet(config$SheetsURLs$sheetURL4)
ddlDate <- config$Settings$ddlDate
maxHoursDelayed <- config$Settings$maxHoursDelayed


# function to create final DDL
## ddlDate <- final deadline date, in the format "2023-04-06"
## maxHoursDelayed <- the numeric number of hours a student may be late

compare_answers <- function(config, Testnr) {
  
  # Get the relevant URLs and lecture based on Testnr
  sheetURL <- config$SheetsURLs[[paste0("sheetURL", Testnr)]]
  lecture <- config$Lectures[[paste0("Lecture", Testnr)]]
  
  # Read the Google Sheet
  data <- read_sheet(sheetURL)
  data$'Sygnatura czasowa' <- as.POSIXct(data$'Sygnatura czasowa')
  
  data <- data[order(data$'Sygnatura czasowa'), ]
  data <- data[!duplicated(data$`Student ID`), ]
  
  ddlDate <- as.Date(lecture$ddlDate)
  data <- data[as.Date(data$'Sygnatura czasowa') <= ddlDate, ]
  
  # Get the relevant answers from config.yaml
  tasks <- lecture[names(lecture) != c("Code", "ddlDate")]
  task_ids <- paste0("Task", seq_along(tasks))
  task_answers <- unlist(tasks)
  
  # Execute the code from Lecture$Code
  tryCatch({
    eval(parse(text = lecture$Code))
  }, error = function(e) {
    cat("Error occurred while executing the code from Lecture", Testnr, "$Code:\n")
    print(e)
  })
  
  # Initialize a score counter
  total_score <- 0
  
  # Initialize a results dataframe
  results <- data.frame(
    Name = character(nrow(data)),
    Student_ID = character(nrow(data)),
    stringsAsFactors = FALSE
  )
  
  # Add columns for task scores
  results[, task_ids] <- 0
  
  # Iterate over each row in the Google Sheet data
  for (i in 1:nrow(data)) {
    # Get the applicant's answers
    applicant_answers <- data[i, 7:ncol(data)]
    
    # Clean the applicant's answers for columns other than Task1 and Task2
    applicant_answers[, -c(1, 2)] <- lapply(
      applicant_answers[, -c(1, 2)],
      function(answer) trimws(gsub("\\s+", "", answer))
    )
    
    task_scores <- rep(0, length(task_ids))
    start_time <- Sys.time()
    for (j in 1:2) {
      if (task_scores[j] == 0) {
        expected_answer <- tryCatch({
          eval(parse(text = deparse(task_answers[j])))
        }, error = function(e) {
          cat("Error occurred while evaluating expected answer for", task_ids[j], ":\n")
          print(e)
        })
        
        applicant_answer <- tryCatch({
          eval(parse(text = deparse(applicant_answers[j])))
        }, error = function(e) {
          cat("Error occurred while evaluating applicant answer for", task_ids[j], ":\n")
          print(e)
        })
        
        if (!inherits(expected_answer, "error") && !inherits(applicant_answer, "error") &&
            (isTRUE(identical(applicant_answer, expected_answer)) ||
             (!is.na(applicant_answers[j]) && !is.na(task_answers[j]) && applicant_answers[j] == task_answers[j]))) {
          task_scores[j] <- 1
        } else {
          # Perform the test using test_that and catch the error if it fails
          tryCatch({
            test_that(paste("Applicant", i, "-", task_ids[j]), {
              expect_equal(applicant_answer, expected_answer)
            })
            task_scores[j] <- 1
          }, error = function(e) {
            task_scores[j] <- 0
            cat("Test failed for Applicant", i, "-", task_ids[j], "\n")
          })
        }
      }
    }
    
    
    
    
    for (j in 1:length(task_ids)) {
      if (j >= 3 && task_scores[j] == 0) {
        expected_answer <- tryCatch({
          eval(parse(text = task_answers[j]))
        }, error = function(e) {
          cat("Error occurred while evaluating expected answer for", task_ids[j], ":\n")
          print(e)
        })
        
        applicant_answer <- tryCatch({
          eval(parse(text = applicant_answers[j]))
        }, error = function(e) {
          cat("Error occurred while evaluating applicant answer for", task_ids[j], ":\n")
          print(e)
        })
        
        if (!inherits(expected_answer, "error") && !inherits(applicant_answer, "error") &&
            (isTRUE(identical(applicant_answer, expected_answer)) ||
             (!is.na(applicant_answers[j]) && !is.na(task_answers[j]) && applicant_answers[j] == task_answers[j]))) {
          task_scores[j] <- 1
        } else {
          # Perform the test using test_that and catch the error if it fails
          tryCatch({
            test_that(paste("Student", data$`Full Name`[i], "-", task_ids[j]), {
              expect_equal(applicant_answer, expected_answer)
            })
            task_scores[j] <- 1
          }, error = function(e) {
            task_scores[j] <- 0
            cat("Test failed for Student", data$`Full Name`[i], "-", task_ids[j], "\n")
          })
        }
      }
    }
    
    # Calculate the total score for the applicant
    applicant_score <- sum(task_scores)
    
    # Print the applicant's score
    cat("Applicant", i, "score:", applicant_score, "\n")
    
    # Update the total score
    total_score <- total_score + applicant_score
    end_time <- Sys.time()
    execution_time <- end_time - start_time
    # Update the results dataframe with Name and Student ID
    results$Name[i] <- data$`Full Name`[i]
    results$Student_ID[i] <- data$`Student ID`[i]
    results$Execution_Time[i] <- as.numeric(execution_time)
    # Update the results dataframe with task scores
    results[i, task_ids] <- task_scores
  }
  
  # Print the total score
  cat("Total score:", total_score, "\n")
  
  # Return the results dataframe
  return(results)
}


Table3 <- compare_answers(config, 4)


