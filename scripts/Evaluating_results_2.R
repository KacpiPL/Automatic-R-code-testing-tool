# install.packages("googlesheets4")
# install.packages("gargle")
# install.packages("yaml")
# install.packages("testthat")

library(googlesheets4)
library(dplyr)
library(yaml)
library(testthat)

#rm(list=ls())

# Load values from config
config <- yaml.load_file("config.yaml")


# create_final_ddl
## function to create final DDL
# Input:
## ddlDate <- final deadline date, in the format "2023-04-06"
## maxHoursDelayed <- the numeric number of hours a student may be late
# Output:
## Final, impassable ddl

create_final_ddl <- function(ddlDate, maxHoursDelayed){
  ddlDate <- as.POSIXct(ddlDate, tz = "UTC")
  if(maxHoursDelayed == 0){
    ddlDate <- ddlDate + 24*60*60 - 1
  } else{
    ddlDate <- ddlDate + (24 + maxHoursDelayed) * 60 * 60
  }
  return(ddlDate)
}

# filter_answers
# function to filter answers
## only the first answer can be checkd
## answer must be sent before the final DDL - ddlDate
# Input:
## df <- dataframe to be filtered
## ddlDate <- final deadline 
# Output:
## Filtered dafatrame
  
filter_answers <- function(df, ddlDate){
  df <- df %>%
    filter(df[[1]] <= ddlDate)
  
  df <- df %>%
    group_by(df[[5]]) %>%
    slice(which.min(df[[1]])) %>%
    ungroup()
  return(df)
}

# filter_data_by_email
## function to filter answers by getting only students' data
# Input:
## data - dataframe to be filteres
## students only <- True/False value from config
# Output:
## filtered dataframe

filter_data_by_email <- function(data, students_only) {
  if (students_only) {
    data <- subset(data, grepl("@student\\.uw\\.edu\\.pl$", `Adres e-mail...4`))
  } else {
    data <- data
  }
  return(data)
}


compare_answers <- function(config, Testnr) {
  
  # Getting relevant features from config file using Testnr
  
  sheetURL <- config$SheetsURLs[[paste0("sheetURL", Testnr)]]
  lecture <- config$Lectures[[paste0("Lecture", Testnr)]]
  ddlDate <- lecture$ddlDate
  maxHoursDelayed <- config$Settings$maxHoursDelayed
  students_only <- config$Settings$Students_only
  
  # Read the Google Sheet
  data <- read_sheet(sheetURL)
  
  # create final dll
  ddlDate <- create_final_ddl(ddlDate, maxHoursDelayed)
  
  # filter dataframe
  data <- filter_answers(data, ddlDate)
  data <- filter_data_by_email(data, students_only)
  
  # Get the relevant answers from config.yaml
  tasks <- lecture[names(lecture) != c("Code", "ddlDate")]
  task_ids <- paste0("Task", seq_along(tasks))
  task_answers <- unlist(tasks)
  
  # Execute the code from Lecture$Code. [Trying to catch an error]
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
  
  # Set results as zero to be changed
  results[, task_ids] <- 0
  
  # Iterate over each row in the Google Sheet data
  for (i in 1:nrow(data)) {
    
    # Get the answers for specific student
    applicant_answers <- data[i, 7:ncol(data)]
    
    # Clean the student's answers for columns other than Task1 and Task2
    applicant_answers[, -c(1, 2)] <- lapply(
      applicant_answers[, -c(1, 2)],
      function(answer) trimws(gsub("\\s+", "", answer))
    )
    
    task_scores <- rep(0, length(task_ids))
    start_time <- Sys.time()
    

    for (j in 1:length(task_ids)) {
      if (j >= 1 && task_scores[j] == 0) {
        expected_answer <- tryCatch({
          eval(parse(text = task_answers[j]))
        }, error = function(e) {
          cat("Error occurred while evaluating student answer for", task_ids[j], ":\n")
          print(e)
        })
        
        applicant_answer <- tryCatch({
          eval(parse(text = applicant_answers[j]))
        }, error = function(e) {
          cat("Error occurred while evaluating student answer for", task_ids[j], ":\n")
          print(e)
        })
        
        comparison_result <- tryCatch({
          is_equal <- identical(applicant_answer, expected_answer)
          if (is.na(is_equal)) {
            is_equal <- !is.na(applicant_answer) && !is.na(expected_answer) && applicant_answer == expected_answer
          }
          is_equal
        }, error = function(e) {
          cat("Error occurred while comparing student answer for", task_ids[j], ":\n")
          print(e)
        })
        
        if (!inherits(expected_answer, "error") && !inherits(applicant_answer, "error") && !inherits(comparison_result, "error") && comparison_result) {
          task_scores[j] <- 1
        } else {
          # Perform the test using test_that and catch the error if it fails
          tryCatch({
            test_that(paste("Student", data$`Student ID`[i], "-", task_ids[j]), {
              expect_equal(applicant_answer, expected_answer)
            })
            task_scores[j] <- 1
          }, error = function(e) {
            task_scores[j] <- 0
            cat("Test failed for Student", data$`Student ID`[i], "-", task_ids[j], "\n")
          })
        }
      }
    }
    
    # Calculate the total score for the applicant
    applicant_score <- sum(task_scores)
    
    # Print the applicant's score
    cat("Student no. ", data$`Student ID`[i], "score:", applicant_score, "\n")
    
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



