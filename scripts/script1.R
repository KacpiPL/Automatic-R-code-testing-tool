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

compare_answers <- function(config, sheetURL) {
  # Read the Google Sheet
  data <- read_sheet(sheetURL)
  
  # Get the relevant answers from config.yaml
  lecture <- config$Lectures$Lecture4
  tasks <- lecture[names(lecture) != c("Code", "ddlDate")]
  task_ids <- paste0("Task", seq_along(tasks))
  task_answers <- unlist(tasks)
  
  # Execute the code from Lecture4$Code
  tryCatch({
    eval(parse(text = lecture$Code))
  }, error = function(e) {
    cat("Error occurred while executing the code from Lecture4$Code:\n")
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
    
    
    for (j in 1:3) {
      if (j >= 1 & j < 3 && task_scores[j] == 0) {
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
    
    
    
    # Compare the applicant's answers with expected answers
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
    
    # Update the results dataframe with Name and Student ID
    results$Name[i] <- data$`Full Name`[i]
    results$Student_ID[i] <- data$`Student ID`[i]
    
    # Update the results dataframe with task scores
    results[i, task_ids] <- task_scores
  }
  
  # Print the total score
  cat("Total score:", total_score, "\n")
  
  # Return the results dataframe
  return(results)
}







Table3 <- compare_answers(config, config$SheetsURLs$sheetURL4)











createFinalDDL <- function(ddlDate, maxHoursDelayed){
  
  ddlDate <- as.POSIXct(ddlDate, tz = "UTC")
  
  if(maxHoursDelayed == 0){
    ddlDate <- ddlDate + 24*60*60 - 1
  } else{
    ddlDate <- ddlDate + (24 + maxHoursDelayed) * 60 * 60
  }
  return(ddlDate)
}

# function to filter answers
## only the first answer can be checkd
## answer must be sent before the final DDL - ddlDate

## df <- df to be filtered
## ddlDate <- final ddlDate

filterAnswers <- function(df, ddlDate){
  df <- df %>%
    filter(df[[1]] <= ddlDate)
  
  df <- df %>%
    group_by(df[[5]]) %>%
    slice(which.min(df[[1]])) %>%
    ungroup()
    
  return(df)
}

ddlDate <- createFinalDDL(ddlDate, maxHoursDelayed)
data1 <- filterAnswers(data1, ddlDate)


##### Unit tests - test 4 #####

# function to return TRUE/FALSE basing on test

## function uses expect_equal but does not print result
## it returns TRUE/FALSE

## studentResult <- student result
## expectedResult

expectEqualQuiet <- function(studentResult, expectedResult) {
  output <- tryCatch(
    expr = {
      expect_equal(studentResult, expectedResult)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  return(output)
}

# task 1
## 
createDataTest4Task1 <- function() {
  v1 <- c(1:8)
  v2 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  v3 <- c("A", "B", "C", "D", "E", "F", "G", "H")
  v4 <- c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5)
  v5 <- as.factor(c("X", "Y", "X", "Y", "X", "Y", "X", "Y"))
  
  mySet1 <- data.frame(v1, v2, v3, v4, v5)
  return(mySet1)
}

## 1a
codeTest4Task1a <- function(df){
  return(capture.output(print(df[5,])))
}

runStudentCodeTest4Task1a <- function(mySet1, row, column) {
  return(capture.output(eval(parse(text=data1[[row, column]]))))
}

test4Task1a <- function(df, row, column) {
  
  studentResult <- runStudentCodeTest4Task1a(df, row, column)
  expectedResult <- codeTest4Task1a(df)
  
  return(expectEqualQuiet(studentResult, expectedResult))
}

test4Task1a(createDataTest4Task1(), 1, 9)

## 1b
codeTest4Task1b <- function(df){
  colnames(df)[2] <- "column02"
  return(df)
}

runStudentCodeTest4Task1b <- function(mySet1, row, column){
  eval(parse(text=data1[[row, column]]))
  return(mySet1)
}

test4Task1b <- function(df, row, column){
  
  studentResult <- runStudentCodeTest4Task1b(df, row, column)
  expectedResult <- codeTest4Task1b(df)
  
  return(expectEqualQuiet(studentResult, expectedResult))
}

test4Task1b(createDataTest4Task1(), 1, 10)





