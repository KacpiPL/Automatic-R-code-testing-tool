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

data1 <- read_sheet(config$SheetsURLs$sheetURL1)
data2 <- read_sheet(config$SheetsURLs$sheetURL2)
ddlDate <- config$Settings$ddlDate
maxHoursDelayed <- config$Settings$maxHoursDelayed


# function to create final DDL
## ddlDate <- final deadline date, in the format "2023-04-06"
## maxHoursDelayed <- the numeric number of hours a student may be late

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

test4_1b <- test_that("Test4Task1b", {
  mySet1 <- createDataTest4Task1()
  
  studentResult <- runStudentCode(mySet1, 1, 10)
  expectedResult <- codeTest4Task1b(mySet1)
  
  expect_equal(studentResult, expectedResult)
})
