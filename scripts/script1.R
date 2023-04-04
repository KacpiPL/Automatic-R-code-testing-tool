# install.packages("googlesheets4")
# install.packages("gargle")
# install.packages("yaml")

library(googlesheets4)
library(dplyr)
library(yaml)

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

