# install.packages("googlesheets4")
#install.packages("gargle")

rm(list=ls())

library(googlesheets4)
library(dplyr)
# library(gargle)

# # Google Sheet authentication
# ## after running that part of code a browser window will open, asking you to grant permission to the googlesheets4 app. 
# options(gargle_oauth_cache = ".secrets")
# options(gargle_oauth_email = "kt.gruca@student.uw.edu.pl")
# googlesheets4::gs4_auth(path = ".secrets/token")

sheet_url1 <- "https://docs.google.com/spreadsheets/d/1QE03KZDkuKQxXUlxtvCtA95i69RqIJkWl7myFfGF_gw/edit?usp=sharing"
sheet_url2 <- "https://docs.google.com/spreadsheets/d/1pleNQoKcDmoXbsp8qr0DvqFsOrCLPlPMzlHkhn6AHF8/edit?usp=sharing"

data1 <- read_sheet(sheet_url1)
data2 <- read_sheet(sheet_url2)

# function to create final DDL

createFinalDDL <- function(DDL_date, maxHoursDelayed){
  
  DDL_date <- as.POSIXct(DDL_date, tz = "UTC")
  
  if(maxHoursDelayed == 0){
    DDL_date <- DDL_date + 24*60*60 - 1
  } else{
    DDL_date <- DDL_date + 24*60*60 + maxHoursDelayed * 60 * 60
  }
  return(DDL_date)
}


ddl_date <- createFinalDDL("2023-04-06", 0)

