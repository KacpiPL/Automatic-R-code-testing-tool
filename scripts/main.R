library(R6)
library(googlesheets4)
library(googlesheets4)
library(dplyr)
library(yaml)
library(testthat)
library(stringr)


rm(list=ls())

# We refactored the code from Evaluating_results.R to the Evaluating_results_OOP.R but unfortunately,
# during the last hours we found out that the function inside the class Test is not working correctly.
# We do not have any idea why - it should be working.

# It does not evaluate the answers in the proper way, and, because of lack of time we had to come back 
# to the proof of concept solution - that is the reason why part of the code is written functionally and part
# using OOP...

# Load values from config
config <- yaml.load_file("config.yaml")

# Evaluate answers from 3rd test
test_3_results <- compare_answers(config, 3)

# Initialize a new ResultsManager
test_3_res_mngr <- ResultsManager$new(config = config,
                                      test_results_df = test_3_results,
                                      test_number = 3)
# view results
test_3_res_mngr$test_results_df

# update results
test_3_res_mngr$update_test_results()