library(R6)
library(googlesheets4)
library(googlesheets4)
library(dplyr)
library(yaml)
library(testthat)
library(stringr)


rm(list=ls())

# Load values from config
config <- yaml.load_file("config.yaml")

# Initialize a new object test_4
test_4_obj <- Test$new(config = config, 
                       testnr = 4)
# run function to score tests
test_4_obj$score_test()

# view results
test_4_obj$results

# Initialize a new ResultsManager
test_4_res_mngr <- ResultsManager$new(config = config,
                                      test_results_df = test_4_obj$results,
                                      test_number = 4)
# view results
test_4_res_mngr$test_results_df

# update results
test_4_res_mngr$update_test_results()
