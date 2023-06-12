# take_max_points
## function to count max possible number of points to get in test
# input:
## df with points per every task and execution time
# output:
## max_points to achieve in one test
# assumptions:
## 1 task = 1 point
## first task in column number 3

take_max_points <- function(df){
  df_cols <- colnames(df)
  max_points <- 0
  for (i in df_cols){
    if (grepl("Task", i)) {
      i_length <- nchar(i)
      last_char <- as.numeric(substr(i, i_length, i_length))
      if (last_char > max_points){
        max_points <- last_char
      }
    }
  }
  return(max_points)
}

# add_sum
## function to add % result of test
# Input:
## df with points per every task and execution time
# Output:
## df with column with calculated percentage points per student

add_sum <- function(df){
  max_points <- take_max_points(df)
  last_col <- 2 + max_points
  df$sum <- rowSums(Table3[, c(3:last_col)])
  df$percentage <- round((df$sum / max_points) * 100, 0)
  df <- subset(df, select = -c(sum))
  return(df)
}

# define_range
## function to define range using numbers, not e.g. "A1"...
# Input: 
## start_row, start_col - numbers of start columns
## end_row, end_col - numbers of start rows
# Output:
## cell_limits object with range
# Note:
# if we do not define the last row/column in range it is ok
# it is better to define both (function sometimes does not work properly with just aratr parameters)
# but if we do not define both row and column in range it is not ok

define_range <- function(start_row, start_col, end_row, end_col){
  range <- cell_limits(ul = c(start_row, start_col), lr = c(end_row, end_col))
  return(range)
}

# find_column_index 
## function to find column index of the chosen test in all_results_df
# Input:
# test number - number of the test
# df - dataframe with all results
# Output:
## test_column_index - index of column of chosen test

find_test_column_index <- function(test_number, df){
  test_number <- as.character(test_number)
  list <- colnames(df)
  index <- sapply(list, function(x) grepl(test_number, x))
  test_column_index <- as.numeric(which(index))
  return(test_column_index)
}

# update_test_results
# function to update results from othe chosen test
# Input:
## test_result_df - df with specific test results (e.g. from number 4)
## all_results_df - google sheet with current status of results
## all_results_gs - gs4_get object to the google sheet with results
## test_column_index - index of the column in all_results to the appropriate test
# Output:
## updated google sheet (all result) with the chosen test
update_test_results <- function(test_result_df, all_results_df, all_results_gs, column_index){
  # take the result and id of student in test_results_df
  for (row in 1:nrow(test_results_df)) {
    # take id and result from test_results_df
    student_id <- test_results_df[row,2]
    student_result <- test_results_df[row, ncol(test_results_df)]
    
    # find student row index in all_results_df
    # +1 because we do not count colnames here
    student_row_index <- which(all_results_df$'Student ID' == student_id) + 1
    
    # num of rows in all_results_df
    # +1 because we do not count colnames here
    last_row_index <- nrow(all_results_df) + 1
    
    # check if the student is in the column
    if(length(student_row_index) > 0) {
      # if yes append the result to the appropriate cell
      range <- define_range(student_row_index, column_index, student_row_index, column_index)
      range_write(
        all_results_gs,
        data = as.data.frame(student_result),
        sheet = NULL,
        range = range,
        col_names = FALSE,
        reformat = TRUE
      )
    } else {
      # if not add his/her id to the first column, add the result
      # refresh all_results_df
      # add ID
      range <- define_range(last_row_index+1, 1, last_row_index+1, 1)
      range_write(
        all_results_gs,
        data = as.data.frame(student_id),
        sheet = NULL,
        range = range,
        col_names = FALSE,
        reformat = TRUE
      )
      # add result
      range <- define_range(last_row_index+1, column_index, last_row_index+1, column_index)
      range_write(
        all_results_gs,
        data = as.data.frame(student_result),
        sheet = NULL,
        range = range,
        col_names = FALSE,
        reformat = TRUE
      )
      # refresh all_results_df
      all_results_df <- read_sheet(config$Settings$ResultsURL)
    }
  }
}

test_results_df <- Table3
all_results_df <- read_sheet(config$Settings$ResultsURL)
all_results_gs <- gs4_get(config$Settings$ResultsURL)
column_index <- find_test_column_index(4, all_results_df)

update_test_results(test_results_df, all_results_df, all_results_gs, column_index)

