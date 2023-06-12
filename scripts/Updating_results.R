# take_max_points function

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

# add_sum function

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

# define_range function
# Input: None

# Output:
## cell_limits object with range

# Define the range using numeric column and row indexes
# if we do not define the last row/column in range it is ok
# but if we do not define both row and column in range it is not ok

define_range <- function(start_row, start_col, end_row, end_col){
  range <- cell_limits(ul = c(start_row, start_col), lr = c(end_row, end_col))
  return(range)
}

# find_column_index function
# of the test in all_results_df

# Input:
# test number - number of the test
# df - dataframe with all results

find_column_index <- function(test_number, df){
  test_number <- as.character(test_number)
  list <- colnames(df)
  index <- sapply(list, function(x) grepl(test_number, x))
  test_column_index <- as.numeric(which(index))
  return(test_column_index)
}

# write the data to the specific range
range_write(
  results_gs,
  data = data,
  sheet = NULL,
  range = range,
  col_names = FALSE,
  reformat = TRUE
)


test_results_df <- Table3
all_results_df <- read_sheet(config$Settings$ResultsURL)
all_results_gs <- gs4_get(config$Settings$ResultsURL)

range <- define_range(5, 2, 5, NA)


column_index <- find_column_index(4, all_results_df)


# take the result and id of student in test_results_df
for (row in 1:nrow(test_results_df)) {
  student_id <- test_results_df[row,2]
  student_result <- test_results_df[1, ncol(test_results_df)]
  
  # find student row index in all_results_df
  student_row_index <- which(all_results_df$'Student ID' == student_id)
  
  # check if the student is in the column
  if(length(student_row_index) > 0) {
    print(paste("Value is in the column at index:", student_row_index))
  } else {
    print("Value is not in the column.")
  }
  
}


colnames(all_results_df)
