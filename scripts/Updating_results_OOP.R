#install.packages("R6")
library(R6)
library(googlesheets4)

ResultsManager <- R6Class("ResultsManager",
                          private = list(
                            all_results_df = NULL,       # private
                            all_results_gs = NULL,       # private
                            column_index = NULL,         # private
                            config = NULL,               # private
                            
                            .take_max_points = function(df){
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
                            },
                            
                            .add_sum = function(df){
                              max_points <- private$.take_max_points(df)
                              last_col <- 2 + max_points
                              df$sum <- rowSums(df[, c(3:last_col)])
                              df$percentage <- round((df$sum / max_points) * 100, 0)
                              df <- subset(df, select = -c(sum))
                              return(df)
                            },
                            
                            .define_range = function(start_row, start_col, end_row, end_col){
                              range <- cell_limits(ul = c(start_row, start_col), lr = c(end_row, end_col))
                              return(range)
                            },
                            
                            .find_test_column_index = function(test_number, df){
                              test_number <- as.character(test_number)
                              list <- colnames(df)
                              index <- sapply(list, function(x) grepl(test_number, x))
                              test_column_index <- as.numeric(which(index))
                              return(test_column_index)
                            }
                          ),
                          
                          public = list(
                            test_number = NULL,
                            test_results_df = NULL,     # public
                            
                            initialize = function(config, test_results_df, test_number) {
                              
                              stopifnot(is.list(config))
                              stopifnot(is.data.frame(test_results_df))
                              stopifnot(is.numeric(test_number))
                              
                              self$test_results_df <- private$.add_sum(test_results_df)
                              self$test_number <- test_number
                              
                              private$config <- config
                              private$all_results_df <- googlesheets4::read_sheet(private$config$Settings$ResultsURL)
                              private$all_results_gs <- googlesheets4::gs4_get(private$config$Settings$ResultsURL)
                              private$column_index <- private$.find_test_column_index(self$test_number, private$all_results_df)
                            },
                            
                            update_test_results = function(){
                              for (row in 1:nrow(self$test_results_df)) {
                                student_id <- self$test_results_df[row,2]
                                student_result <- self$test_results_df[row, ncol(self$test_results_df)]
                                student_row_index <- which(private$all_results_df$'Student ID' == student_id) + 1
                                last_row_index <- nrow(private$all_results_df) + 1
                                
                                if(length(student_row_index) > 0) {
                                  range <- private$.define_range(student_row_index, private$column_index, student_row_index, private$column_index)
                                  range_write(private$all_results_gs, data = as.data.frame(student_result), sheet = NULL, range = range, col_names = FALSE, reformat = TRUE)
                                } else {
                                  # add ID
                                  range <- private$.define_range(last_row_index+1, 1, last_row_index+1, 1)
                                  range_write(private$all_results_gs, data = as.data.frame(student_id), sheet = NULL, range = range, col_names = FALSE, reformat = TRUE)
                                  # add result
                                  range <- private$.define_range(last_row_index+1, private$column_index, last_row_index+1, private$column_index)
                                  range_write(
                                    private$all_results_gs,
                                    data = as.data.frame(student_result),
                                    sheet = NULL,
                                    range = range,
                                    col_names = FALSE,
                                    reformat = TRUE
                                  )
                                  private$all_results_df <- googlesheets4::read_sheet(private$config$Settings$ResultsURL)
                                }
                              }
                            }))

# run the code
config <- yaml.load_file("config.yaml")

# Initialize a new ResultsManager
test4 <- ResultsManager$new(config = config,
                            test_results_df = test4_df,
                            test_number = 4)

test4$test_results_df
test4$update_test_results()







