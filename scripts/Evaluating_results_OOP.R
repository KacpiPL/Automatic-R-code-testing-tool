Test <- R6Class("Test",
                private = list(
                  config = NULL,
                  seed_value = NULL,
                  task_col = NULL,
                  task_ids = NULL,
                  total_score = NULL,
                  
                  # create_final_ddl
                  ## function to create final DDL
                  # Input:
                  ## ddlDate <- final deadline date, in the format "2023-04-06"
                  ## maxHoursDelayed <- the numeric number of hours a student may be late
                  # Output:
                  ## Final, impassable ddl
                  
                  .create_final_ddl = function(ddlDate, maxHoursDelayed){
                    ddlDate <- as.POSIXct(ddlDate, tz = "UTC")
                    if(maxHoursDelayed == 0){
                      ddlDate <- ddlDate + 24*60*60 - 1
                    } else{
                      ddlDate <- ddlDate + (24 + maxHoursDelayed) * 60 * 60
                    }
                    return(ddlDate)
                  },
                  
                  # filter_answers
                  # function to filter answers
                  ## only the first answer can be checkd
                  ## answer must be sent before the final DDL - ddlDate
                  # Input:
                  ## df <- dataframe to be filtered
                  ## ddlDate <- final deadline 
                  # Output:
                  ## Filtered dafatrame
                  .filter_answers = function(df, ddlDate){
                    df <- df %>%
                      filter(df[[1]] <= ddlDate)
                    
                    df <- df %>%
                      group_by(df$'Student ID') %>%
                      slice(which.min(df[[1]])) %>%
                      ungroup()
                    return(df)
                  },
                  
                  # filter_data_by_email
                  ## function to filter answers by getting only students' data
                  # Input:
                  ## df - dataframe to be filteres
                  ## students only <- True/False value from config
                  # Output:
                  ## filtered dataframe
                  .filter_data_by_email = function(df, students_only) {
                    if (students_only) {
                      df <- subset(df, grepl("@student\\.uw\\.edu\\.pl$", `Adres e-mail`))
                    } else {
                      df <- df
                    }
                    return(df)
                  }
                ),
                
                public = list(
                  testnr = NULL,
                  ddlDate = NULL,
                  students_only = NULL,
                  maxHoursDelayed = NULL,
                  data = NULL,
                  tasks = NULL,
                  task_answers = NULL,
                  lecture = NULL,
                  results = NULL,
                  
                  initialize = function(config, testnr) {
                    
                    # Validity checking
                    stopifnot(is.list(config))
                    stopifnot(is.numeric(config$Settings$Seed))
                    stopifnot(is.numeric(config$Settings$Starting_Task_Col))
                    stopifnot(is.numeric(config$Settings$maxHoursDelayed))
                    stopifnot(is.logical(config$Settings$Students_only))
                    stopifnot(is.numeric(testnr))
                    
                    # Initialize private variables
                    private$config <- config
                    private$seed_value <- config$Settings$Seed
                    private$task_col <- config$Settings$Starting_Task_Col
                    private$total_score <- 0
                    
                    # Initialize public variables
                    self$testnr <- testnr
                    self$maxHoursDelayed <- config$Settings$maxHoursDelayed
                    self$students_only <- config$Settings$Students_only
                    self$data <- read_sheet(config$SheetsURLs[[paste0("sheetURL", self$testnr)]])
                    self$lecture <- config$Lectures[[paste0("Lecture", self$testnr)]]
                    self$ddlDate <- self$lecture$ddlDate
                    self$tasks <- self$lecture[names(self$lecture) != c("Code", "ddlDate")]
                    
                    self$task_answers <- unlist(self$tasks)
                    private$task_ids <- paste0("Task", seq_along(self$tasks))  # needs to be defined after tasks
                  },
                  
                  # Main function, to evaluate answers
                  score_test = function() {
                    
                    # Filter data
                    self$ddlDate <- private$.create_final_ddl(self$ddlDate, self$maxHoursDelayed)
                    self$data <- private$.filter_answers(self$data, self$ddlDate)
                    self$data <- private$.filter_data_by_email(self$data, self$students_only)
                    
                    # Initialize the dataframe with results after filtering data
                    self$results <- data.frame(
                      Name = character(nrow(self$data)),
                      Student_ID = character(nrow(self$data)),
                      stringsAsFactors = FALSE
                    )
                    
                    set.seed(private$seed_value)
                    
                    # Execute the code from Lecture$Code. [Trying to catch an error]
                    tryCatch({
                      eval(parse(text = self$lecture$Code))
                    }, error = function(e) {
                      cat("Error occurred while executing the code from Lecture", self$testnr, "$Code:\n")
                      print(e)
                    })
                    
                    # Set results as zero to be changed
                    self$results[, private$task_ids] <- 0
                    
                    # Iterate over each row in the Google Sheet data
                    for (i in 1:nrow(self$data)) {
                      
                      # Get the answers for specific student
                      applicant_answers <- self$data[i, private$task_col:ncol(self$data)]
                      
                      # Clean the student's answers for columns other than Task1 and Task2
                      applicant_answers[, -c(1, 2)] <- lapply(
                        applicant_answers[, -c(1, 2)],
                        function(answer) trimws(gsub("\\s+", "", answer))
                      )
                      
                      task_scores <- rep(0, length(private$task_ids))
                      start_time <- Sys.time()
                      
                      for (j in 1:length(private$task_ids)) {
                        if (isTRUE(identical( str_trim(self$task_answers[j]),str_trim(applicant_answers[j])))) {
                          task_scores[j] <- 1
                        }
                      }
                      
                      
                      
                      
                      for (j in 1:length(private$task_ids)) {
                        if (j >= 1 && task_scores[j] == 0) {
                          expected_answer <- tryCatch({
                            eval(parse(text = self$task_answers[j]))
                          }, error = function(e) {
                            cat("Error occurred while evaluating student answer for", private$task_ids[j], ":\n")
                            print(e)
                          })
                          
                          if (isTRUE(identical(applicant_answers[j], expected_answer))) {
                            # If the applicant's answer is identical to the expected answer
                            task_scores[j] <- 1
                          } else {
                            # Proceed with evaluating the applicant's answer
                            applicant_answer <- tryCatch({
                              set.seed(seed_value)
                              eval(parse(text = applicant_answers[j]))
                            }, error = function(e) {
                              cat("Error occurred while evaluating student answer for", private$task_ids[j], ":\n")
                              print(e)
                            })
                            
                            if (!inherits(expected_answer, "error") && !inherits(applicant_answer, "error") &&
                                (isTRUE(identical(applicant_answer, expected_answer)) ||
                                 (!is.na(applicant_answers[j]) && !is.na(self$task_answers[j]) && applicant_answers[j] == self$task_answers[j]))) {
                              task_scores[j] <- 1
                            } else {
                              # Perform the test using test_that and catch the error if it fails
                              tryCatch({
                                test_that(paste("Student", self$data$`Student ID`[i], "-", private$task_ids[j]), {
                                  expect_equal(applicant_answer, expected_answer)
                                })
                                task_scores[j] <- 1
                              }, error = function(e) {
                                task_scores[j] <- 0
                                cat("Test failed for Student", self$data$`Student ID`[i], "-", private$task_ids[j], "\n")
                              })
                            }
                          }
                        }
                      }
                      
                      
                      # Calculate the total score for the applicant
                      applicant_score <- sum(task_scores)
                      
                      # Print the applicant's score
                      cat("Student no. ", self$data$`Student ID`[i], "score:", applicant_score, "\n")
                      
                      # Update the total score
                      private$total_score <- private$total_score + applicant_score
                      end_time <- Sys.time()
                      execution_time <- end_time - start_time
                      # Update the results dataframe with Name and Student ID
                      self$results$Name[i] <- self$data$`Full Name`[i]
                      self$results$Student_ID[i] <- self$data$`Student ID`[i]
                      self$results$Execution_Time[i] <- as.numeric(execution_time)
                      # Update the results dataframe with task scores
                      self$results[i, private$task_ids] <- task_scores
                    }
                    
                    # Print the total score
                    cat("Total score:", private$total_score, "\n")
                  }
                ))

