# take_max_points function
# input:
## df
# output:
## max_points to achieve in one test
# assumption - 1 task = 1 point

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
## df
## max_points <- taken from previous function
# Output:
## df with column with calculated percentage points per student

add_sum <- function(df, max_points){
  last_col <- 2 + max_points
  df$sum <- rowSums(Table3[, c(3:last_col)])
  df$percentage <- round((df$sum / max_points) * 100, 0)
  df <- subset(df, select = -c(sum))
  return(df)
}






