
# take_max_points function
# input:
## df
# output:
## max_points to achieve in one test
# assumption:
## 1 task = 1 point

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



