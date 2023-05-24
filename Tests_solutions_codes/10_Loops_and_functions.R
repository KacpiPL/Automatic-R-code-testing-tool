## 100%

### Tasks ######################################################################

# 1. Write your own function "max75" that returns the 75% of the maximum value of a 
# given variable. You can assume that the variable is a numeric. Use the name 
# maximum75 for the temporary calculations done in the function.

# Use the template:

# ... <- ....{           # paste that in 1-definition field in the test
#    maximum75<-...      # paste that in 1-operation field in the test
# return(maximum75)      
#}

max75<-function(maximum75){
  maximum75<-0.75*maximum75;return(maximum75)}

max75(10)

# 2. Modify the loop so that it prints out only the values divisible by 3. 
# TIP: check out the %% symbol :)

for(j in seq(2,20,4)){
  if(j%%3==0) { # paste the condition that you wrote in the 2) field in the test
    print(j)
  }
}


# 3. Using the "next" instruction write a loop which will print out only the
# text values longer than 5 characters.

textVector <- c("Anna", "longitude", "bike", "car", "Sandra") 

for(text in textVector){if(nchar(text)<5) next;print(text)}

# 4. You have a matrix like so:
myMatrix <- matrix(NA, nrow=10, ncol=10)
myMatrix

# a) Create a loop which will go by row and fill in the values to look like this:
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    2    3    4    5    6    7    8    9    10
# [2,]    1    2    3    4    5    6    7    8    9    10
# [3,]    1    2    3    4    5    6    7    8    9    10
# [4,]    1    2    3    4    5    6    7    8    9    10

# Use the following template:
# for(row in 1:nrow(myMatrix)){
#   ....... # paste your code for the body of the loop in the 4a) field in the test
# }

for (i in 1:ncol(myMatrix)){myMatrix[,i]=i}

# b) Write a loop which will reassign the values within myMatrix to look like this:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]     2    3    4    5    6    7    8    9   10    11
# [2,]     3    4    5    6    7    8    9   10   11    12
# [3,]     4    5    6    7    8    9   10   11   12    13
# [4,]     5    6    7    8    9   10   11   12   13    14
# [5,]     6    7    8    9   10   11   12   13   14    15
# [6,]     7    8    9   10   11   12   13   14   15    16
# [7,]     8    9   10   11   12   13   14   15   16    17
# [8,]     9   10   11   12   13   14   15   16   17    18
# [9,]    10   11   12   13   14   15   16   17   18    19
# [10,]   11   12   13   14   15   16   17   18   19    20

# Use the following template:

# for(row in 1:nrow(myMatrix)){
#   for(col in 1:ncol(myMatrix)){
#     .... # paste your code for the body of the loop in the 4b) field in the test
#   }
# }

for (row in 1:nrow(myMatrix)){
  for (col in 1:ncol(myMatrix)){
    myMatrix[row,col] = row + col
  }
}


# c) Write a loop similar to the one in b) that will now reassign the values of 
# the matrix to look like this

# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    2    3    4    5    6    7    8    9    10
# [2,]    2    4    6    8   10   12   14   16   18    20
# [3,]    3    6    9   12   15   18   21   24   27    30
# [4,]    4    8   12   16   20   24   28   32   36    40
# [5,]    5   10   15   20   25   30   35   40   45    50
# [6,]    6   12   18   24   30   36   42   48   54    60
# [7,]    7   14   21   28   35   42   49   56   63    70
# [8,]    8   16   24   32   40   48   56   64   72    80
# [9,]    9   18   27   36   45   54   63   72   81    90
# [10,]   10   20   30   40   50   60   70   80   90   100



# 5. Write a function myMulti which will create a matrix with multiplication 
# table of size n x n, where n will be the argument of your function.

# Use the following template for writing your answer:

# myMulti <- ...... # paste the code from this line to slot 5a) in the test
#   myMatrix <- 
#   for(......)
#     for(........)
#        myMatrix....... # paste the code from this line to slot 5b) in the test
#   .....
#   return(...)

myMulti<-function(n){
  myMatrix <- matrix(NA, nrow=n, ncol=n)
  for (row in 1:nrow(myMatrix)){
    for (col in 1:ncol(myMatrix)){
      myMatrix[row,col]=row*col
    }
  }
  return(myMatrix)
}

myMulti(10)

# 6. Write a function which will take a text vector of package names from CRAN
# and will check if they are installed. If not - it will install them and load them,
# and if they are already installed - the function will just load them.