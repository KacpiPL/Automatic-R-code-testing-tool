### Tasks ######################################################################

# 1. Create and add unique names to five vectors of length 8. Make their types 
# diverse. Create a dataframe named "mySet1" out of created vector.

vector1 = c(1:8)
vector2 = c("Kacper", "Karol", "Pawel", "Andrzej", "Mikolaj", "Wiktor", "Jedrzej", "Jan")
vector3 = c(170, 175, 178, 180, 190, 174, 181, 173)
vector4 = c(30, 35, 33, 41, 29, 36, 38, 34)
vector5 = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")

mySet1 = data.frame(vector1, vector2, vector3, vector4, vector5)

# a) Show the 5th row of created dataframe.

print(mySet1[5,])

# b) Change the name of the second column of mySet1 dataframe to "column02"

colnames(mySet1)[2]="column02"

# c) Show 7 first rows of mySet1 dataframe. Use two different methods - with 
# indexes and with a function.

print(mySet1[1:7,])
head(mySet1,7)

# 2. Use iris dataset. Using indexing show values of every 3rd row between 
# 40th and 120th observations. Try to use a one-liner (shorten the code so that 
# it fits in one line only, without any intermediate steps).

head(iris)

print(iris[seq(from=40,to=120,by=3),])

# 3. Use built-in "women" dataset. 
head(women)

# a) Change type of the first column to character.
women[,1]=as.character(women[,1])
str(women)

# b) Add two new rows to the dataset with made-up numbers. Make sure that you 
# don't loose the types of variables in the main dataframe in the process. 

new_row_1 = c(80, 180)
new_row_2 = c(90,190)

women = rbind(women, new_row_1, new_row_2)

# c) Add new variable to the dataset and name it "shoe_size". Using runif function
# create the values for this variable. Shoe size must be an integer between 35 and 42.

women$shoe_size=round(runif(nrow(women),min=35,max=42),digits=0)