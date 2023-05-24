## 100%

### Tasks ######################################################################

# 1. Create a factor with values "a", "b", "c" of length 7. Add labels "Letter A",
# "Letter B", "Letter C". Summarize factor values.

factor1 = factor(x=c("a","b","c","c","b","b","a"),
                 levels=c("a","b","c"),
                 labels=c("Letter A","Letter B", "Letter C"),
                 ordered=FALSE)

# what does it mean to summarize???

factor1

# 2. Create a numeric vector with values 1-4 and length 10. You can use any function
# for creating the vector. Values can be ordered randomly. Summarize the variable 
# and check its type. Then use this vector to create an ordered factor. Set levels
# to "low" "medium" "high" "very high". Summarize the value and compare it to the initial vector.


vector1=sample(1:4,10,replace=TRUE)
factor2=factor(x=vector1,
               levels=c(1,2,3,4),
               labels=c("low","medium","high","very high"),
               ordered=TRUE)

factor2

# 3. Create a matrix with 5 rows and 2 columns, filled with zeros. Save it to "table" 
# variable. 
# a) fill 1st column with values 3, 
# b) set 3rd element of 2nd column to 20. 
# c) Print values of the 2nd column. Check the type of the values in this column. 
# d) Change the 4th element of the 2nd column to "twelve". Print values of the 
# second column again. Check their type. What is different? 
# e) What is the type of the values of the first column? Why?

?matrix

table=matrix(data=0,nrow=5,ncol=2)
# 3a
table[,1]=3
table
# 3b
table[3,2]=20
table
print(table[,2])
class(table[,2])
table[4,2]="twelve"
print(table[,2])
class(table[,2])
class(table[,1])


# 4. Create four variables with different types (vectors, matrices, single values).
# Create a list out of these objects named "myList". 
# a) Now get the second element of the list and add an additional value to it. 
# Save the change so that it will be visible in the list. 
# b) Add new elements at the end of the list - make it a 6-element vector of any type.  
# c) Print the 4th element of the last object in the list. 
# d) Change the value of the 5th element of that last object to NA.

var_1=c("hello","world","what","a","nice","day")
var_2=seq(from=1,to=6,by=1)
var_3=matrix(data=3,nrow=3,ncol=3)
var_4=5
myList=list(var_1,var_2,var_3,var_4)
myList[[2]]=append(myList[[2]],0)
myList[[5]]=sample(2:4,6,replace=TRUE)
print(myList[[length(myList)]][4])
myList[[length(myList)]][5]=NA

myList