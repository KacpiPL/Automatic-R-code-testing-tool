## 100%

### Tasks ######################################################################

# 1. a) Using USArrests data (built-in dataset) draw a histogram to show the 
# distribution of the Assault variable.

hist(USArrests$Assault)

# b) Add labels above the bins (check the documentation)

hist(USArrests$Assault,labels=TRUE)

# c) Add a title "USA assault distribution" to the plot created in point 1a).

hist(USArrests$Assault,main="USA assault distribution")

# 2. a) Load the insurance.csv dataset into R (medical cost folder) and name it 
# insurance. Check if data is properly loaded and the types of variables are correct.

insurance <- read.csv("./data/graphics - medical cost personal dataset/insurance.csv")

# b) Convert sex variable into factor type.

insurance$sex<-as.factor(insurance$sex)

# c) Do the same to the smoker and region variables.

insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

# 3. a) Using the insurance dataset prepare a correlation graph between age, 
# bmi and charges. When calling the columns use the indexing by column names. 
# Make it so that your graph is created by only one line of code. Use the 
# default parameters of corrplot function (don't change anything yet).
# Hint: use the corrplot() function from the corrplot package. You can assume
# that corrplot package in loaded in R.
# Hint 2: remember to draw the graph from the correlation table 
#made with the cor() function.

corrplot(cor(insurance[,c("age","bmi","charges")]))

# b) Arrange the variables on the graph using the order given by hierarchical
# clustering algorithm (hclust).

corrplot(cor(insurance[,c("age","bmi","charges")]),order="hclust")

# c) Modify the plot that was created in b). Change the area of the graph so 
# that the lower triangle shows the numerical values and the upper triangle 
# shows the representation using circles. 
# Hint: look at the function corrplot.mixed(). 

corrplot.mixed(cor(insurance[,c("age","bmi","charges")]),order="hclust",lower="number",upper="circle")

# d) Prepare a boxplot of the variable charges by region. Change the axis 
# titles to "Medical charges" and "Region"

boxplot(insurance$charges~insurance$region,xlab="Region",ylab="Medical charges")

# e) Modify the boxplot and add more styling to it. Name the axis, change 
# color of the elements, etc. Play with the arguments of plot function.