## 95%

### Tasks ######################################################################

# 1. a) Load the dataset "Life Expectancy Data.csv" into R and name it "life" 

life<-read.csv("./data/dataset - life expectancy/Life Expectancy Data.csv",sep=",",dec=".")

# b) Preview its structure and summarise the values (two lines of code).
str(life)
summary(life)
# c) Filter the dataset - show data for 2013 only (use the $ notation where you can).
summary(subset(life,subset=life$Year==2013))

# Summarize the values of the subset (summary()) without saving the data to a 
# separate intermediate variable.
# d) Calculate median of life.expectancy for Developing Countries (status variable) 
# in 2010. Use only one line of code, with no intermediate objects. Get the numerical result.
median(subset(life$Life.expectancy,subset=life$Year==2010&life$Status=='Developing'),na.rm=TRUE)

# e) What the average Polio vaccination share was over the world in the year 2014?
mean(subset(life$Polio,subset=life$Year==2014),na.rm=TRUE)

# 2. a) Create a subset of "life" dataset for year 2008 only, name it life2008.
life2008<-subset(life,subset=life$Year==2008)
life2008



# b) Remove rows which include missing values from your dataset.
life2008<-na.omit(life2008)

# c) Build a linear model for the "life2008" dataset, in which the dependent (y) variable
# will be the GDP, and the regressors (x) will be Polio, Alcohol and infant.deaths
# (in that order). Name the output object model2008.
model2008<-lm(GDP~Polio+Alcohol+infant.deaths,data=life2008)
# d) Check the summary of the modelling results and the structure of output.
summary(model2008)
str(model2008)
# e) Print out the coeficient for infant.deaths (use $ notation where possible).
model2008$coefficients["infant.deaths"]
# f) Calculate the variance of the absolute difference between real GDP values 
# and the values fitted by your model (fitted.values element). Hint: use abs() function.
var(abs(model2008$fitted.values-life2008$GDP))