## 93%

### Tasks ######################################################################

# 1. read the description of the clients' personality analysis data and load it 
# into R (clients.csv file) as a variable named "clients". 

clients<-read.csv("data/clients.csv")

# 2. preview the structure of the data and check what classes have been assigned 
# to the variables in question.

str(clients)

# 3. Check if there are any missing observations in the set. 
# a) Which variables include missing values?

tableWithNA = clients[!complete.cases(clients),]

sum_na = lapply(clients, function (y) sum(is.na(y)))
rm(sum_na)

# b) Input the missing values with the mean or median value from the variable. 
# Before completing the values, consider what values the variable takes. 
# If they are numbers, are they integers (e.g. year of birth)? If so, complete 
# these values according to the nature of the variable (we don't want the year 
# 1995.832, do we? ;)).

# My NA's:

# Year_Birth
# MntWines
# Response

# Year_Birth
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(mean(clients$Year_Birth, na.rm = TRUE), 0)

# MntWines
clients$MntWines[is.na(clients$MntWines)] <- round(mean(clients$MntWines, na.rm = TRUE), 0)

# response

clients$Response[is.na(clients$Response)] <- median(clients$Response, na.rm = TRUE)

median(clients$Response, na.rm = TRUE)


# c) What code do you use to fill the missing values of Year_Birth (if any)?

clients$Year_Birth[is.na(clients$Year_Birth)]<-round(mean(clients$Year_Birth,na.rm=TRUE),0)


# 4. a) Check that all missing observations have been completed. If not, repeat step 3.
# b) What code would you use to show all the rows which still have some missing data?

tableWithNA2 = clients[!complete.cases(clients),]


# 5. a) Consider which variables are worth converting to a "factor" type? 
# Hint: these will usually be text variables with a few specific, recurring 
# values. They can also be variables that are represented by numbers, but do 
# not have a "numerical sense" - e.g. the variable "education" and the values 
# 2, 3, 4, which actually represent successive stages of education (logical sense) 
# rather than the exact number of years of education (numerical sense).

# b) What code would you use to transform the Marital_Status variable (shortest code possible)?

summary(factor(clients$Marital_Status))

clients$Marital_Status <- factor(clients$Marital_Status, 
                                 levels = c("Absurd", "Alone", "Cycle", "Divorced", "Married", "Single", "Together", "Widow", "YOLO"), 
                                 labels = c("Single", "Single", "Cycle", "Single", "Married", "Single", "In Relationship", "Single", "Single"))

#clients$Marital_Status<-factor(clients$Marital_Status,levels=c("Absurd","Alone","Cycle","Divorced","Married","Single","Together","Widow","YOLO"),labels=c("Single","Single","Cycle","Single","Married","Single","In Relationship","Single","Single"))

summary(factor(clients$Marital_Status))

# 6. a) Consider which of the previously identified variables would be worth 
# converting to an 'ordered factor' type (ordered categorical variable).
# Hint: An 'ordered factor' type variable should contain a logical order of 
# levels - e.g. an 'education' variable with values of 'primary', 'secondary' 
# and 'tertiary'. In this case, it may be worthwhile to keep the different 
# levels in order. Another typical example of an ordered factor variable is survey 
# responses recorded using a Likert scale (https://en.wikipedia.org/wiki/Likert_scale).


# b) What code would you use to transform the Education variable? Let's assume that 
# 2n means secondary education and graduation is equal to BA defence.
summary(factor(clients$Education))

clients$Education <- factor(clients$Education,
                            levels = c("Basic", "2n", "Graduation", "Master", "phD"),
                            labels = c("Basic", "Secondary Education", "BA Defence", "Master", "PhD"),
                            ordered = TRUE)

summary(factor(clients$Education))

#clients$Education<-factor(clients$Education,levels=c("Basic","2n","Graduation","Master","phD"),labels=c("Basic","Secondary Education","BA Defence","Master","PhD"),ordered=TRUE)

# 7. Transform the variables identified in steps 5 and 6 into the appropriate classes.

# 8. Save results for future reference! Use an RData file with name "clientsInR".

save(list=ls(all.names=TRUE),file="data/clientsInR.rda")