## 92%

### Tasks ######################################################################

### Use the built-in dataset CO2 for the following tasks:

# 1. Print values of CO2 uptake from the largest to the smallest.
CO2[order(CO2$uptake, decreasing = TRUE),]

# 2. Show the rows of CO2 dataset, where the Type is set to Quebec and Treatment to chilled.

CO2[CO2$Type=="Quebec"&CO2$Treatment=="chilled",]
# 3. Show the rows of CO2 dataset, where the uptake is higher than 40 and the 
# dataset is sorted by the conc value from the smallest to the largest.
# Bonus point for keeping the whole code in just one line. If you need to create
# an intermediate object - name it 'temp'.
CO2[CO2$uptake>40,][order(CO2[CO2$uptake>40,]$uptake,decreasing=TRUE),]

# 4. How to get a random ordering of a CO2 dataset? TIP: You may want to get a 
# vector with random indices that will come from order(unif(...)) results. 
# See section "Picking random rows from data" for reference.
# Bonus point for writing the code in just one line with no intermediate objects.

CO2[order(runif(nrow(CO2))),]

### Run this code before doing the next tasks
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")


# 5. Show rows of missCO2 dataset, which have at lease one missing value.
missCO2[rowSums(is.na(missCO2))>0,]


# 6. Fill in the missing uptake values with value 20.
missCO2[is.na(missCO2$uptake),"uptake"]=20

# 7. Fill in the missing conc values with the mean of conc.
missCO2[is.na(missCO2$conc),"conc"]=mean(missCO2$conc,na.rm=TRUE)

# 8. Extract the numeric values from weight variable and store them in the new 
# column "weightNumber". Bonus point for keeping the code in one line, 
# without any intermediate objects.
missCO2$weightNumber=as.integer(gsub('kg','',missCO2$weight))