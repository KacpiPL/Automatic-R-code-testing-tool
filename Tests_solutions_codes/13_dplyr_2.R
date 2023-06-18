## 100%

### Tasks ######################################################################

### IMPORTANT! In these tasks please use tidyverse functions whenever possible!!!
### Use the pipeline notation!

# 1. Using the onePanelTask dataset transform wide panel data to long format.
# name the numerical variable "Sales" and the time variable "Year". Try to do all 
# these transformations in just one function in one line.

onePanelTask%>%pivot_longer(cols=!City,names_to="Year",values_to="Sales")

# 2. Fill in the missing values in favItalianDish variable from the tidyrData to "pizza".
# Make sure that you are not making any changes in the favMovie variable. 
# Use tidyr function for that.

tidyrData

tidyrData%>%replace_na(list(favItalianDish="pizza"))


# 3. Unite information from the year, month, day columns in tidyrData. Name 
# the created variable as "birthday" and convert it to Date type (as.Date)

tidyrData%>%unite(col="birthday", year, month, day, sep="-", remove=T)%>%mutate(birthday=as.Date(birthday))


# 4. Extract the third favourite fruit of the person with ID equal to 3. 

tidyrData$favFruit[[3]][1,3]




classof(tidyRdata)