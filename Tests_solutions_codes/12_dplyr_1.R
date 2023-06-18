## 100%

### Tasks ######################################################################

rm(list=ls())
### IMPORTANT! In these tasks please use tidyverse functions whenever possible!!!
### Use the pipeline notation!

# 1. Read the gapminder_full data to a tibble format (use readr package). Name
# the variable "gapminder".

gapminder<-read_csv("./data/dataset - gapminder world/gapminder_full.csv")

# 2. Filter the dataset to get information on 1962 year only.
# Please use the pipeline operator.

gapminder%>%filter(year==1962)

# 3. Create a new variable population1000 which will store the population numbers
# in thousands. Use the mutate command and save the result to your dataset.
# TIP: Divide raw numbers by 1000.

population1000<-gapminder%>%mutate(population/1000)

# 4. Prepare a summary table which will sore the median population count on
# each continent. Use one line of code with pipeline operators.
# TIP: use group_by, and then summarize().

gapminder%>%group_by(continent)%>%summarize(medianPop=median(population,na.rm=T))

# 5. In the full dataset prepare a variable maxCountry which will store the maximum
# gdp value obtained for a specific country in the whole researched period.
# TIP: use group_by, and then mutate to do the calculations in groups.
# Remember to ungroup your data at the end and store the result in the dataframe.

gapminder<-gapminder%>%group_by(country)%>%mutate(maxCountry=max(gdp_cap,na.rm=T))%>%ungroup()

# 6. Using previously created variable show for each country in each year
# the gdp reached its maximum.
# TIP: you can use comparison between gdp_cap and maxCountry variable

gapminder%>%group_by(country)%>%select(country, year, gdp_cap) %>% filter(gdp_cap == max(gdp_cap))%>%ungroup()


# 7. Add a sorting step to the codes from the previous task. Arrange the filtered
# data to see for which country the maximum gdp was in the furthest moment of time.
# You will see which countries "developed backwards".

gapminder%>%group_by(country)%>%select(country,year,gdp_cap)%>%filter(gdp_cap==max(gdp_cap))%>%arrange(year,desc(gdp_cap))%>%ungroup()



###################################################


gapminder %>% group_by(continent) %>% summarize(medianPop = median(population, na.rm=T))

# 5. In the full dataset prepare a variable maxCountry which will store the maximum
# gdp value obtained for a specific country in the whole researched period.
# TIP: use group_by, and then mutate to do the calculations in groups.
# Remember to ungroup your data at the end and store the result in the dataframe.

gapminder1 <- gapminder %>% group_by(country) %>% mutate(maxCountry = max(gdp_cap, na.rm = T))

# 6. Using previously created variable show for each country in each year
# the gdp reached its maximum.
# TIP: you can use comparison between gdp_cap and maxCountry variable

gapminder2 <- gapminder1 %>% select(country, year, gdp_cap) %>% filter(gdp_cap == max(gdp_cap)) %>% ungroup()


# 7. Add a sorting step to the codes from the previous task. Arrange the filtered
# data to see for which country the maximum gdp was in the furthest moment of time.
# You will see which countries "developed backwards".

gapminder3 <- gapminder2 %>% arrange(year, desc(gdp_cap))