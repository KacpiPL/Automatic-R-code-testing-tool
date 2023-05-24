## 100%

### Tasks ######################################################################

# 1. a) Using USArrests data (built-in dataset) draw a histogram to show the 
# distribution of the Murder variable.

hist(USArrests$Murder)

# b) Select the Zissou1 palette from wesanderson and use a vector of 10 continuous 
# colours from this set to change the colour of the histogram bars.
# When submitting the answer you can assume that the package is already loaded.

hist(USArrests$Murder,col=wes_palette(name="Zissou1",n=10,type="continuous"))

# c) Create a histogram for the Rape variable and color the bins with Moonrise1
# palette - discrete colours, vector with 4 colours, that will be reused in the plot.

hist(USArrests$Rape,col=wes_palette(name="Moonrise1",n=4,type="discrete"))

# d) Change the graphical environment settings (two columns, one row)

par(mfrow=c(1,2))

# e) Draw the two graphs side by side.

hist(USArrests$Murder,col=wes_palette(name="Zissou1",n=10,type="continuous"))
hist(USArrests$Rape,col=wes_palette(name="Moonrise1",n=4,type="discrete"))

# f) reset the graphical environment

dev.off()

# 2. a) Load the insurance.csv dataset into R (medical cost folder) and name it 
# insurance. Check if data is properly loaded and the types of variables are correct.
# Convert sex, smoker and region variables into factor type.

insurance <- read.csv("./data/graphics - medical cost personal dataset/insurance.csv")
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

# b) Prepare a boxplot of the variable charges by region. Change the axis 
# titles to "Medical charges" and "Region"

boxplot(charges~region,data=insurance,xlab="Region",ylab="Medical Charges")

# c) Change the colour of the 'boxes' according to the region and add an 
# appropriate legend. Use a palette viridis with 4 discrete colors. You can assume
# that the viridis package is loaded when submitting the answer.

boxplot(charges~region,data=insurance,xlab="Region",ylab="Medical Charges",col=viridis(4))

# d) Create a legend in the topright corner of the plot. Name the elements exactly
# as the names of the categories are shown in your plot. Hint: you can use the levels()
# function to get the names automatically.
# Make sure that the colours of your legend match the colours of the boxes.
# When submitting the answer provide just the line of code with legend creation.

legend("topright",levels(insurance$region),fill=viridis(4))

# 3. a) Load the Tokyo 2021 dataset dataset from the olympic games folder 
# and store it in the games variable.

games <- read.csv("./data/graphics - olympic games 2021/Tokyo 2021 dataset.csv")

# b) We will prepare a bar chart showing, in sequence, the ten countries that 
# have won the most silver Olympic medals. Start by creating a new dataset "silver10"
# that will store the 10 countries that have won the most Silver medals,
# and order that dataset by the Silver.Medal variable in decreasing order. First
# create a dataset sorted by the Silver Medal variable, and then limit it 
# to the first 10 observations only. Try to combine these steps and to this
# operation in one line only. Submit the shortest code that works for you.

silver10 <- head(games[order(games$Silver,decreasing = TRUE),],10)

# c) Using function barplot prepare a plot for the Silver Medal variable.

barplot(silver10$Silver.Medal)


# d) Add lables under the bars (check the names.arg parameter in the barplot function)
# For the labels use the values of NOCCode function.

barplot(silver10$Silver.Medal, names.arg=silver10$NOCCode)

# e) Add title "Top 10 silver medals".

title("Top 10 silver medals")

# f) Modify the style of the text, change the axis title. Add a chosen colour 
# palette and make the plot more interesting. Play with modification of different 
# plot elements. Export your plot to png file and name it by your 
# student ID number. Submit that plot to the test :)

install.packages("RColorBrewer")
library(RColorBrewer)

plt <- barplot(silver10$Silver.Medal, names.arg=silver10$NOCCode, 
               xlab = "Country",
               ylab = "Number of silver medals",
               col = brewer.pal(name="Spectral",n=10))
title("Top 10 countries by number of silver medals won at the Tokyo Olympics, 2021")

legend("topright", as.expression(silver10$Team.NOC), fill=brewer.pal(name="Spectral",n=10))