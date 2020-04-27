rm(list=ls())

#Load the data from the provided source via a web request
myBuildingPermits1 = read.csv(url("https://s3.amazonaws.com/cc-analytics-datasets/Building_Permits.csv"))

#exploratory data analysis
#get names of all the columns
names(myBuildingPermits)
#rename the first column
names(myBuildingPermits)[1] <- 'X'

#1. Number of rows and columns in the dataset
nrow(myBuildingPermits)
ncol(myBuildingPermits)

#2. Total different types of construction
#install.packages("dplyr")
library(dplyr)
count(myBuildingPermits, const_type)
###OR
#as.data.frame(table(myBuildingPermits$const_type))
#3. Mean and median number of stories
mean(myBuildingPermits$numberstories, na.rm = TRUE )
median((myBuildingPermits$numberstories), na.rm = TRUE)

#4. Standard deviation for the X and Y coordinates of the permits
sd(myBuildingPermits$X, na.rm = TRUE)
sd(myBuildingPermits$Y, na.rm = TRUE)

#Plot the distributions for each of the following features: Estimated Project Cost and Issue Date Month.
#install.packages("ggplot2")
library(ggplot2)

ggplot(myBuildingPermits, aes(estprojectcost, fill = cut(estprojectcost, 100))) +
  geom_histogram(show.legend = FALSE) + 
  labs(x = "Estimated Project Cost", y = "Frequency") +
  ggtitle("Histogram of Estimated Project Cost")

#The frequency of smaller values is so high, that the higher cost are not visible in the graph.
#lets get more details about this feature

min(myBuildingPermits$estprojectcost)
#0

max(myBuildingPermits$estprojectcost)
#1.7e+08

count(myBuildingPermits, estprojectcost < 250000)
#1 FALSE                      17535
#2 TRUE                      124418
#90% of our records have an estimated project cost that is below $250,000

count(myBuildingPermits, estprojectcost < 1000000)
#1 FALSE                      2937
#2 TRUE                     139016

#Lets plot the Histogram of Issue Date Month
ggplot(myBuildingPermits, aes(issueddate_mth, fill = cut(issueddate_mth, 100))) +
    geom_histogram(show.legend = FALSE, bins = 12) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Issue Date Month", y = "Frequency") +
  ggtitle("Histogram of Issue Date Month") 

table(myBuildingPermits$issueddate_mth)
#    1     2     3     4     5     6     7     8     9    10    11    12 
# 9915  9646 12189 12508 13498 13897 12239 13285 11648 11486  9317  8891 

count(myBuildingPermits, is.na(issueddate_mth))
#1 FALSE                   138519
#2 TRUE                      3434

#The above results show the frequency of permits issued in each month and 
#the number of records that did not have any month issued value in them (3434 records).
#We see from the histogram graph that permits are issued well throughout the year, 
# and the frequency is higher in the spring and summer months and relatively, 
#decreases in the Winter season.

#Regression Analysis

myRegressionData = subset(myBuildingPermits, 
              numberstories <= 3 & const_type == "V  B" & workclass == "New Building", 
              select = c("issueddate_yr", "estprojectcost"))

#When we look at out myRegressionData, we see that the Permit Issue Yr field has some missing 
#values. We'll replace those values with the median of that column.  Median is suitable for 
#continuous data with outliers. From the graphs above we see that our data has outliers


NA2median = function(x) { 
  x[is.na(x)] <- round(median(x, na.rm = TRUE),0)
  x
}
myRegressionData = as.data.frame(sapply(myRegressionData, NA2median))

#in this new dataframe myRegressionData, the dependent variable will be the Estimated project cost
#and the independent variable will be Permit Issue Year
#myScaledData = as.data.frame(scale(myRegressionData))
plot(myRegressionData$issueddate_yr, myRegressionData$estprojectcost)

myLinearModel = lm(estprojectcost ~ issueddate_yr, myRegressionData)
summary(myLinearModel)

#This summary shows the estimate of the intercept and slope of Permit Issued Yr, 
#their standard errors, the test statistics and p value for hypothesis  test that the 
#intercept and slope is zero. 
#Our null hypothesis is that the slope of the linear model is zero.
#Estimate: A slope of 12060 means that on average, a unit increase of Permit Issued Year 
#will increase the Project Cost by $12060, all else remaining the same.
#Std Error: If we were to do the test again with a different sample dataset, we will get a 
#different value of the slope and intercept. The standard error column will give us the approx 
#value of the different range of values of intercept and slope.
#The statistical p-value is <2e-16 which is an extremely small value, and this tells us that 
#we reject our null-hypothesis test. That is, 12060 value is significant. 
#Residual std error or RMSE is the average error of the model, and how well our model does at 
#predicting the value of Project Cost. So, our model is off by $825,800.We also see the 
#R-Squared and adjusted R-squared as well as the hypothesis test and p-value for a test that 
#all the coefficients of the model are zero.

abline(myLinearModel, col = "red")


predInputYr = data.frame(issueddate_yr = myRegressionData$issueddate_yr)
myRegressionData$PredCost = predict(myLinearModel, predInputYr)
myMSE = mean((myRegressionData$estprojectcost - myRegressionData$PredCost)^2)
myMSE
#The MSE (Mean Squared Error) is really bad. We could add more columns to get a better regression model. 
#We could divide the data into train, test and validation sets, and run regression.
#TO improve the slope and get a good model, the best technique is to add more columns to our
#regression model. Also, another way is to use other modelling techniques such as NN, 
#or multivariate regression, or Decision Trees.
