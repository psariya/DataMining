rm(list=ls())

#Load the data from the provided source via a web request
myBuildingPermits = read.csv(url("https://s3.amazonaws.com/cc-analytics-datasets/Building_Permits.csv"))

#exploratory data analysis
#get names of all the columns
names(myBuildingPermits)
#rename the first column
names(myBuildingPermits)[1] <- 'X'
#create copy of input dataset
myBuildingPermits1 = myBuildingPermits
#myBuildingPermits = myBuildingPermits1
#Number of rows and columns in the dataset
nrow(myBuildingPermits)
ncol(myBuildingPermits)

#install.packages("dplyr")
library(dplyr)

#Remove columns that have only one distinct value in them:
myBP = myBuildingPermits %>% select_if(~ length(unique(.)) > 1)

#drop columns that have dates in them (column names ending with 'date')
#install.packages(tidyverse)
#library(tidyverse) 
myBP <- myBP %>% select(-ends_with("date"))
myBP

#Mean and median of "number of stories"
mean(myBuildingPermits$numberstories, na.rm = TRUE )
median((myBuildingPermits$numberstories), na.rm = TRUE)

#Standard deviation for the X and Y coordinates of the permits
sd(myBuildingPermits$X, na.rm = TRUE)
sd(myBuildingPermits$Y, na.rm = TRUE)

#Plot the distributions for each of the following features: Estimated Project Cost and Issue Date Month.
#install.packages("ggplot2")
library(ggplot2)

boxplot(myBP$estprojectcost)

#This data has outliers. lets remove them using IQR score

lh <- quantile(myBP$estprojectcost,probs=0.25)	#Lower hinge (first quartile)
uh <- quantile(myBP$estprojectcost,probs=0.75)	#Upper hinge (third quartile)
step<- 1.5 * (uh-lh)	                          #Define the step as 1.5×IQR

eliminated <- subset(myBP, myBP$estprojectcost >= (lh - step) & myBP$estprojectcost <= (uh + step))
boxplot(eliminated)

#Issuedate-month
myBP %>% group_by(myBP$issueddate_mth) %>% summarise(count=n())

# OR
data.frame(table(myBP$issueddate_mth))


