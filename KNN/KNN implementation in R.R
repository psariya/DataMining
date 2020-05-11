# Source: https://rpubs.com/Nitika/kNN_Iris

require("class")
require("datasets")

data("iris") # load Iris Dataset
str(iris)summary(iris) #view statistical summary of dataset
head(iris) #view top  rows of dataset

set.seed(99) # required to reproduce the results
rnum<- sample(rep(1:150)) # randomly generate numbers from 1 to 150
rnum

iris<- iris[rnum,] #randomize "iris" dataset
head(iris)

# Normalize the dataset between values 0 and 1
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new<- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
head(iris.new)

# subset the dataset
iris.train<- iris.new[1:130,]
iris.train.target<- iris[1:130,5]
iris.test<- iris.new[131:150,]
iris.test.target<- iris[131:150,5]
summary(iris.new)

model1<- knn(train=iris.train, test=iris.test, cl=iris.train.target, k=16)
#model1

table(iris.test.target, model1)










