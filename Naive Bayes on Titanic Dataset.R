titanic = read.csv(file.choose())
head(titanic)

#Exploratory Data Analysis
titanic$PassengerId <- NULL
titanic$Name <- NULL
titanic$Ticket <- NULL
titanic$Cabin <- NULL
titanic$SibSp <- NULL
titanic$Parch <- NULL
titanic$Embarked <- NULL
head(titanic)

titanic$Sex <- as.factor(titanic$Sex)
titanic$Survived <- as.factor((titanic$Survived))
head(titanic)

# Return the column names containing missing observations
#list_na <- colnames(titanic)[ apply(titanic, 2, anyNA) ]
#list_na

# Create mean
m = round(mean(titanic$Age, na.rm = TRUE),0)

#replace the NA ages
titanic$Age <- replace(titanic$Age, is.na(titanic$Age), m)

head(titanic)

#split into train and test
train = titanic[1:650,]
test = titanic[651:891,]

##Bayesian
#install.packages("klaR")
library ("klaR")

model <- NaiveBayes(Survived ~ ., data=train)

#test the model
predictions <- predict(model, test)

#check for accuracy using confusion matrix
install.packages("caret")
library("caret")
confusionMatrix(test$Survived, predictions$class)














