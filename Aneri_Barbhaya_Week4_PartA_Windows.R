#installing the required packages
install.packages("MASS")
install.packages("randomForest")
install.packages("ipred")

#loading the required packages
library(ipred)
library(MASS)
library(randomForest)
library(e1071)

data("fgl")
str(fgl)
set.seed(17)

#Create a Random Forest model with default parameters
fgl.rf<-randomForest(type~.,
                     data = fgl,
                     mtry=2,
                     importance=TRUE,
                     do.trace=100)
#fetching results
print(fgl.rf)

#implementing the K-fold cross validation
set.seed(131)
error.rf<-numeric(10)
for(i in 1:10) error.rf[i]<-
  errorest(type~.,
           data = fgl,
           model=randomForest,
           mtry=2)$error
#printing the results
summary(error.rf)

set.seed(563)
#implementing the SVM model obver same data
error.svm<-numeric(10)
for(i in 1:10)error.svm[i]<-
  errorest(type~.,data = fgl,
           model=svm,
           cost=10,
           gama=1.5)$error
#Comparing the two model results
summary(error.svm)

#plotting variable importance
par(mfrow=c(2,2))
for(i in 1:4)
  plot(sort(fgl.rf$importance[,i],decreasing = TRUE),
       type = "h",
       main = paste("measure",i))
