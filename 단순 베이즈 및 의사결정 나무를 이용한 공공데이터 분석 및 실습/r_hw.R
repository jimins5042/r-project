options(max.print=1000000)
library("dplyr")
library(gmodels)
library(e1071)
setwd("C:/Users/김지민/Desktop/DataSet")
mydata <- read.csv("data.csv", header = TRUE)

sampleData <- sample_n(mydata, 10000)

HTN_Data <- sampleData[,c(6, 1, 2, 7)]
#HTN_Data

colSums(is.na(HTN_Data))

name <- names(HTN_Data)
for(i in 1:length(name)){
  print(name[i])
  print(table(HTN_Data[name[i]]))
}


train <- HTN_Data[1:8000, -1]
test <- HTN_Data[8001:10000, -1]
train_labels <- HTN_Data[1:8000, 1]
test_labels <- HTN_Data[8001:10000, 1]

HTN_classifier <- naiveBayes(train, train_labels, laplace = 1)

Answer_Rate <- predict(HTN_classifier, test)
CrossTable(Answer_Rate, test_labels)
print(sum(Answer_Rate==test_labels)*100/length(Answer_Rate))

#=================================================================

library(rpart)
library(rpart.plot)
library(party)
library(caret)


HTN_Data$DIS <- as.factor(HTN_Data$DIS)

c <- rpart(DIS~., HTN_Data)
c
plot(c, compress = T, margin = 0.3)
text(c, cex=1.5)

prp(c, type = 4, extra = 2)

str(HTN_Data)

set.seed(1234)
ind <- sample(2, nrow(HTN_Data), replace = TRUE, prob = c(0.8, 0.2))
#ind

train_tree_data <- HTN_Data[ind==1,]
test_tree_data <- HTN_Data[ind==2, ]

str(train_tree_data)

tree <- ctree(DIS~., data = train_tree_data)
tree
plot(tree)

testpredict <- predict(tree, newdata = test_tree_data)

confusionMatrix(testpredict, test_tree_data$DIS)


