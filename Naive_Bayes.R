##NAIVE BAYES ASSIGNMENT 2 VIC UNI
library(dplyr)
library(ggplot2)
library(psych)
mushroom = read.csv("/Users/AmitShahi/Downloads/Datawarehousefiles/mushrooms.csv")

str(mushroom)
mushroom$type <- as.factor(mushroom$type)
str(mushroom)
View(mushroom)

table(mushroom$type) 

mushroom %>%
  ggplot(aes(x=type,y=odor, fill=type))+
  geom_boxplot()+
  ggtitle("Box Plot")
mushroom %>%
  ggplot(aes(x=type,y=habitat, fill=type))+
  geom_boxplot()+
  ggtitle("Box Plot")

mushroom %>% ggplot(aes(x=odor, fill = type)) +
  geom_density(alpha=0.8, color = 'black') +
  ggtitle("Density Plot")

#Data Partition
data <- sample(2, nrow(mushroom), replace = TRUE, prob = c(0.80,0.20))
data
trainD <- mushroom[data==1,]
str(trainD)
nrow(trainD)
testD <- mushroom[data==2,]
str(testD)
nrow(testD)
# Bayes Theorem  
# P(A|B) = p(A) * P(B|A)/P(B)
#let's make a model
library(e1071)
library(rminer)
library(naivebayes)
e1071model <- naiveBayes(type ~.,data=trainD)  #type is our response variable
e1071model # after running this model you can see the probability of e and p
#e ->0.5148545    p->0.4851455
#In training data about 51.48% of data points belong to type edible, and rest 48.51% belong to poisonous
#let's make a prediction
e1071prediction <- predict(e1071model,testD)
head(cbind(e1071prediction,testD))
e1071prediction





# Confusion Matrix
p1 <- predict(e1071model,testD)
(tab1 <- table(p1, testD$type))
#838 mushroom which are edibile in reality are also correctly predicted to be of edible mushroom
#650 mushroom which are poisonous in reality are correctly predicted to be of poisonous mushroom
#98 mushrooms which are poisonous in reality are predicted edible by model. (miscalculation goes here)
#8 mushroom which are edible in reality are predicted poisonous by the model (miscalculation)




#Misclassification
1-sum(diag(tab1))/sum(tab1)  #0.06649937
#Misclassification is  6.64% on testing
#100%-6.64%= 93.36% is our accuracy in naive bayes model

mmetric(testD$type, e1071prediction, c("ACC", "PRECISION", "TPR", "F1"))




