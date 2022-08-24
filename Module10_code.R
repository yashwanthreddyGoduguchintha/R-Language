# Clear Global environment
rm(list=ls())

#install.packages(c("C50","rpart","rpart.plot","party","randomForest"))

#C5.0 Algorithm
library(C50)
#Recursive Partitioning And Regression Trees
library(rpart)
library(rpart.plot)
#library(party)
#Random Forest Algorithm
library(randomForest)

#Predict flower species(classify)
iris <- iris
head(iris)
dim(iris)
names(iris) 
str(iris)
table(iris$Species)

#split train and test

s = sample(150,100)

iris_train = iris[s,]
iris_test = iris[-s,]

dim(iris_train)
dim(iris_test)

#################
#C5.0 good classification technique but does not provide good plots. For that rpart is good

Model_C50 <-C5.0(x=iris_train[,-5],y=iris_train[,5])
#Model_C50
summary(Model_C50)

#Predicting on Train
P1_train=predict(Model_C50,iris_train)

#P1_train
table(iris_train[,5],Predicted=P1_train)

#Predicting on Test

P1_test = predict(Model_C50,iris_test)
#P1_test
table(iris_test[,5],Predicted=P1_test)

#################
#rpart
#################


Model_rpart= rpart(Species~.,data=iris_train, method="class")

#plot(Model_rpart, main="Classifcation Tree", margin=0.15, uniform=TRUE)
#text(Model_rpart, use.n=T)

Model_rpart
summary(Model_rpart)


#rpart.plot(Model_rpart,type=3)
rpart.plot(Model_rpart)
rpart.rules(Model_rpart)

#Predicting on Train
P1_train_rpart=predict(Model_rpart,iris_train,type="class")
table(iris_train[,5],predicted=P1_train_rpart)

#Predicting on Test
P1_test_rpart=predict(Model_rpart,iris_test,type="class")

# minsplit and minbucket

# The option minbucket provides the smallest number of observations that are 
# allowed in a terminal node. 
# If a split decision breaks up the data into a node with less than the minbucket, 
# it won’t accept it.

# The minsplit parameter is the smallest number of observations in the parent node 
# that could be split further. The default is 20. 
# If you have less than 20 records in a parent node, 
#it is labeled as a terminal node.

# minsplit the minimum number of observations that must exist in a node in order for a
# split to be attempted.
# minbucket the minimum number of observations in any terminal <leaf> node. 
#If only one of minbucket or minsplit is specified, the code either sets minsplit to
# minbucket*3 or minbucket to minsplit/3, as appropriate.


#####

# Random Forest works on the same principle as Decision Tress; 
# however, it does not select all the data points and variables in each of the trees. 
# It randomly samples data points and variables in each of the tree that it creates and 
# then combines the output at the end. 
# It removes the bias that a decision tree model might introduce in the system. 
# Also, it improves the predictive power significantly.

#Random Forest

random_forest <- randomForest(Species~.,data=iris_train, ntree=101,importance=T)

print(random_forest)

# The first parameter specifies our formula: Species ~ . 
# (we want to predict Species using each of the remaining columns of data).

# ntree defines the number of trees to be generated. 
# It is typical to test a range of values for this parameter (i.e. 100,200,300,400,500) 
# and choose the one that minimises the OOB estimate of error rate.

# mtry is the number of features used in the construction of each tree. 
# Note that the default values are different for 
# classification (sqrt(p) where p is number of variables in x) 
# and regression (p/3)

# importance enables the algorithm to calculate variable importance.

# The OOB estimate of error rate is a useful measure to discriminate between 
# different random forest classifiers. 
# We could, for instance, vary the number of trees or the number of variables to be considered, 
# and select the combination that produces the smallest value for this error rate.

#nodesize= min number of samples within the terminal nodes. controls the complexity of the trees.

# maxnodes = maximum number of terminal nodes. Another way to control complexity of the trees.
# More nodes => deeper and more complex trees
# Less nodes => shallower trees

random_forest$importance

varImpPlot(random_forest)

#Each features’s importance is assessed based on two criteria:
  
# -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when 
# that particular variable is omitted from the training set. 
# Caveat: if two variables are somewhat redundant, 
# then omitting one of them may not lead to massive gains in prediction performance, 
# but would make the second variable more important.
# 
# -MeanDecreaseGini: GINI is a measure of node impurity. 
# Think of it like this, if you use this feature to split the data, how pure will the nodes be? 
# Highest purity means that each node contains only elements of a single class. 
# Assessing the decrease in GINI when that feature is omitted leads to an 
# understanding of how important that feature is to split the data correctly.



#predict in Train set
pred_model_train <-predict(random_forest,iris_train,type="response", norm.votes=TRUE)

#predict in test set
pred_model_test <-predict(random_forest,iris_test,type="response", norm.votes=TRUE)

# response => predicted classes (the classes with majority vote)

# norm.votes => Should the vote counts be normalised?