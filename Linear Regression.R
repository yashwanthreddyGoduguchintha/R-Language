#read data

#strings as factors = TRUE
customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv",stringsAsFactors = T)

#load libraraies
#install.packages(c("caTools","ModelMetrics"))

library(caTools)
library(ModelMetrics)
library(ggplot2)  

#split the data

sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.7)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

#another way of splitting 
rows = seq(1,nrow(customer_churn))

trainRows = sample(rows,(70*nrow(customer_churn))/100)

train1 = customer_churn[trainRows,] 
test1 = customer_churn[-trainRows,]

#Linear regression model - training

#lm(Y ~ X, data=training_data)-> model1

lm(MonthlyCharges ~ tenure, data=train1)-> model1

#Summary of the model
summary(model1)

#predict on test

predict(model1, newdata=test1)->predicted_values

#data frame - test actuals and predicted ones
data.frame(Actual=test1$MonthlyCharges,Predicted=predicted_values)->final_data

#Residual
final_data$Actual - final_data$Predicted -> final_data$error

#RMSE
sqrt(mean((final_data$error)^2)) -> rmse1
rmse1

# Root Mean Squared Error 
# It's the square root of the average of squared differences between actual and predicted values
# Since the errors are squared before they are averaged, 
# the RMSE gives a relatively high weight to large errors.
# RMSE is useful when large errors are particularly undesirable.

rmse(final_data$Actual,final_data$Predicted)

#MAE
mae(final_data$Actual,final_data$Predicted)

# Mean Absolute Error
# all individual differences are weighted equally in the average 

#---------------------------------------------------
#considering all variables other than MonthlyCharges as independent variables

train1$customerID <- NULL
test1$customerID <- NULL

lm(MonthlyCharges~tenure+PhoneService+InternetService,data=train1)-> model2

#summary
summary(model2)

predict(model2,newdata=test1)-> result
cbind(Actual=test1$MonthlyCharges,Predicted=result)->final_data2
as.data.frame(final_data2)-> final_data2

(final_data2$Actual-final_data2$Predicted)->error2

cbind(final_data2,error2) -> final_data2

sqrt(mean((final_data2$error2)^2))-> rmse2
rmse2

rmse(final_data2$Actual,final_data2$Predicted)

#MAE
mae(final_data2$Actual,final_data2$Predicted)

#-----------------------------------------------------------------------------------------------
#Multiple Linear Regression

sample.split(customer_churn$tenure,SplitRatio = 0.65)-> split_model
subset(customer_churn, split_model==T)->train
subset(customer_churn, split_model==F)->test

nrow(train)
nrow(test)

lm(tenure~MonthlyCharges+gender+InternetService+Contract, data=train)-> mod1
predict(mod1,test)->result1

cbind(Actual=test$tenure,Predicted=result1)->final_data1
head(final_data1)
class(final_data1)

as.data.frame(final_data1)->final_data1
class(final_data1)
final_data1$Actual - final_data1$Predicted ->error1
head(error1)
cbind(final_data1,error1)-> final_data1
head(final_data1)

sqrt(mean((final_data1$error1)^2))->rmse1

#-------------------------------------------------

lm(tenure~Partner+PhoneService+TotalCharges+PaymentMethod,data=train)-> mod2
predict(mod2,test)-> result2
cbind(Actual=test$tenure,Predicted=result2)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2
head(error2)

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2,na.rm=T))->rmse2

#---------------------------------------------------
#Assumptions

ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()
ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()+geom_smooth(method = "lm")

lm(TotalCharges~tenure, data = customer_churn)->mod1
predict(mod1,data=customer_churn)-> result1
cbind(Actual=customer_churn$TotalCharges, Predicted=result1)-> final_data1

head(final_data1)
as.data.frame(final_data1)->final_data1
final_data1$Actual -final_data1$Predicted  -> error1
cbind(final_data1,error1)-> final_data1

ggplot(data= final_data1, aes(x=Predicted, y=error1)) + geom_point()

qqnorm(final_data1$error1)







