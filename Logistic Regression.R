#read data

#strings as factors = TRUE

customer_churn <- read.csv("C:/Users/Abhishek/Desktop/Rsession/customer_churn.csv",stringsAsFactors = T)

library(caTools)
library(ROCR)
library(caret)
library(e1071)
library(ggplot2)

customer_churn$Churn_new <- ifelse(customer_churn$Churn == "Yes",1,0)
customer_churn$Churn_new <- as.factor(customer_churn$Churn_new)

#barplot
barplot(table(customer_churn$Churn_new))

table(customer_churn$Churn_new)
round(prop.table(table(customer_churn$Churn_new)) * 100)

df <- data.frame(table(customer_churn$Churn))

ggplot(df,aes(x=Var1,y=Freq,fill=Var1)) + geom_bar(stat="identity")+ 
  geom_text(aes(label=Freq),vjust=1.6) + xlab("Churn") + 
  ggtitle("Churn Distribution") +
  theme_minimal()



#Split the data into train and test data sets

rows=seq(1,nrow(customer_churn))

trainRows=sample(rows,(70*nrow(customer_churn))/100)
train1 = customer_churn[trainRows,] 
test1 = customer_churn[-trainRows,]


#----------------------------------------

#logistic regression model

glm(Churn_new ~ MonthlyCharges+Partner+InternetService, data=train1, family = "binomial")-> mod_log

#summary of the model
summary(mod_log)

#AIC (Akaike Information Criteria) 
#The analogous metric of adjusted R² in logistic regression is AIC. 
#AIC is the measure of fit which penalizes model for the number of model coefficients. 
#Therefore, we always prefer model with minimum AIC value.

# Null Deviance and Residual Deviance 
# Null Deviance indicates the response predicted by a model with nothing but an intercept. Lower the value, better the model. 
# Residual deviance indicates the response predicted by a model on adding independent variables. Lower the value, better the model.



#prediction on test

predict(mod_log, test1, type="response")->result_log


table(test1$Churn_new, result_log>=0.4)

final_data <- data.frame(Actual=test1$Churn_new,Predicted=result_log)

final_data$predit_values <- ifelse(final_data$Predicted>=0.4,1,0)
final_data$predit_values <- as.factor(final_data$predit_values)

confusionMatrix(final_data$predit_values, final_data$Actual)


prediction(result_log,test1$Churn_new) -> predict_log

performance(predict_log,"tpr","fpr") -> roc_curve
plot(roc_curve)
plot(roc_curve, colorize=T)

auc.tmp <- performance(predict_log,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#Higher the area under curve, better the prediction power of the model.

#---------------------------------------------------------
#Multiple Logistic Regression


glm(Churn_new~gender+Partner+InternetService+MonthlyCharges, data=train1, family = "binomial")-> mod_log1

predict(mod_log1,newdata=test1,type="response")->result_log1
head(result_log1)
range(result_log1)
table(test$Churn, result_log1>0.4)

#--

glm(Churn_new~PaymentMethod+TechSupport+tenure+PaperlessBilling, data=train1, family = "binomial")-> mod_log2
predict(mod_log2,newdata=test1,type="response")->result_log2
head(result_log2)

range(result_log2)
table(test$Churn, result_log2>0.4)

#--
  
glm(Churn_new~Contract+Dependents+MultipleLines+DeviceProtection, data=train1, family = "binomial")-> mod_log3
predict(mod_log3,newdata=test1,type="response")->result_log3
head(result_log3)

range(result_log3)
table(test$Churn, result_log3>0.4)

#-----------------------------------------------------------------------------------------
#ROCR


glm(Churn~MonthlyCharges, data=train1, family = "binomial")-> mod_log
predict(mod_log,newdata=test,type="response")->result_log
head(result_log)
range(result_log)

table(test1$Churn, result_log>0.1)

prediction(result_log,test1$Churn) -> predict_log
performance(predict_log,"acc")->acc
plot(acc)

table(test$Churn, result_log>0.4)
performance(predict_log,"tpr","fpr") -> roc_curve
plot(roc_curve)
plot(roc_curve, colorize=T)

auc.tmp <- performance(predict_log,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


##############################################################


#logistic regression model

glm(Churn ~ MonthlyCharges, data= customer_churn, family="binomial") ->log_mod1
summary(log_mod1)

predict(log_mod1,data.frame(MonthlyCharges=50),type="response")
predict(log_mod1,data.frame(MonthlyCharges=77),type="response")
predict(log_mod1,data.frame(MonthlyCharges=20:100),type="response")

glm(Churn~tenure, data= customer_churn, family="binomial") ->log_mod2
summary(log_mod2)

predict(log_mod2,data.frame(tenure=10),type="response")
predict(log_mod2,data.frame(tenure=70),type="response")
predict(log_mod2,data.frame(tenure=10:70),type="response")
