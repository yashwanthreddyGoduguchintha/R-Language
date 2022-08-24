
customer_churn <- read.csv("C:/Users/Abhishek/Desktop/Rsession/customer_churn.csv",stringsAsFactors = T)

#install.packages("dplyr")
library(dplyr)
library(fastDummies)
library(caret)
library(corrplot)

df1 <- customer_churn[,c(2,4,5,9)]

apply(df1, 2 , function(x) length(unique(x)))

df2 <- dummy_cols(df1)

df3 <- dummy_cols(df1, remove_first_dummy = TRUE)

df4 <- dummy_cols(df1, remove_first_dummy = TRUE, remove_selected_columns=T)

#A dummy column is one which has a value of one when a categorical event occurs and a zero 
#when it doesnt occur. In most cases this is a feature of the event/person/object being described.

#In the function dummy_cols, the names of these new columns are concatenated to the original 
#column and separated by an underscore.

#The final option for dummy_cols() is remove_first_dummy which by default is FALSE. 
#If TRUE, it removes the first dummy variable created from each column. 

mtcars <- mtcars

cor_mat <- cor(mtcars)
corrplot(cor_mat)
corrplot(cor_mat,"number")

high_corr <- findCorrelation(cor_mat,cutoff = 0.8)
high_corr
high_corr <- findCorrelation(cor_mat,cutoff = 0.8,names = T)
high_corr

mtcars <- mtcars[,-c(1,2,3)]




