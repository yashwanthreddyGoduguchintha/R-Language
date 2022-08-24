# Clear Global environment
rm(list=ls())

customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

library(dplyr)

customer_churn %>% select("tenure","MonthlyCharges","TotalCharges")-> customer_features
head(customer_features)

kmeans(customer_features$MonthlyCharges,3)-> k_month
cbind(Month=customer_features$MonthlyCharges, Clusters=k_month$cluster) ->month_group
head(month_group)

as.data.frame(month_group)->month_group
month_group %>% filter(Clusters==1)-> month_group_1
month_group %>% filter(Clusters==2)-> month_group_2
month_group %>% filter(Clusters==3)-> month_group_3

head(month_group_1)
head(month_group_2)
head(month_group_3)

##############


kmeans(customer_features$tenure,3)->tenure_group
tenure_group


cbind(Tenure=customer_features$tenure, Clusters=tenure_group$cluster) ->tenure_group_data
head(tenure_group_data)

as.data.frame(tenure_group_data)->tenure_group_data
tenure_group_data %>% filter(Clusters==1)-> tenure_group_data1
tenure_group_data %>% filter(Clusters==2)-> tenure_group_data2
tenure_group_data %>% filter(Clusters==3)-> tenure_group_data3

head(tenure_group_data1)
head(tenure_group_data2)
head(tenure_group_data3)

#--

na.omit(customer_features)->customer_features 
kmeans(customer_features$TotalCharges,3)->k_total

cbind(Total=customer_features$TotalCharges, Clusters=k_total$cluster) ->total_group
head(total_group)

as.data.frame(total_group)->total_group
total_group %>% filter(Clusters==1)-> total_group1
total_group %>% filter(Clusters==2)-> total_group2
total_group %>% filter(Clusters==3)-> total_group3

head(total_group1)
head(total_group2)
head(total_group3)

#######################################################
