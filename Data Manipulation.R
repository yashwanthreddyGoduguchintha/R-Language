
#Read the file

customer_churn <- read.csv("C:/Users/Abhishek/Desktop/sessions/codes/customer_churn.csv",stringsAsFactors = T)

customer_churn <- read.csv("customer_churn.csv",stringsAsFactors = T)

#install.packages("dplyr")
library(dplyr)

mat1<-matrix(c(1,2,3,4),nrow=2)

mat1

apply(mat1,1,sum)
apply(mat1,2,sum)

#Margin = 1 indicates rows; 2 indicates columns ; c(1,2) indicates both rows and columns

#matrix m
m <- matrix(c(1:10,11:20,31:40),nrow = 10, ncol = 3)
#apply a function column wise
apply(m, 2 , function(x) length(x) - 1)

#number of missing values in a data frame

apply(customer_churn, 2 , function(x) sum(is.na(x)))
colSums(is.na(customer_churn))

apply(customer_churn, 2 , function(x) length(unique(x)))

apply(customer_churn[,c(1:10)],2, function(x) length(unique(x)))

#----------------------------------------------------

my_list <- list(a=c(1,2,3),b=c(2,3,4),c=c(3,4,5))  
lapply(my_list, mean)

#sapply() will try to simplify the output of lapply() if possible
sapply(my_list, mean)

#class of all columns
sapply(customer_churn,class)
table(sapply(customer_churn,class))

tapply(customer_churn$tenure, customer_churn$Partner, mean)
tapply(customer_churn$MonthlyCharges, customer_churn$Churn, mean)

#-------------------------------

#multivariate apply
x<-c(1:5) 
b<-c(6:10)

mapply(sum, x, b)


#------------------------------------------------------

#The first argument is a data frame
#the subsequent arguments -> what to do with data frame using the variables
#The result is also a data frame


select(customer_churn,2) ->c_2
select(customer_churn,6) ->c_6
select(customer_churn,1,4,7,12) ->c_bunch
select(customer_churn,5:10) ->c_5_10
select(customer_churn,gender) ->c_gender
select(customer_churn,gender,Partner,tenure) ->c_gpt
select(customer_churn,gender:Contract) ->c_gender_contract

select(customer_churn,starts_with("Stream")) -> c_stream
select(customer_churn,ends_with("Charges")) -> c_charges
select(customer_churn,contains("ing")) -> c_ing

#-----------------------------------------------------

filter(customer_churn,gender=="Female") -> c_female  
filter(customer_churn, MonthlyCharges>100) -> c_high_paying_customers

filter(customer_churn,gender=="Female" & MonthlyCharges>100)->c_high_female
filter(customer_churn,gender=="Female" , MonthlyCharges>100)->c_high_female1

filter(customer_churn,StreamingTV=="Yes" & StreamingMovies=="Yes")->c_stream
filter(customer_churn,tenure>50 & InternetService == "DSL" & Contract=="One year") -> c_tic
filter(customer_churn,PaymentMethod=="Electronic check" | PaymentMethod=="Mailed check" ) -> c_pay
filter(customer_churn,(Contract=="One year" | Contract=="Two year") & gender=="Female"  )->c_con_gen
filter(customer_churn,(InternetService == "DSL" | InternetService == "Fiber optic") &(tenure>50 & MonthlyCharges >100))->c_complicated

#base
df_contract <- customer_churn[customer_churn$Contract %in% c("One year","Two year"), ]
#dplyr
df_contract2 <- filter(customer_churn,Contract %in% c("One year","Two year"))

df_contract3 <- filter(customer_churn,!Contract %in% c("One year","Two year"))

# Typical Comparison Operators to filter rows:
# ==
# !=
# > 
# <
# >=
# <=
# %in%

#Filter NA's

na_df <- filter(customer_churn,is.na(TotalCharges))

#---------------------------------------------------

#mutate(customer_churn, Age= ifelse(SeniorCitizen==0, sample(x=16:55),sample(x=56:100))) -> customer_churn

customer_churn <- mutate(customer_churn,customer_category=ifelse(MonthlyCharges < 45, "Low_Paying", 
                                          ifelse(MonthlyCharges < 90,"Medium_paying","High_paying")))

class(customer_churn$customer_category)
customer_churn$customer_category <- as.factor(customer_churn$customer_category)

#dplyr
customer_churn <- mutate(customer_churn,ID=1:7043,ID2 = 2)
#base
customer_churn$ID3 <- 1:nrow(customer_churn)

customer_churn$churn_new <- ifelse(customer_churn$Churn=="Yes",1,0)


##-------------------------------------------------------

sample_n(customer_churn,10)->random_10
sample_n(customer_churn,100)->random_100

sample_frac(customer_churn,0.1)->random_10percent
sample_frac(customer_churn,0.5)->random_50percent

#--------------------------------------------------------

summarise(customer_churn,mean_tenure=mean(tenure))  
summarise(customer_churn,mean_MC=mean(MonthlyCharges))
summarise(customer_churn,mean_TC=mean(TotalCharges,na.rm=T))

#base
round(mean(customer_churn$tenure),2)

#---------------------------------------------------

df1 <- summarise(group_by(customer_churn,InternetService),mean_tenure=mean(tenure))  
df2 <- summarise(group_by(customer_churn,Partner),mean_MonthlyCharges=mean(MonthlyCharges))
df3 <- summarise(group_by(customer_churn,InternetService),mean_MonthlyCharges=mean(MonthlyCharges))

#----------------------------------------------------

new <- arrange(customer_churn, MonthlyCharges)
new1 <- arrange(customer_churn, desc(MonthlyCharges))

#-------------- remove duplicate rows ----------------

#based on all the columns
cust <- customer_churn %>% distinct()

#based on a particular column
# .keep_all is used to keep all the variables in the data

#cust1 <- customer_churn %>% distinct(column_name, .keep_all = T)

#------------------------------------------------------
#dplyr
customer_churn %>% select(1:5) -> c_15 
dim(c_15)
#base
c_55 <- customer_churn[,1:5]
dim(c_55)

#select rows 
#base
customer_churn[1:10,] -> df5
#dplyr
customer_churn %>% slice(1:10) -> df_110

customer_churn %>% slice_head(n=3) -> first_3
customer_churn %>% slice_tail(n=3) -> last_3

#rename columns
customer_churn %>% rename(customer_ID =customerID) -> customer_churn

customer_churn %>% 
  select(1:5) %>% 
  filter( gender =="Male") -> c_15_male

customer_churn %>% 
  filter(InternetService=="DSL") %>% 
  group_by(gender) %>% 
  summarise(mean_mc = mean(MonthlyCharges)) -> df7

customer_churn %>% 
  group_by(InternetService) %>% 
  summarise(mean_mc = mean(MonthlyCharges)) -> df

customer_churn %>% 
  group_by(PaymentMethod) %>% 
  summarise(mean_tenure = mean(tenure), count = n()) -> new_df

customer_churn %>% 
  group_by(PaymentMethod) %>% 
  summarise(mean_tenure=mean(tenure)) %>% 
  arrange(desc(PaymentMethod)) -> df8

customer_churn %>% 
  select(1,2,10:21) %>% 
  filter(Contract=="One year" | Contract=="Two year") %>% 
  arrange(Contract) -> c_contract

customer_churn %>% 
  filter(PaperlessBilling=="No") %>% 
  group_by(TechSupport) %>%
  summarise(mean_tenure=mean(tenure)) -> df9

#------------------------------------------------------

#base

x <- c(1,2,2,4,5,6,6,6,7,8,1,2,3,3,3)

#1
duplicated(x)
x[duplicated(x)]
y <- x[!duplicated(x)]

#dataframe column wise
#df <- df[!duplicated(df$column_name),]

#2
unique(x)

##################

#replace all NA values with 0
customer_churn <- customer_churn %>% replace(is.na(.),0)
sum(is.na(customer_churn))



