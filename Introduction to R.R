read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")->churn
churn1 <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")
churn2 = read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")
----------------
churn1$MonthlyCharges[1]-1 -> churn1$MothlyCharges[1]
head(churn1)

churn1$TotalCharges[2]+1 -> churn1$TotalCharges[2]
head(churn1)

churn1$TotalCharges[9]*0.9 -> churn1$TotalCharges[9]
head(churn1)

churn1$MonthlyCharges[3]/2-> churn1$MothlyCharges[3]
head(churn1)

#----------------------------
churn1$tenure>60 -> c_tenure
head(c_tenure)

subset(churn1,c_tenure=T)->c_tenure
head(c_tenure)

churn1$MonthlyCharges<30-> c_mon
head(c_mon)

#---------------------
churn1$gender == "Male" & churn1$SeniorCitizen==1 -> c_ms
head(c_ms)



churn1$InternetService == "DSL" | churn1$InternetService =="Fiber optic" -> c_internet
head(c_internet)
subset(churn1, c_internet ==T) -> c_internet
head(c_internet)

churn1$SeniorCitizen==!1 -> c_not_senior
head(c_not_senior)
subset(churn1, c_not_senior ==T) -> c_not_senior
head(c_not_senior)

##############################

