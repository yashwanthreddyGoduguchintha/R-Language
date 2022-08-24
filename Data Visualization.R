
customer_churn <- read.csv("C:/Users/Abhishek/Desktop/Rsession/customer_churn.csv",stringsAsFactors = T)

customer_churn <- read.csv("customer_churn.csv",stringsAsFactors = T)

#load libraries
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# sapply(customer_churn, function(x) length(unique(x)))
# colSums(is.na(customer_churn))

#base
plot(customer_churn$Dependents)
plot(customer_churn$Dependents, col="coral")
plot(customer_churn$Dependents, col="coral",
     xlab="Dependents",ylab="Frequency",main="Dependents Plot")

#------
plot(customer_churn$PhoneService)  
plot(customer_churn$PhoneService,col="aquamarine4")
plot(customer_churn$PhoneService,col="aquamarine4",
     xlab="Phone Service",
     ylab="Frequency",
     main="Distribution of PhoneService")

#--------

plot(customer_churn$Contract)  
plot(customer_churn$Contract,col="palegreen4",
     xlab="Contract",ylab = "Frequency",
     main="Distribution of Contract")

#---------------------------

hist(customer_churn$tenure)  

hist(customer_churn$tenure,
     col="steelblue4",
     border="white",xlab="Tenure",
     main="Distribution of Tenure",
     xlim = c(0,80))

hist(customer_churn$tenure,
     col="steelblue4",
     border="white",xlab="Tenure",
     main="Distribution of Tenure",
     breaks=10)

#to specify the range of values in X and Y - axis, 
#we can use "xlim" and "ylim" parameters

#col to define color

# the width of each of the bar could be decided by "breaks"
#--------------------------

plot(x = customer_churn$tenure,y= customer_churn$TotalCharges,
     type="p",pch=20 ,
     xlab = "Tenure", ylab ="Total Charges", main = " Scatter Plot")

# x and y: the coordinates of points to plot
# type : the type of graph to create
# type=p: for points (by default)
# type=l: for lines
# type=b: for both; points are connected by a line
# pch : plotting character, i.e., symbol to use

#------------------- GGPLOT Visualizations -----------------------------------

#ggplot(data = data_set, mapping = aes(<mapping>)) + <geom_function>()
#geoms = Geometric elements

ggplot(data = customer_churn,aes(x=tenure)) + 
  geom_histogram() 

ggplot(data = customer_churn, aes(x=tenure)) + 
  geom_histogram(bins = 50)

ggplot(data = customer_churn, aes(x=tenure))+
  geom_histogram(bins=20,fill="palegreen4",col="black")

ggplot(customer_churn,aes(x=tenure)) + 
  geom_histogram(bins=20, fill= "steelblue4",col="white") +
  xlab("Tenure of customers") + ylab("Frequency") + 
  ggtitle("Distribution of Tenure of customers")

ggplot(customer_churn,aes(x=tenure)) + 
  geom_histogram(binwidth=5, boundary = 0,fill= "steelblue4",col="white") +   
  xlab("Tenure of customers") + ylab("Frequency") + 
  ggtitle("Distribution of Tenure of customers")

# binwidth - width of 5 and 
# boundary = 0 ensures that the binning starts at an integer value

ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+
  geom_histogram(bins=20,col="black")+
  xlab("Tenure")+ylab("Frequency") + 
  ggtitle("Distribution of Tenure")

ggplot(data = customer_churn, aes(x=tenure, fill=Churn))+
  geom_histogram(binwidth=5, boundary = 0,col="black")+
  xlab("Tenure")+ylab("Frequency") + 
  ggtitle("Distribution of Tenure")

ggplot(data = customer_churn, aes(x=tenure)) +
  geom_histogram(bins=20,fill="steelblue3",col="black",alpha=0.5) +
  stat_bin(bins=20, geom="text", color="black", aes(label=..count..), vjust = -0.5) +
  labs(title = "Tenure Distribution", x="Tenure",y="Frequency") +
  theme(plot.title = element_text(hjust=0.5,face="bold")) 

ggplot(data = customer_churn, aes(x=MonthlyCharges)) +
  geom_histogram(bins=20,fill="steelblue4",col="black",alpha=0.5) +
  stat_bin(bins=20, geom="text", color="black", aes(label=..count..), vjust = -0.3) +
  labs(title = "Monthly Charges Distribution", x="Monthly Charges",y="Frequency") +
  theme(plot.title = element_text(hjust=0.5,size=18)) 

#---------------------- Bar Plot ---------------------------------

ggplot(data = customer_churn,aes(x=Dependents)) + geom_bar()

ggplot(data = customer_churn,aes(x=Dependents)) + 
  geom_bar(fill="steelblue4") +
  labs(x="Dependents", y = "Count", title = "Dependents Distribution")

ggplot(data = customer_churn,
       aes(x=Dependents,fill=DeviceProtection))+geom_bar() + 
  labs(title = "Bar chart", x="Dependents",y="Frequency")

table(customer_churn$Dependents)
table(customer_churn$Dependents,customer_churn$DeviceProtection)

ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection)) +
  geom_bar(position="dodge",col="black") + 
  scale_fill_manual(values = c("steelblue3","palegreen3","orange3"))

#-------------------- Scatter Plot -----------------------------------------

#assign plot to a variable
scatter <- ggplot(customer_churn,aes(y=TotalCharges,x=tenure)) 

#draw the plot
#correct syntax for adding layers
scatter +
  geom_point()  

#this will not add the new layer
scatter
+ geom_point()

#adding transparency (alpha) to avoid overplotting
scatter +
  geom_point(alpha = 0.3)

#adding color
scatter +
  geom_point(alpha = 0.3, col = "steelblue4")

# ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure)) + 
#   geom_point(col="slateblue3")

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Partner)) + 
  geom_point() 

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=InternetService))+
  geom_point()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=InternetService))+
  geom_point()+
  scale_color_manual(values = c("steelblue4","sienna4","goldenrod"))+
  ggtitle("Relationship between tenure and total charges")

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, 
                                 col=OnlineSecurity))+
  geom_point()

#------------------------ BOXPLOT ------------------------


ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+
  geom_boxplot()  

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+
  geom_boxplot(fill="yellowgreen")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+
  geom_boxplot()

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod))+
  geom_boxplot()

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=PaperlessBilling))+
  geom_boxplot()

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=""))+
  geom_boxplot()

summary(customer_churn$MonthlyCharges)

#------------------------------------

ggplot(data = customer_churn,aes(x=tenure,fill=InternetService))+
  geom_histogram(bins = 10,col="black") +
  facet_grid(~InternetService) + 
  ggtitle("Tenure Histogram- Internet Service Wise") +
  xlab("Tenure") + ylab("Count") + theme_bw()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Contract)) + 
  geom_point() + 
  facet_grid(~Contract) + 
  xlab("Tenure") + ylab("Total Charges") +
  ggtitle("Total Charges vs. Tenure") + theme_bw()

#----------------------------

  