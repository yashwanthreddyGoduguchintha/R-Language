# Clear Global environment
rm(list=ls())

library(ggplot2)
library(dplyr) 
library(splitstackshape)
library(corrplot)

#strings as factors = TRUE

customer_churn <- read.csv("C:/Users/Abhishek/OneDrive/Desktop/sessions/codes/customer_churn.csv", stringsAsFactors = T)

customer_churn <- read.csv("customer_churn.csv",stringsAsFactors = T)

#------------------------------------------------------------

#sample
sample(1:100,3)
sample(customer_churn$customerID,5)
sample(customer_churn$customerID,20)


die <- 1:6
dice <- sample(die,size = 2, replace = T)
sum(dice)


roll <- function() {
  die <- 1:6
  dice <- sample(die,size = 2, replace = T)
  sum(dice)
}

roll()

# Stratified Sampling -------------------------------------

df <- data.frame(table(customer_churn$Churn))

ggplot(df,aes(x=Var1,y=Freq,fill=Var1)) + geom_bar(stat="identity")+ 
  geom_text(aes(label=Freq),vjust = 1.6)+
  xlab("Churn") + ggtitle("Churn Distribution") + theme_bw()

#using stratified function

sub_df <- stratified(customer_churn, "Churn", 1000)

sub_df2 <- stratified(customer_churn, "Churn", .1)

#using dplyr package

sub_df3 <- customer_churn %>%
  group_by(Churn) %>%
  sample_n(1000)

sub_df4 <- customer_churn %>%
  group_by(Churn) %>%
  sample_frac(.1)

#--------------------------------------------------------------

#mean

mean(customer_churn$MonthlyCharges)
mean(customer_churn$TotalCharges,na.rm = T)

#median
median(customer_churn$MonthlyCharges)

# Mode

#method1

gmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#method2

# names function on the table returns the names of the values represented in the table. 
# choose the name which corresponds to the value which has been counted most. 

mymode <- function(vec){
  mt <- table(vec)
  names(mt)[mt == max(mt)]
}

xyz <- c(1,1,1,1,1,4,5,6,2,2,3,4,1,1,1,1,1)

mymode(xyz)
gmode(xyz)

gmode(customer_churn$PhoneService)
mymode(customer_churn$PhoneService)

#-----------------------------------------------------------------

#range
range(c(1,2,3,4,5))
range(customer_churn$MonthlyCharges)


# absolute frequencies
df1 <- data.frame(table(customer_churn$gender))
df2 <- data.frame(table(customer_churn$InternetService))

# relative frequencies
df3 <- data.frame(table(customer_churn$InternetService)/length(customer_churn$InternetService))

# relative frequencies in percent
(table(customer_churn$InternetService)/length(customer_churn$InternetService)) * 100
#2
prop.table(table(customer_churn$InternetService)) * 100

#barplot
barplot(table(customer_churn$gender))
barplot(table(customer_churn$gender)/length(customer_churn$gender))

#---------------------------------------------------------------

# Quantiles are a generalization of the idea of the median. 
# The median is the value which splits the data into two equal parts. 
# Similarly, a quantile partitions the data into other proportions. 
# For example, a 25 %-quantile splits the data into two parts such that at 
# least 25 % of the values are less than or equal to the quantile and at least 
# 75 % of the values are greater than or equal to the quantile.

summary(customer_churn$tenure)

quantile(customer_churn$tenure)
quantile(customer_churn$tenure, probs=c(0,0.25,0.5,0.75,1))

#The interquartile range is defined as the difference between the 75th and 25th quartiles
# is the range of the middle 50% of the data

IQR(customer_churn$tenure)

#variance and standard deviation

var(customer_churn$tenure)
sd(customer_churn$tenure)

customer_churn$month <- 5

var(customer_churn$month)
sd(customer_churn$month)


#----------------------------------------------------------------

# Scaling and Normalization

#we scale our data before employing a distance based algorithm so that all the 
#features contribute equally to the result.

# Standardization is a scaling technique where the values are centered around the mean with 
# a unit standard deviation. 
# This means that the mean of the attribute becomes zero and the resultant distribution 
# has a unit standard deviation.

customer_churn$sc_mon <-  scale(customer_churn$MonthlyCharges)

mean(customer_churn$MonthlyCharges)
mean(customer_churn$sc_mon)

sd(customer_churn$MonthlyCharges)
sd(customer_churn$sc_mon)

#Normalization

#Normalization is a scaling technique in which values are shifted and rescaled so that they end up 
#ranging between 0 and 1. 
#It is also known as Min-Max scaling.

norml <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

customer_churn$norm_mon <-  norml(customer_churn$MonthlyCharges)

#-----------------------------------------------------------------

?mtcars

# There are seven visualization methods (parameter method) in corrplot package, 
# "circle", "square", "ellipse", "number", "shade", "color", "pie".
# 
# Positive correlations are displayed in blue and negative correlations in red color. 
# Color intensity and the size of the circle are proportional to the correlation coefficients.

#correlation matrix
M <- cor(mtcars)

corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "ellipse")
corrplot(M, method = "number") # Display the correlation coefficient

#corrplot.mixed() is a wrapped function for mixed visualization style.

corrplot.mixed(M)

corrplot.mixed(M, lower.col = "black", number.cex = .8)

# Layout
# There are three layout types (parameter type):
#   
# "full" (default) : display full correlation matrix
# "upper" : display upper triangular of the correlation matrix
# "lower" : display lower triangular of the correlation matrix

corrplot(M, type = "upper")
corrplot(M, type = "lower")

###########################


sub1 <- customer_churn %>% select(tenure,MonthlyCharges,TotalCharges )
sub1 <- na.omit(sub1)

cor_sub1 <- cor(sub1)
cor_sub1

corrplot(cor_sub1)
corrplot.mixed(cor_sub1)

