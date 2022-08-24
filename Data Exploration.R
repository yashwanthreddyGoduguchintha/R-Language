#set working directory
#getwd()

# move focus to source editor = Ctrl +1
# move focus to console = Ctrl +2
# clean console = Ctrl + l
# Run current line = Ctrl + Enter / Cmd + Enter
# comment multiple lines = ctrl + shift + c

setwd("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents")

#install package
#install.packages("name of the package")

library(readxl)
#df <- read_excel("name and extension of your file")


#read csv files
customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

customer_churn <- read.csv("customer_churn.csv")


#Read TXT files with read.table()
#If you have a .txt or a tab-delimited text file, you can import it with the R function read.table()

#df <- read.table("file_name.txt")

#-----------------Vectors------------------------------

num1 <- c(1,2,3,4,5)
num3 <- c(num1,6)
num2 <- 10:20
num4 <- c(10:20,34,39)

a <- seq(1,10)
b <- LETTERS[seq(1,10)]
c <- letters[seq(1,10)]

char1 <- c("a","b","c")
char2 <- c("this","is","sparta")

my_log1 <- c(TRUE,FALSE,TRUE,FALSE)
my_log2 <- c(T,F,T,F)

#to know what variables and functions are defined in your workspace
ls()
ls.str()

#deleting variables
rm(num1)
rm(num2,num3)
rm(list = ls())

#length of a vector
length(num1)
length(char1)

#data type 
class(char1)
class(num1)

#access elements of vectors
char2[1]
my_log2[c(1,3)]

# Ignore first element
num1[-1]

#select 1st to 3rd elements
num1[1:3]
# Invert sign of index to exclude instead of select
num1[-(1:3)]

#---------Missing values----------------------------------

a <- c(NA,6,7,8,NA,NA)
is.na(a) #whether any missing values are there in "a" or not

#subset non-missing values
x <- a[!is.na(a)]

#total number of missing values
sum(is.na(a))
#position of missing values
which(is.na(a))

mean(a)
mean(a,na.rm = T)

a <- ifelse(is.na(a),0,a)


#-----------LIST----------------------------------

my_list1 <- list(1,"a",TRUE)
my_list2 <- list(c(1,2),c("a","b"),c(TRUE,FALSE))
#print the lists
my_list1
my_list2

#access elements of a list
my_list1[[2]]
my_list2[[3]][2]

#Another list
Fruit_list <- list(Apple = 85, Banana = 45, Guava = 100)

Fruit_list
Fruit_list$Apple
#-----------------------------------------------

#Matrix

mat1<-matrix(c(1,2,3,4),nrow=2,byrow = T)
mat2<-matrix(c("a","b","c","d"),nrow=2,byrow = T)
mat3<-matrix(c(T,F,T,F),nrow=2,byrow = T)

mat1[1,]
mat1[,1]
mat1[2,1]

#-------------------------------------------------

#FACTOR  
my_data<-c("Male","Female","Female","Male")
my_data <- as.factor(my_data)
class(my_data)

#-------------------------------------------------
# DATAFRAME

df <- data.frame(Name=c("Sam","Bob"),Age=c(32,48),stringsAsFactors=F)

#strings as factors
customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv",stringsAsFactors = T)

#without strings as factors
customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

#reading particular set of columns
customer_churn <- read.csv("customer_churn.csv")[,1:10]
#strings as factors
customer_churn <- read.csv("customer_churn.csv",stringsAsFactors = T)

customer_churn <- read.csv("C:/Users/Abhishek/Desktop/Rsession/customer_churn.csv",stringsAsFactors = T)

#column names
names(customer_churn)
colnames(customer_churn)

#df <- data.frame(customer_churn[1:20,c(1,2,3,4)])

a <- customer_churn[,c(1,3,6)]
b <- customer_churn[,2:5]

d <- customer_churn[3,]
e <- customer_churn[c(3,5,7),]
f <- customer_churn[5:10,]

df1 <- customer_churn[4:8,2:5]
df2 <- customer_churn[50:60,c(2,3)]
df3 <- customer_churn[c(100:200,1000:2000),5:8]
df4 <- customer_churn[,c("gender","Partner")]


df1 <- customer_churn[,c(1,4,5,6,7,12)]
#remove columns
df1$DeviceProtection <- NULL
df2 <- df1[,-c(1,2)]

# rename a column
# names(df)[names(df) == "old_column_name"] <- "new_column_name"

names(customer_churn)[names(customer_churn) == "customerID"] <- "customer_ID"

names(customer_churn)

#----------------------------------------
#Decision Making

  if(10>20){
    print("10 is less than 20")
  }
#------------
  if(10<20){
    print("10 is less than 20")
  }
#-----------
  if(10>20){
    print("10 is less than 20")
  }else{
    print("10 is greater than 20")
  }
#----------------------------------------------------

sum(is.na(customer_churn))

colSums(is.na(customer_churn))

#selecting only missing values (rows)
miss_info <- customer_churn[is.na(customer_churn$TotalCharges),]

#extract rows with NA in any coulmns
miss_any_rows <- customer_churn[rowSums(is.na(customer_churn)) > 0 , ]

# ways of imputing missing value
customer_churn$TotalCharges <- ifelse(is.na(customer_churn$TotalCharges),0,customer_churn$TotalCharges)

#1
customer_churn$TotalCharges <- ifelse(is.na(customer_churn$TotalCharges),mean(customer_churn$TotalCharges,na.rm = T),customer_churn$TotalCharges)
#2
customer_churn$TotalCharges[is.na(customer_churn$TotalCharges)] <- mean(customer_churn$TotalCharges, na.rm = TRUE)

#remove missing rows from the dataframe
df <- na.omit(customer_churn)

#-----------------------------------------------

#Looping

# for(value in that) {
#   do this
# }

a<-1:9

for(i in a) {
  print(i*2)
}

b <- c(23,45,43,11,67,41)

for(i in 1:4){
  print(b[i])
}

#-------------------------------------------------------

i=1

while (i<=10) {
  print(i+2)
  i<-i+1
}
#----------------------------------------------------------
 
str(customer_churn)

head(customer_churn)
head(customer_churn,10)
head(customer_churn,2)

tail(customer_churn)
tail(customer_churn,3)
tail(customer_churn,10)

nrow(customer_churn)
ncol(customer_churn)

dim(customer_churn)

max(c(1,2,3,4,5))
max(customer_churn$MonthlyCharges)

min(c(1,2,3,4,5))
min(customer_churn$MonthlyCharges)

mean(c(1,2,3,4,5))
mean(customer_churn$MonthlyCharges)

summary(customer_churn$MonthlyCharges)
summary(customer_churn$gender)

#--------------------------------------------------------------

#rbind(): combining vectors or lists with equal number of columns. 
#All columns must be of the same data type.
#rbind() is the equivalent of stacking data sets on top of each other. 

data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student  
student
student <- rbind(student,c("Anne",75))
student

data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student1
data.frame(Name=c("Raj","Penny"),Marks=c(92,65)) -> student2
merge_student <- rbind(student1,student2)
merge_student

#-------------------------------------------------------------

#cbind(): combining vectors or lists with equal number of rows. 
#cbind() is horizontal combination of data. 

data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student
student

student <- cbind(student,Grade=c("A","C"))
student

data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student1
data.frame(Grade=c("A","C"),Subject=c("ML","ML")) -> student3
merge_student2 <- cbind(student1,student3)
merge_student2

#-------------------------------------------------------------

#Merging Dataframes

data.frame(Department=c("Tech","Analytics","Support"),Location=c("Chicago","New York","Boston")) -> Department2

data.frame(Name=c("Sam","Bob","Anne"),Salary=c(75000,105000,120000),Department=c("Tech","Sales","Analytics")) -> Employee

merge(Employee,Department2,by="Department") #inner join
merge(Employee,Department2,by="Department",all = T) #outer join
merge(Employee,Department2,by="Department",all.x  = T) #left join
merge(Employee,Department2,by="Department",all.y  = T) #right join

#x:data frame1.
#y:data frame2.
#by.x, by.y: The names of the columns that are common to both x and y. The default is to use the columns with common names between the two data frames.
#all, all.x, all.y:Logical values that specify the type of merge. The default value is all=FALSE (meaning that only the matching rows are returned).

#Inner Join: To keep only rows that match from the data frames, specify the argument all=FALSE.
#Outer Join:To keep all rows from both data frames, specify all=TRUE.
#Left Join:To include all the rows of your data frame x and only those from y that match, specify x=TRUE.
#Right Join:To include all the rows of your data frame y and only those from x that match, specify y=TRUE.

#---------------------------------------------------

#Own defined functions

# func_name <- function(argument) {
#   statements
# }

#my_function <- function() {}

Add_five <- function(x){
    x+5
  }

Add_five(3)
Add_five(c(10,15,20))

#############################

mul_fun <- function(a) {
  for(i in 1:a) {
    b <- i*2
    print(b)
  }
}

mul_fun(5)

#####################

fn <- function(x) { 
  s=sum(x)
  return(s)	
}

#Calling a function
v1=c(1,2,3,4,5)
v2=c(11,12,13,12,13)
fn(v1)
fn(v2)

###########################

#names to lowercase

names(customer_churn) <- tolower(names(customer_churn))

