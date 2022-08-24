rm(list=ls())

library(dplyr)

#Consider mtacrs data of R-datasets
data(mtcars)
mydata <- data.frame(mtcars)

sum(is.na(mydata))
#mydata <- na.omit(mydata) 
# listwise deletion of missing

summary(mydata)
str(mydata)

mydata <- scale(mydata) # standardize variables 
summary(mydata)
apply(mydata,2,sd)

# so that all the features contribute equally to the result

###-------------------------    Hierarchical Clustering     ------------------------###

# Ward's method 
d <- dist(mydata, 
          method = "euclidean") # distance matrix
#d
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram

groups <- cutree(fit, k=4) # cut tree into 4 clusters
groups

# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red") 

mtcars$cluster <- groups

###-------------------------    K- means Clustering     ------------------------###


# Determine number of clusters by considering the withinness measure
wss <- 0
for(i in 1:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}


#Scree Plot
plot(x = 1:15, y = wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main = "Scree plot") 


# K-Means Cluster Analysis with k = 5
fit <- kmeans(mydata, 5) # 5 cluster solution

str(fit)
#study the model and metrics

#With-in sum of squares in each cluster 
fit$withinss 

sum(fit$withinss) 
fit$tot.withinss

#To check cluster number of each row in data
fit$cluster

#Cluster Centers 
fit$centers 

# get cluster means
#aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster label to the actual data frame
mydata <- data.frame(mydata,fit$cluster) 

mtcars1 <- data.frame(mtcars,fit$cluster) 

#append cluster IDs to actual mtcars data
mtcars_new <- cbind(mtcars,fit$cluster)
colnames(mtcars_new)[12] <- "ClusterID"
mtcars_new$ClusterID <- as.factor(mtcars_new$ClusterID)


summ_kmeans <- mtcars_new %>% 
                group_by(ClusterID) %>% 
                summarise(mean_mpg = mean(mpg), mean_disp = mean(disp), 
                mean_hp = mean(hp), mean_weight = mean(wt),
                count = n())



