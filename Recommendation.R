library(recommenderlab)
library(ggplot2)

data("MovieLense")
#MovieLense@data
#class(MovieLense@data)

as.vector(MovieLense@data) -> vector_ratings
unique(vector_ratings)

vector_ratings[vector_ratings!=0]->vector_ratings
table(vector_ratings)->table_ratings
table_ratings

qplot(vector_ratings)

colCounts(MovieLense) -> views_per_movie
views_per_movie

data.frame(MovieName=names(views_per_movie),Views=views_per_movie)->table_views
head(table_views)

library(dplyr)
library(ggplot2)

table_views %>% arrange(desc(Views))->table_views
head(table_views)

ggplot(table_views[1:5,],aes(x=MovieName,y=Views))+geom_bar(stat = "identity")
ggplot(table_views[1:5,],aes(x=MovieName,y=Views, fill=MovieName))+geom_bar(stat = "identity")
ggplot(table_views[1:5,],aes(x=MovieName,y=Views, fill=Views))+geom_bar(stat = "identity")


colMeans(MovieLense)->average_rating
head(average_rating)

qplot(average_rating)+ggtitle("Distribution of average rating")

MovieLense[rowCounts(MovieLense)>50, colCounts(MovieLense)>100]-> rating_movies  

sample(x=c(T,F),size=nrow(rating_movies),replace = T, prob = c(0.9,0.2)) ->split_movie
rating_movies[split_movie,]->recc_train
rating_movies[!split_movie,]->recc_test

Recommender(data = recc_train,method="UBCF")->recc_model_ubcf
n_recommended_ubcf<-6

predict(object=recc_model_ubcf,newdata=recc_test,n=n_recommended_ubcf)->recc_predicted_ubcf
recc_predicted_ubcf

recc_predicted_ubcf@items[[1]]->user1_movie_numbers
user1_movie_numbers
recc_predicted_ubcf@itemLabels[user1_movie_numbers]

recc_predicted_ubcf@items[[5]]->user5_movie_numbers
recc_predicted_ubcf@itemLabels[user5_movie_numbers]

Recommender(data = recc_train,method="IBCF")->recc_model_ibcf
n_recommended_ibcf<-6
predict(object=recc_model_ibcf,newdata=recc_test,n=n_recommended_ibcf)->recc_predicted_ibcf


recc_predicted_ibcf@items[[1]]->user1_movie_numbers
recc_predicted_ibcf@itemLabels[user1_movie_numbers]

recc_predicted_ibcf@items[[5]]->user5_movie_numbers
recc_predicted_ibcf@itemLabels[user5_movie_numbers]

