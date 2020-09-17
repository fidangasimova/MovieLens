#######################
# PROJECT MovieLens
######################
library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
#IMPORTANT: Make sure you do NOT use the validation set (the final hold-out test set) to train your algorithm. 
#The validation set (the final hold-out test set) should ONLY be used to test your final algorithm. 
#You should split the edx data into a training and test set or use cross-validation.


################################
# Create edx set, validation set
################################

# Code to generate edx data set

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                 col.names = c("userId", "movieId", "rating", "timestamp"))
#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies, stringsAsFactors=TRUE) %>% mutate(movieId = as.numeric(levels(movieId))[movieId], 
                                                                  title = as.character(title),
                                                                  genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

#################################################
# Validation set will be 10% of MovieLens data  #
#################################################

# Code to generate the validation set
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(ratings, movies, test_index, temp, movielens, removed)

#########################################
# Validation Data processing 
#validation <-validation[1:100000,]
#Sort data by movieId 
validation<-arrange(validation, by=movieId)

# Split title and year into separate columns
validation<-extract(validation, title, c("title", "release_year"), "(.*)\\((\\d{4})\\)$")

# Convert year character into to an integer
validation<-transform(validation, release_year = as.numeric(release_year))

# Transform the rating timestamp to datetime year
validation<-transform(validation, rating_year = year(as_datetime(timestamp)))

#Split genres into single columns per genre
validation_genre<-separate_rows(validation, genres, sep = "\\|")
#####################################################
# REMOVE ME LATER

#edx_1 <- edx[1:100000,]

############################
#  edx Data processing     #
###########################

# edx summary
summary(edx)

# Number of variables
ncol(edx)

# Number of observarions
nrow(edx)

# Sort data by movieId 
edx<-arrange(edx, by=movieId)

# Split title and year into separate columns
edx<-extract(edx, title, c("title", "release_year"), "(.*)\\((\\d{4})\\)$")

# Convert year character into to an integer
edx<-transform(edx, release_year = as.numeric(release_year))

# Transform the rating timestamp to datetime year
#edx<-transform(edx, rating_time = round_date(as_datetime(timestamp), unit="month"))
edx<-transform(edx, rating_year = year(as_datetime(timestamp))) 
#remove variable rating_time
#edx<-within(edx, rm(rating_time))

#Create an age_years variable that represents time in years between release time ant time when rating was given
edx<-edx%>%mutate(age_years=abs(rating_year-release_year))
  
# Split genres into single columns per genre
edx_genre<-separate_rows(edx, genres, sep = "\\|")


# Check missing values
sum(is.na(edx))

head(edx,10)

##############################################
#   Visualization and Descriptive statistics #
#############################################
library(ggplot2)

#######################
# USERS and MOVIES    #
#######################

# Top 10 of movies with greatest number of ratings
edx%>%group_by(movieId, userId)%>%mutate(count=n())%>%
  top_n(10, count)%>%arrange(desc(count))%>%select(title, count)

edx%>%mutate(movieId=unique(movieId))%>%group_by(title)%>%mutate(count=n())%>%
  top_n(10)%>%arrange(desc(count))%>%select(title=unique(title))

# Number of different users and movies in edx
edx %>%summarize(n_users = n_distinct(userId),
                 n_movies = n_distinct(movieId))


# Distribution of number of ratings among Users and Movies.
#Change the format from scientific to numerical
options(scipen = 999)
p_1<-edx %>% 
  count(movieId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("blue"), col=I("red"), alpha=.2) +
  scale_x_continuous(trans = "log10") + 
  geom_density(col = "black", lwd=0.2)+
  ggtitle("Number of Movies vs. Proportion of Ratingscount") +
  labs(x="n Movies", y="Proportions of n Ratings")+
  theme(plot.title = element_text(hjust = 0.5))

p_2<-edx %>% 
  count(userId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("grey"), col=I("red"), alpha=.2) +
  scale_x_continuous(trans = "log10") + 
  geom_density(col = "black", lwd=0.2)+
  ggtitle("Number of Users vs Proportion of Ratingscount") +
  labs(x="n Users" , y="Proportion of n Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(p_1, p_2, nrow = 1)

############################
# Distribution of Ratings  #
###########################

# Top 10 most given ratings 
edx%>%group_by(rating)%>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Number of ratings given each rating categories from 0.5 to 5 stars
edx %>% ggplot(aes(rating)) + geom_bar(fill=I("blue"), col=I("grey"), alpha=.2)+
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+
  ggtitle("Number of Ratings") +
  labs(x="Ratings", y="n of Ratings")+
  theme(plot.title = element_text(hjust = 0.5))
# As we can see 3 and 4 are the most given ratings   

##########
# GENRE  #
##########

# Number of movies in different genres
edx_genre%>%group_by(genres) %>%
  summarize(count = n())%>% top_n(10)%>%arrange(desc(count))

# Number of different genres
edx_genre%>%summarize(genre = n_distinct(genres))

# Number of ratings in each of the genre
edx_genre %>% summarize(genre = n_distinct(genres))%>%group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Number of ratings for each genre
edx_genre%>%group_by(genres)%>%summarize(count=n())%>%
ggplot(aes(x= reorder(genres, count), y=count))+
  geom_bar(stat='identity', color="blue")+
  ggtitle("Number of Ratings for each Genre") +
  coord_flip(y=c(0, 4000000))+
  labs(x="Genre", y="n Ratings")+
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  theme(plot.title = element_text(hjust = 0.5))
# Drama, Comedy and Action genre received the most ratings 

# Number of ratings over years when rating was given for each genre
# Change the format from scientific to normal
options(scipen = 999)
edx_genre%>%na.omit() %>% filter(rating>4)%>% mutate(genres = as.factor(genres))%>%
  group_by(genres,rating_year)%>%
  summarise(count=n())%>%filter(count > 1000)%>%
  arrange(desc(count))%>%
  ggplot(aes(rating_year, count, color = genres))+
  geom_line()+
  ggtitle("Number of Ratings over Years of Rating") +
  labs(x="Rating Years", y="n of Ratings") +
  scale_x_continuous(limits=c(1995, 2008, by=1))+
  theme(plot.title = element_text(hjust = 0.5))


rm(p_1, p_2,test_index)
  
#Drama remained a popular choice over years by receiving the most number of ratings from 3.5 and above. 
# Due to limits of memory only top 10 of genres with highest number of ratings will be represented
# Boxplot of ratings for each genre
edx_genre%>%group_by(genres)%>%
  filter(genres  %in% c("Drama”, “Comedy", "Action", "Thriller", "Adventure"))%>%
  mutate(count=n(), avg_rating=mean(rating), se_rating=sd(rating)/sqrt(count))%>%
  ggplot(aes(x=reorder(genres, avg_rating), y=avg_rating, ymin = avg_rating-2*se_rating, ymax = avg_rating+2*se_rating), width=0.1, size=1.3 )+
  geom_point()+geom_errorbar()+
  ggtitle("Error Bar by Genre") +
  labs(x="Genre" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))

###########
# TIME   #
##########

# Average rating over years when rating was given
edx %>% group_by(rating_year) %>%summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_year, avg_rating)) +
  geom_point() +geom_smooth()+
  scale_y_continuous(breaks=seq(0, 5, by= 0.5))+
  scale_x_continuous(breaks=seq(1995, 2018, by= 5))+
  ggtitle("Average Rating over Rating Year") +
  labs(x="Rating Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))

# Highest number of given ratings 
edx%>%group_by(rating, rating_year)%>%
  summarise(n=n(), max=max(n))%>%
  arrange(desc(max))
# Need to save
# In more detail to compare release with rating year:
# Average rating over release vs. rating years
p_3<-edx%>%group_by(release_year)%>%
  mutate(count=n(), avg_rating=mean(rating), se_rating=sd(rating)/sqrt(count))%>%
  ggplot(aes(release_year, avg_rating, ymin=avg_rating-2*se_rating, ymax=avg_rating+2*se_rating),width=0.1, size=1.3 )+
  geom_line()+geom_point()+geom_errorbar()+
  ggtitle("Average Rating over Release Years with Error Bar") +
  labs(x="Release Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))

p_4<- edx%>%group_by(rating_year)%>%
  mutate(count=n(), avg_rating=mean(rating), se_rating=sd(rating)/sqrt(count))%>%
  ggplot(aes(rating_year, avg_rating, ymin=avg_rating-2*se_rating, ymax=avg_rating+2*se_rating), width=0.1, size=1.3 )+
  geom_line()+geom_point()+geom_errorbar()+
  #scale_x_continuous(breaks=seq(1930, 2018, by= 1))+
  ggtitle("Average Rating over Rating Years with Error Bar") +
  labs(x="Rating Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))
gridExtra::grid.arrange(p_3, p_4, nrow = 1)

# Average rating over the age of movies
edx%>%group_by(age_years) %>% summarize(avg_rating = mean(rating))%>%arrange(desc(avg_rating))%>%
  ggplot(aes(age_years, avg_rating)) +
  scale_x_continuous(breaks=seq(0, 100, by= 10))+
  geom_point() + geom_smooth()+
  ggtitle("Average Rating over Movie Age") +
  labs(x="Age of Movie" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))
#The average rating went up with time past the release but dropped again for movies older then 70 years
#############
# Method    #
#############
# Generate training and test sets of edx
library(caret)
set.seed(111)
test_index <- createDataPartition(y=edx$rating, times = 1, p = 0.2, list = FALSE)
test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]  

# To make sure we don’t include users and movies in the test set that do not appear in the training set, 
# remove these entries using the semi_join function:
test_set <- test_set %>% 
semi_join(train_set, by = "movieId") %>%
semi_join(train_set, by = "userId")

# For the evaluation of the model RMSE function will be used
RMSE <- function(true_ratings, predicted_ratings)
{ sqrt(mean((true_ratings - predicted_ratings)^2))}

# (M1) Model with movie effects
# least squares estimate b_m is just the average of Yu,i − μˆ for each movie i.
#Average rating 
mu <- mean(train_set$rating)

#Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))

# Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  mutate(pred = mu + b_m) %>%
  pull(pred)

# Calculate RMSE of Model 1
rmes_1<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- data_frame(Model = "Movie Effect", RMSE = rmes_1)
rmse_results%>% knitr::kable()

# (M2) Model with movie and user effect
# Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
# and b_m and estimating b_u as the average of yu,i − μˆ − bi
mu <- mean(train_set$rating)

#Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
#Estimate user effect b_u
user_ef <- train_set %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
#Estimate predicted rating
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef, by='userId') %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

#Calculate RMSE of Model 2
rmse_2<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie and User Effect Model",
                                     RMSE = rmse_2))

rmse_results%>% knitr::kable()

# (M3) Model with movie, user and genre effect
# Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
# and b_m and estimating b_u as the average of yu,i − μˆ − bi

# Before adding the genre effect split genres into single columns per genre
train_set_g<-separate_rows(train_set, genres, sep = "\\|")
test_set_g<-separate_rows(test_set, genres, sep = "\\|")

#Average rating
mu <- mean(train_set_g$rating)

#Estimate movie effect b_m
movie_ef <- train_set_g %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
#Estimate user effect b_u
user_ef <- train_set_g %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
#Estimate genre effect b_g
genre_ef<-train_set_g%>%
  group_by(genres)%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef, by="userId")%>%
  summarize(b_g = mean(rating - mu - b_m- b_u))
#Estimate predicted ratings
predicted_ratings <- test_set_g %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef, by='userId') %>%
  left_join(genre_ef, by='genres') %>%
  mutate(pred = mu + b_m + b_u + b_g) %>%
  pull(pred)
#Calculate RMSE of Model 3
rmse_3<-RMSE(predicted_ratings, test_set_g$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie, User and Genre Effect Model",
                                     RMSE = rmse_3))

rmse_results%>% knitr::kable()

# (M4) Model with movie, user, genre and age effect
# Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
# and b_m and estimating b_u as the average of yu,i − μˆ − bi

# Average rating
mu <- mean(train_set_g$rating)
#Estimate movie effect b_m
movie_ef <- train_set_g %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
#Estimate user effect b_u
user_ef <- train_set_g %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
#Estimate genre effect b_g
genre_ef<-train_set_g%>%
  group_by(genres)%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef, by="userId")%>%
  summarize(b_g = mean(rating - mu - b_m- b_u))
#Estimate age effect b_a
age_ef<-train_set_g%>%
  group_by(age_years)%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef, by="userId")%>%
  left_join(genre_ef, by="genres") %>%
  summarize(b_a = mean(rating - mu - b_m- b_u - b_g))
#Estimate predicted ratings
predicted_ratings <- test_set_g %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef, by='userId') %>%
  left_join(genre_ef, by='genres') %>%
  left_join(age_ef, by ="age_years")%>%
  mutate(pred = mu + b_m + b_u + b_g  +b_a) %>%
  pull(pred)

rmse_4<-RMSE(predicted_ratings, test_set_g$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie, User, Genre and Age Effect Model",
                                     RMSE = rmse_4))

rmse_results%>% knitr::kable()
#######################################################
# Regularization Method with  λ as a tuning parameter #
#######################################################

# (M5) model with the regularized movie effect with tuning parameter
# Choosing the tuning parameter lambda by using cross-validation

lambda <- seq(0, 20, 0.10)

rmses_m_ef_reg <- sapply(lambda, function(l){
  
mu <- mean(train_set$rating)
  
b_m <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())

predicted_ratings <-test_set %>%
    left_join(b_m, by = "movieId") %>%
    mutate(pred = mu + b_m) %>% pull(pred)
  
rmses_m_ef_reg<-return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambda, rmses_m_ef_reg)

# (M5) the model with the regularized movie effect  
# tuning parameter l with minimal RMSE

l_1<-lambda[which.min(rmses_m_ef_reg)]
l_1
#Average rating  
mu <- mean(train_set$rating)
#Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_1), n_m = n())
#Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  mutate(pred = mu + b_m) %>%
  pull(pred)
#Calculate RMSE of Model 5
rmse_5<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie Effect Model",
                                     RMSE = rmse_5))

rmse_results%>% knitr::kable()

# (M6) model with the regularized regularized movie + user effect with tuning parameter 
# Choosing the tuning parameter lambda by using cross-validation

lambda <- seq(0, 10, 0.10)

rmses_mu_ef_reg <- sapply(lambda, function(l){

mu <- mean(train_set$rating)
  
b_m <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())
  
b_u <- train_set %>%
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n()+l), n_u = n())
  
predicted_ratings <-test_set %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% 
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

rmses_mu_ef_reg<-return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambda, rmses_mu_ef_reg)

# (M6) Model with the regularized movie + user effect 
# tuning parameter l_2 with minimal RMSE 
l_2<-lambda[which.min(rmses_mu_ef_reg)]
l_2
#Average rating
mu <- mean(train_set$rating)

#Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_2), n_m= n())
#Estimate user effect b_u 
user_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_2), n_u = n())
# Estimate predicted ratings 
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg, by = "userId") %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

#Calculate RMSE of Model 6
rmse_6<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie and User Effect Model",
                                     RMSE = rmse_6))

rmse_results%>% knitr::kable()

# (M7) model with the regularized movie + user + genre effect with tuning parameter lambda
# Choosing the tuning parameter lambda by using cross-validation


lambda <- seq(0, 20, 0.10)

rmses_mug_ef_reg <- sapply(lambda, function(l){
  
mu <- mean(train_set_g$rating)
  
b_m <- train_set_g %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())
  
b_u <- train_set_g %>%
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n()+l), n_u = n())
  
b_g <- train_set_g %>%
  group_by(genres)%>%
  left_join(b_m, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l), n_u = n())
  
predicted_ratings <-test_set_g %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% 
  left_join(b_g, by = "genres") %>% 
  mutate(pred = mu + b_m + b_u +b_g) %>%
  pull(pred)
  
rmses_mug_ef_reg<-return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambda, rmses_mug_ef_reg)

# (M7) Model with the regularized movie + user+ genre effect 
# tuning parameter l_3 with minimal RMSE 
l_3<-lambda[which.min(rmses_mug_ef_reg)]
l_3

# Average rating
mu <- mean(train_set_g$rating)
#Estimate movie effect b_m
movie_ef_reg <- train_set_g %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_3), n_m= n())
#Estimate user effect b_u
user_ef_reg <- train_set_g %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_3), n_u = n())
# Estimate genre effect b_g
genre_ef_reg <- train_set_g %>%
  group_by(genres)%>%
  left_join(movie_ef_reg, by="movieId") %>%
  left_join(user_ef_reg, by="userId") %>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l_3), n_g = n())
# Estimate predicted ratings 
predicted_ratings <- test_set_g %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg, by = "userId") %>%
  left_join(genre_ef_reg, by="genres")%>%
  mutate(pred = mu + b_m + b_u + b_g) %>%
  pull(pred)

#Calculate RMSE of Model 7
rmse_7<-RMSE(predicted_ratings, test_set_g$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie, User and Genre Effect Model",
                                     RMSE = rmse_7))

rmse_results%>% knitr::kable()

# (M8) model with the regularized movie + user + genre + age effect with tuning parameter lambda
# Choosing the tuning parameter lambda by using cross-validation

lambda <- seq(0, 20, 0.10)

rmses_mug_ef_reg <- sapply(lambda, function(l){
  
  mu <- mean(train_set_g$rating)
  
  b_m <- train_set_g %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())
  
  b_u <- train_set_g %>%
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l), n_u = n())
  
  b_g <- train_set_g %>%
    group_by(genres)%>%
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l), n_g = n())
  
  b_a <- train_set_g %>%
    group_by(age_years)%>%
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    summarize(b_a = sum(rating - b_g - b_m - b_u - mu)/(n()+l), n_a = n())
  
  predicted_ratings <-test_set_g %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>% 
    left_join(b_g, by = "genres") %>%
    left_join(b_a, by = "age_years")%>%
    mutate(pred = mu + b_m + b_u + b_g + b_a) %>%
    pull(pred)
  
  rmses_muga_ef_reg<-return(RMSE(predicted_ratings, test_set_g$rating))
})
qplot(lambda, rmses_muga_ef_reg)

# (M8) Model with the regularized movie + user+ genre + age effect 
# tuning parameter l with minimal RMSE 
l_4<-lambda[which.min(rmses_muga_ef_reg)]
l_4
# Average rating
mu <- mean(train_set_g$rating)
#Estimate movie effect b_m
movie_ef_reg <- train_set_g %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_4), n_m= n())
#Estimate user effect b_u
user_ef_reg <- train_set_g %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_4), n_u = n())
# Estimate genre effect b_g
genre_ef_reg <- train_set_g %>%
  group_by(genres)%>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l_4), n_g = n())
#Estimate age effect b_a
age_ef_reg <- train_set_g %>%
  group_by(age_years)%>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  summarize(b_a = sum(rating - b_g - b_m - b_u - mu)/(n()+l_4), n_a = n())
# Estimate predicted ratings 
predicted_ratings <- test_set_g %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  left_join(age_ef_reg,   by = "age_years")%>%
  mutate(pred = mu + b_m + b_u + b_g + b_a) %>%
  pull(pred)

#Calculate RMSE of Model 8
rmse_8<-RMSE(predicted_ratings, test_set_g$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie, User, Genre and Age Effect Model",
                                     RMSE = rmse_8))

rmse_results%>% knitr::kable()



