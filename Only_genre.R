# # # # # # # # # # # # # # # # # # # # # # # # 
#  PROJECT I: Movie Recommendation            #
# # # # # # # # # # # # # # # # # # # # # # # # 

setwd("MovieLens")

# # # # # # # # # # # # 
#  Load libraries
# # # # # # # # # # # # 

library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(data.table)

# # # # # # # # # # # # 
#  Create edx data 
# # # # # # # # # # # #

#  Code to generate edx data set
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies, stringsAsFactors=TRUE) %>% mutate(movieId = as.numeric(levels(movieId))[movieId], 
                                                                  title = as.character(title),
                                                                  genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# # # # # # # # # # # # # # # # # # # # # # 
#  Validation set will be 10% of edx data   
# # # # # # # # # # # # # # # # # # # # # # 

#  Code to generate the validation set
set.seed(675)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#  Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#  Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(ratings, movies, test_index, temp, movielens, removed)

# # # # # # # # # # # # # # # # # 
#   edx Data processing         # 
# # # # # # # # # # # # # # # # # 

#  edx summary
summary(edx)

#  Number of variables
ncol(edx)

#  Number of observations
nrow(edx)

#  Split title and year into separate columns by using regex
edx<-extract(edx, title, c("title", "release_year"), "(.*)\\((\\d{4})\\)$")

#  Convert year character into to an integer
edx<-transform(edx, release_year = as.numeric(release_year))

#  Transform the rating timestamp to datetime year
edx<-transform(edx, rating_year = year(as_datetime(timestamp))) 

#  Create an age_years variable that represents time in years
#  between release time and time when rating was given
edx<-edx%>%mutate(age_years=as.numeric(rating_year)-as.numeric(release_year))%>%filter(age_years>=0)

#  Check missing values
sum(is.na(edx))

head(edx,10)

# # # # # # # # # # # # # # # # # # 
#  Validation Data processing     #
# # # # # # # # # # # # # # # # # #

#  Split title and year into separate columns by using regex
validation<-extract(validation, title, c("title", "release_year"), "(.*)\\((\\d{4})\\)$")

#  Convert year character into to an integer
validation<-transform(validation, release_year = as.numeric(release_year))

#  Transform the rating timestamp to datetime year
validation<-transform(validation, rating_year = year(as_datetime(timestamp)))

#  Create an age_years variable that represents time in years
#  between release time and time when rating was given
validation<-validation%>%mutate(age_years=as.numeric(rating_year)-as.numeric(release_year))%>%filter(age_years>=0)

# # # # # # # # # # # # # # # # # # # # # # # # # # 
#    Visualization and Descriptive statistics     # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # 
#  USERS and MOVIES       # 
# # # # # # # # # # # # # # 

#  Top 10 of movies with greatest number of ratings
edx%>%group_by(movieId, title)%>%
  summarise(count = n())%>%arrange(desc(count))%>%
  top_n(10, count)

#  Number of unique users, movies and movie titles in edx
edx %>%summarize(n_users  = n_distinct(userId),
                 n_movies = n_distinct(movieId),
                 n_title  = n_distinct(title))

#  Distribution of number of ratings among Users and Movies.
# Change the format from scientific to numerical
options(scipen = 999)
p_1<-edx %>% 
  count(movieId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("blue"), col=I("red"), alpha=.2) +
  scale_x_continuous(trans = "log10") + 
  geom_density(col = "black", lwd=0.2)+
  # ggtitle("Number of Movies vs. Proportion of Ratingscount") +
  labs(x="n Movies", y="Proportions of n Ratings")+
  theme(plot.title = element_text(hjust = 0.5))

p_2<-edx %>% 
  count(userId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("grey"), col=I("red"), alpha=.2) +
  scale_x_continuous(trans = "log10") + 
  geom_density(col = "black", lwd=0.2)+
  # ggtitle("Number of Users vs Proportion of Ratingscount") +
  labs(x="n Users" , y="Proportion of n Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

p<-grid.arrange(p_1, p_2, nrow = 1)

#  Save p_1 and p_2 in one image
ggsave("n_ratings_Users_Movies.png", p)

# # # # # # # # # # # # # # # # # # # 
#  Distribution of Ratings               
# # # # # # # # # # # # # # # # # # # 

#  Top 10 most given ratings 
edx%>%group_by(rating)%>%
  summarize(count = n()) %>%
  arrange(desc(count))

#  Number of ratings given each rating categories from 0.5 to 5 stars
edx %>% ggplot(aes(rating)) + geom_bar(fill=I("blue"), col=I("grey"), alpha=.2)+
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+
  ggtitle("Number of Ratings") +
  labs(x="Ratings", y="n of Ratings")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Number of Ratings.png")
#  As we can see 3 and 4 are the most given ratings   

# # # # # # # # # # 
#  GENRE          # 
# # # # # # # # # # 


#  Split genres into single columns per genre, rename in edx_genre
edx_genre<-separate_rows(edx, genres, sep = "\\|")

#  Number of movies in different genres
edx_genre%>%group_by(genres) %>%
  summarize(count = n())%>% top_n(10)%>%arrange(desc(count))

#  Number of different genres
edx_genre%>%summarize(genre = n_distinct(genres))

#  Number of ratings for each genre
edx_genre%>%group_by(genres, rating)%>%
  summarize(count = n()) %>%
  arrange(desc(count))

#  Number of ratings for each genre
edx_genre %>% group_by(genres) %>% 
  summarize(count=n())%>%
  ggplot(aes(x= reorder(genres, count), y=count))+
  geom_bar(stat='identity', color="blue")+
  ggtitle("Number of Ratings for each Genre") +
  coord_flip(y=c(0, 5000000))+
  labs(x="Genre", y="n Ratings")+
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Number of Ratings for each Genre.png")
#  Drama, Comedy and Action genre received the most ratings 

#  Number of ratings over years when rating was given for each genre

#  Change format from scientific to normal
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
ggsave("Number of Ratings over Years of Rating.png")

#  Drama remained a popular choice over years by receiving the most number of ratings from 3.5 and above. 
#  Due to limits of memory only top 10 of genres with highest number of ratings will be represented
#  Boxplot of ratings for each genre

edx_genre%>%filter(genres  %in% c("Drama”, “Comedy", "Action", "Thriller", "Adventure", "Romance", "Sci-Fi", "Crime", "Fantasy", 
                                  "Children", "Horror", "Mystery", "War", "Animation", "Musical", "Western", "Film-Noir", "Documentary", "IMAX" ))%>%
  mutate(count = n(), avg_rating = mean(rating), se_rating = sd(rating)/sqrt(count))%>%
  ggplot(aes(x = reorder(genres, rating), y = rating))+
  geom_boxplot()+
  ggtitle("Boxplot by Genre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+
  ylab("Rating")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Boxplot by Genre.png")

rm(edx_genre)

# # # # # # # # # # # 
#  TIME             # 
# # # # # # # # # # # 

#  Average rating over years when rating was given
edx %>% group_by(rating_year) %>%summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_year, avg_rating)) +
  geom_point() + geom_smooth()+
  scale_y_continuous(breaks=seq(0, 5, by= 0.5))+
  scale_x_continuous(breaks=seq(1995, 2018, by= 5))+
  ggtitle("Average Rating over Rating Year") +
  labs(x="Rating Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Average Rating over Rating Year.png")

#  Highest number of given ratings for each year
edx%>%group_by(rating, rating_year)%>%
  summarise(n=n(), max=max(n))%>%
  arrange(desc(max))

#  In more detail to compare release with rating year:
#  Average rating over release vs. rating years
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
  # scale_x_continuous(breaks=seq(1930, 2018, by= 1))+
  ggtitle("Average Rating over Rating Years with Error Bar") +
  labs(x="Rating Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))
pl<-grid.arrange(p_3, p_4, nrow = 1)
ggsave("Average rating over release vs. rating years.png", pl)

#  Average rating over the age of movies
edx%>%group_by(age_years) %>% summarize(avg_rating = mean(rating))%>%arrange(desc(avg_rating))%>%
  ggplot(aes(age_years, avg_rating)) +
  scale_x_continuous(breaks=seq(0, 100, by= 10))+
  geom_point() + geom_smooth()+
  ggtitle("Average Rating over Movie Age") +
  labs(x="Age of Movie" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Average Rating over Movie Age.png")

# The average rating went up with time past the release but dropped again for movies older then 70 years

# # # # # # # #  
#  Method     # 
# # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  Generate train and test sets, 20% of edx data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(110)
test_index <- createDataPartition(y=edx$rating, times = 1, p = 0.2, list = FALSE)
test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]  

#  To make sure we don’t include users and movies in the test set that do not appear in the training set, 
#  remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# RMSE FUNCTION: For the evaluation of the model RMSE function will be used
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2, na.rm = T))
}

# # # # # # # # # # # # # # # # # # # # 
#  (M1) Model with movie effects
# # # # # # # # # # # # # # # # # # # # 

#  least squares estimate b_m is just the average of Yu,i − μˆ for each movie i.
# Average rating 
mu <- mean(train_set$rating)

# Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))

#  Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  mutate(pred = mu + b_m) %>%
  pull(pred)

#  Calculate RMSE of Model 1
rmes_1<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- data_frame(Model = "(M1) Movie Effect", RMSE = rmes_1)
rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # 
#  (M2) Model with movie and user effect
# # # # # # # # # # # # # # # # # # # # # # # 

#  Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
#  and b_m and estimating b_u as the average of yu,i − μˆ − bi

# Average rating
mu <- mean(train_set$rating)

# Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
# Estimate user effect b_u
user_ef <- train_set %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
# Estimate predicted rating
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef,  by='userId') %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

# Calculate RMSE of Model 2
rmse_2<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M2) Movie and User Effect Model",
                                     RMSE = rmse_2))

rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # 
#  (M3) Model with movie, user and genre effect
# # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
#  and b_m and estimating b_u as the average of yu,i − μˆ − bi
#  Average rating
mu <- mean(train_set$rating)
#  Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))%>%ungroup()
#  Estimate user effect b_u
user_ef <- train_set %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m)) 
#  Estimate genre effect b_g
genre_ef<-train_set%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef, by="userId")%>%
  group_by(genres)%>%
  summarize(b_g = mean(rating - mu - b_m- b_u)) 

#  Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef,  by='userId') %>%
  left_join(genre_ef, by='genres') %>%
  mutate(pred = mu + b_m + b_u + b_g) %>%
  pull(pred)

#  Calculate RMSE of Model 3
rmse_3<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M3) Movie, User and Genre Effect Model",
                                     RMSE = rmse_3))
rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  (M4) Model with movie, user, genre and age effect
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Yu,i = μ+bi +bu +εu,i, compute an approximation by computing μˆ 
#  and b_m and estimating b_u as the average of yu,i − μˆ − bi
#  Average rating
mu <- mean(train_set$rating)
# Estimate movie effect b_m
movie_ef <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
#  Estimate user effect b_u
user_ef <- train_set %>%
  left_join(movie_ef, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))
#  Estimate genre effect b_g
genre_ef<-train_set%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef, by="userId")%>%
  group_by(genres)%>%
  summarize(b_g = mean(rating - mu - b_m- b_u))
#  Estimate age effect b_a
age_ef<-train_set%>%
  left_join(movie_ef, by="movieId")%>%
  left_join(user_ef,  by="userId")%>%
  left_join(genre_ef, by="genres") %>%
  group_by(age_years)%>%
  summarize(b_a = mean(rating - mu - b_m - b_u - b_g))
# Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef, by='movieId') %>%
  left_join(user_ef,  by='userId') %>%
  left_join(genre_ef, by='genres') %>%
  left_join(age_ef,   by ="age_years")%>%
  mutate(pred = mu + b_m + b_u + b_g  + b_a) %>%
  pull(pred)

rmse_4<-RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M4) Movie, User, Genre and Age Effect Model",
                                     RMSE = rmse_4))

rmse_results%>% knitr::kable()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  Regularization Method with lambda as a tuning parameter  # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  (M5) Model with the regularized movie effect with lambda
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Choosing the tuning parameter lambda 
lambda <- seq(0, 15, 0.50)

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
ggsave("lambda 1.png")

#  Choose tuning parameter l with minimal RMSE
l_1<-lambda[which.min(rmses_m_ef_reg)]
l_1
# Average rating  
mu <- mean(train_set$rating)
# Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_1), n_m = n())
# Estimate predicted ratings
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  mutate(pred = mu + b_m) %>%
  pull(pred)
# Calculate RMSE of Model 5
rmse_5<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M5) Regularized Movie Effect Model",
                                     RMSE = rmse_5))

rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
#  (M6) Model with the regularized regularized movie + user effect with tuning parameter lambda
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Choose tuning parameter lambda
lambda <- seq(0, 15, 0.50)

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
ggsave("lambda 2.png")

#  tuning parameter l_2 with minimal RMSE 
l_2<-lambda[which.min(rmses_mu_ef_reg)]
l_2
# Average rating
mu <- mean(train_set$rating)
# Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_2), n_m= n())
# Estimate user effect b_u 
user_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_2), n_u = n())
#  Estimate predicted ratings 
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg, by = "userId") %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

# Calculate RMSE of Model 6
rmse_6<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M6) Regularized Movie and User Effect Model",
                                     RMSE = rmse_6))

rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  (M7) Model with the regularized movie + user + genre effect with tuning parameter lambda
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Choose tuning parameter lambda 
lambda <- seq(0, 15, 0.50)

rmses_mug_ef_reg <- sapply(lambda, function(l){
  
  mu <- mean(train_set$rating)
  
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())
  
  b_u <- train_set %>%
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l), n_u = n())
  
  b_g <- train_set %>%
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres)%>%
    summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l), n_g = n())
  
  predicted_ratings <-test_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>% 
    left_join(b_g, by = "genres") %>% 
    mutate(pred = mu + b_m + b_u +b_g) %>%
    pull(pred)
  
  rmses_mug_ef_reg<-return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambda, rmses_mug_ef_reg)
ggsave("lambda 3.png")

#  tuning parameter l_3 with minimal RMSE 
l_3<-lambda[which.min(rmses_mug_ef_reg)]
l_3
#  Average rating
mu <- mean(train_set$rating)
# Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_3), n_m= n())
# Estimate user effect b_u
user_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_3), n_u = n())
#  Estimate genre effect b_g
genre_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by="movieId") %>%
  left_join(user_ef_reg, by="userId") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l_3), n_g = n())
#  Estimate predicted ratings 
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg, by = "userId") %>%
  left_join(genre_ef_reg, by="genres")%>%
  mutate(pred = mu + b_m + b_u + b_g) %>%
  pull(pred)

# Calculate RMSE of Model 7
rmse_7<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M7) Regularized Movie, User and Genre Effect Model",
                                     RMSE = rmse_7))

rmse_results%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  (M8) Model with the regularized movie + user + genre + age effect with tuning parameter lambda
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Choose tuning parameter lambda 
lambda <- seq(0, 15, 0.50)

rmses_muga_ef_reg <- sapply(lambda, function(l){
  
  mu <- mean(train_set$rating)
  
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l), n_m = n())
  
  b_u <- train_set %>%
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l), n_u = n())
  
  b_g <- train_set %>%
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres)%>%
    summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l), n_g = n())
  
  b_a <- train_set %>%
    left_join(b_m, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(age_years)%>%
    summarize(b_a = sum(rating - b_g - b_m - b_u - mu)/(n()+l), n_a = n())
  
  predicted_ratings <-test_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>% 
    left_join(b_g, by = "genres") %>%
    left_join(b_a, by = "age_years")%>%
    mutate(pred = mu + b_m + b_u + b_g + b_a) %>%
    pull(pred)
  
  rmses_muga_ef_reg<-return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambda, rmses_muga_ef_reg)
ggsave("lambda 4.png")

#  tuning parameter l_4 with minimal RMSE 
l_4<-lambda[which.min(rmses_muga_ef_reg)]
l_4
#  Average rating
mu <- mean(train_set$rating)
# Estimate movie effect b_m
movie_ef_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_4), n_m= n())
# Estimate user effect b_u
user_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_4), n_u = n())
#  Estimate genre effect b_g
genre_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l_4), n_g = n())
# Estimate age effect b_a
age_ef_reg <- train_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  group_by(age_years)%>%
  summarize(b_a = sum(rating - b_g - b_m - b_u - mu)/(n()+l_4), n_a = n())
#  Estimate predicted ratings 
predicted_ratings <- test_set %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  left_join(age_ef_reg,   by = "age_years")%>%
  mutate(pred = mu + b_m + b_u + b_g + b_a) %>%
  pull(pred)

# Calculate RMSE of Model 8
rmse_8<-RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="(M8) Regularized Movie, User, Genre and Age Effect Model",
                                     RMSE = rmse_8))

rmse_results%>% knitr::kable()

# # # # # # # # # # # 
#  Final Model
# # # # # # # # # # # 

#  tuning parameter l with minimal RMSE 
l_4<-lambda[which.min(rmses_muga_ef_reg)]
l_4
#  Average rating
mu <- mean(edx$rating)
# Estimate movie effect b_m
movie_ef_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+l_4), n_m= n())
# Estimate user effect b_u
user_ef_reg <- edx %>%
  left_join(movie_ef_reg, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n() +l_4), n_u = n())
#  Estimate genre effect b_g
genre_ef_reg <- edx %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_m - b_u - mu)/(n()+l_4), n_g = n())
# Estimate age effect b_a
age_ef_reg <- edx %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  group_by(age_years)%>%
  summarize(b_a = sum(rating - b_g - b_m - b_u - mu)/(n()+l_4), n_a = n())
#  Estimate predicted ratings 
predicted_ratings <- validation %>%
  left_join(movie_ef_reg, by = "movieId") %>%
  left_join(user_ef_reg,  by = "userId") %>%
  left_join(genre_ef_reg, by = "genres")%>%
  left_join(age_ef_reg,   by = "age_years")%>%
  mutate(pred = mu + b_m + b_u + b_g + b_a) %>%
  pull(pred)

# Calculate RMSE of Final Model
rmse_final<-RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Final Regularized Movie, User, Genre and Age Effect Model",
                                 RMSE = rmse_final))

rmse_results%>% knitr::kable()
