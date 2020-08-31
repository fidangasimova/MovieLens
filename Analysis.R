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


#data("movielens")

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

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

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
#####################################################

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# REMOVE ME LATER
edx <- edx[1:100000,]

#############################################################
#           Data processing                               ##
#############################################################

# edx summary
summary(edx)

#Number of variables
ncol(edx)

#Number of observarions
nrow(edx)

#Sort data by movieId 
edx<-arrange(edx, by=movieId)

# Split title and year into separate columns
edx<-extract(edx, title, c("title", "release_year"), "(.*)\\((\\d{4})\\)$")

# Convert year character into to an integer
edx<-transform(edx, release_year = as.numeric(release_year))

#Split genres into single columns per genre
edx<-separate_rows(edx, genres, sep = "\\|")

#Transform the rating timestamp to datetime year
#edx<-transform(edx, rating_time = round_date(as_datetime(timestamp), unit="month"))
edx<-transform(edx, rating_year = year(as_datetime(timestamp))) 
#remove variable rating_time
within(edx, rm(rating_time))
#Check missing values
sum(is.na(edx))

head(edx,10)

##############################################
#   Visualization and Descriptive statistics #
#############################################
library(ggplot2)

#######################
# USERS and MOVIES

# Top 10 of movies with greatest number of ratings
edx%>%group_by(movieId)%>%mutate(count=n())%>%
  top_n(10)%>%arrange(desc(count()))

#Number of different users and movies in edx
edx %>%summarize(n_users = n_distinct(userId),
                 n_movies = n_distinct(movieId))

# Distribution of number of ratings among Users and Movies.
p_1<-edx %>% 
  count(movieId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("blue"), col=I("red"), alpha=.2) +
  scale_x_log10() + 
  geom_density(col = "black", lwd=0.2)+
  ggtitle("Number of Movies vs. Proportion of Ratingscount") +
  labs(subtitle  = " ", 
    x="n Movies" , 
    y="Proportions of n Ratings")+
  theme(plot.title = element_text(hjust = 0.5))

p_2<-edx %>% 
  count(userId) %>% 
  ggplot(aes(n, y=..density..)) + 
  geom_histogram(bins=30, fill=I("grey"), col=I("red"), alpha=.2) +
  scale_x_continuous(trans = "log10") + 
  geom_density(col = "black", lwd=0.2)+
  ggtitle("Number of Users vs Proportion of Ratingscount") +
  labs(subtitle = " ",
       x="n Users" , 
       y="Proportion of n Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(p_1, p_2, nrow = 1)

###########################################
# Distribution of Ratings

# Top 10 most given ratings 
edx%>%group_by(rating)%>%summarize(count = n()) %>%
  arrange(desc(count))

# Number of ratings given each rating categories from 0.5 to 5 stars
edx %>% ggplot(aes(rating)) + geom_bar(fill=I("blue"), col=I("grey"), alpha=.2)+
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+
  ggtitle("Number of Ratings") +
  labs(x="Ratings" , y="Number of Ratings")+
  theme(plot.title = element_text(hjust = 0.5))
# As we can see 3 and 4 are the most given ratings   

########################################
# GENRE

#Number of movies in different genres
edx%>%group_by(genres) %>%
  summarize(count = n()) 

# Number of different genres
edx%>%summarize(genre = n_distinct(genres))

#Number of ratings in each of the genre
edx %>% group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Number of ratings for each genre
edx%>%group_by(genres)%>%summarize(count=n())%>%
ggplot(aes(x= reorder(genres, count), y=count))+
  geom_bar(stat='identity', color="blue")+
  ggtitle("Number of Ratings for each Genre") +
  coord_flip(y=c(0, 50000))+
  labs(x="Genre", y="n Ratings")+
  geom_text(aes(label= count), hjust=-0.5, size=2) +
  theme(plot.title = element_text(hjust = 0.5))
# Drama, Comedy and Action genre received the most ratings 

# Number of ratings starting from 3.5 by genre over years for each Genre
edx%>%filter(rating>=3.5)%>%group_by(genres,rating_time)%>%
  summarise(count=n()) %>%filter(count>1000)%>%arrange(desc(count))%>%
  ggplot(aes(rating_time, count, color=genres) )+
  geom_line()+
  #scale_x_continuous(breaks=seq((1930, 2018, by= 1)))+
  ggtitle("Number of Ratings from 3.5 to 5 over Years") +
  labs(x="years", y="Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))
  
#Drama remained a popular choice over years by receiving the most number of ratings from 3.5 and above. 

# Boxplot of ratings for each genre
edx%>%group_by(genres)%>%
  mutate(count=n(), avg_rating=mean(rating), se_rating=sd(rating)/sqrt(count))%>%
  ggplot(aes(x=reorder(genres, avg_rating), y=avg_rating, ymin = avg_rating-2*se_rating, ymax = avg_rating+2*se_rating), width=0.1, size=1.3 )+
  geom_point()+geom_errorbar()+
  ggtitle("Error Bar by Genre") +
  labs(x="Genre" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))



###############################################
# TIME

# Average rating over years when rating was given
edx %>% group_by(rating_year) %>%summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_year, avg_rating)) +
  geom_point() +geom_smooth()+
  scale_y_continuous(breaks=seq(0, 5, by= 0.5))+
  ggtitle("Average Rating over Rating Year") +
  labs(x="Rating Year" , y="Average Rating") +
  theme(plot.title = element_text(hjust = 0.5))

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

###########################################################################
# Method

#Outcome
y<-edx$ratings

#Predictors
x<-edx
  
# Generate training and test sets
set.seed(107)
test_index <- createDataPartition(y=edx$ratings, times = 1, p = 0.5, list = FALSE)
test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]  

#To make sure we don’t include users and movies in the test set that do not appear in the training set, 
#we remove these entries using the semi_join function:
  
  test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

  
  
RMSE <- function(true_ratings, predicted_ratings){ sqrt(mean((true_ratings - predicted_ratings)^2))
  }
