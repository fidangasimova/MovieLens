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
#           Data cleaning and exporation                  ##
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
edx<-transform(edx, rating_time = round_date(as_datetime(timestamp), unit="month"))
  
#Check missing values
sum(is.na(edx))

head(edx,10)

#HNumber of different users and movies are in the edx
edx %>%summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#Number of movies in different genres
edx%>%group_by(genres) %>%
summarize(count = n()) 
edx

# Number of different genres
edx%>%summarize(genre = n_distinct(genres))
edx

#Number of ratings in each of the genre
edx %>% group_by(genres) %>%e
summarize(count = n()) %>%
arrange(desc(count))
edx

# Top 10 of movies with greatest number of ratings
edx%>%group_by(movieId)%>%mutate(count=n())%>%
top_n(10)%>%arrange(desc(count()))

# Top 10 most given ratings 
edx%>%group_by(rating)%>%summarize(count = n()) %>%
arrange(desc(count))
edx

####################################
#   Visualization
####################################
library(ggplot2)

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


# Number of ratings given each rating categorie from 0.5 to 5 stars
edx %>% ggplot(aes(rating)) + geom_bar(fill=I("blue"), col=I("grey"), alpha=.2)+
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+
  ggtitle("Number of Ratings") +
  labs(subtitle = " ", x="Ratings" , y="Number of Ratings")+
  theme(plot.title = element_text(hjust = 0.5))
    

#Number of ratings for each genre
edx%>%group_by(genres)%>%summarize(count=n())%>%
ggplot(aes(x= reorder(genres, count), y=count))+
  geom_bar(stat='identity')+
  ggtitle("Number of Ratings for each Genre") +
  coord_flip(y=c(0, 50))+
  labs(x="Genre", y="n Ratings")+
  geom_text(aes(label= count), hjust=-0.5, size=2) +
  theme(plot.title = element_text(hjust = 0.5))
  
# Number of ratings 4, 4.5 and 5 by genre over years for each Genre
edx%>%filter(rating==4| rating==4.5 | rating==5, n()>100)%>%group_by(rating_time, genres)%>%
  summarise(count=n())%>%
  ggplot(aes(rating_time, count, color=genres) )+
  #scale_x_continuous(breaks=seq(rating_time))+
  ggtitle("Number of Ratings 4, 4.5 and 5 over Years") +
  labs(subtitle = " ", x="years" , y="Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()


# Boxplot of ratings for each genre








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


