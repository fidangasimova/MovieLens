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


data("movielens")

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

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
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
edx <- edx[1:100,]

#############################################################
#           Data cleaning and exporation                  ##
#############################################################

# edx summary
summary(edx)
str(edx)
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
edx<-edx %>% separate_rows(genres, sep = "\\|")

#Transform the rating timestamp to datetime year
edx<-transform(edx, rating_time = round_date(as_datetime(timestamp), unit="month"))
  
#Check missing values
sum(is.na(edx))

head(edx,10)

#How many zeros were given as ratings in the edx dataset?
sum(edx$rating==0)
edx %>% filter(rating == 0) %>% tally()

#How many different users and movies are in the edx dataset?
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#Number of movies in different genres
edx%>%group_by(genres) %>%
  summarize(count = n()) 
edx

# Number of different genres
edx%>%summarize(genre = n_distinct(genres))
edx

#How many movie ratings are in each of the following genres in the edx dataset?
edx %>% group_by(genres) %>%e
  summarize(count = n()) %>%
  arrange(desc(count))
edx

# Top 10 of movies with greatest number of ratings?
edx%>%group_by(movieId)%>%mutate(count=n())%>%top_n(5)%>%arrange(desc(count()))
#or

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


### What are the top 10 most given ratings in order from most to least?
edx%>%group_by(rating)%>%summarize(count = n()) %>%
arrange(desc(count))
edx

####################################
#   Visualization
####################################
library(ggplot2)

# Distribution of ratings
edx%>%summarize(average=mean(rating), sd=sd(rating))%>%ggplot(aes(x=release_year, ))


genreCountPlot <- edx%>%ggplot(aes(x = reorder(genres,genres))) + 
  geom_bar()+labs(x="Genre", y="Number of movies")+
  geom_text(aes(label=count), hjust=0.1, size=4)
  
edx%>%ggplot(x, aes(reorder(genres, rating, median, order=TRUE), wing)) + geom_boxplot()


genreCountPlot <- genreCountPlot + theme(axis.text.x = element_text(angle = 90, 
                                                                    hjust = 1))
genreCountPlot <- genreCountPlot + ylab("number of movies") + xlab("genre")
genreCountPlot <- genreCountPlot + coord_flip()
print(genreCountPlot)

p1<-edx%>%ggplot(aes(x=reorder(rating, y=..count..)))+geom_histogram()

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


