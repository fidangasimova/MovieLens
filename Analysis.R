##############################
# FINAL PROJECT 
library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)

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

# Validation set will be 10% of MovieLens data
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#############################################################
ncol(edx)
nrow(edx)

#How many zeros were given as ratings in the edx dataset?
sum(edx$rating==0)
edx %>% filter(rating == 0) %>% tally()

#How many threes were given as ratings in the edx dataset?
sum(edx$rating==3)
edx %>% filter(rating == 3) %>% tally()

#How many different movies are in the edx dataset?How many different users are in the edx dataset?
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
n_distinct(edx$movieId)

###How many movie ratings are in each of the following genres in the edx dataset?
# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



###Which movie has the greatest number of ratings?
edx%>%group_by(movieId)%>%mutate(count=n())%>%top_n(5)%>%arrange(desc(count()))
#or
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


###What are the five most given ratings in order from most to least?

edx%>%group_by(rating)%>%summarize(count = n()) %>%
  arrange(desc(count))

