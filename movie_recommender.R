###############################################################################################
# File:         movie_recommender.R
# Description:  This file contains implementation of data pre-processing, ML training and model evaluation.
# Name:         Chu Siu Kay Alan
# Student No.:  189222006
###############################################################################################
library(data.table)
library("recommenderlab")
library(tidyverse)
library(proxy)
library(dplyr)
library(caret)
library(RCurl)
library(rjson)

###############################################################################################
# Load the data sets to data frames (START)
###############################################################################################
setwd("C:/Users/user/MScDataScience/CETM46/Assignment2/ml-latest-small")
movies_df<-read.csv("movies.csv", encoding="UTF8")
tags_df<-read.csv("tags.csv", encoding="UTF8")
ratings_df<-read.csv("ratings.csv", encoding="UTF8")
links_df<-read.csv("links.csv", encoding="UTF8")
###############################################################################################
# Load the data sets to data frames (END)
###############################################################################################

# OMDB API KEY to get extra information of a movie
# e.g. Movie Posters URL
omdbapi.key<-"35b0b051"

# Check the structure of movie_df
str(movies_df)

###############################################################################################
# Add Movie Posters URL to movies.csv (START)
###############################################################################################

###############################################################################################
# Function get_imdbid
#
# Description:  Return a valid IMDB ID
# Input:        inMovieId - MovieLens movie ID
# Output:       Return the corresponding IMDB ID with given MovieLens ID
###############################################################################################
get_imdbid <- function(inMovieId){
  return(paste0("tt",links_df[links_df$movieId==inMovieId,]$imdbId))
}

###############################################################################################
# Function get_movie_title
#
# Description:  Return a movie title for a given movie ID
# Input:        inMovieId - MovieLens movie ID
#               plusSignNeeded - If TRUE, SPACE between words in the movie title is replaced wtih "+"
#                                If FALSE, original movie title is returned
# Output:       Return a movie title for a given movie ID
###############################################################################################
get_movie_title <- function(inMovieId, plusSignNeeded){
  t <- movies_df[movies_df$movieId==inMovieId,]$title
  #print(t)
  t <- substr(t,1,nchar(t)-7)
  print(t)
  if (substr(t,nchar(t)-4,nchar(t)) == ", The"){
    t <- substr(t,1,nchar(t)-5)
  }
  if (plusSignNeeded){
    t <- gsub(" ","+",t)
  }
  print(t)
  return(t)
}

###############################################################################################
# Function get_movie_url
#
# Description:  Return a movie poster URL for a given movie ID
# Input:        inMovieId - MovieLens movie ID
# Output:       Return a movie poster URL for a given movie ID
###############################################################################################
get_movie_url <-function(inMovieId) {
  out <- tryCatch(
    {
      if (is.null(inMovieId)) {
        inMovieId<-0
      } 
      full_url<-sprintf("http://www.omdbapi.com/?i=%s&apikey=%s",get_imdbid(inMovieId),"35b0b051")
      movie_data_df <- fromJSON(getURL(full_url))
      head(movie_data_df)
      # If the movie poster URL is not retrieved successfully with IMDBID, retry it using movie title.
      if (is.null(movie_data_df$Poster) || movie_data_df$Poster=="N/A") {
        full_url<-sprintf("http://www.omdbapi.com/?t=%s&apikey=%s",get_movie_title(inMovieId,TRUE),"35b0b051")
        movie_data_df <- fromJSON(getURL(full_url))
      }
      if (is.null(movie_data_df$Poster) || movie_data_df$Poster=="N/A") {
        return("movie_star.jpg")
      }
      return(movie_data_df$Poster)  
    },
    error=function(cond) {
      message(cond)
      # Choose a return value in case of error
      return("movie_star.jpg")
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return("movie_star.jpg")
    },
    finally={
      message("Processed")
    }
  )    
  return(out)
}

# Add a year column
movies_df <- movies_df %>%
  mutate(year = substr(title,nchar(title)-4,nchar(title)-1))
#Save the new structured movies_df to movies.csv

# Initialize the new column POSTER to NA
movies_df['poster']=NA

# Assign the Movie Poster URL to the new column POSTER
for (i in 1:nrow(movies_df)) {
  if (is.na(movies_df$poster[i])) {
    movies_df$poster[i]<-get_movie_url(movies_df$movieId[i]) 
  }
}


write_csv(movies_df,"movies1.csv")

###############################################################################################
# Add Movie Posters URL to movies.csv (END)
###############################################################################################


###############################################################################################
#
# Content-based recommender (START)
#
###############################################################################################

# Data-preprocessing
#
# Separate the genre list into separate columns
#
genres <- as.data.frame(movies_df$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:7)

genres_df <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))

genres_vector <- genres_df$genres
genres_vector <- sort(genres_vector[-length(genres_vector)])

movies_df2 <- movies_df %>%
  separate_rows(genres, sep = "\\|")

genre_matrix <- matrix(0,nrow(movies_df),length(genres_vector)) #empty matrix
#genre_matrix[1,] <- genres_vector #set first row to genre list
colnames(genre_matrix) <- genres_vector #set column names to genre list

movies_df1 <- cbind(movies_df,genre_matrix)
movies_df[movies_df$movieId==780,]

t<-movies_df[movies_df$movieId==355,]$title
t <- substr(t,1,nchar(t)-7)
if (substr(t,nchar(t)-4,nchar(t)) == ", The"){
  t <- substr(t,1,nchar(t)-6)
}
t

# Add an average rating column
ratings_avg_df <- ratings_df %>%
  group_by(movieId) %>%
  dplyr::summarize(avgRating = mean(rating, na.rm=TRUE))

get_imdbid <- function(i_movieId){
  return(paste0("tt",links_df[links_df$movieId==i_movieId,]$imdbId))
}

get_imdbid(1)
movies_df

get_watched_movies <- function(inUserId){
  m1 <- ratings_df[ratings_df$userId==inUserId,]
  m2 <- m1[order(-m1$timestamp),]$movieId
  return(m2[1:10])
}
get_watched_movies(1)


ratings_df[ratings_df$userId==1,]


  #print("Hello")
  currUser=1
  m_posters=NULL
  m <- get_watched_movies(currUser)
  for (i in 1:length(m)){
    m_posters <- append(m_posters, '<img src="')
    m_posters <- append(m_posters, get_movie_url(1))
    m_posters <- append(m_posters, paste0('" height="',poster.height,'" width="',poster.width,'">'))
  }
  print(m_posters)



#iterate through matrix
for (i in 1:nrow(movies_df2)) {
  movies_df1[movies_df1$movieId == movies_df2$movieId[i],][movies_df2$genres[i]] <- 1
}
print(i)

length(unique(ratings_df$movieId))

movies_df1

min(ratings_df$movieId)

bin_ratings_df <- ratings_df
for (i in 1:nrow(bin_ratings_df)){
  if (bin_ratings_df[i,3] > 3){
    bin_ratings_df[i,3] <- 1
  }
  else{
    bin_ratings_df[i,3] <- -1
  }
}

#Transform the binary rating dataframe
bin_ratings_df1 <- reshape2::dcast(bin_ratings_df, movieId~userId, value.var = "rating", na.rm=FALSE)
#Replace NA with 0
for (i in 1:ncol(bin_ratings_df1)){
  bin_ratings_df1[which(is.na(bin_ratings_df1[,i]) == TRUE),i] <- 0
}
bin_ratings_df1 = bin_ratings_df1[,-1] 


#Remove rows that are not rated from movies dataset
movieIds <- unique(movies_df2$movieId) #9742
ratingmovieIds <- unique(bin_ratings_df$movieId) #9724
movies_df3 <- movies_df1[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies_df3) <- NULL
#Remove rows that are not rated from genre_matrix2
#genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
genre_matrix3 <- movies_df3 %>% select(1,4:22)
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
user_profiles_matrix = matrix(0,length(genres_vector),length(unique(bin_ratings_df$userId)))
for (c in 1:ncol(bin_ratings_df1)){
  for (i in 2:ncol(genre_matrix3)){
    user_profiles_matrix[i-1,c] <- sum((genre_matrix3[,i]) * (bin_ratings_df1[,c]))
  }
}

user_profiles_matrix[,1]

#Convert to Binary scale
for (i in 1:nrow(user_profiles_matrix)){
  for (j in 1:ncol(user_profiles_matrix)) {
    if (user_profiles_matrix[i,j] <= 0){
      user_profiles_matrix[i,j] <- 0
    }
    else {
      user_profiles_matrix[i,j] <- 1
    }
  }
}

cb_movies<-NULL
for (i in 1:ncol(user_profiles_matrix)){
  
  print(i)
  user_profile1 <- user_profiles_matrix[,i] #Each user profile
  
  #Exclude the movies that have already been watched
  ratingmovieIds <- unique(bin_ratings_df[bin_ratings_df$userId==i,]$movieId) 
  genre_matrix4 <- genre_matrix3[-which((movieIds %in% ratingmovieIds) == TRUE),]
  
  sim_mat <- rbind.data.frame(user_profile1, genre_matrix4[,2:20])
  sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
  
  #Calculate Jaccard distance between user profile and all movies
  library(proxy)
  sim_results <- dist(sim_mat, method = "Jaccard")
  nrow(sim_results)
  ncol(sim_results)
  sim_results <- as.data.frame(as.matrix(sim_results[2:9725]))
  sorted_results <- sim_results[order(sim_results$V1), ]
  #rows <- which(sim_results == min(sim_results))
  rows <- which(sim_results <= sorted_results[10])[1:10]
  
  cb_movies <- rbind(cb_movies, cbind(i,movies_df3[rows,1]))
}
colnames(cb_movies) <- c("userId","movieId")
write.csv(cb_movies,"cb_movies.csv")

head(sim_results)
?min

#Recommended movies
movies_df3[rows,2:3]
user_profile <- rbind.data.frame(user_profile1)
colnames(user_profile) <- genres_vector

###############################################################################################
#
# Content-based recommender (END)
#
###############################################################################################

###############################################################################################
#
# Colaborative Filtering recommender (START)
#
###############################################################################################

## create a matrix with ratings
m <- matrix(nrow=length(unique(ratings_df$userId)), ncol=length(unique(ratings_df$movieId)), 
            dimnames = list(user=unique(ratings_df$userId),
                      item=unique(ratings_df$movieId)
            ))
m
rownames(m)[1:10]


for (i in 1:nrow(ratings_df)){
  #print(ratings_df[i,]$movieId)
  curMovieId <- ratings_df[i,]$movieId
  curUserId <- ratings_df[i,]$userId
  curMovieIdx <- grep(paste0("^",curMovieId,"$"), colnames(m))
  curUserIdx <- grep(paste0("^",curUserId,"$"), rownames(m))
  #print(m[ grep(ratings_df[i,]$movieId, rownames(m)), grep(ratings_df[i,]$userId, rownames(m)) ] )
  m[curUserIdx,curMovieIdx] <- ratings_df[i,]$rating
}

#rr_matrix <- new("realRatingMatrix", data = m)
rr_matrix <- as(m, "realRatingMatrix")

head(as(rr_matrix[1:5,], "list")[[1]])


# Check the real ralting matrix rr_matrix
hist(rowCounts(rr_matrix))
hist(colCounts(rr_matrix))
mean(rowMeans(rr_matrix))
hist(rowMeans(rr_matrix))
nratings(rr_matrix)


# Train / Test Split

# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
set.seed(2020)
scheme <- evaluationScheme(rr_matrix, method="cross-validation", k=10, train=0.9, given=-1, goodRating=4)


algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "ALS items" = list(name="ALS", param=NULL),
  "ALS implicit items" = list(name="ALS_implicit", param=NULL),
  "user-based CF Jaccard 5" = list(name="UBCF",  param=list(normalize = "center",method="jaccard",nn=5)),
  "user-based CF Jaccard 40" = list(name="UBCF",  param=list(normalize = "center",method="jaccard",nn=40)),
  "user-based CF Consine" = list(name="UBCF", param=list(normalize = "center",method="cosine",nn=5)),
  "user-based CF Pearson" = list(name="UBCF", param=list(normalize = "center",method="pearson",nn=5)),
  # "user-based CF Euclidean" = list(name="UBCF",  param=list(normalize = "center",method="euclidean",nn=5)),
  # "item-based CF Euclidean" = list(name="IBCF", param=list(normalize = "center",method="euclidean",k=5)),
  "SVD approximation" = list(name="SVD", param=list(k = 5)))

## run algorithms
results <- evaluate(scheme, algorithms, type = "topNList", n=c(1,5,seq(10, 100, 10)))
results <- evaluate(scheme, algorithms, type = "ratings", n=c(1,5,seq(10, 100, 10)))
avg(results$`user-based CF Jaccard`)

recommenderRegistry$get_entry_names()

getConfusionMatrix(results[[4]])

# Plot the results
plot(results, legend="topleft")


# creation of recommender model based on ALS-implicit (BEST)
rec.als_imp <- Recommender(getData(scheme, "train"), method="ALS_implicit", param=NULL)
# creation of recommender model based on POPULARITY (2nd BEST)
rec.popular <- Recommender(getData(scheme, "train"), method="POPULAR", param=NULL)

# making predictions for all users
p.als_imp <- as(predict(rec.als_imp, rr_matrix, type="topNList", n=10, ),"list")
cf_als_imp_movies <- NULL
for (i in 1:length(p.als_imp)){
  df_tmp <- data.frame(userId=c(i),movieId=as(p.als_imp[[i]],"vector"))
  cf_als_imp_movies <- rbind(cf_als_imp_movies, df_tmp)
}
p.popular <- as(predict(rec.popular, rr_matrix, type="topNList", n=10),"list")
cf_popular_movies <- NULL
for (i in 1:length(p.popular)){
  df_tmp <- data.frame(userId=c(i),movieId=as(p.popular[[i]],"vector"))
  cf_popular_movies <- rbind(cf_popular_movies, df_tmp)
}

# Save prediction results
write.csv(cf_als_imp_movies,"cf_als_imp_movies.csv")
write.csv(cf_popular_movies,"cf_popular_movies.csv")


# save the recommender models
saveRDS(rec.als_imp, file = "rec_als_imp.rds")
saveRDS(rec.popular, file = "rec_popular.rds")

###############################################################################################
#
# Colaborative Filtering recommender (END)
#
###############################################################################################
