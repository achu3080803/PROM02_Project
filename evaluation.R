###############################################################################################
# File:         evaluation.R
# Description:  This file contains codes for performing evaluation on the algorithms used in movie recommender system.
# Name:         Chu Siu Kay Alan
# Student No.:  189222006
###############################################################################################
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(proxy)
library(dplyr, warn.conflicts = FALSE)
library(neo4r)
library(magrittr)
library(dplyr)
library(purrr)
library(visNetwork)
library(data.table)
library("recommenderlab")

source("./global.R", local=TRUE)

setwd("C:/Users/user/MScDataScience/PROM02/PROM02_Project")


# Connect to neo4j
con <- neo4j_api$new(
  #  url = "http://localhost:7474", 
  url = "http://58.176.98.68:7474",
  user = "neo4j", 
  password = "bmw123"
)

# Get all user IDs
all_userid_t <- "MATCH (p:Person) WHERE exists (p.loginId) RETURN p.loginId order by p.loginId" %>% 
  call_neo4j(con, type = "row") 
all_userid_df <- as.data.frame(all_userid_t)
head(all_userid_df)

split_data <- function(userId, training_split){
  
  # Remove all REVIEWED2 
  result <- paste0("MATCH(u:Person {loginId: ",userId,"})-[r2:REVIEWED2]-() DELETE r2") %>%
    call_neo4j(con, type = "row")
  # Initialize split to EMPTY string
  result <- paste0("MATCH(u:Person {loginId: ",userId,"})-[r:REVIEWED]-() SET r.split=''") %>%
    call_neo4j(con, type = "row")
  
  # Find out the total number of ratings
  result <- paste(" MATCH  (u:Person {loginId: ",userId,"})-[r:REVIEWED]->(m:Movie) RETURN count(r) as rating_cnt") %>% 
    call_neo4j(con, type = "row") 
  
  total_rating <- result$rating_cnt[1,]$value
  print(total_rating)
  split_num <- round(total_rating * training_split / 100)
  print(split_num)
  
  # Split the ratings to TRAIN / TEST
  split_query <- paste(" MATCH  (u:Person {loginId: ",userId,"})-[r:REVIEWED]->(m:Movie) ",
                       " with m,r,u ",
                       " order by r.timestamp limit ",split_num," ",
                       " MATCH (u)-[r1:REVIEWED]->(m) SET r1.split='TRAIN' ") %>% 
    call_neo4j(con, type = "row")
  
  split_query <- paste(" MATCH  (u:Person {loginId: ",userId,"})-[r:REVIEWED]->(m:Movie) ",
                       " WHERE r.split <> 'TRAIN' ",
                       " SET r.split = 'TEST' ") %>% 
    call_neo4j(con, type = "row")
  
  split_num1 <- round(split_num / 2) 
  print(split_num1)
  # Add fake REVIEWED2 relation for training as this is a time series
  split_query <- paste(" MATCH  (u:Person {loginId: ",userId,"})-[r:REVIEWED]->(m:Movie) ",
                       " with m,r,u ",
                       " order by r.timestamp limit ",split_num1," ",
                       " MERGE (u)-[r2:REVIEWED2]->(m) ",
                       " ON CREATE SET r2.rating=r.rating, r2.timestamp=r.timestamp, r2.review_date=r.review_date ",
                       " ON MATCH SET r2.rating=r.rating, r2.timestamp=r.timestamp, r2.review_date=r.review_date ") %>% 
    call_neo4j(con, type = "row") 
  
}

# Test
#split_data(1, 50)

# Train/Test Split the data to 50/50 for all users
for(i in 1:nrow(all_userid_df)) {
  split_data(all_userid_df[i,1], 50)
}

#split_data(all_userid_df[1,1], 50)

optimize <- function(topN) {
  
  result_df <- copy(all_userid_df)
  names(result_df)<- c("login")
  # Add columns and initialize them to 0
  for (i in 1:length(topN)) {
    result_df[,paste0("param",i)] <- 0
  }
  for (i in 1:length(topN)) {
    limit <- topN[i]
    #print(paste0("limit: |",limit,"|"))
    
    for(j in 1:nrow(all_userid_df)) {
    #for(u in 414:414) {
      u <- all_userid_df[j,1]
      q <- paste0(" MATCH  (u:Person {loginId: ",u,"})-[r:REVIEWED2]->(m:Movie) ",
      #q <- paste0(" MATCH  (u:Person)-[r:REVIEWED2]->(m:Movie) ",            
                 " WITH m, r, u ",
                 " ORDER BY r.rating DESC, r.timestamp DESC LIMIT ",limit," ",
                 " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(t)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(other:Movie) ",
                 " WHERE NOT EXISTS( (u)-[:REVIEWED2]->(other)) ",
                 " WITH m, other, COUNT(t) AS intersection, COLLECT(t.name) AS i ",
                 " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(mt) ",
                 " WITH m,other, intersection,i, COLLECT(mt.name) AS s1 ",
                 " MATCH (other)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(ot) ",
                 " WITH m,other,intersection,i, s1, COLLECT(ot.name) AS s2 ",
                 " WITH m,other,intersection,s1,s2 ",
                 " WITH DISTINCT m,other,intersection,s1+[x IN s2 WHERE NOT x IN s1] AS union, apoc.text.join(s1, '|') AS s1_txt, apoc.text.join(s2, '|') AS s2_txt ",
                 " WITH '\\''+m.movieId+'\\'' AS source_id, other.movieId AS movie_id, other.title AS title, other.avg_rating AS avg_rating, other.poster AS poster,((1.0*intersection)/SIZE(union)) AS jaccard ",
                 " WITH collect(source_id) AS source_id_list, movie_id, title, avg_rating, poster, sum(jaccard) AS jaccard_sum ",
                 " WITH apoc.text.join(source_id_list, ',') AS source_id_csv, movie_id, title, avg_rating, poster, jaccard_sum AS score ",
                 " WITH source_id_csv, movie_id, title, avg_rating, poster, score ",
                 " ORDER BY score DESC LIMIT 10 ",
                 " MATCH (m1:Movie {movieId: movie_id})-[r3:REVIEWED]-(u:Person {loginId: ",u,"}) ",
                 " WHERE r3.split='TRAIN' ",
                 " RETURN u.loginId AS loginId, count(m1) AS score",
                 " ORDER BY loginId"
                 )
      result <- q %>% call_neo4j(con, type = "row")
      
      if (length(result$score) > 0) {
        
        print(paste("Result: Param:",limit,"Login ID:",result$loginId[1,]$value, "Score",result$score[1,]$value))
        
        result_df[result_df$login==u,i+1] <- result$score[1,]$value
      }
      
    }
  }
  return (result_df)
}

result_df <- optimize(c(5,10,15))
#Save the result
write.csv(result_df,"result_df.csv")


update_profile_favorite_limit <- function()
{
  #Default value of the favorite limit
  fav_limit=5
  
  for (i in 1:nrow(result_df)) {
    login <- result_df[i,]$login
    param1 <- result_df[i,]$param1
    param2 <- result_df[i,]$param2
    param3 <- result_df[i,]$param3
    max_value <- max(param1, param2, param3)
    
    print(paste0("login: ",login))
    print(paste0("param1: ",param1))
    print(paste0("param2: ",param2))
    print(paste0("param3: ",param3))
    print(paste0("max_value: ",max_value))
    if (param1 == max_value) {
      fav_limit <- 5
    }
    else if (param2 == max_value) {
      fav_limit <- 10
    }
    else if (param3 == max_value) {
      fav_limit <- 15
    }
    else {
      fav_limit <- 5
    }
    
    upd_query <- paste0("MATCH (u:Person {loginId: ",login,"}) SET u.profile_favorite_limit=",fav_limit)
    result <- upd_query %>% call_neo4j(con, type = "row")
  }
}

update_profile_favorite_limit()

#Evaluate each algorithms

##################################
# Content-based Filtering
##################################

#create data frame with 0 rows and 3 columns
cb_result1 <- data.frame(matrix(ncol = 2, nrow = 0))

#provide column names
colnames(cb_result1) <- c('userId', 'movieId')

for(i in 1:nrow(all_userid_df)) {
  result <- getContentBasedMovies(all_userid_df[i,1],10,"Y")
  for (j in 1:nrow(result$title)){ 
    new_row <- c(all_userid_df[i,1], result$movie_id[j,]$value)
    cb_result1 <- rbind(cb_result1, new_row)
  }
}
# Save the result to a file
write.csv(cb_result1, "results/cb_result1_graph.csv")

#################################
#Collaborative Filtering
#################################

#create data frame with 0 rows and 3 columns
cf_result1 <- data.frame(matrix(ncol = 2, nrow = 0))

#provide column names
colnames(cf_result1) <- c('userId', 'movieId')

for(i in 1:nrow(all_userid_df)) {
  result <- getCollaborativeFilteringMovies(all_userid_df[i,1],10,"Y")
  for (j in 1:nrow(result$title)){ 
    new_row <- c(all_userid_df[i,1], result$movie_id[j,]$value)
    cf_result1 <- rbind(cf_result1, new_row)
  }
}
# Save the result to a file
write.csv(cf_result1, "results/cf_result1_graph.csv")


############################################
#Content-based Filtering (by Actor/Actress)
############################################

#create data frame with 0 rows and 3 columns
cb_result2 <- data.frame(matrix(ncol = 2, nrow = 0))

#provide column names
colnames(cb_result2) <- c('userId', 'movieId')

for(i in 1:nrow(all_userid_df)) {
  result <- getActorMovies(all_userid_df[i,1],10,"Y")
  for (j in 1:nrow(result$title)){ 
    new_row <- c(all_userid_df[i,1], result$movie_id[j,]$value)
    cb_result2 <- rbind(cb_result2, new_row)
  }
}
# Save the result to a file
write.csv(cb_result2, "results/cb_result2_graph.csv")

###############################################################################################
#
# Content-based recommender (START)
#
###############################################################################################

## Load movie data
setwd("C:/Users/user/MScDataScience/CETM46/Assignment2/ml-latest-small")
movies_df<-read.csv("movies.csv", encoding="UTF8")

## Extract training data set
query <- "MATCH (u:Person)-[r:REVIEWED {split: 'TRAIN'}]-(m:Movie) RETURN u.loginId as userId, m.movieId as movieId, r.rating as rating, r.timestamp as timestamp"
result <- query %>% call_neo4j(con, type = "row")

#create data frame with 0 rows and 4 columns
#ratings_df <- data.frame(matrix(ncol = 4, nrow = 0))

ratings_df <- data.frame(Characters=character(),Characters=character(),Doubles=double(),Characters=character())

for (i in 1:nrow(result$movieId)){
  new_row <- c(result$userId[i,]$value, result$movieId[i,]$value, as.double(as.character(result$rating[i,]$value)), result$timestamp[i,]$value)
  ratings_df <- rbind(ratings_df, new_row)
}
nrow(ratings_df)


#provide column names
colnames(ratings_df) <- c('userId', 'movieId', 'rating_c', 'timestamp_c')

ratings_df <- ratings_df%>%mutate(rating=as.double(as.character(ratings_df$rating_c)),.after=rating_c)
ratings_df <- ratings_df%>%mutate(timestamp=as.numeric(as.character(ratings_df$timestamp_c)),.after=timestamp_c)

str(ratings_df)


ratings_df <- ratings_df[c(-3,-5)]

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


get_imdbid <- function(i_movieId){
  return(paste0("tt",links_df[links_df$movieId==i_movieId,]$imdbId))
}

get_imdbid(1)
movies_df

get_watched_movies <- function(inUserId){
  m1 <- ratings_df[ratings_df$userId==inUserId,]
  m2 <- m1[order(m1$timestamp, decreasing = TRUE),]$movieId
  return(m2[1:10])
}
get_watched_movies(1)



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
#for (i in 1:2){
  
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
  sim_results <- as.data.frame(as.matrix(sim_results[2:nrow(sim_results)]))
  sorted_results <- sim_results[order(sim_results$V1), ]
  #rows <- which(sim_results == min(sim_results))
  rows <- which(sim_results <= sorted_results[10])[1:10]
  
  cb_movies <- rbind(cb_movies, cbind(i,movies_df3[rows,1]))
}
colnames(cb_movies) <- c("userId","movieId")

# Save prediction results
write.csv(cb_movies,"results/cb_movies.csv")

cb_movies <- read.csv("results/cb_movies.csv", encoding="UTF8")
cb_movies <- cb_movies[-1]


###############################################################################################
#
# Content-based recommender (END)
#
###############################################################################################


###############################################################################################
#
# Colaborative Filtering recommender (START)
#
# Matrix Factorization
#
###############################################################################################

## Extract training data set
query <- "MATCH (u:Person)-[r:REVIEWED {split: 'TRAIN'}]-(m:Movie) RETURN u.loginId as userId, m.movieId as movieId, r.rating as rating, r.timestamp as timestamp"
result <- query %>% call_neo4j(con, type = "row")

#create data frame with 0 rows and 4 columns
ratings_df <- data.frame(matrix(ncol = 4, nrow = 0))



for (i in 1:nrow(result$movieId)){
  new_row <- c(result$userId[i,]$value, result$movieId[i,]$value, result$rating[i,]$value, result$timestamp[i,]$value)
  ratings_df <- rbind(ratings_df, new_row)
}
nrow(ratings_df)

#provide column names
colnames(ratings_df) <- c('userId', 'movieId', 'rating', 'timestamp')

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
  "ALS implicit items" = list(name="ALS_implicit", param=NULL))

## run algorithms
results <- recommenderlab::evaluate(scheme, algorithms, type = "topNList", n=c(1,5,seq(10, 100, 10)))
avg(results$`ALS implicit items`)


recommenderRegistry$get_entry_names()

getConfusionMatrix(results[[4]])

# Plot the results
plot(results, legend="topleft")

# creation of recommender model based on ALS-implicit (BEST)
rec.als_imp <- Recommender(getData(scheme, "train"), method="ALS_implicit", param=NULL)

# making predictions for all users
p.als_imp <- as(predict(rec.als_imp, rr_matrix, type="topNList", n=10, ),"list")
cf_als_imp_movies <- NULL
for (i in 1:length(p.als_imp)){
  df_tmp <- data.frame(userId=c(i),movieId=as(p.als_imp[[i]],"vector"))
  cf_als_imp_movies <- rbind(cf_als_imp_movies, df_tmp)
}

# Save prediction results
write.csv(cf_als_imp_movies,"results/cf_als_imp_movies.csv")


# save the recommender models
saveRDS(rec.als_imp, file = "rec_als_imp.rds")


###############################################################################################
#
# Collaborative Filtering recommender (END)
#
###############################################################################################

###############################################################################################
#
# Evaluation
#
###############################################################################################


DCG_pos <- function(rel, p)
{
  # if(missing(p)) p <- length(rel)
  # stopifnot(p <= length(rel))
  
  rel/log(p + 1, 2)
}

IDCG_pos <- function(rel, p)
{
  # if(missing(p)) p <- length(rel)
  # stopifnot(p <= length(rel))
  
  2^(rel - 1)/log(p + 1, 2)
}

nDCG_pos <- function(rel, p)
{
  # if(missing(p)) p <- length(rel)
  # stopifnot(p <= length(rel))
  
  DCG_pos(rel, p) / IDCG_pos(rel, p)
}

evaluate <- function(pred_result) {
  
  # Add a column and initialize it as 0
  pred_result <- pred_result %>% 
    mutate(CG = 0, DCG=0, nDCG = 0)
  
  for (i in 1:nrow(pred_result)) {
    #for (i in 1:1) {
    query <- paste0("MATCH (u:Person {loginId: ",pred_result[i,]$userId,"})-[:REVIEWED {split: 'TEST'}]-(m:Movie {movieId: '",pred_result[i,]$movieId,"'}) RETURN COUNT(m) as movie_cnt ")
    print (query)
    result <- query %>% call_neo4j(con, type = "row")
    
    movie_cnt <- result$movie_cnt[1,]$value
    print(movie_cnt)
    
    pred_result[i,]$CG = movie_cnt
    if (movie_cnt > 0) {
      pred_result[i,]$DCG = DCG_pos(movie_cnt,pred_result[i,]$pos)
      pred_result[i,]$nDCG = nDCG_pos(movie_cnt,pred_result[i,]$pos)
    }
  }
  
  return (pred_result)
}

a <- nDCG_pos(1,2)

# Add position column to the result
pos <- c(1,2,3,4,5,6,7,8,9,10)
cb_result1p <- cbind(cb_result1,pos)
cf_result1p <- cbind(cf_result1,pos)
cb_result2p <- cbind(cb_result2,pos)

# Test the proposed algorithms
pred_result1 <- evaluate(cb_result1p)
pred_result2 <- evaluate(cf_result1p)
pred_result3 <- evaluate(cb_result2p)

pred_CG_score1 <- sum(pred_result1$CG)
pred_CG_score2 <- sum(pred_result2$CG)
pred_CG_score3 <- sum(pred_result3$CG)

pred_DCG_score1 <- sum(pred_result1$DCG)
pred_DCG_score2 <- sum(pred_result2$DCG)
pred_DCG_score3 <- sum(pred_result3$DCG)

# Save the result to a file
write.csv(pred_result1, "results/pred_result1.csv")
write.csv(pred_result2, "results/pred_result2.csv")
write.csv(pred_result3, "results/pred_result3.csv")


# Merge the score
overall_pred_result <- rbind(pred_result1, pred_result2)
overall_pred_result <- rbind(overall_pred_result, pred_result3)

overall_pred_result1<-distinct(overall_pred_result)
overall_pred_score1 <- sum(overall_pred_result1$found)

print(paste0("Overall Score of Hybrid Recommender System: ",overall_pred_score1))

write.csv(overall_pred_result1, "results/overall_pred_result1.csv")

# Test the baseline algorithm
cf_als_imp_moviesp <- cbind(cf_als_imp_movies,pos)
pred_result4 <- evaluate(cf_als_imp_moviesp)
pred_CG_score4 <- sum(pred_result4$CG)
pred_DCG_score4 <- sum(pred_result4$DCG)

# Save the result to a file
write.csv(pred_result4, "results/pred_result4.csv")

cb_moviesp <- cbind(cb_movies,pos)
pred_result5 <- evaluate(cb_moviesp)
pred_CG_score5 <- sum(pred_result5$CG)
pred_DCG_score5 <- sum(pred_result5$DCG)

# Save the result to a file
write.csv(pred_result5, "results/pred_result5.csv")

show_evaluation <- function()
{
  print("#########################################################")
  print("Evaluation Summary")
  print("#########################################################")
  print(paste0("Proposed KGCBF (Favorite Movies)  CG:",pred_CG_score1," DCG:",pred_DCG_score1))
  print(paste0("Proposed KGCF                     CG:",pred_CG_score2," DCG:",pred_DCG_score2))
  print(paste0("Proposed KGCBF (Favorite Actor)   CG:",pred_CG_score3," DCG:",pred_DCG_score3))
  print(paste0("Baseline CF (ALS Implicit)        CG:",pred_CG_score4," DCG:",pred_DCG_score4))
  print(paste0("Baseline CBF (Genres)             CG:",pred_CG_score5," DCG:",pred_DCG_score5))
}

show_evaluation()

DCG_pos(1,10)
