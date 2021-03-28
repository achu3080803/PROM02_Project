###############################################################################################
# File:         optimization.R
# Description:  This file contains codes for performing optimization on the proposed Content-Based Filtering.
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

