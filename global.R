###############################################################################################
# File:         global.R
# Description:  This file contains the intialization steps and some common functions of the application.
# Name:         Chu Siu Kay Alan
# Student No.:  189222006
###############################################################################################
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(proxy)
library(dplyr)
library(neo4r)
library(magrittr)
library(dplyr)
library(purrr)
library(visNetwork)

###############################################################################################
# Load the data sets to data frames (START)
###############################################################################################
#setwd("C:/Users/user/MScDataScience/PROM02/PROM02_Project")
cb_movies_df<-read.csv("ml-latest-small/cb_movies.csv", encoding="UTF8")
cf_als_imp_movies_df<-read.csv("ml-latest-small/cf_als_imp_movies.csv", encoding="UTF8")
cf_popular_movies_df<-read.csv("ml-latest-small/cf_popular_movies.csv", encoding="UTF8")
movies_df<-read.csv("ml-latest-small/movies1.csv", encoding="UTF8")
tags_df<-read.csv("ml-latest-small/tags.csv", encoding="UTF8")
ratings_df<-read.csv("ml-latest-small/ratings.csv", encoding="UTF8")
links_df<-read.csv("ml-latest-small/links.csv", encoding="UTF8")
nodes <- data.frame(id = 1:20, label = 1:20)
edges <- data.frame(from = c(1:20), to = c(2:20,1))

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
head(ratings_df)

G2 <-"MATCH a=(p:Person {name: 'Tom Hanks'})-[r:ACTED_IN]->(m:Movie) RETURN a;" %>% 
  call_neo4j(con, type = "graph") 

# We'll just unnest the properties
G2$nodes <- G2$nodes %>%
  unnest_nodes(what = "properties")
#head(G2$nodes)  

# Add a new column
G2$nodes$group <- unlist(G2$nodes$label)  
G2$nodes$label <- G2$nodes$title

G2$nodes[G2$nodes$group=="Person",]$label = G2$nodes[G2$nodes$group=="Person",]$name
G2$nodes$title <- G2$nodes$group

# Turn the relationships :
G2$relationships <- G2$relationships %>%
  unnest_relationships() %>%
  select(from = startNode, to = endNode, label = type)
#head(G2$relationships)

#visNetwork::visNetwork(G2$nodes, G2$relationships)

###############################################################################################
# Load the data sets to data frames (END)
###############################################################################################




# Add an average rating column to movies_df
ratings_avg_df <- ratings_df %>%
  group_by(movieId) %>%
  dplyr::summarize(avgRating = mean(rating, na.rm=TRUE))
movies_df <- movies_df %>% inner_join(ratings_avg_df, by="movieId")

# Compose genres_df
genres_df <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  select(genres,year,avgRating)

genres_df <- genres_df %>%
  mutate(year = as.numeric(year))

# Clear all NAs
genres_df <- na.omit(genres_df)

# Add average rating to tags_df
tags_df <- tags_df %>% left_join(ratings_avg_df, by='movieId')

# Add movie production year column to tags_df
year_df<-movies_df %>%
  select(movieId, year)
tags_df <- tags_df %>% left_join(year_df, by="movieId")

# Derive year of rating from timestamp column
ratings_df <- ratings_df %>%
  mutate(ratedYear=substr(as.POSIXct(ratings_df$timestamp, origin='1970-01-01', tz="UTC"),1,4))
ratings_df <- ratings_df %>% left_join(year_df, by="movieId")
ratings_avg_df  <- ratings_avg_df %>% left_join(year_df, by="movieId")

###############################################################################################
# Function getRecentlyRatedMovies
#
# Description:  Return a neo list that contains the graph data of recently rated movies by the given user. 
#               Using "memoise" to automatically cache the results
# Input:        userID - User ID
#               recLimit - number of records to be returned
# Output:       Return a neo list that contains the graph data of recently rated movies by the given user. 
###############################################################################################
getRecentlyRatedMovies <- memoise(function(loginID, recLimit) {
  
  print("global.R")
  print(loginID)
  print(recLimit)
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste("MATCH a=(m:Movie)-[r:REVIEWED]-(p:Person {loginId: ",loginID,"}) return a order by r.timestamp desc limit ",recLimit, sep="")
  print(query)
  G <- query %>% 
    call_neo4j(con, type = "graph") 
  
  #head(G)
  # We'll just unnest the properties
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties")
  #head(G$nodes) 
  #length(G$nodes)
  
  # Add new columns
  G$nodes$node_type <- unlist(G$nodes$label)  
  G$nodes$label <- G$nodes$title
  G$nodes$movie_title <- G$nodes$title
  
  G$nodes[G$nodes$node_type=="Person",]$label = G$nodes[G$nodes$node_type=="Person",]$name
  G$nodes$title <- G$nodes$node_type
  
  # Turn the relationships :
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    select(from = startNode, to = endNode, label = type, rating = rating, review_date = review_date)
  
  # Add rating column to nodes
  # nrow(G$relationships)
  # nrow(G$nodes)
  G$nodes$user_rating<-0.0
  for (i in 1:nrow(G$relationships)){
    for (j in 1:nrow(G$nodes)) {
      if (G$nodes[j,]$id==G$relationships[i,]$to) {
        user_rating <- G$relationships[i,]$rating 
        G$nodes[j,]$user_rating <- as.numeric(user_rating)
        # print(i)
        # print(j)
        # print(G$relationships[i,]$to)
        # print(G$relationships[i,]$rating)
      }
    }
  }
  
  return (G)
})

# m<-getRecentlyRatedMovies(4,10)
# head(m$nodes)
# head(m$relationships)
# length(m$nodes)
#m$nodes[1,]$label
#m$nodes[2,]$poster
# m$nodes[1,]$title
# m1<-m$nodes[-1,]
# m1<-m$nodes[-(m$nodes[]$node_type=="Person"),]
# head(m1)
#m1$poster
#head(G$nodes)
# m$nodes[m$nodes[]$node_type=="Movie",]
# m$nodes
###############################################################################################
# Function getFavoriteMovies
#
# Description:  Return a neo list that contains the graph data of the movies that have high ratings given by the user. 
#               Using "memoise" to automatically cache the results
# Input:        userID - User ID
#               recLimit - number of records to be returned
# Output:       Return a neo list that contains the graph data of recently rated movies by the given user. 
###############################################################################################
getFavoriteMovies <- memoise(function(loginID, recLimit) {
  
  print("global.R")
  print(loginID)
  print(recLimit)
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste("MATCH a=(m:Movie)-[r:REVIEWED]-(p:Person {loginId: ",loginID,"}) return a order by r.rating desc limit ",recLimit, sep="")
  print(query)
  G <- query %>% 
    call_neo4j(con, type = "graph") 
  
  #head(G)
  # We'll just unnest the properties
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties")
  #head(G$nodes) 
  #length(G$nodes)
  
  # Add new columns
  G$nodes$node_type <- unlist(G$nodes$label)  
  G$nodes$label <- G$nodes$title
  G$nodes$movie_title <- G$nodes$title
  
  G$nodes[G$nodes$node_type=="Person",]$label = G$nodes[G$nodes$node_type=="Person",]$name
  G$nodes$title <- G$nodes$node_type
  
  # Turn the relationships :
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    select(from = startNode, to = endNode, label = type, rating = rating, review_date = review_date)
  
  # Add rating column to nodes
  # nrow(G$relationships)
  # nrow(G$nodes)
  G$nodes$user_rating<-0.0
  for (i in 1:nrow(G$relationships)){
    for (j in 1:nrow(G$nodes)) {
      if (G$nodes[j,]$id==G$relationships[i,]$to) {
        user_rating <- G$relationships[i,]$rating 
        G$nodes[j,]$user_rating <- as.numeric(user_rating)
        # print(i)
        # print(j)
        # print(G$relationships[i,]$to)
        # print(G$relationships[i,]$rating)
      }
    }
  }
  
  return (G)
})

###############################################################################################
# Function getContentBasedMovies
#
# Description:  Return a neo list that contains tibbles of the movie attributes that are similar to users' favorite movies. 
#               Using "memoise" to automatically cache the results
# Input:        userID - User ID
#               recLimit - number of records to be returned
# Output:       Return a neo list that contains tibbles of the movie attributes that are similar to users' favorite movies. 
###############################################################################################
getContentBasedMovies <- memoise(function(loginID, recLimit) {
  
  print("global.R")
  print(loginID)
  print(recLimit)
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste("MATCH (u:Person {loginId: ",loginID,"})-[r:REVIEWED]->(m:Movie) ",
                 " WITH m, r, u ",
                 " ORDER BY r.rating DESC LIMIT 5 ",
                 " MATCH (m)-[:HAS_GENRE|ACTED_IN|DIRECTED]-(t)-[:HAS_GENRE|ACTED_IN|DIRECTED]-(other:Movie) ",
                 " WHERE NOT EXISTS( (u)-[:REVIEWED]->(other) ) ",
                 " WITH m, other, COUNT(t) AS intersection, COLLECT(t.name) AS i ",
                 " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED]-(mt) ",
                 " WITH m,other, intersection,i, COLLECT(mt.name) AS s1 ",
                 " MATCH (other)-[:HAS_GENRE|:ACTED_IN|:DIRECTED]-(ot) ",
                 " WITH m,other,intersection,i, s1, COLLECT(ot.name) AS s2 ",
                 " WITH m,other,intersection,s1,s2 ",
                 " WITH m,other,intersection,s1+[x IN s2 WHERE NOT x IN s1] AS union, apoc.text.join(s1, '|') AS s1_txt, apoc.text.join(s2, '|') AS s2_txt ",
                 " RETURN m.title, other.title, other.avg_rating, other.poster, s1_txt,s2_txt,((1.0*intersection)/SIZE(union)) AS jaccard ORDER BY jaccard DESC LIMIT ", recLimit, sep="")
  print(query)
  R <- query %>% 
    call_neo4j(con, type = "row") 

  return (R)
})
# m <- getContentBasedMovies(1,10)
# head(m)
# print(m$other.title[1,])
# print(m$other.avg_rating[1,])
# p<-m$other.title[1,]$value
# print(p)
# nrow(m$other.title)

###############################################################################################
# Function getCollaborativeFilteringMovies
#
# Description:  Return a neo list that contains the graph data of the movies that are similar to users' favorite movies. 
#               Using "memoise" to automatically cache the results
# Input:        userID - User ID
#               recLimit - number of records to be returned
# Output:       Return a neo list that contains the graph data of recently rated movies by the given user. 
###############################################################################################
getCollaborativeFilteringMovies <- memoise(function(loginID, recLimit) {
  
  print("global.R")
  print(loginID)
  print(recLimit)
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste("MATCH (u1:Person {loginId: ",loginID,"})-[x:REVIEWED]->(movie:Movie) ",
                 " WITH u1, gds.alpha.similarity.asVector(movie, x.rating) AS u1Vector ",
                 " MATCH (u2:Person)-[x2:REVIEWED]->(movie:Movie) WHERE u2 <> u1 ",
                 " WITH u1, u2, u1Vector, gds.alpha.similarity.asVector(movie, x2.rating) AS u2Vector ",
                 " WHERE size(apoc.coll.intersection([v in u1Vector | v.category], [v in u2Vector | v.category])) > 10 ",
                 " WITH u1, u2,  gds.alpha.similarity.pearson(u1Vector, u2Vector, {vectorType: 'maps'}) AS similarity ",
                 " ORDER BY similarity DESC ",
                 " LIMIT 10 ",
                 " MATCH (u2)-[r:REVIEWED]->(m:Movie) WHERE NOT EXISTS( (u1)-[:REVIEWED]->(m) ) ",
                 " RETURN m.title, m.avg_rating, m.poster, SUM( similarity * r.rating) AS score ",
                 " ORDER BY score DESC LIMIT ", recLimit, sep="")
  print(query)
  R <- query %>% 
    call_neo4j(con, type = "row") 
  
  return (R)
})

# m <- getCollaborativeFilteringMovies(1,10)
# head(m)
# print(m$m.title[1,])
# print(m$m.avg_rating[1,])
# p<-m$m.title[1,]$value
# print(p)
# nrow(m$m.title)

###############################################################################################
# Function getTermMatrix
#
# Description:  Return a matrix that contains genres of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
#               min_rating - Minimum average ratings that the movie should have
#               max_rating - Maximum average ratings that the movie should have
# Output:       Return a matrix that contains genres of the movies that satisfies the input criteria.
###############################################################################################
getTermMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
  
  print("global.R")
  print(from_year)
  print(to_year)
  print(min_rating)
  print(max_rating)
  genres_df1 <- genres_df %>%
    filter(between(genres_df$year, from_year, to_year), between(genres_df$avgRating, min_rating, max_rating))
  
  print("# of genre_df1")
  print(nrow(genres_df1))
  
  # print(min(genres_df$year))
  # print(max(genres_df$year))
  if (nrow(genres_df1) == 0 ){
    text <- c("NOTHING")
  }
  else {
    text <- as.vector(genres_df1$genres)
  }
  
  
  myCorpus = Corpus(VectorSource(text))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

###############################################################################################
# Function getTermMatrix
#
# Description:  Return a matrix that contains tags of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
#               min_rating - Minimum average ratings that the movie should have
#               max_rating - Maximum average ratings that the movie should have
# Output:       Return a matrix that contains tags of the movies that satisfies the input criteria.
###############################################################################################
getTagMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
  
  print("global.R")
  print(from_year)
  print(to_year)
  print(min_rating)
  print(max_rating)
  tags_df1 <- tags_df %>%
    filter(between(year, from_year, to_year), between(tags_df$avgRating, min_rating, max_rating))
  
  print("# of tags_df1")
  print(nrow(tags_df1))
  
  # print(min(genres_df$year))
  # print(max(genres_df$year))
  if (nrow(tags_df1) == 0 ){
    text <- c("NOTHING")
  }
  else {
    text <- as.vector(tags_df1$tag)
  }
  
  
  myCorpus = Corpus(VectorSource(text))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


###############################################################################################
# Function getRatingHistogramDF
#
# Description:  Return a data frame that contains ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains ratings of the movies that satisfies the input criteria. 
###############################################################################################
getRatingHistogramDF <- memoise(function(from_year, to_year) {
  
  print("global.R - ratingHistogram")
  print(from_year)
  print(to_year)
  ratings_df1 <- ratings_df %>%
    filter(between(year, from_year, to_year)) 
  
  print("# of ratings_avg_df1")
  print(nrow(ratings_df1))
  
  return(ratings_df1)
})


###############################################################################################
# Function getAvgRatingHistogramDF
#
# Description:  Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
###############################################################################################
getAvgRatingHistogramDF <- memoise(function(from_year, to_year) {
  
  print("global.R - avgRatingHistogram")
  print(from_year)
  print(to_year)
  ratings_avg_df1 <- ratings_avg_df %>%
    filter(between(year, from_year, to_year)) 
  
  print("# of ratings_avg_df1")
  print(nrow(ratings_avg_df1))
  
  return(ratings_avg_df1)
})

###############################################################################################
# Function getUserRatingHistogramDF
#
# Description:  Return a data frame that contains histogram information about number of ratings given by each user. 
#               Using "memoise" to automatically cache the results
# Input:        
# Output:       Return a data frame that contains histogram information about number of ratings given by each user.
###############################################################################################
getUserRatingHistogramDF <- memoise(function() {
  
  print("global.R - userRatingHistogram")

  user_ratings_df1 <- ratings_df %>%
    group_by(ratings_df$userId, ratings_df$ratedYear) %>%
    summarise(r_cnt_by_user=n())
  colnames(user_ratings_df1) <- c("userId","ratedYear","cntByUser")
  user_ratings_df2 <- aggregate(user_ratings_df1[,3], list(user_ratings_df1$ratedYear), mean)
  colnames(user_ratings_df2)<- c("year","avgCntPerUser")
  #user_ratings_df2$year <- as.integer(user_ratings_df2$year)
  return(user_ratings_df2)
})

###############################################################################################
# Function getTop10MovieDF
#
# Description:  Return a data frame that contains top 10 movies that satisfy the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting year of the ratings
#               to_year   - Ending year of the ratings
#               minRatingCnt - Minimum number of ratings the movies should have
# Output:       Return a data frame that contains top 10 movies that satisfy the input criteria.
###############################################################################################
getTop10MovieDF <- memoise(function(from_year, to_year, minRatingCnt) {
  
  print("global.R - top10Movie")
  
  top10_df1 <- ratings_df %>%
    filter(between(ratedYear, from_year, to_year))
  
  # Find the average rating for each movie in each year
  top10_df2 <- aggregate(top10_df1[,3], list(top10_df1$movieId), mean)
  colnames(top10_df2)<- c("movieId", "avgRating")
  
  top10_df3 <- top10_df1 %>%
    group_by(movieId) %>%
    summarise(ratingCnt=n())
  
  top10_df4 <- left_join(top10_df2,top10_df3,c("movieId"))
  
  # Remove the movies that has number of ratings less than minRatingCnt
  top10_df5 <- top10_df4 %>%
    filter(ratingCnt>=minRatingCnt)
  
  top10_df6 <- top10_df5 %>%
    arrange(desc(top10_df5$avgRating, top10_df5$ratingCnt))
  
  top10_df7 <- head(top10_df6,10)
  
  return(top10_df7)
})



