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

#G2 <-"MATCH a=(p:Person {name: 'Tom Hanks'})-[r:ACTED_IN]->(m:Movie) RETURN a;" %>% 
G2 <- 'MATCH a=(m:Movie {movieId: "1210"})-[:HAS_GENRE|ACTED_IN|DIRECTED|PRODUCED_BY|PRODUCED_IN]-(t)-[:HAS_GENRE|ACTED_IN|DIRECTED|PRODUCED_BY|PRODUCED_IN]-(other:Movie {movieId: "122886"}) return a' %>% 
  call_neo4j(con, type = "graph") 

# We'll just unnest the properties
G2$nodes <- G2$nodes %>%
  unnest_nodes(what = "properties")
#head(G2$nodes)  

# Add a new column
G2$nodes$group <- unlist(G2$nodes$label)  
G2$nodes$shape <- "circle" 
G2$nodes$image <- "" 
G2$nodes$icon <- vector(mode = "list", length = 10)
G2$nodes$label <- G2$nodes$title

typeof(G2$nodes[1,]$shape)

#G2$nodes[G2$nodes$group=="Movie",]$label = G2$nodes[G2$nodes$group=="Movie",]$title
G2$nodes[G2$nodes$group=="Movie",]$shape = "image"
G2$nodes[G2$nodes$group=="Movie",]$image = G2$nodes[G2$nodes$group=="Movie",]$poster
G2$nodes[G2$nodes$group=="Person",]$shape =""
G2$nodes[G2$nodes$group=="Person",]$label = G2$nodes[G2$nodes$group=="Person",]$name
G2$nodes[G2$nodes$group=="Company",]$label = G2$nodes[G2$nodes$group=="Company",]$name
G2$nodes[G2$nodes$group=="Genre",]$label = G2$nodes[G2$nodes$group=="Genre",]$name
G2$nodes[G2$nodes$group=="Country",]$label = G2$nodes[G2$nodes$group=="Country",]$name
G2$nodes$title <- G2$nodes$group

test<-list(list(code = "f007", color = "red"),list(code = "f007", color = "red"),list(code = "f007", color = "red"))
l<-length(G2$nodes[G2$nodes$group=="Person",]$icon)

test1<-G2$nodes[G2$nodes$group=="Person",]$icon
iconList <- vector(mode = "list", length = l)
iconList<-list(code = "f007", color = "red")
df<-data.frame(iconList)
df <- rbind(df, list(code = "f007", color = "red"))
rbind(df, list(code = "f007", color = "red"))

typeof(df[1,])
typeof(G2$nodes[G2$nodes$group=="Person",]$icon)
typeof(G2$nodes[G2$nodes$group=="Person",])
print(df[1,])
G2$nodes[G2$nodes$group=="Person",]$icon = list(list(code = "f007", color = "red"))
print(G2$nodes[G2$nodes$group=="Person",]$icon[1])

# Turn the relationships :
G2$relationships <- G2$relationships %>%
  unnest_relationships() %>%
  select(from = startNode, to = endNode, label = type)

typeof(G2$nodes)

G2nodesdf <- data.frame(G2$nodes)
visNetwork(G2$nodes, G2$relationships) %>% 
  visNodes(font = list(color = "#ffffff")) %>% 
  visGroups(groupname="Person",shape = "icon", icon = list(code = "f007", color = "red")) %>%
  visGroups(groupname="Movie",shape = "image") %>%
  visGroups(groupname="Company",shape = "dot") %>%
  visGroups(groupname="Genre",shape = "dot") %>%
  visGroups(groupname="Country",shape = "dot") %>%
  visEdges(font = list(color = "#ffffff", strokeColor = "#000000")) %>%
  visPhysics(barnesHut = list(springConstant=0)) %>%
  addFontAwesome()

visNetwork(G2$nodes, G2$relationships) %>% 
  visGroups(groupname="Person",shape = "icon", icon = list(code = "f007", color = "red")) %>%
  addFontAwesome()

d <- paste0("a",":","b")
params <- unlist(strsplit(d,":"))
print(params[2])
print(params)

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

f<-getFavoriteMovies(7,10)

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
  # query <- paste(" MATCH (u:Person {loginId: ",loginID,"})-[r:REVIEWED]->(m:Movie) ",
  #                " WITH m, r, u ",
  #                " ORDER BY r.rating DESC LIMIT 5 ",
  #                " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(t)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(other:Movie) ",
  #                " WHERE NOT EXISTS( (u)-[:REVIEWED]->(other) ) ",
  #                " WITH m, other, COUNT(t) AS intersection, COLLECT(t.name) AS i ",
  #                " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(mt) ",
  #                " WITH m,other, intersection,i, COLLECT(mt.name) AS s1 ",
  #                " MATCH (other)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(ot) ",
  #                " WITH m,other,intersection,i, s1, COLLECT(ot.name) AS s2 ",
  #                " WITH m,other,intersection,s1,s2 ",
  #                " WITH DISTINCT m,other,intersection,s1+[x IN s2 WHERE NOT x IN s1] AS union, apoc.text.join(s1, '|') AS s1_txt, apoc.text.join(s2, '|') AS s2_txt ",
  #                " RETURN DISTINCT m.movieId AS source_id, m.title AS source_title, other.movieId AS movie_id, other.title AS title, other.avg_rating AS avg_rating, other.poster AS poster, s1_txt,s2_txt,((1.0*intersection)/SIZE(union)) AS jaccard ORDER BY jaccard DESC LIMIT ", recLimit, sep="")
  query <- paste(" MATCH (u:Person {loginId: ",loginID,"})-[r:REVIEWED]->(m:Movie) ",
                 " WITH m, r, u ",
                 " ORDER BY r.rating DESC LIMIT 5 ",
                 " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(t)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(other:Movie) ",
                 " WHERE NOT EXISTS( (u)-[:REVIEWED]->(other) ) ",
                 " WITH m, other, COUNT(t) AS intersection, COLLECT(t.name) AS i ",
                 " MATCH (m)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(mt) ",
                 " WITH m,other, intersection,i, COLLECT(mt.name) AS s1 ",
                 " MATCH (other)-[:HAS_GENRE|:ACTED_IN|:DIRECTED|:PRODUCED_BY|:PRODUCED_IN]-(ot) ",
                 " WITH m,other,intersection,i, s1, COLLECT(ot.name) AS s2 ",
                 " WITH m,other,intersection,s1,s2 ",
                 " WITH DISTINCT m,other,intersection,s1+[x IN s2 WHERE NOT x IN s1] AS union, apoc.text.join(s1, '|') AS s1_txt, apoc.text.join(s2, '|') AS s2_txt ",
                 " WITH '\\''+m.movieId+'\\'' AS source_id, other.movieId AS movie_id, other.title AS title, other.avg_rating AS avg_rating, other.poster AS poster,((1.0*intersection)/SIZE(union)) AS jaccard ",
                 " WITH collect(source_id) AS source_id_list, movie_id, title, avg_rating, poster, sum(jaccard) AS jaccard_sum ",
                 " WITH apoc.text.join(source_id_list, ',') AS source_id_csv, movie_id, title, avg_rating, poster, jaccard_sum AS score",
                 " RETURN source_id_csv, movie_id, title, avg_rating, poster, score ",
                 " ORDER BY score DESC LIMIT ", recLimit, sep="")
  print(query)
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
  query <- paste(" MATCH (u1:Person {loginId: ",loginID,"})-[x:REVIEWED]->(movie:Movie) ",
                 " WITH u1, gds.alpha.similarity.asVector(movie, x.rating) AS u1Vector ",
                 " MATCH (u2:Person)-[x2:REVIEWED]->(movie:Movie) WHERE u2 <> u1 ",
                 " WITH u1, u2, u1Vector, gds.alpha.similarity.asVector(movie, x2.rating) AS u2Vector ",
                 " WHERE size(apoc.coll.intersection([v in u1Vector | v.category], [v in u2Vector | v.category])) > 10 ",
                 " WITH u1, u2,  gds.alpha.similarity.pearson(u1Vector, u2Vector, {vectorType: 'maps'}) AS similarity ",
                 " ORDER BY similarity DESC ",
                 " LIMIT 10 ",
                 " MATCH (u2)-[r:REVIEWED]->(m:Movie) WHERE NOT EXISTS( (u1)-[:REVIEWED]->(m) ) ",
                 " RETURN DISTINCT u1.loginId AS u1_loginId, u2.loginId AS u2_loginId, m.movieId AS movie_id, m.title AS title, m.avg_rating AS avg_rating, m.poster AS poster, SUM( similarity * r.rating) AS score ",
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
# Function getCollaborativeFilteringMovies
#
# Description:  Return a neo list that contains the graph data of the movies that are similar to users' favorite movies. 
#               Using "memoise" to automatically cache the results
# Input:        userID - User ID
#               recLimit - number of records to be returned
# Output:       Return a neo list that contains the graph data of recently rated movies by the given user. 
###############################################################################################
getActorMovies <- memoise(function(loginID, recLimit) {
  
  print("global.R - getActorMovies")
  print(loginID)
  print(recLimit)
  
  # loginID <- 4
  # recLimit <- 10
  # query <- paste(" MATCH (u:Person {loginId: ",loginID,"})-[r:REVIEWED]->(m:Movie) ",
  #                " WITH m, r, u ",
  #                " ORDER BY r.rating DESC LIMIT 20 ",
  #                " MATCH (m)<-[:ACTED_IN]-(a:Person)-[:ACTED_IN]->(rec) ",
  #                " WHERE NOT EXISTS( (u)-[:REVIEWED]->(rec)) ",
  #                " WITH DISTINCT m, rec, COUNT(a) AS as ",
  #                " WITH collect(m.movieId) as source_id, rec, sum(as) AS as1 ",
  #                " WITH apoc.text.join(source_id, ',') AS source_id_csv, rec, as1 ",
  #                " RETURN source_id_csv, rec.movieId AS movie_id, rec.title AS title, rec.avg_rating AS avg_rating, rec.poster AS poster, as1 AS score ORDER BY score DESC LIMIT ",recLimit, sep="")
  # 
  query <- paste( " MATCH (u:Person {loginId: ",loginID,"})-[r:REVIEWED]->(m:Movie) ",
                  " WITH m, r, u ",
                  " ORDER BY r.rating DESC LIMIT 20 ",
                  " MATCH (m)<-[:ACTED_IN]-(a:Person)-[:ACTED_IN]->(rec) ",
                  " WHERE NOT EXISTS( (u)-[:REVIEWED]->(rec)) ",
                  " WITH DISTINCT m, rec, COUNT(a) AS as ",
                  " WITH '\\''+m.movieId+'\\'' AS source_id, rec, as ",
                  " WITH collect(source_id) as source_id, rec, sum(as) AS as1 ",
                  " WITH apoc.text.join(source_id, ',') AS source_id_csv, rec, as1 ",
                  " RETURN source_id_csv, rec.movieId AS movie_id, rec.title AS title, rec.avg_rating AS avg_rating, rec.poster AS poster, as1 AS score ORDER BY score DESC LIMIT ",recLimit, sep="")
  
  
  print(query)
  R <- query %>% 
    call_neo4j(con, type = "row") 
  
  return (R)
})


# m <- getActorMovies(1,10)
# p<-m$rec.title[1,]$value
# print(p)

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
# Description:  Return a matrix that contains genres of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
#               min_rating - Minimum average ratings that the movie should have
#               max_rating - Maximum average ratings that the movie should have
# Output:       Return a matrix that contains genres of the movies that satisfies the input criteria.
###############################################################################################
# getTermMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
#   
#   print("global.R - getTermMatrix")
#   print(from_year)
#   print(to_year)
#   print(min_rating)
#   print(max_rating)
#   to_year1<-to_year+1
#   
#   query <- paste(" MATCH (g:Genre)-[HAS_GENRE]-(m:Movie) ",
#                  " WHERE m.year >= ",from_year," and m.year <= ",to_year1," ",
#                  " AND m.avg_rating >= ",min_rating," and m.avg_rating <= ",max_rating," ",
#                  " RETURN g.name as genre", sep="")
#   
#   print(query)
#   R <- query %>% 
#     call_neo4j(con, type = "row") 
#   
#   # print(min(genres_df$year))
#   # print(max(genres_df$year))
#   if (nrow(R$genre) == 0 ){
#     text <- c("NOTHING")
#   }
#   else {
#     text <- as.vector(R$genre[,1]$value)
#   }
#   
#   myCorpus = Corpus(VectorSource(text))
#   myDTM = TermDocumentMatrix(myCorpus,
#                              control = list(minWordLength = 1))
#   
#   m = as.matrix(myDTM)
#   
#   sort(rowSums(m), decreasing = TRUE)
# })
 # t<-getTermMatrix(1905,2018,1,5)
 # print(t)
 # 
 # t<-getTermMatrix1(1905,2018,1,5)
 # print(t)

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
# getTagMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
#   
#   print("global.R - getTagMatrix")
#   print(from_year)
#   print(to_year)
#   print(min_rating)
#   print(max_rating)
#   to_year1<-to_year+1
#   
#   query <- paste(" MATCH (p:Person)-[t:TAGGED]-(m:Movie) ",
#                  " WHERE m.year >= ",from_year," and m.year <= ",to_year1," ",
#                  " AND m.avg_rating >= ",min_rating," and m.avg_rating <= ",max_rating," ",
#                  " RETURN t.tag as tag", sep="")
#   
#   print(query)
#   R <- query %>% 
#     call_neo4j(con, type = "row") 
#   
#   # print(min(genres_df$year))
#   # print(max(genres_df$year))
#   if (nrow(R$tag) == 0 ){
#     text <- c("NOTHING")
#   }
#   else {
#     text <- as.vector(R$tag[,1]$value)
#   }
#   
#   myCorpus = Corpus(VectorSource(text))
#   myDTM = TermDocumentMatrix(myCorpus,
#                              control = list(minWordLength = 1))
#   
#   m = as.matrix(myDTM)
#   
#   sort(rowSums(m), decreasing = TRUE)
# })

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
# Function getRatingHistogramDF
#
# Description:  Return a data frame that contains ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains ratings of the movies that satisfies the input criteria. 
###############################################################################################
# getRatingHistogramDF <- memoise(function(from_year, to_year) {
#   
#   print("global.R - getRatingHistogramDF")
#   print(from_year)
#   print(to_year)
#   to_year1<-to_year+1
#   
#   # from_year <- 2000
#   # to_year1 <- 2002
#   # min_rating <- 1
#   # max_rating <- 5
#   
#   query <- paste(" MATCH (u:Person)-[r:REVIEWED]->(m:Movie) ",
#                  " WHERE m.year >= ",from_year," and m.year <= ",to_year1," ",
#                  " RETURN u.loginId as userId, m.movieId as movieId, m.avg_rating as avg_rating, r.rating as rating, r.timestamp as timestamp, m.year as year", sep="")
#   
#   print(query)
#   R <- query %>% 
#     call_neo4j(con, type = "row") 
#   
#   print("# of ratings_avg_df1")
#   print(nrow(R$movieId))
#   
#   rating_df1 <- data.frame(
#     userId = R$userId[,1]$value,
#     movieId = R$movieId[,1]$value,
#     avgRating = R$avg_rating[,1]$value,
#     rating = R$rating[,1]$value,
#     timestamp = R$timestamp[,1]$value,
#     year = R$year[,1]$value
#   )
#   return(rating_df1)
# })
#t <- getRatingHistogramDF(1907,2002)
#print(t)

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
# Function getAvgRatingHistogramDF
#
# Description:  Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
###############################################################################################
# getAvgRatingHistogramDF <- memoise(function(from_year, to_year) {
#   
#   print("global.R - getAvgRatingHistogramDF")
#   print(from_year)
#   print(to_year)
#   to_year1<-to_year+1
#   
#   # from_year <- 2000
#   # to_year1 <- 2002
#   # min_rating <- 1
#   # max_rating <- 5
#   
#   query <- paste(" MATCH (m:Movie) ",
#                  " WHERE m.year >= ",from_year," and m.year <= ",to_year1," ",
#                  " RETURN m.movieId as movieId, m.avg_rating as avg_rating, m.year as year", sep="")
#   
#   print(query)
#   R <- query %>% 
#     call_neo4j(con, type = "row") 
#   
#   print("# of ratings_avg_df1")
#   print(nrow(R$movieId))
#   
#   rating_avg_df1 <- data.frame(
#     movieId = R$movieId[,1]$value,
#     avgRating = R$avg_rating[,1]$value,
#     year = R$year[,1]$value
#   )
#   return(rating_avg_df1)
# })

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
# getTop10MovieDF <- memoise(function(from_year, to_year, minRatingCnt) {
#   
#   print("global.R - top10Movie")
#   
#   top10_df1 <- ratings_df %>%
#     filter(between(ratedYear, from_year, to_year))
#   
#   # Find the average rating for each movie in each year
#   top10_df2 <- aggregate(top10_df1[,3], list(top10_df1$movieId), mean)
#   colnames(top10_df2)<- c("movieId", "avgRating")
#   
#   top10_df3 <- top10_df1 %>%
#     group_by(movieId) %>%
#     summarise(ratingCnt=n())
#   
#   top10_df4 <- left_join(top10_df2,top10_df3,c("movieId"))
#   
#   # Remove the movies that has number of ratings less than minRatingCnt
#   top10_df5 <- top10_df4 %>%
#     filter(ratingCnt>=minRatingCnt)
#   
#   top10_df6 <- top10_df5 %>%
#     arrange(desc(top10_df5$avgRating, top10_df5$ratingCnt))
#   
#   top10_df7 <- head(top10_df6,10)
#   
#   return(top10_df7)
# })

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

  print("global.R - getTop10MovieDF")
  print(from_year)
  print(to_year)
  print(minRatingCnt)
  
  # loginID <- 4
  # recLimit <- 10
  to_year1 = to_year + 1
  
  query <- paste(" MATCH (m:Movie)-[r:REVIEWED]-(u:Person) ",
                 " WITH m,r, datetime({year: ",from_year,"}) as y1, datetime({year: ",to_year1,"}) as y2 ",
                 " WHERE r.timestamp >= y1.epochSeconds and r.timestamp < y2.epochSeconds ",
                 " WITH m.title AS title, m.poster as poster, AVG(r.rating) AS avg_rating, COUNT(r.rating) as rating_cnt ",
                 " WHERE rating_cnt >= ", minRatingCnt,
                 " RETURN title, poster, avg_rating, rating_cnt ",
                 " ORDER BY avg_rating DESC LIMIT 10", sep="")
  
  print(query)
  R <- query %>% 
    call_neo4j(con, type = "row") 
  
  return (R)
})

# m<-getTop10MovieDF(2000,2001,5)
# head(m)

###############################################################################################
# Function getContentBasedMovieGraph
#
# Description:  Return a neo list that contains the graph data of the movies that are similar to users' favorite movies
#               The output returned can be used in visNetwork to display the graph
#               Using "memoise" to automatically cache the results
# Input:        sourceMovieIdCsv - CSV list of Movie ID of the Source Movie
#               recMovieId - Movie ID of the Recommended Movie
# Output:       Return a neo list that contains the graph data of the movies that are similar to users' favorite movies 
###############################################################################################
getContentBasedMovieGraph <- memoise(function(sourceMovieIdCsv, recMovieId) {
  
  print("global.R - getContentBasedMovieGraph")

  
  # loginID <- 4
  # recLimit <- 10
  query <- paste('MATCH a=(m:Movie)-[:HAS_GENRE|ACTED_IN|DIRECTED|PRODUCED_BY|PRODUCED_IN]-(t)-[:HAS_GENRE|ACTED_IN|DIRECTED|PRODUCED_BY|PRODUCED_IN]-(other:Movie {movieId: "',recMovieId,'"}) WHERE m.movieId IN [',sourceMovieIdCsv,'] return a', sep="")
  print(query)
  G <- query %>% 
    call_neo4j(con, type = "graph") 
  
  # We'll just unnest the properties
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties")
  
  # Add a new column
  G$nodes$group <- unlist(G$nodes$label)  
  #G$nodes$shape <- "dot" 
  # G$nodes$image <- "" 
  # G$nodes$label <- G$nodes$title
  # 
  # #G$nodes[G$nodes$group=="Movie",]$shape = "image"
  # G$nodes[G$nodes$group=="Movie",]$image = G$nodes[G$nodes$group=="Movie",]$poster
  # G$nodes[G$nodes$group=="Person",]$image = "user_icon_red.png"
  # G$nodes[G$nodes$group=="Person",]$label = G$nodes[G$nodes$group=="Person",]$name
  # G$nodes[G$nodes$group=="Company",]$image = "film_company_icon_green.png"
  # G$nodes[G$nodes$group=="Company",]$label = G$nodes[G$nodes$group=="Company",]$name
  # G$nodes[G$nodes$group=="Genre",]$image = "genre_icon_pink.png"
  # G$nodes[G$nodes$group=="Genre",]$label = G$nodes[G$nodes$group=="Genre",]$name
  # G$nodes[G$nodes$group=="Country",]$image = "country_icon_blue.png"  
  # G$nodes[G$nodes$group=="Country",]$label = G$nodes[G$nodes$group=="Country",]$name
  # G$nodes$title <- G$nodes$group
  # G$nodes$title <- paste('<p style="color:Black;font-size:14px">',G$nodes$group,'</p>')
  
  # Turn the relationships :
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    select(from = startNode, to = endNode, label = type)
  
  return (G)
})

###############################################################################################
# Function getCollaborativeFilteringMovieeGraph
#
# Description:  Return a neo list that contains the graph data of the movies that the other users who are similar to user like
#               The output returned can be used in visNetwork to display the graph
#               Using "memoise" to automatically cache the results
# Input:        u1_loginId - Login ID of the user1
#               u2_loginId - Login ID of the user who is similar to the user1
#               recMovieId - Movie ID of the Recommended Movie
# Output:       Return a neo list that contains the graph data of the movies that the other users who are similar to user like
###############################################################################################
getCollaborativeFilteringMovieeGraph <- memoise(function(u1_loginId, u2_loginId, recMovieId) {
  
  print("global.R - getContentBasedMovieGraph")
  
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste('MATCH a=(u1:Person {loginId: ',u1_loginId,'})-[x:REVIEWED]->(movie:Movie)<-[y:REVIEWED]-(u2:Person {loginId: ',u2_loginId,'})-[z:REVIEWED]->(rec:Movie {movieId: "',recMovieId,'"}) return a', sep="")
  print(query)
  G <- query %>% 
    call_neo4j(con, type = "graph") 
  
  # We'll just unnest the properties
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties")
  
  # Add a new column
  G$nodes$group <- unlist(G$nodes$label)  
  #G$nodes$shape <- "dot" 
  # G$nodes$image <- "" 
  # G$nodes$label <- G$nodes$title
  # 
  # #G$nodes[G$nodes$group=="Movie",]$shape = "image"
  # G$nodes[G$nodes$group=="Movie",]$image = G$nodes[G$nodes$group=="Movie",]$poster
  # G$nodes[G$nodes$group=="Person",]$image = "www/user_icon_red.png"
  # G$nodes[G$nodes$group=="Person",]$label = G$nodes[G$nodes$group=="Person",]$name
  # G$nodes[G$nodes$group=="Company",]$label = G$nodes[G$nodes$group=="Company",]$name
  # G$nodes[G$nodes$group=="Genre",]$label = G$nodes[G$nodes$group=="Genre",]$name
  # G$nodes[G$nodes$group=="Country",]$label = G$nodes[G$nodes$group=="Country",]$name
  # G$nodes$title <- G$nodes$group
  # G$nodes$title <- paste('<p style="color:Black;font-size:14px">',G$nodes$group,'</p>')
  
  # Turn the relationships :
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    select(from = startNode, to = endNode, label = rating)
  G$relationships$label <- paste("Rating: ",G$relationships$label)

  return (G)
})

#k<-getCollaborativeFilteringMovieeGraph(1,210,122886)

###############################################################################################
# Function getActorMovieGraph
#
# Description:  Return a neo list that contains the graph data of the movies that are acted by the same actors of user's favorite movies
#               The output returned can be used in visNetwork to display the graph
#               Using "memoise" to automatically cache the results
# Input:        sourceMovieIdCsv - CSV list of Movie ID of the Source Movie
#               recMovieId - Movie ID of the Recommended Movie
# Output:       Return a neo list that contains the graph data of the movies that are acted by the same actors of user's favorite movies 
###############################################################################################
getActorMovieGraph <- memoise(function(sourceMovieIdCsv, recMovieId) {
  
  print("global.R - getActorMovieGraph")
  
  
  # loginID <- 4
  # recLimit <- 10
  query <- paste('MATCH a=(m:Movie)-[:ACTED_IN]-(t)-[:ACTED_IN]-(other:Movie {movieId:"',recMovieId,'"}) WHERE m.movieId IN [',sourceMovieIdCsv,'] return a', sep="")
  print(query)
  G <- query %>% 
    call_neo4j(con, type = "graph") 
  
  # We'll just unnest the properties
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties")
  
  # Add a new column
  G$nodes$group <- unlist(G$nodes$label)  
  #G$nodes$shape <- "dot" 
  # G$nodes$image <- "" 
  # G$nodes$label <- G$nodes$title
  # 
  # #G$nodes[G$nodes$group=="Movie",]$shape = "image"
  # G$nodes[G$nodes$group=="Movie",]$image = G$nodes[G$nodes$group=="Movie",]$poster
  # G$nodes[G$nodes$group=="Person",]$label = G$nodes[G$nodes$group=="Person",]$name
  # G$nodes[G$nodes$group=="Company",]$label = G$nodes[G$nodes$group=="Company",]$name
  # G$nodes[G$nodes$group=="Genre",]$label = G$nodes[G$nodes$group=="Genre",]$name
  # G$nodes[G$nodes$group=="Country",]$label = G$nodes[G$nodes$group=="Country",]$name
  # G$nodes$title <- paste('<p style="color:Black;font-size:14px">',G$nodes$group,'</p>')
  
  # Turn the relationships :
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    select(from = startNode, to = endNode, label = type)
  
  return (G)
})
