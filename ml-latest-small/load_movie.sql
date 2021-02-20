:auto USING PERIODIC COMMIT 500

//Remove all nodes and relationships
//MATCH (n) DETACH DELETE n

// Create movies
LOAD CSV WITH HEADERS FROM 'file:///movie/movies1.csv' AS row
MERGE (m:Movie {movieId: row.movieId})
  ON CREATE SET m.movieId = row.movieId, m.title = row.title, m.year = row.year, m.poster = row.poster
WITH m, row
UNWIND split(row.genres, '|') AS genre
MERGE (g:Genre {name: genre})
MERGE (m)-[r:HAS_GENRE]->(g);

CREATE CONSTRAINT MovieUC1 ON (m:Movie) ASSERT m.movieId IS UNIQUE;
CREATE INDEX MovieIDX1 FOR (m:Movie) ON (m.title, m.avg_rating);
CREATE INDEX MovieIDX2 FOR (m:Movie) ON (m.year, m.avg_rating);

//DROP CONSTRAINT ON (m:Movie) ASSERT m.movieId IS UNIQUE;
//DROP INDEX ON :Movie(title)

//Create tags
LOAD CSV WITH HEADERS FROM 'file:///movie/tags.csv' AS row
MATCH (m:Movie {movieId: row.movieId})
MERGE (p:Person {name: "User"+row.userId, loginId: apoc.convert.toInteger(row.userId)})
  ON CREATE SET p.name = "User"+row.userId, p.loginId = apoc.convert.toInteger(row.userId)
WITH p, m, row
MERGE (p)-[t:TAGGED]->(m)
  ON CREATE SET t.tag=row.tag, t.timestamp=apoc.convert.toInteger(row.timestamp);

//Create users
LOAD CSV WITH HEADERS FROM 'file:///movie/ratings.csv' AS row
MATCH (m:Movie {movieId: row.movieId})
MERGE (p:Person {name: "User"+row.userId, loginId: apoc.convert.toInteger(row.userId)})
  ON CREATE SET p.name = "User"+row.userId, p.loginId = apoc.convert.toInteger(row.userId)
WITH p, m, row
MERGE (p)-[r:REVIEWED]->(m)
  ON CREATE SET r.rating=row.rating, r.timestamp=row.timestamp;

CREATE CONSTRAINT PersonUC1 ON (p:Person) ASSERT p.loginId IS UNIQUE;
CREATE INDEX PersonIDX1 FOR (p:Person) ON (p.name);

//DROP CONSTRAINT ON (p:Person) ASSERT p.loginId IS UNIQUE;
//DROP INDEX ON :Person(name)

//Load Links
LOAD CSV WITH HEADERS FROM 'file:///movie/links.csv' AS row
MERGE (m:Movie {movieId: row.movieId})
  ON MATCH SET m.imdbId = row.imdbId, m.tmdbId = row.tmdbId

MATCH(n:Movie) where size(n.title) > 7
with n 
set n.title=substring(n.title,0,size(n.title)-7)

CREATE INDEX ReviewIDX1 FOR (r:REVIEWED) ON (r.timestamp);

// Create Actors
match(m:Movie) 
WITH "http://www.omdbapi.com/?t="+replace(m.title," ","+")+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND split(value.Actors,',') AS actor
MERGE (p:Person {name: rtrim(ltrim(actor))})
MERGE (p)-[r:ACTED_IN]->(m)
return m.title, value.Actors;

match(m:Movie) 
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND split(value.Actors,',') AS actor
MERGE (p:Person {name: rtrim(ltrim(actor))})
MERGE (p)-[r:ACTED_IN]->(m)
return m.title, value.Actors;

// Create Director and Country
match(m:Movie) 
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
MERGE (p:Person {name: rtrim(ltrim(value.Director))})
MERGE (p)-[r1:DIRECTED]->(m)
return value.Director;

// Create Country
match(m:Movie) 
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND split(value.Country,',') AS country
MERGE (c:Country {name: rtrim(ltrim(country))})
MERGE (m)-[r2:PRODUCED_IN]->(c)
return country;

// Create Company
match(m:Movie) 
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND split(value.Production,',') AS production
MERGE (c:Company {name: rtrim(ltrim(production))})
MERGE (m)-[r:PRODUCED_BY]->(c)
return production;

// Create Poster
match(m:Movie)
WHERE m.title starts with "Seven ("
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND split(value.Poster,',') AS poster
SET m.poster = poster
return poster;

// Create Plot
match(m:Movie)
WITH "http://www.omdbapi.com/?i=tt"+m.imdbId+"&apikey=35b0b051" AS url, m
CALL apoc.load.json(url) YIELD value
UNWIND value.Plot AS plot
SET m.plot = plot
return plot;

// Convert the timestamp in REVIEWED relationship
match (m:Movie)-[r:REVIEWED]-(p:Person) set r.timestamp = apoc.convert.toInteger(r.timestamp);
match (m:Movie)-[r:REVIEWED]-(p:Person) set r.review_date = apoc.date.format(apoc.convert.toInteger(r.timestamp),"s","yyyy-MM-dd HH:mm:ss zzz");

// Convert the rating to Float
match (m:Movie)-[r:REVIEWED]-(p:Person) set r.rating = apoc.convert.toFloat(r.rating);

// Calculate the average rating
call {match (m:Movie)-[r:REVIEWED]-(p:Person)  return m.title as title1, round(avg(r.rating),1) AS avg_rating1} 
return title1, avg_rating1

call {match (m:Movie)-[r:REVIEWED]-(p:Person)  return m.movieId as movieId1, m.title as title1, round(avg(r.rating),1) AS avg_rating1} 
match (m1:Movie {movieId:movieId1}) set m1.avg_rating = avg_rating1

// Convert the movie production year to Integer
match (m:Movie) set m.year = apoc.convert.toInteger(m.year);

// Convert the loginId to Integer
match(p:Person) set p.loginId = apoc.convert.toInteger(p.loginId)

MATCH (m:Movie)-[r:REVIEWED]-(p:Person {loginId: 1 }) 
WITH m,r,p,apoc.date.currentTimestamp()/1000 as ts
WHERE m.movieId = "260" SET r.rating = 4.0, r.timestamp=ts, r.review_date = apoc.date.format(apoc.convert.toInteger(ts),"s","yyyy-MM-dd HH:mm:ss zzz")