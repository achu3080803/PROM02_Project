## app.R ##
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(RCurl)
library(rjson)


source("./global.R", local=TRUE)

###############################################################################################
# UI (START)
###############################################################################################
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title = "Movie Recommender"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommended Movies", tabName = "recommended_movies", icon = icon("film")),
      menuItem("Basic Analysis", tabName = "analysis", icon = icon("th")),
      menuItem("Deep Analysis", tabName = "graph", icon = icon("th"))
    )
  ),
  dashboardBody(
    includeCSS("stars.css"),
    tags$style(HTML("
                    .content, .container-fluid {color: #fff; background-color: #000;}
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#000000
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#00000;
                    border-left-color:#000000;
                    border-right-color:#000000;
                    border-top-color:#000000;
                    background:#000000
                    }
                    
                    #sidebar {
                    background-color: #000000;
                    }

    ")),
    tabItems(
      tabItem(
              tabName = "recommended_movies",
              fluidRow(
                #selectizeInput("selectUser", "Login as", unique(ratings_df$userId))
                selectizeInput("selectUser", "Login as", all_userid_df$value)
              ),
              # fluidRow(
              #   h1("Recently rated by you"),
              #   htmlOutput("watchedMovies")
              # ),
              fluidRow(
                h1("Your favorite movies"),
                htmlOutput("favoriteMovies")
              ),
              fluidRow(
                h1("Movies similar to what you like"),
                htmlOutput("cbMovies")
              ),
              fluidRow(
                h1("Others similar to you like these movies"),
                htmlOutput("cfPopularMovies")
              ),
              fluidRow(
                h1("Movies with your favorite actors / actresses"),
                htmlOutput("cbActorMovies")
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                htmlOutput("top10Movies")
              ),
              fluidRow(
                tags$style(".box {background-color:#000000;}"),
                box( 
                  width = 10,
                  sliderInput("ratedYear",
                              "Year of Ratings:",
                              step=1,
                              min = min(as.integer(ratings_df$ratedYear)),  max = max(as.integer(ratings_df$ratedYear)), value = c(min(as.integer(ratings_df$ratedYear)),max(as.integer(ratings_df$ratedYear)))
                  ),
                  sliderInput("minRatingCnt",
                              "Minimum Number of Ratings:",
                              step=1,
                              min = 0,  max = 50, value = c(25)
                  )
                )
              ),
              fluidRow(
                h1("Analysis on Movie Genres and Tags"),
                sidebarLayout(
                  # Sidebar with a slider and selection inputs
                  sidebarPanel(
                    tags$style(".well {background-color:#000000;}"),
                    sliderInput("year",
                                "Production Year:",
                                min = min(genres_df$year),  max = max(genres_df$year), value = c(min(genres_df$year),max(genres_df$year))),
                    sliderInput("rating",
                                "Average Rating:",
                                min = 1,  max = 5, value = c(1,5)),
                    actionButton("update", "Update"),
                    hr(),
                    sliderInput("freq",
                                "Minimum Frequency:",
                                min = 1,  max = 50, value = 15),
                    sliderInput("max",
                                "Maximum Number of Words:",
                                min = 1,  max = 30,  value = 100)
                  ),
                  # Show Word Cloud
                  mainPanel(
                    column(5,
                           h1("Movie Genres"),
                           plotOutput("genreWordCloud")
                    ),
                    column(5,
                           h1("Movie Tags"),
                           plotOutput("tagWordCloud")
                    )
                  )
                )
              ),
              fluidRow(
                h1("Analysis on Ratings")
                ,
                sidebarLayout(
                  # Sidebar with a slider and selection inputs
                  sidebarPanel(
                    tags$style(".well {background-color:#000000;}"),
                    sliderInput("year2",
                                "Production Year:",
                                min = min(ratings_df$year),  max = max(ratings_df$year), value = c(min(ratings_df$year),max(ratings_df$year)))
                  ),
                  # Show Rating Distribution
                  mainPanel(
                    column(5,
                           h1("Historgram of Movie Ratings"),
                           plotOutput("ratingHistogram")
                    ),
                    column(5,
                           h1("Distribution of Average Movie Ratings"),
                           plotOutput("avgRatingHistogram")
                    )
                  )
                ),
                hr(),
                h1("Average Number of Ratings per User"),
                box(width=12, 
                    plotOutput("userRating")
                )
              )
              
      ),
      tabItem(
        tabName = "graph",
        fluidRow(
          h1("Deep Analysis"),
          #selectizeInput("selectUser", "Login as", unique(ratings_df$userId))
          selectizeInput("selectUser1", "User", all_userid_df$value),
          selectizeInput("selectCategory", "Category", c("Movies similar to what you like","Others similar to you like these movies","Movies with your favorite actors / actresses"))
        ),
        fluidRow(
          htmlOutput("selectedMovieCategory"),
          x <- uiOutput('radioTest'),
          actionButton('submit', label = "Submit"),
          br(),
          #print(paste("Radiobutton response is:", "reply")),
          textOutput('text')
        ),
         # visNetworkOutput("myNetId",
         #                  height <- "1200",
         #                  width <- "600"),
         visNetworkOutput("movieGraph",
                         height <- "1200",
                         width <- "1200")
        #)
      )
    )
  )
)
###############################################################################################
# UI (END)
###############################################################################################

###############################################################################################
# Server (START)
###############################################################################################
server <- function(input, output) {
  
  
  poster.height=200
  poster.width=150
  omdbapi.key="35b0b051"

  set.seed(122)
  histdata <- rnorm(500)
  
  ###############################################################################################
  #
  # Recommender TAB (START)
  #
  ###############################################################################################

  ###############################################################################################
  # Function get_curr_login
  #
  # Description:  Return the current selected USER ID
  # Input:        
  # Output:       Return the current selected USER ID
  ###############################################################################################
  get_curr_login <- reactive({
    return(input$selectUser)
  })
  
  ###############################################################################################
  # Function output$watchedMovies
  #
  # Description:  Return the movie posters of the movies that are rated by the user
  # Input:        
  # Output:       Return the movie posters of the movies that are rated by the user
  ###############################################################################################
  # output$watchedMovies<-renderText({
  #   #print("Hello")
  #   currUser=get_curr_login()
  #   m_posters=NULL
  #   m <- get_watched_movies(currUser)
  #   print(currUser)
  #   print(m)
  #   m_posters <- append(m_posters, '<div>')
  #   for (i in 1:length(m)){
  #     movie_title <- get_movie_title(m[i],FALSE)
  #     movie_rating <- get_movie_rating(m[i])
  #     star_rating <- movie_rating/5 * 100
  #     m_posters <- append(m_posters, '<div class="gallery">')
  #     m_posters <- append(m_posters, paste0('<img src="',get_movie_url(m[i]),'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
  #     m_posters <- append(m_posters, '<div class="ratings">')
  #     m_posters <- append(m_posters, '<div class="empty-stars"></div>')
  #     m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
  #     m_posters <- append(m_posters, '</div>')
  #     m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
  #     m_posters <- append(m_posters, '</div>')
  #   }
  #   m_posters <- append(m_posters, '</div>')
  #   print(m_posters)
  # })
  
  ###############################################################################################
  # Function output$watchedMovies
  #
  # Description:  Return the movie posters of the movies that are rated by the user
  # Input:        
  # Output:       Return the movie posters of the movies that are rated by the user
  ###############################################################################################
  output$watchedMovies<-renderText({
    print("watchedMovies")
    currUser=get_curr_login()
    m_posters=NULL
    m_graph <- getRecentlyRatedMovies(currUser,10)
    m_movies <- m_graph$nodes[m_graph$nodes[]$node_type=="Movie",]
    
    #print(currUser)
    #print(length(m_graph$nodes))
    l <- nrow(m_movies)
    m_posters <- append(m_posters, '<div>')
    for (i in 1:nrow(m_movies)){
      movie_title <- m_movies[i,]$movie_title
      movie_poster <- m_movies[i,]$poster
      movie_rating <- m_movies[i,]$user_rating
      star_rating <- movie_rating/5 * 100
      m_posters <- append(m_posters, '<div class="gallery">')
      m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
       m_posters <- append(m_posters, '<div class="ratings">')
       m_posters <- append(m_posters, '<div class="empty-stars"></div>')
       m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
       m_posters <- append(m_posters, '</div>')
      m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
      m_posters <- append(m_posters, '</div>')
    }
    m_posters <- append(m_posters, '</div>')
    print(m_posters)
  })
  
  ###############################################################################################
  # Function output$favoriteMovies
  #
  # Description:  Return the movie posters of the movies that are rated by the user
  # Input:        
  # Output:       Return the movie posters of the movies that are rated by the user
  ###############################################################################################
  output$favoriteMovies<-renderText({
    print("favoriteMovies")
    currUser=get_curr_login()
    m_posters=NULL
    m_graph <- getFavoriteMovies(currUser,10)
    m_movies <- m_graph$nodes[m_graph$nodes[]$node_type=="Movie",]
    
    #print(currUser)
    #print(length(m_graph$nodes))
    l <- nrow(m_movies)
    m_posters <- append(m_posters, '<div>')
    for (i in 1:nrow(m_movies)){
      movie_title <- m_movies[i,]$movie_title
      movie_poster <- m_movies[i,]$poster
      movie_rating <- m_movies[i,]$user_rating
      star_rating <- movie_rating/5 * 100
      m_posters <- append(m_posters, '<div class="gallery">')
      m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
      m_posters <- append(m_posters, '<div class="ratings">')
      m_posters <- append(m_posters, '<div class="empty-stars"></div>')
      m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
      m_posters <- append(m_posters, '</div>')
      m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
      m_posters <- append(m_posters, '</div>')
    }
    m_posters <- append(m_posters, '</div>')
    print(m_posters)
  })
  
  ###############################################################################################
  # Function output$cbMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the Content-based Recommender
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the Content-based Recommender
  ###############################################################################################
  # output$cbMovies<-renderText({
  #   #print("Hello")
  #   currUser=get_curr_login()
  #   m_posters=NULL
  #   m <- get_cb_movies(currUser)
  #   #print(currUser)
  #   #print(m)
  #   m_posters <- append(m_posters, '<div>')
  #   for (i in 1:length(m)){
  #     movie_title <- get_movie_title(m[i],FALSE)
  #     movie_rating <- get_movie_avg_rating(m[i])
  #     star_rating <- movie_rating/5 * 100
  #     m_posters <- append(m_posters, '<div class="gallery">')
  #     m_posters <- append(m_posters, paste0('<img src="',get_movie_url(m[i]),'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
  #     m_posters <- append(m_posters, 'Avg. <div class="ratings">')
  #     m_posters <- append(m_posters, '<div class="empty-stars"></div>')
  #     m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
  #     m_posters <- append(m_posters, '</div>')
  #     m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
  #     m_posters <- append(m_posters, '</div>')
  #   }
  #   m_posters <- append(m_posters, '</div>')
  #   print(m_posters)
  # })
  
  ###############################################################################################
  # Function output$cbMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the Content-based Recommender
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the Content-based Recommender
  ###############################################################################################
  output$cbMovies<-renderText({
    print("cbMovies")
    currUser=get_curr_login()
    m_posters=NULL
    m_movies <- getContentBasedMovies(currUser,10)
    
    #print(currUser)
    #print(length(m_graph$nodes))
    m_posters <- append(m_posters, '<div>')
    for (i in 1:nrow(m_movies$title)){
      movie_title <- m_movies$title[i,]$value
      movie_poster <- m_movies$poster[i,]$value
      movie_rating <- m_movies$avg_rating[i,]$value
      star_rating <- movie_rating/5 * 100
      m_posters <- append(m_posters, '<div class="gallery">')
      m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
      m_posters <- append(m_posters, '<div class="ratings">')
      m_posters <- append(m_posters, '<div class="empty-stars"></div>')
      m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
      m_posters <- append(m_posters, '</div>')
      m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
      m_posters <- append(m_posters, '</div>')
    }
    m_posters <- append(m_posters, '</div>')
    print(m_posters)
  })
  
  ###############################################################################################
  # Function output$cfPopularMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the Popular Recommender
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the Popular Recommender
  ###############################################################################################
  # output$cfPopularMovies<-renderText({
  #   #print("Hello")
  #   currUser=get_curr_login()
  #   m_posters=NULL
  #   m <- get_cf_popular_movies(currUser)
  #   print(currUser)
  #   print(m)
  #   m_posters <- append(m_posters, '<div>')
  #   for (i in 1:length(m)){
  #     movie_title <- get_movie_title(m[i],FALSE)
  #     movie_rating <- get_movie_avg_rating(m[i])
  #     star_rating <- movie_rating/5 * 100
  #     m_posters <- append(m_posters, '<div class="gallery">')
  #     m_posters <- append(m_posters, paste0('<img src="',get_movie_url(m[i]),'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
  #     m_posters <- append(m_posters, 'Avg. <div class="ratings">')
  #     m_posters <- append(m_posters, '<div class="empty-stars"></div>')
  #     m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
  #     m_posters <- append(m_posters, '</div>')
  #     m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
  #     m_posters <- append(m_posters, '</div>')
  #   }
  #   m_posters <- append(m_posters, '</div>')
  #   print(m_posters)
  # })
  
  ###############################################################################################
  # Function output$cfPopularMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the Content-based Recommender
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the Content-based Recommender
  ###############################################################################################
  output$cfPopularMovies<-renderText({
    print("cfPopularMovies")
    currUser=get_curr_login()
    m_posters=NULL
    m_movies <- getCollaborativeFilteringMovies(currUser,10)
    
    #print(currUser)
    #print(length(m_graph$nodes))
    m_posters <- append(m_posters, '<div>')
    for (i in 1:nrow(m_movies$title)){
      movie_title <- m_movies$title[i,]$value
      movie_poster <- m_movies$poster[i,]$value
      movie_rating <- m_movies$avg_rating[i,]$value
      star_rating <- movie_rating/5 * 100
      m_posters <- append(m_posters, '<div class="gallery">')
      m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
      m_posters <- append(m_posters, '<div class="ratings">')
      m_posters <- append(m_posters, '<div class="empty-stars"></div>')
      m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
      m_posters <- append(m_posters, '</div>')
      m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
      m_posters <- append(m_posters, '</div>')
    }
    m_posters <- append(m_posters, '</div>')
    print(m_posters)
  })
  
  ###############################################################################################
  # Function output$cbActorMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  ###############################################################################################
  # output$cbActorMovies<-renderText({
  #   #print("Hello")
  #   currUser=get_curr_login()
  #   m_posters=NULL
  #   m <- get_cf_als_imp_movies(currUser)
  #   print(currUser)
  #   print(m)
  #   m_posters <- append(m_posters, '<div>')
  #   for (i in 1:length(m)){
  #     movie_title <- get_movie_title(m[i],FALSE)
  #     movie_rating <- get_movie_avg_rating(m[i])
  #     star_rating <- movie_rating/5 * 100
  #     m_posters <- append(m_posters, '<div class="gallery">')
  #     m_posters <- append(m_posters, paste0('<img src="',get_movie_url(m[i]),'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
  #     m_posters <- append(m_posters, 'Avg. <div class="ratings">')
  #     m_posters <- append(m_posters, '<div class="empty-stars"></div>')
  #     m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
  #     m_posters <- append(m_posters, '</div>')
  #     m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
  #     m_posters <- append(m_posters, '</div>')
  #   }
  #   m_posters <- append(m_posters, '</div>')
  #   print(m_posters)
  # })
  
  ###############################################################################################
  # Function output$cbActorMovies
  #
  # Description:  Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  ###############################################################################################
  output$cbActorMovies<-renderText({
    print("cbActorMovies")
    currUser=get_curr_login()
    m_posters=NULL
    m_movies <- getActorMovies(currUser,10)
    
    #print(currUser)
    #print(length(m_graph$nodes))
    m_posters <- append(m_posters, '<div>')
    for (i in 1:nrow(m_movies$title)){
      movie_title <- m_movies$title[i,]$value
      movie_poster <- m_movies$poster[i,]$value
      movie_rating <- m_movies$avg_rating[i,]$value
      star_rating <- movie_rating/5 * 100
      m_posters <- append(m_posters, '<div class="gallery">')
      m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
      m_posters <- append(m_posters, '<div class="ratings">')
      m_posters <- append(m_posters, '<div class="empty-stars"></div>')
      m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
      m_posters <- append(m_posters, '</div>')
      m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
      m_posters <- append(m_posters, '</div>')
    }
    m_posters <- append(m_posters, '</div>')
    print(m_posters)
  })
  
  ###############################################################################################
  # Function get_movie_url
  #
  # Description:  Return the url of movie poster of the given movie ID
  # Input:        
  # Output:       Return the url of movie poster of the given movie ID
  ###############################################################################################
  get_movie_url <-function(inMovieId) {
    p <- movies_df[movies_df$movieId==inMovieId,]$poster
    return(p)
  }
  
  ###############################################################################################
  # Function get_imdbid
  #
  # Description:  Return the corresponding IMDB ID of the given MovieLens ID
  # Input:        
  # Output:       Return the corresponding IMDB ID of the given MovieLens ID
  ###############################################################################################
  get_imdbid <- function(inMovieId){
    return(paste0("tt",links_df[links_df$movieId==inMovieId,]$imdbId))
  }
  
  ###############################################################################################
  # Function get_movie_title
  #
  # Description:  Return the movie title of the given MovieLens ID
  # Input:        
  # Output:       Return the movie title  of the given MovieLens ID
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
  # Function get_watched_movies
  #
  # Description:  Return a vector that contains the most recent 10 movie ID rated by the given User 
  # Input:        User ID 
  # Output:       Return a vector that contains the most recent 10 movie ID rated by the given User
  ###############################################################################################
  get_watched_movies <- function(inUserId){
    m1 <- ratings_df[ratings_df$userId==inUserId,]
    m2 <- m1[order(-m1$timestamp),]$movieId
    #m2 <- as.data.frame(c(1,2))
    return(m2[1:10])
  }
  
  ###############################################################################################
  # Function get_movie_rating
  #
  # Description:  Return a vector that contains the ratings of a given movie 
  # Input:        Movie ID (MovieLens)
  # Output:       Return a vector that contains the ratings of a given movie
  ###############################################################################################
  get_movie_rating <- function(inMovieId){
    r <- ratings_df[ratings_df$movieId==inMovieId,]$rating
    return(r)
  }
  
  ###############################################################################################
  # Function get_movie_avg_rating
  #
  # Description:  Return the average ratings of a given movie 
  # Input:        Movie ID (MovieLens)
  # Output:       Return the average ratings of a given movie
  ###############################################################################################
  get_movie_avg_rating <- function(inMovieId){
    r <- movies_df[movies_df$movieId==inMovieId,]$avgRating
    return(r)
  }
  
  ###############################################################################################
  # Function get_cb_movies
  #
  # Description:  Return the movie ID of the movies recommended by Content-based Recommender for the given user
  # Input:        User ID
  # Output:       Return the movie ID of the movies recommended by Content-based Recommender for the given user
  ###############################################################################################
  get_cb_movies <- function(inUserId){
    m <- cb_movies_df[cb_movies_df$userId==inUserId,]$movieId
    return(m)
  }
  
  ###############################################################################################
  # Function get_cf_popular_movies
  #
  # Description:  Return the movie ID of the movies recommended by Popular Recommender for the given user
  # Input:        User ID
  # Output:       Return the movie ID of the movies recommended by Popular Recommender for the given user
  ###############################################################################################
  get_cf_popular_movies <- function(inUserId){
    m <- cf_popular_movies_df[cf_popular_movies_df$userId==inUserId,]$movieId
    return(m)
  }
  
  ###############################################################################################
  # Function get_cf_als_imp_movies
  #
  # Description:  Return the movie ID of the movies recommended by ALS-implicit Recommender for the given user
  # Input:        User ID
  # Output:       Return the movie ID of the movies recommended by ALS-implicit Recommender for the given user
  ###############################################################################################
  get_cf_als_imp_movies <- function(inUserId){
    m <- cf_als_imp_movies_df[cf_als_imp_movies_df$userId==inUserId,]$movieId
    return(m)
  }
  ###############################################################################################
  #
  # Recommender TAB (END)
  #
  ###############################################################################################
  

  ###############################################################################################
  #
  # Analysis TAB (START)
  #
  ###############################################################################################
  
  ###############################################################################################
  # Top 10 Movies (START)
  ###############################################################################################
  
  ###############################################################################################
  # Function getTop10Movie
  #
  # Description:  Reactive function to return the top 10 movies that satisfy the given criteria
  # Input:        
  # Output:       Reactive function to return the top 10 movies that satisfy the given criteria
  ###############################################################################################
  getTop10Movie <- reactive({
    # Change when the "update" button is pressed...
    #input$update
    # ...but not for anything else
    getTop10MovieDF(input$ratedYear[1],input$ratedYear[2], input$minRatingCnt[1])
  })

  ###############################################################################################
  # Function output$top10Movie
  #
  # Description:  Render function to return the HTML for composing top 10 movies result
  # Input:        
  # Output:       HTML for composing top 10 movies result
  ###############################################################################################
  # output$top10Movies<-renderText({
  #   #print("Hello")
  #   m_posters=NULL
  #   m_df <- getTop10Movie()
  #   m <- m_df$movieId
  #   print(m)
  #   print("Top 10 Movies")
  #   movie_title <- ""
  #   movie_rating <- 0
  #   m_posters <- append(m_posters, (paste0("<h1>Top 10 Movies (",input$ratedYear[1],"-",input$ratedYear[2],")</h1>")))
  #   if (length(m)>0) {
  #     m_posters <- append(m_posters, '<div>')
  #     for (i in 1:length(m)){
  #       movie_title <- get_movie_title(m[i],FALSE)
  #       movie_rating <- m_df[i,]$avgRating
  #       
  #       star_rating <- movie_rating/5 * 100
  #       m_posters <- append(m_posters, '<div class="gallery">')
  #       m_posters <- append(m_posters, paste0('<img src="',get_movie_url(m[i]),'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
  #       m_posters <- append(m_posters, '<div class="ratings">')
  #       m_posters <- append(m_posters, '<div class="empty-stars"></div>')
  #       m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
  #       m_posters <- append(m_posters, '</div>')
  #       m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
  #       m_posters <- append(m_posters, '</div>')
  #     }
  #     m_posters <- append(m_posters, '</div>')
  #   }
  #   else{
  #     m_posters <- append(m_posters, '<div><H1 style="text-align:center">No Movies Found</H1></div>')
  #   }
  #   #print(m_posters)
  #   return(m_posters)
  # })
  
  ###############################################################################################
  # Function output$top10Movie
  #
  # Description:  Render function to return the HTML for composing top 10 movies result
  # Input:        
  # Output:       HTML for composing top 10 movies result
  ###############################################################################################
  output$top10Movies<-renderText({
    #print("Hello")
    m_posters=NULL
    m_movies <- getTop10Movie()
    print("Top 10 Movies")
    
    movie_title <- ""
    movie_rating <- 0
    m_posters <- append(m_posters, (paste0("<h1>Top 10 Movies (",input$ratedYear[1],"-",input$ratedYear[2],")</h1>")))
    if (length(m_movies)>0) {
      m_posters <- append(m_posters, '<div>')
      for (i in 1:nrow(m_movies$title)){
        
        movie_title <- m_movies$title[i,]$value
        movie_poster <- m_movies$poster[i,]$value
        movie_rating <- m_movies$avg_rating[i,]$value
        star_rating <- movie_rating/5 * 100
        m_posters <- append(m_posters, '<div class="gallery">')
        m_posters <- append(m_posters, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster.height,'" width="',poster.width,'" ContentType="Images/jpeg" >'))
        m_posters <- append(m_posters, '<div class="ratings">')
        m_posters <- append(m_posters, '<div class="empty-stars"></div>')
        m_posters <- append(m_posters, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
        m_posters <- append(m_posters, '</div>')
        m_posters <- append(m_posters, paste0('<div class="desc" >',movie_title,'</div>'))
        m_posters <- append(m_posters, '</div>')
      }
      m_posters <- append(m_posters, '</div>')
    }
    else{
      m_posters <- append(m_posters, '<div><H1 style="text-align:center">No Movies Found</H1></div>')
    }
    #print(m_posters)
    return(m_posters)
  })
  
  ###############################################################################################
  # Top 10 Movies (END)
  ###############################################################################################
  
  ###############################################################################################
  # WordCloud of Genres and Tags (START)
  ###############################################################################################
  
  ###############################################################################################
  # Function terms
  #
  # Description:  Reactive function to a matrix of genres that satisfy the criteria
  # Input:        
  # Output:       Matrix of genres that satisfy the criteria
  ###############################################################################################
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        print(input$year[1])
        print(input$year[2])
        getTermMatrix(input$year[1],input$year[2],input$rating[1],input$rating[2])
      })
    })
  })
  
  ###############################################################################################
  # Function tags
  #
  # Description:  Reactive function to a matrix of tags that satisfy the criteria
  # Input:        
  # Output:       Matrix of tags that satisfy the criteria
  ###############################################################################################
  tags <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTagMatrix(input$year[1],input$year[2],input$rating[1],input$rating[2])
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  ###############################################################################################
  # Function genreWordCloud
  #
  # Description:  Render function to plot the wordcloud of genres
  # Input:        
  # Output:       Wordcloud of genres
  ###############################################################################################
  output$genreWordCloud <- renderPlot({
    v <- terms()
    print(paste0("wordcloud V: ",class(v),": ",length(names(v))))
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  ###############################################################################################
  # Function tagWordCloud
  #
  # Description:  Render function to plot the wordcloud of tags
  # Input:        
  # Output:       Wordcloud of tags
  ###############################################################################################
  output$tagWordCloud <- renderPlot({
    v <- tags()
    wordcloud_rep(names(v), v, scale=c(4,0.5), 
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  ###############################################################################################
  # WordCloud of Genres and Tags (END)
  ###############################################################################################
  
  ###############################################################################################
  # Rating Analysis (START)
  ###############################################################################################
  
  ###############################################################################################
  # Function ratingHistogram
  #
  # Description:  Reactive function to return data frame of ratings that satisfy the given criteria
  # Input:        
  # Output:       Data frame of ratings that satisfy the given criteria
  ###############################################################################################
  ratingHistogram <- reactive({
    getRatingHistogramDF(input$year2[1],input$year2[2])
  })
  
  ###############################################################################################
  # Function avgRatingHistogram
  #
  # Description:  Reactive function to return data frame of average ratings that satisfy the given criteria
  # Input:        
  # Output:       Data frame of average ratings that satisfy the given criteria
  ###############################################################################################
  avgRatingHistogram <- reactive({
    getAvgRatingHistogramDF(input$year2[1],input$year2[2])
  })
  
  ###############################################################################################
  # Function output$ratingHistogram
  #
  # Description:  Render function to return the plot of the histogram of movie ratings that satisfy the given criteria
  # Input:        
  # Output:       The plot of the histogram of movie ratings that satisfy the given criteria
  ###############################################################################################
  output$ratingHistogram <- renderPlot({
    v <- ratingHistogram()
   
    print("# of Rating")
    print(nrow(v))
    
    # ratings_df2 <- ratings_df %>%
    #   filter(between(year, input$year2[1],input$year2[2]))
    
    ggplot(data=v, aes(rating)) + 
    geom_histogram(breaks=seq(0, 5, by=0.5), 
                   alpha = .7,
                   aes(fill=..count..)) + 
      labs(title=paste0("Histogram for Movie Ratings (",input$year2[1],"-",input$year2[2],")"), x="Rating", y="Count") +
      scale_fill_gradient("Count", low="green", high="red")
  })
  
  ###############################################################################################
  # Function output$avgRatingHistogram
  #
  # Description:  Render function to return the plot of the histogram of average movie ratings that satisfy the given criteria
  # Input:        
  # Output:       The plot of the histogram of average movie ratings that satisfy the given criteria
  ###############################################################################################
  output$avgRatingHistogram <- renderPlot({
    v <- avgRatingHistogram()
    
    print("# of Avg Rating")
    print(nrow(v))
    
    ggplot(data=v, aes(avgRating)) + 
      geom_histogram(breaks=seq(0, 5, by=0.5), 
                     alpha = .7,
                     aes(fill=..count..)) + 
      labs(title=paste0("Distribution for Average Movie Ratings (",input$year2[1],"-",input$year2[2],")"), x="Average Movie Rating", y="Count") +
      scale_fill_gradient("Count", low="green", high="red")
  })
  
  ###############################################################################################
  # Function output$userRating
  #
  # Description:  Render function to return the plot of the average number of ratings given by each user for a certain period
  # Input:        
  # Output:       The plot of the average number of ratings given by each user for a certain period
  ###############################################################################################
  output$userRating <- renderPlot({
    v <- getUserRatingHistogramDF()
    
    print("# of Rating")
    print(nrow(v))
    print(v)
    
    ggplot(data=v, aes(year, avgCntPerUser)) + 
      geom_col(fill="red") + 
      labs(title="Average Number of Ratings Given per User", x="Year", y="Average # of Ratings per User")
  })
  
  ###############################################################################################
  # Rating Analysis (END)
  ###############################################################################################
  
  ###############################################################################################
  #
  # Analysis TAB (END)
  #
  ###############################################################################################

  ###############################################################################################
  #
  # Graph TAB (START)
  #
  ###############################################################################################
  
  ###############################################################################################
  # Function get_analyze_curr_login
  #
  # Description:  Return the current selected USER ID
  # Input:        
  # Output:       Return the current selected USER ID
  ###############################################################################################
  get_analyze_user <- reactive({
    return(input$selectUser1)
  })
  
  ###############################################################################################
  # Function get_analyze_movie_category
  #
  # Description:  Return the current selected Movie Category
  # Input:        
  # Output:       Return the current selected Movie Category
  ###############################################################################################
  get_analyze_movie_category <- reactive({
    return(input$selectCategory)
  })
  
  ###############################################################################################
  # Function compose_movie_tile_html
  #
  # Description:  Return a piece of HTML text that represents a movie tile for display
  # Input:        
  # Output:       Return a piece of HTML text that represents a movie tile for display
  ###############################################################################################
  compose_movie_tile_html <- function(movie_title, movie_poster, poster_height, poster_width, star_rating){
    movie_tile <- NULL
    # movie_tile <- append(movie_tile, '<div class="gallery">')
    # movie_tile <- append(movie_tile, paste0('<img src="',movie_poster,'" alt="',movie_title,'" height="',poster_height,'" width="',poster_width,'" ContentType="Images/jpeg" >'))
    # movie_tile <- append(movie_tile, '<div class="ratings">')
    # movie_tile <- append(movie_tile, '<div class="empty-stars"></div>')
    # movie_tile <- append(movie_tile, paste0('<div class="full-stars", style="width:',star_rating,'%"></div>'))
    # movie_tile <- append(movie_tile, '</div>')
    # movie_tile <- append(movie_tile, paste0('<div class="desc" >',movie_title,'</div>'))
    # movie_tile <- append(movie_tile, '</div>')
    
    movie_tile <- paste0('<div class="gallery">',
                         '<img src="',movie_poster,'" alt="',movie_title,'" height="',poster_height,'" width="',poster_width,'" ContentType="Images/jpeg" >',
                         '<div class="ratings">',
                         '<div class="empty-stars"></div>',
                         '<div class="full-stars", style="width:',star_rating,'%"></div>',
                         '</div>',
                         '<div class="desc" >',movie_title,'</div>',
                         '</div>')
    
    return (movie_tile)
    
  }
  
  ###############################################################################################
  # Function output$selectedMovieCategory
  #
  # Description:  Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  # Input:        
  # Output:       Return the movie posters of the movies that are recommended by the users' favorite actors / actresses
  ###############################################################################################
  output$selectedMovieCategory<-renderText({
    print("selectedMovieCategory")
    currUser=get_analyze_user()
    currCategory=get_analyze_movie_category()
    m_posters=NULL
    m_movies=NULL
    rb_choices=NULL
    

    if (currCategory == "Movies similar to what you like"){
      m_movies <- getContentBasedMovies(currUser,10)
    } else if (currCategory == "Others similar to you like these movies"){
      m_movies <- getCollaborativeFilteringMovies(currUser,10)
    } else if (currCategory == "Movies with your favorite actors / actresses") {
      m_movies <- getActorMovies(currUser,10)
    }

    if (length(m_movies)>0) {
      m_posters <- append(m_posters, (paste0("<h1>",currCategory,"</h1>")))
    # 
    #   m_posters <- append(m_posters, '<div>')
    #   for (i in 1:nrow(m_movies$title)){
    #     movie_title <- m_movies$title[i,]$value
    #     movie_poster <- m_movies$poster[i,]$value
    #     movie_rating <- m_movies$avg_rating[i,]$value
    #     star_rating <- movie_rating/5 * 100
    #     m_tile <- compose_movie_tile_html(movie_title,movie_poster,poster.height,poster.width,star_rating)
    #     m_posters <- append(m_posters, m_tile)
    #   }
    #   m_posters <- append(m_posters, '</div>')
    # }
    # else {
    #   m_posters <- append(m_posters, (paste0("<h1>No movies to analyze</h1>")))
    }

    print(m_posters)
  })
  
  output$radioTest <- renderUI({
    print("radioTest")
    currUser=get_analyze_user()
    currCategory=get_analyze_movie_category()
    m_posters=NULL
    m_movies=NULL
    rb_choiceNames=list()
    rb_choiceValues=list()
    
    
    if (currCategory == "Movies similar to what you like"){
      m_movies <- getContentBasedMovies(currUser,10)
    } else if (currCategory == "Others similar to you like these movies"){
      m_movies <- getCollaborativeFilteringMovies(currUser,10)
    } else if (currCategory == "Movies with your favorite actors / actresses") {
      m_movies <- getActorMovies(currUser,10)
    }
    
    if (length(m_movies)>0) {
      
      for (i in 1:nrow(m_movies$title)){
        movie_id <- m_movies$movie_id[i,]$value
        movie_title <- m_movies$title[i,]$value
        movie_poster <- m_movies$poster[i,]$value
        movie_rating <- m_movies$avg_rating[i,]$value
        star_rating <- movie_rating/5 * 100
        m_tile <- compose_movie_tile_html(movie_title,movie_poster,poster.height,poster.width,star_rating)
        rb_choiceNames <- append(rb_choiceNames, m_tile)
        rb_choiceValues <- append(rb_choiceValues, movie_id)
      }
      for (i in 1:length(rb_choiceNames)){
        rb_choiceNames[[i]]<-HTML(rb_choiceNames[[i]])
      }
      print("rb_choiceNames:")
      print(rb_choiceNames)
      print("rb_choiceValues:")
      print(rb_choiceValues)
    }
    else {
      rb_choiceNames <- c("No Movies")
    }
    
    # test_rb <- HTML("abc")
    # test_rb <- list(HTML("abc"), HTML("def"))
    # 
    # test_rb1 <- list(HTML("abc"))
    # test_rb1 <- append(test_rb1, HTML("def"))
    # test_rb1[[2]]<-HTML(test_rb1[[2]])
    
    # The options are dynamically generated on the server
    radioButtons('movieChoice', currCategory, choiceNames=rb_choiceNames, choiceValues=rb_choiceValues, inline=TRUE)
    
    # radioButtons("rb", "Choose one:",
    #              choiceNames = list(
    #                icon("calendar"),
    #                #HTML("<p style='color:red;'>Red Text</p>"),
    #                #HTML(m_tile),
    #                HTML(rb_choiceNames[[1]]),
    #                "Normal text"
    #              ),
    #              choiceValues = list(
    #                "icon", "html", "text"
    #              ))
  })
  observe({
    input$submit
    
    isolate(
      output$text <- renderText({
        paste("Radiobutton response is:", input$movieChoice )
      })
    )
  })
  
  output$txt <- renderText({
    paste("You chose", input$rb)
  })
  
  output$myNetId <- renderVisNetwork({
    visNetwork(nodes, edges)
  })
  
  output$movieGraph <- renderVisNetwork({
    rrG <- getRecentlyRatedMovies(1,10)
    visNetwork(rrG$nodes, rrG$relationships) %>% 
      visNodes(font = list(color = "#ffffff")) %>% 
      visEdges(font = list(color = "#ffffff", strokeColor = "#000000")) %>%
      visPhysics(barnesHut = list(springConstant=0))
  })
  
  ###############################################################################################
  #
  # Graph TAB (END)
  #
  ###############################################################################################
  
  
}
###############################################################################################
# Server (END)
###############################################################################################

shinyApp(ui, server)