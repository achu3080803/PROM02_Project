## app.R ##
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(RCurl)
library(rjson)
library(ShinyRatingInput)


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
      menuItem("Movie Rating", tabName = "movie_rating", icon = icon("film")),
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
              fluidRow(
                h1("Top 10 Movies"),
                htmlOutput("top10Movies2")
              ),
              fluidRow(
                h1("Recently rated by you"),
                htmlOutput("watchedMovies")
              ),
              # fluidRow(
              #   h1("Your favorite movies"),
              #   htmlOutput("favoriteMovies")
              # ),
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
      tabItem(
              tabName = "movie_rating",
              fluidRow(
                h1("Movie Rating"),
                box(
                  width = 2,
                  selectizeInput("selectUser2", "Login as", all_userid_df$value),
                )
              ),
              fluidRow(
                h1("Movie Search"),
                box(
                  width = 10,
                  column(7,textInput("searchMovieTitle", "Please enter Movie Title below:")),
                  #column(3,actionButton("movieSearch", "Search"))
                )
              ),
              fluidRow(
                y <- uiOutput("radioMovieTilesForSearch"),
                HTML("<br>"),
                HTML("<br>")
              ),
              fluidRow(
                h1("Give your rating here:"),
                box(
                  width = 10,
                  z <- uiOutput("chosenMovie")
                  )
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
          h1("User's favorite movies"),
          htmlOutput("favoriteMovies")
        ),
        fluidRow(
          htmlOutput("selectedMovieCategory"),
          x <- uiOutput('radioMovieTiles'),
          #actionButton('submit', label = "Submit"),
          #br(),
          #print(paste("Radiobutton response is:", "reply")),
          #textOutput('text')
        ),
         # visNetworkOutput("myNetId",
         #                  height <- "1200",
         #                  width <- "600"),
         visNetworkOutput("movieGraph",
                         height <- "2000",
                         width <- "1000")
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
  # Function output$top10Movie2
  #
  # Description:  Render function to return the HTML for composing top 10 movies result
  # Input:        
  # Output:       HTML for composing top 10 movies result
  ###############################################################################################
  output$top10Movies2<-renderText({
    #print("Hello")
    m_posters=NULL
    min_year = min(as.integer(ratings_df$ratedYear))
    max_year = max(as.integer(ratings_df$ratedYear))
    m_movies <- getTop10MovieDF(min_year,max_year, 50)
    print("Top 10 Movies 2")
    
    movie_title <- ""
    movie_rating <- 0

    
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
    currUser=get_analyze_user()
    m_posters=NULL
    m_graph <- getFavoriteMovies(currUser,20)
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
  # Movie Rating TAB (START)
  #
  ###############################################################################################
  
  ###############################################################################################
  # Function get_search_user
  #
  # Description:  Return the current selected USER ID
  # Input:        
  # Output:       Return the current selected USER ID
  ###############################################################################################
  get_search_user <- reactive({
    return(input$selectUser2)
  })
  
  ###############################################################################################
  # Function get_search_user
  #
  # Description:  Return the current selected USER ID
  # Input:        
  # Output:       Return the current selected USER ID
  ###############################################################################################
  get_search_movie_title <- reactive({
    return(input$searchMovieTitle)
  })
  
  ###############################################################################################
  # Function output$radioMovieTilesForSearch
  #
  # Description:  Reactive function to a matrix of genres that satisfy the criteria
  # Input:        
  # Output:       Matrix of genres that satisfy the criteria
  ###############################################################################################
  output$radioMovieTilesForSearch <- renderUI({
    
    input$movieSearch
    
    print("radioMovieTilesForSearch")
    i_movie_title=get_search_movie_title()
    m_posters=NULL
    m_movies=NULL
    rb_choiceNames=list()
    rb_choiceValues=list()
    
    m_movies <- searchdMovies(i_movie_title,30)
    #m_movies <- NULL
    
    if (length(m_movies)>0) {
      
      for (i in 1:nrow(m_movies$title)){
        movie_id <- m_movies$movie_id[i,]$value
        movie_title <- m_movies$title[i,]$value
        movie_poster <- m_movies$poster[i,]$value
        movie_rating <- m_movies$avg_rating[i,]$value
        movie_score <- NULL
        star_rating <- movie_rating/5 * 100
        m_tile <- compose_movie_tile_html(movie_title,movie_poster,poster.height,poster.width,star_rating,movie_score)
        rb_choiceNames <- append(rb_choiceNames, m_tile)
        rb_choiceValues <- append(rb_choiceValues, paste0(movie_id,";",movie_title,";",movie_poster))
      }
      for (i in 1:length(rb_choiceNames)){
        rb_choiceNames[[i]]<-HTML(rb_choiceNames[[i]])
      }
      print("rb_choiceNames:")
      print(rb_choiceNames)
      print("rb_choiceValues:")
      print(rb_choiceValues)
      
      # The options are dynamically generated on the server
      radioButtons('movieChoice2', "", choiceNames=rb_choiceNames, choiceValues=rb_choiceValues, inline=TRUE)
    }
    else {
      #rb_choiceNames <- c("No Movies")
      movie_id <- 0
      movie_title <- "No Movie Found"
      movie_poster <- "movie_star.jpg"
      movie_rating <- 0
      movie_score <- NULL
      star_rating <- movie_rating/5 * 100
      m_tile <- compose_movie_tile_html(movie_title,movie_poster,poster.height,poster.width,star_rating,movie_score)
      HTML(m_tile)
    }
    
    # The options are dynamically generated on the server
    #radioButtons('movieChoice2', "", choiceNames=rb_choiceNames, choiceValues=rb_choiceValues, inline=TRUE)
    
  })
  
  observeEvent(input$submitRating, {
      
    if (!is.null(input$movieChoice2) && !is.null(input$movieRating)) {
      if (input$movieChoice2 != "" && input$movieRating != "") {
        login_id <- input$selectUser2
        params<-unlist(strsplit(input$movieChoice2, ";"))
        movie_id <- params[1]
        user_rating <- input$movieRating
        print(paste("Login: ",login_id," Movie: ",movie_id," Rating: ",user_rating))  
        updateMovieRating(login_id, movie_id, user_rating)
      }
    }
  })
  
  # observe({
  #   input$submitRating
  #   
  #   isolate(
  #     output$text <- renderText({
  #       paste("Radiobutton response is:", input$movieChoice2 )
  #     })
  #   )
  # })

  ###############################################################################################
  # Function submitRating
  #
  # Description:  Reactive function to a matrix of genres that satisfy the criteria
  # Input:        
  # Output:       Matrix of genres that satisfy the criteria
  ###############################################################################################
  # submitRating <- reactive({
  #   # Change when the "submitRating" button is pressed...
  #   input$submitRating
  #   # ...but not for anything else
  #   print(paste("submitRating: ",input$movieChoice2))  
  #   return (input$movieChoice2)
  #   
  # })  
  # 
  # output$text <- renderText({
  #   v <- submitRating()
  #   isolate({
  #     paste("Selected movie is:", v,"<br>", "" )
  #   })
  # })
  
  output$chosenMovie <- renderUI({
    movieChoice <- unlist(strsplit(as.character(input$movieChoice2),";"))
    movie_title <- movieChoice[2]
    movie_title_html <- HTML(paste('<font size="+2">',movie_title,'</font>'))
    poster_height <- poster.height * 1.5
    poster_width <- poster.width * 1.5
    movie_poster <- HTML(paste('<img src="',movieChoice[3],'" alt="',movie_title,'" size=+2 height="',poster_height,'" width="',poster_width,'" ContentType="Images/jpeg" >'))
    #print(movie_poster)

    tagList(div(movie_poster, 
                div(movie_title_html, 
                    div(ratingInput("movieRating", label="", dataStop=5, dataFractions=2)
                    )
                )
            ),
            div(HTML("<br>"),actionButton("submitRating", "Submit"))
    )
    
  })
  
  output$movieRating <- renderText({
    paste('<font size="+2">The movie was rated as',input$movieRating,'</font>')
  })
  
  ###############################################################################################
  #
  # Movie Rating TAB (END)
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
  compose_movie_tile_html <- function(movie_title, movie_poster, poster_height, poster_width, star_rating, score){
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
                         '<div class="full-stars", style="width:',star_rating,'%"></div>')
    if(!is.null(score)) {
      movie_tile <- paste0(movie_tile,'<div class="desc" >Score: ',round(score,2),'</div>')
    }
    movie_tile <- paste0(movie_tile,
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
    }

    print(m_posters)
  })
  
  output$radioMovieTiles <- renderUI({
    print("radioMovieTiles")
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
        movie_score <- m_movies$score[i,]$value
        star_rating <- movie_rating/5 * 100
        m_tile <- compose_movie_tile_html(movie_title,movie_poster,poster.height,poster.width,star_rating,movie_score)
        rb_choiceNames <- append(rb_choiceNames, m_tile)
        if (currCategory == "Movies similar to what you like"){
          source_id_csv <- m_movies$source_id_csv[i,]$value
          rb_choiceValues <- append(rb_choiceValues, paste0(source_id_csv,":",movie_id))
        } else if (currCategory == "Others similar to you like these movies"){
          u1_loginId <- m_movies$u1_loginId[i,]$value
          u2_loginId <- m_movies$u2_loginId[i,]$value
          rb_choiceValues <- append(rb_choiceValues, paste0(u1_loginId,":",u2_loginId,":",movie_id))
        } else if (currCategory == "Movies with your favorite actors / actresses") {
          source_id_csv <- m_movies$source_id_csv[i,]$value
          rb_choiceValues <- append(rb_choiceValues, paste0(source_id_csv,":",movie_id))
        }
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
    
    # The options are dynamically generated on the server
    radioButtons('movieChoice', "", choiceNames=rb_choiceNames, choiceValues=rb_choiceValues, inline=TRUE)

  })
  # observe({
  #   input$submit
  #   
  #   isolate(
  #     output$text <- renderText({
  #       paste("Radiobutton response is:", input$movieChoice )
  #     })
  #   )
  # })
  
  
  output$myNetId <- renderVisNetwork({
    visNetwork(nodes, edges)
  })
  
  output$movieGraph <- renderVisNetwork({

    currCategory=get_analyze_movie_category()
    
    mc <- input$movieChoice
    
    if (!is.null(mc)) {
      if (currCategory == "Movies similar to what you like"){
        params<-unlist(strsplit(input$movieChoice, ":"))
        G <- getContentBasedMovieGraph(params[1],params[2])
      } else if (currCategory == "Others similar to you like these movies"){
        params<-unlist(strsplit(input$movieChoice, ":"))
        G <- getCollaborativeFilteringMovieeGraph(params[1],params[2],params[3])
      } else if (currCategory == "Movies with your favorite actors / actresses") {
        params<-unlist(strsplit(input$movieChoice, ":"))
        G <- getActorMovieGraph(params[1],params[2])
      }
    
    
      # visNetwork(G$nodes, G$relationships) %>% 
      #   visNodes(font = list(color = "#ffffff")) %>% 
      #   visEdges(font = list(color = "#ffffff", strokeColor = "#000000")) %>%
      #   visPhysics(barnesHut = list(springConstant=0)) %>%
      #   addFontAwesome()
      
      G$nodes$image <- "" 
      G$nodes$label <- G$nodes$title
      
      #G$nodes[G$nodes$group=="Movie",]$shape = "image"
      G$nodes[G$nodes$group=="Movie",]$image = G$nodes[G$nodes$group=="Movie",]$poster
      G$nodes[G$nodes$group=="Person",]$image = "user_icon_red.png"
      G$nodes[G$nodes$group=="Person",]$label = G$nodes[G$nodes$group=="Person",]$name
      G$nodes[G$nodes$group=="Company",]$image = "film_company_icon_green.png"
      G$nodes[G$nodes$group=="Company",]$label = G$nodes[G$nodes$group=="Company",]$name
      G$nodes[G$nodes$group=="Genre",]$image = "genre_icon_pink.png"
      G$nodes[G$nodes$group=="Genre",]$label = G$nodes[G$nodes$group=="Genre",]$name
      G$nodes[G$nodes$group=="Country",]$image = "country_icon_blue.png"  
      G$nodes[G$nodes$group=="Country",]$label = G$nodes[G$nodes$group=="Country",]$name
      G$nodes$title <- G$nodes$group
      G$nodes$title <- paste('<p style="color:Black;font-size:14px">',G$nodes$group,'</p>')
      
      visNetwork(G$nodes, G$relationships) %>% 
        visNodes(font = list(color = "#ffffff")) %>% 
        visGroups(groupname="Person",shape = "image") %>%
        visGroups(groupname="Movie",shape = "image") %>%
        visGroups(groupname="Company",shape = "image") %>%
        visGroups(groupname="Genre",shape = "image") %>%
        visGroups(groupname="Country",shape = "image") %>%
        visEdges(font = list(color = "#ffffff", strokeColor = "#000000")) %>%
        visPhysics(barnesHut = list(springConstant=0)) %>%
        addFontAwesome()
      
      # nodes <- data.frame(id = 1:3, group = c("Person", "A", "Person"), shape=c("","",""))
      # edges <- data.frame(from = c(1,2), to = c(2,3))
      # 
      # visNetwork(nodes, edges, width = "100%") %>%
      #   visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
      #   visGroups(groupname = "Person", shape = "icon", icon = list(code = "f007", color = "red")) %>%
      #   visLegend() %>%
      #   addFontAwesome()
    }
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