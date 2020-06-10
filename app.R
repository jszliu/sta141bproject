
library(tidyverse)
library(wordcloud2)
library(DT)
library(httr)
library(jsonlite)
library(tidytext)
library(wordcloud2)
library(proxy)
library(tm)

# Return the access token, used for the query
getToken <- function(){
  client_id <- '09419aa4748c4b5b94099c5bd1a3451e'
  client_secret <- '2179e36d865f42488ff4bee8551d619c'
  res <- POST('https://accounts.spotify.com/api/token',
              accept_json(), authenticate(client_id, client_secret),
              body = list(grant_type='client_credentials'),
              encode = 'form', httr::config(http_version=2)) 
  info <- content(res)
  token <- info$access_token
  token
}


# get the artiests by name
get_artists <- function(artist_name, token) {
  res <- GET('https://api.spotify.com/v1/search', 
             query = list(q = artist_name, 
                          type = 'artist', 
                          access_token = token))
  json <- content(res, as = "text")
  dat <- fromJSON(json, flatten = TRUE)
  items <- dat$artists$items
  items$artist_uri = str_replace(items$uri, 'spotify:artist:', '')
  items
}


# get the albums
get_albums <- function(artist_uri, token) {
  res <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums'),
             query = list(access_token = token, 
                          market = "US", 
                          limit = 50, 
                          album_type = "album")) 
  json <- content(res, as = "text")
  dat <- fromJSON(json, flatten = TRUE)
  dat$items
}

get_tracks <- function(album_id, token){
  res <- GET(paste0('https://api.spotify.com/v1/albums/', album_id, '/tracks'),
             query = list(access_token = token, 
                          limit = 50)) 
  json <- content(res, as = "text")
  dat <- fromJSON(json, flatten = TRUE)
  dat$items
}

# artists <- get_artists("Ax", token)
# albums <- get_albums(artists$artist_uri[1], token)

getSongs <- function(artist_uri, token){
  albums <- get_albums(artist_uri, token)
  ids <- albums$id
  dat <- data.frame()
  for(i in 1:length(ids)){
    dat <- rbind(dat, get_tracks(ids[i], token))
  }
  dat
}

getAllData <- function(artists, token){
  dat <- data.frame()
  for(i in 1:nrow(artists)){
    dat <- rbind(dat, getSongs(artists$artist_uri[i], token))
  }
  dat
}


plotCloud <- function(names){
  df <- data.frame(id=1:length(names), song=names, stringsAsFactors = F)
  tokens <- df %>% 
    unnest_tokens(word, song) %>% 
    anti_join(stop_words)  %>% group_by(id) %>%
    count(word, sort = TRUE) %>% 
    arrange(id)
  wordFreq <- data.frame(word=tokens$word, freq=tokens$n, stringsAsFactors = F)
  wordcloud2(wordFreq)
}

getSentiment <- function(uri, token){
  songs <- getSongs(uri, token) 
  df <-tibble(id=1:nrow(songs), name=songs$name) 
  df %>% 
    unnest_tokens(word, name) %>% 
    anti_join(stop_words) %>% 
    group_by(id, word) %>% summarise(n=n()) %>%
    left_join(get_sentiments("bing")) %>% 
    group_by(id) %>% 
    summarize(
      positive = sum(sentiment == "positive", na.rm = TRUE), 
      negative = sum(sentiment == "negative", na.rm = TRUE), 
      netural = n() - positive - negative) %>%
    mutate(
      id,
      sentiment = case_when(
        positive > negative ~ "positive",
        positive < negative ~ "negative",
        TRUE ~ "netural"
      )
    ) %>% left_join(select(df, id, name)) %>% 
    select(id, name, sentiment)
}

token <-  getToken()

ui <- fluidPage(
    titlePanel("Spotify Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("artist", "Search for an artist:"),
      # actionButton("search", "Search"),
      # DTOutput("artistTable"),
      uiOutput("artistSelect")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Profile", htmlOutput("profile")),
                  tabPanel("Albums", DTOutput("albums")),
                  tabPanel("WordCloud", 
                           h3("Word cloud of songs"),
                           wordcloud2Output("word")),
                  tabPanel("Sentiment",  DTOutput("sentiment")),
                  tabPanel("Cluster", plotOutput("plot")),
                  tabPanel("Claim", htmlOutput("claim"))
                  
      )
    )
  )
)


dat <- getSongs("31W5EY0aAly4Qieq6OFu6I", token)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$artistSelect <- renderUI({
    # input$search
    req(input$artist)
    dat <- get_artists(input$artist, token)
    selectInput("artist_uri", "Select an artist",choices = dat$name)
  })

  
  output$albums <- renderDT({
    req(input$artist_uri)
    dat <- get_artists(input$artist, token)
    if(length(dat)!=0){
      uri <- dat$artist_uri[dat$name==input$artist_uri]
      albums <- get_albums(uri, token) 
      if(length(albums)==0){
        data.frame()
      }else{
        albums %>% select(-artists, -images, -external_urls.spotify, -uri, -href, -album_group)
      }
    }else{
      data.frame()
    }
  })
  
  output$sentiment <- renderDT({
    req(input$artist_uri)
    dat <- get_artists(input$artist, token)
    if(length(dat)!=0){
      uri <- dat$artist_uri[dat$name==input$artist_uri]
      df <- getSentiment(uri, token)
      if(length(df)==0){
        data.frame()
      }else{
        df
      }
    }else{
      data.frame()
    }
  })
  
  output$word <- renderWordcloud2({
    # input$search
    req(input$artist)
    req(input$artist_uri)
    dat <- get_artists(input$artist, token)
    if(length(dat)!=0){
      uri <- dat$artist_uri[dat$name==input$artist_uri]
      dat <- getSongs(uri, token)
      if(length(dat)!=0){
        if(!is.null(dat)){
          plotCloud(dat$name)
        }
        
      }
    }
  })
  
  output$profile <- renderUI({
    req(input$artist_uri)
    dat <- get_artists(input$artist, token)
    if(length(dat)!=0){
      dat <- dat[dat$name==input$artist_uri, ]
      genres <- paste0(dat$genres[[1]], collapse=", ")
      tagList(
        h2(dat$name),
        a(dat$href, href=dat$href),
        h3("Genres"),
        p(genres),
        img(src=dat$images[[1]]$url[1]),
        h3("Popularity"),
        p(dat$popularity[[1]]),
        h3("Followers"),
        p(dat$followers.total[[1]]),
      )
    }
  })
  
  output$claim <- renderUI({
    tagList(
      h2("The page is dedicated to people who fought for liberty and equality"),
      img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Black_Lives_Matter_logo.svg/1200px-Black_Lives_Matter_logo.svg.png")
    )
  })
  
  output$plot <- renderPlot({
    req(input$artist)
    req(input$artist_uri)
    dat <- get_artists(input$artist, token)
    if(length(dat)!=0){
      uri <- dat$artist_uri[dat$name==input$artist_uri]
      songs <- getSongs(uri, token) 
      df <-tibble(id=1:nrow(songs), name=songs$name) 
      tokens <- df %>% 
        unnest_tokens(word, name) %>% 
        anti_join(stop_words) %>% 
        group_by(id, word) %>% summarise(n=n()) 
      docsdissim <- dist(as.matrix(cast_dtm(tokens, id, word, n)), method = "cosine")
      h <- hclust(docsdissim, method = "ward.D2")
      plot(h)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
