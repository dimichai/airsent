library(shiny)
library(leaflet)
library(rgdal)
library(plyr)
library(dplyr)
library(SnowballC)

shinyServer(function(input, output) {

  # get the states shapes from data
  states.ogr <- readOGR("./shp/cb_2016_us_state_20m.shp",
                        layer = "cb_2016_us_state_20m", GDAL1_integer64_policy = TRUE)
  
  # read saved tweets
  observe({
    selected.airline <- input$airline_select_main
    if(selected.airline == 'United') {
      if(!exists('tweets.united.located')) {
        load("data/united_tweets_v3.Rda")
      }
      tweets.located <- tweets.united.located
    } else if(selected.airline == 'VirginAmerica') {
      if(!exists('tweets.virgin.located')) {
        load("data/virgin_tweets_v2.Rda")
      }
      tweets.located <- tweets.virgin.located
    } else if(selected.airline == 'SouthwestAir') {
      if(!exists('tweets.southwest.located')) {
        load("data/southwest_tweets_v2.Rda")
      }
      tweets.located <- tweets.southwest.located
    }
    else if(selected.airline == 'JetBlue') {
      if(!exists('tweets.jetblue.located')) {
        load("data/jetblue_tweets_v2.Rda")
      }
      tweets.located <- tweets.jetblue.located
    }
    
    tweets.mean.sentiment <- ddply(tweets.located, .(state), summarize, mean_sent=mean(SVM_LABEL))
    tweets.per.state <- data.frame(table(tweets.located$state))
    colnames(tweets.per.state) <- c("state", "total_tweets")
    tweets.mean.sentiment <- inner_join(tweets.mean.sentiment, tweets.per.state, by="state")
    #rm(tweets.with.location)
    
    # add mean_sent to the data
    states.df <- as.data.frame(states.ogr)
    states.df$NAME <- tolower(states.df$NAME)
    states.df <- left_join(states.df, tweets.mean.sentiment, by=c("NAME"="state"))
    states.ogr$mean_sent <- states.df$mean_sent
    states.ogr$total_tweets <- states.df$total_tweets
    #rm(states.df)
    
    # set the color palette
    pal.bins <- colorBin(
      palette = c("red", "green"),
      domain = states.ogr$mean_sent,
      bins = 4
    )
    
    output$map <- renderLeaflet({
      leaflet(states.ogr) %>%
        setView(lat = 37.09024, lng = -95.712891, zoom=3.5) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = ~pal.bins(mean_sent),
                    highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                    popup = paste(states.ogr$NAME, "<br />",
                                  "Mean Sentiment: ", states.ogr$mean_sent, "<br />",
                                  "Total Tweets: ", states.ogr$total_tweets 
                    )
        ) %>%
        addLegend("bottomright", 
                  pal = pal.bins, 
                  values = ~mean_sent, 
                  title="Average Sentiment",
                  labFormat = function(type, cuts) {
                    return(c("very negative", "negative", "neutral", "positive", "very positive"))
                  }
        )
    })
    
    output$stateStats <- renderDataTable({
      return(tweets.mean.sentiment)
    })
    
    # Render a sample of 100 tweets from the data
    output$sampleData <- renderDataTable({
      tweets.sample <- tweets.located[tweets.located$SVM_PROB > 0.9, ]
      tweets.sample <- tweets.sample[sample(nrow(tweets.sample), 100), ]
      tweets.sample <- subset(tweets.sample, select = c(original_text, SVM_LABEL, state))
      return(tweets.sample)
    })
    
    output$stats <- renderUI({
      # Get states with more than 20 tweets to find min/max sentiment
      states.enough.tweets <- tweets.mean.sentiment[tweets.mean.sentiment$total_tweets > 20, ]
      HTML( paste('Total Tweets:', as.character(nrow(tweets.located)), '<br />',
                  'Average Sentiment: ', as.character(round(mean(tweets.mean.sentiment$mean_sent), 2)), '<hr/ >',
                  'State Coverage: ', paste0(as.character(nrow(tweets.mean.sentiment) / 50 * 100), '%', '<br />'),
                  'State with best opinion(more than 20 tweets): ', 
                  states.enough.tweets[which.max(states.enough.tweets$mean_sent), "state"], '<br />',
                  'State with worst opinion(more than 20 tweets): ',
                  states.enough.tweets[which.min(states.enough.tweets$mean_sent), "state"]
             )
       )
    }) 
  })
  
  # Real Time Functionality
  if(!exists('predict.tweets', mode = 'function')) {
    source('predict_script.R')
  }

  getTweetData <- eventReactive(input$submit, {
    withProgress(message='package loading (one time - might take a while)', min = 0, max = 100, value = 0, {
      input.ntweets <- input$tweet_number
      input.airline <- input$airline_select_realtime
      setProgress(value = 25)
      results <- predict.tweets(input.ntweets, input.airline)
      
      results.summary <- ddply(results, .(state), summarize, mean_sent=mean(SVM_LABEL))
      tweets.per.state <- data.frame(table(results$state))
      colnames(tweets.per.state) <- c("state", "total_tweets")
      results <- inner_join(results.summary, tweets.per.state, by="state")
      
      setProgress(value = 50)
      #results <- subset(results, select = c(original_text, SVM_LABEL))
      setProgress(value = 100)
      
      # add mean_sent to the data
      states.realtime.ogr <- states.ogr
      states.df <- as.data.frame(states.realtime.ogr)
      states.df$NAME <- tolower(states.df$NAME)
      states.df <- left_join(states.df, results, by=c("NAME"="state"))
      states.realtime.ogr$mean_sent <- states.df$mean_sent
      states.realtime.ogr$total_tweets <- states.df$total_tweets
    })
    
    # set the color palette
    pal.realtime.bins <- colorBin(
      palette = c("red", "green"),
      domain = states.realtime.ogr$mean_sent,
      bins = 4
    )
    
    output$realtime_map <- renderLeaflet({
      leaflet(states.realtime.ogr) %>%
        setView(lat = 37.09024, lng = -95.712891, zoom=3.5) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = ~pal.realtime.bins(mean_sent),
                    highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                    popup = paste(states.realtime.ogr$NAME, "<br />",
                                  "Mean Sentiment: ", states.realtime.ogr$mean_sent, "<br />",
                                  "Total Tweets: ", states.realtime.ogr$total_tweets 
                    )
        )
    })
    
    return(results)
  })
  
  output$realtimeData <- renderDataTable({
    getTweetData()
  })
  
  

})
