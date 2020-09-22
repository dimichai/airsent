library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(tm)
library(RTextTools)
library(ggplot2)
library(ggmap)

predict.tweets <- function(ntweets, airline) {
  
  if(!exists('models') | !exists('tweets.dtm.unigram')) {
    source("train_script.R")
  }
  
  
  api.key <- "LZ0pHrKAjsfGmWQQbK9vVWJsL"
  api.secret <- "tzAUkktH2KpkKCi44Tri8VtHqCyPwczj2w1KWqu1i3KCXLRtZh"
  api.access.token <- "1734748808-DST2zhTrxEl5X0yH68pyxa4uoTyEwJ12iO2Tzm4"
  api.access.token.secret <- "d1Y9EzAT9F23t7uZORWk2GcLxsglQ98RRXPquxWeDjvC0"
  
  setup_twitter_oauth(api.key, api.secret, api.access.token, api.access.token.secret)
  tweets <- searchTwitter(
    paste0('@', airline),
    n=ntweets, lang = 'en', 
    #geocode = '40.741895,-73.989308,200km',
    geocode = '37.09024,-95.712891,1400mi',
    resultType = "recent"
  )
  
  tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
  tweets.df <- data.frame(id = tweets.df$id, airline=rep(airline, ntweets), sentiment=NA, text=tweets.df$text, screenName = tweets.df$screenName)
  levels(tweets.df$sentiment) <- c("negative", "positive");
  
  new.tweets.corpus <- VCorpus(VectorSource(tweets.df$text))
  new.tweets.corpus <- tm_map(new.tweets.corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
  toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "@\\w+")
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "/")
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "@")
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "http[[:alnum:]]*")
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "RT @[a-z,A-Z]*: ")
  new.tweets.corpus <- tm_map(new.tweets.corpus, toSpace, "@[a-z,A-Z]*")
  new.tweets.corpus <- tm_map(new.tweets.corpus, content_transformer(tolower))
  new.tweets.corpus <- tm_map(new.tweets.corpus, removeNumbers)
  new.tweets.corpus <- tm_map(new.tweets.corpus, removeWords, stopwords("english"))
  new.tweets.corpus <- tm_map(new.tweets.corpus, removePunctuation)
  new.tweets.corpus <- tm_map(new.tweets.corpus, stripWhitespace)
  new.tweets.corpus <- tm_map(new.tweets.corpus, stemDocument)
  
  new.tweets.filtered.text <- data.frame(text = sapply(new.tweets.corpus, as.character), stringsAsFactors = FALSE)
  new.tweets.dtm <- create_matrix(new.tweets.filtered.text$text, originalMatrix = tweets.dtm.unigram)
  container.new.tweets <- create_container(new.tweets.dtm, as.numeric(tweets.df[,3]),
                                           testSize=1:ntweets, virgin=TRUE)
  new.tweets.results <- classify_model(container.new.tweets, models)
  new.tweets.results.final <- data.frame(id = tweets.df$id, screenName = as.character(tweets.df$screenName),
                                         new.tweets.filtered.text, new.tweets.results, original_text = tweets.df$text)
  new.tweets.results.final$screenName <- as.character(new.tweets.results.final$screenName)
  new.tweets.results.final.relevant <- new.tweets.results.final[new.tweets.results.final$SVM_PROB > 0.7, ]
  
  results.final <- data.frame()
  results.final <- rbind(results.final, new.tweets.results.final.relevant)
  
  # Get users
  tweets.users <- lookupUsers(results.final$screenName, includeNA = TRUE)
  tweets.users.df <- do.call("rbind", lapply(tweets.users, as.data.frame))
  # Keep only the users who have declared a location on their profile
  tweets.users.located <- !is.na(tweets.users.df$location)
  # convert to vector for use in a loop
  tweets.users.location.vector <- tweets.users.df$location[tweets.users.located]
  
  # this will be populated inside the loop
  tweets.users.locations <- data.frame(lon = numeric(0), lat=numeric(0), state=character(0))
  withProgress(message = 'finding tweet locations', min = 0, max = length(tweets.users.location.vector), value = 0, {
    for(loc in tweets.users.location.vector) {
      # get the location from google maps API
      location <- tryCatch(geocode(loc, source = "google", output="more"), 
                           error = function(x) location <- NA)
      
      incProgress(amount = 1)
      
      location.df <- data.frame(lon = NA, lat=NA, state=NA)
      if(!is.na(location[1]) ) {
        if("country" %in% colnames(location)) {
          if(location$country == "United States" & "administrative_area_level_1" %in% colnames(location)) {
            location.df <- data.frame(lon = location$lon, lat = location$lat, state = tolower(location$administrative_area_level_1)) 
          }
        }
      } 
      
      # add the new location to the data frame
      tweets.users.locations <- rbind(tweets.users.locations, location.df)
    }
  }) 
  
  states <- map_data("state")
  tweets.with.location <- cbind(results.final, tweets.users.locations)
  tweets.located <- tweets.with.location[!is.na(tweets.with.location$state), ] #tweets who have non null state
  tweets.located[, "SVM_LABEL"] <- laply(tweets.located[, "SVM_LABEL"], as.numeric) #convert SVM_LABEL to numeric
  
  return(tweets.located)
}
