library(RWeka)
library(tm)
library(RTextTools)
library(e1071)
library(caret)

tweets.raw <- read.csv(file = "data/train_set.csv", colClasses = "character")

analytics.list <- list()

tweets.sentiment <- data.frame(airline=tweets.raw[, "airline"], sentiment=tweets.raw[, "airline_sentiment"], text=tweets.raw[, "text"])

# eliminate neutral tweets
tweets.sentiment <- subset(tweets.sentiment, sentiment != 'neutral')
tweets.sentiment <- droplevels(tweets.sentiment)

tweets.negative <- tweets.sentiment[tweets.sentiment$sentiment == 'negative', ]
# split the negative dataset / 3 
set.seed(6)
tweets.negative <- tweets.negative[sample(nrow(tweets.negative), 3000), ]
tweets.positive <- tweets.sentiment[tweets.sentiment$sentiment == 'positive', ]
tweets.sentiment <- rbind(tweets.negative, tweets.positive)
# Sample the data randomly
tweets.sentiment <- tweets.sentiment[sample(nrow(tweets.sentiment)), ]

# Text Filtering
tweets.corpus <- VCorpus(VectorSource(tweets.sentiment$text))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
tweets.corpus <- tm_map(tweets.corpus, toSpace, "@\\w+")
tweets.corpus <- tm_map(tweets.corpus, toSpace, "/")
tweets.corpus <- tm_map(tweets.corpus, toSpace, "@")
tweets.corpus <- tm_map(tweets.corpus, toSpace, "\\|")
tweets.corpus <- tm_map(tweets.corpus, content_transformer(tolower))
tweets.corpus <- tm_map(tweets.corpus, removeNumbers)
tweets.corpus <- tm_map(tweets.corpus, removeWords, stopwords("english"))
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, stripWhitespace)
tweets.corpus <- tm_map(tweets.corpus, stemDocument)

tweets.dtm.unigram <- DocumentTermMatrix(tweets.corpus)
train.start <- 1;
train.end <- 4827;
test.start <- 4828;
test.end <- 5363;
container.unigram = create_container(tweets.dtm.unigram, as.numeric(tweets.sentiment[,2]),
                                     trainSize=train.start:train.end, testSize=test.start:test.end, virgin=FALSE)

# Linear SVM
models = train_model(container.unigram, algorithm=c("SVM"), kernel = 'linear')

rm(train.start, train.end, test.start, test.end)
rm(container.unigram, tweets.corpus, toSpace, tweets.sentiment, tweets.negative, tweets.positive,
   tweets.raw, analytics.list)
