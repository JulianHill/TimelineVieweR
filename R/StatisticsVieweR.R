Wordcount  <- function(username, no)
{
  require(twitteR)
  require(rCharts)
  require(tm)
  
  #authentication
  #load("auth.Rda")
  data(auth, envir=environment())
  registerTwitterOAuth(twitCred)
  
  
  #get tweets
  #tweets <- userTimeline(username,n=no)
  user <- getUser(username)
  tweets <- userTimeline(username,n=no)
 
  no = length(tweets)
  df = data.frame(date = 1:no, favorites=1:no, retweets=1:no, stringsAsFactors=FALSE)
  
  #fill df
  tweets <- twListToDF(tweets)
  
  tweets.text = tweets$text
  
  df$date <- tweets$created
  df$favorites <- tweets$favoriteCount
  
  df$retweets <- tweets$retweetCount
  
 
  
  
  #df = data.frame(followers = 1, following=1, stringsAsFactors=FALSE)
  #df[1] <- followers
  #df[2] <- following
  
  require(tm)
  
  tweets.text <- gsub("amp", "", tweets.text)
  # build a corpus
  mydata.corpus <- Corpus(VectorSource(tweets.text))
  
  # make each letter lowercase
  mydata.corpus <- tm_map(mydata.corpus, tolower) 
  
  # remove punctuation 
  mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
  
  # remove generic and custom stopwords
  my_stopwords <- c(stopwords('german'),stopwords('english'))
  mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
  
  # build a term-document matrix
  mydata.dtm <- TermDocumentMatrix(mydata.corpus)
  
  # inspect the document-term matrix
  mydata.dtm
  
  # inspect most popular words
  #findFreqTerms(mydata.dtm)
  
  m <- as.matrix(mydata.dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  wordcount <- dPlot(
         y="word",
         x="freq",
         groups="title",
         data=d[order(d$freq,decreasing=TRUE)[1:30],],
         type="bar")
  
  wordcount$xAxis(type="addMeasureAxis", outputFormat="#,")
   wordcount$yAxis(type="addCategoryAxis")
   #d1
  
  wordcount$save("wordcount.html", cdn = TRUE)
  
  
  ##################################################
  
  favorites <- df[,c(1,2)]
  favorites$action <- "favorites"
  favorites$text <- tweets$text
  favorites$no <- 1:nrow(favorites)
  colnames(favorites) <- c("date", "number", "action","text","no")
  
  retweets <- df[,c(1,3)]
  retweets$action <- "retweets"
  retweets$text <- tweets$text
  retweets$no <- 1:nrow(retweets)
  colnames(retweets) <- c("date", "number", "action","text","no")
  interactions <- rbind(favorites, retweets)
 
  interactions$date <- lapply(interactions$date, as.character)
  
      
  # FINAL!
  p2 <- nPlot(number ~ no, group = 'action', data = interactions, type = 'scatterChart')
  p2$xAxis(axisLabel = 'Tweet Number')
  p2$yAxis(axisLabel = 'Interactions (Favorites + Retweets)')
  p2$chart(color = c('red', 'blue'))
  p2$chart(size = '#! function(d){return d.number} !#')
  p2$chart(tooltipContent = "#! function(key, x, y, e){ 
         return e.point.date + ': ' + e.point.text   
} !#")
 # p2
  
 p2$save("interactions.html", cdn = FALSE)
  
  ######################################################################
  #### Linear models:
  x <-tweets$favoriteCount
  y <- 1:length(x)
  lmfit = lm( x ~ y )
  
  favorites_fit <- data.frame(date = tweets$created,fitted.values(lmfit),"favorites_fit",favorites$text,favorites$no)
  colnames(favorites_fit) <- c("date", "number", "action","text","no")
  
  x <-tweets$retweetCount
  y <- 1:length(x)
  lmfit = lm( x ~ y )
  
  retweets_fit <- data.frame(date = tweets$created,fitted.values(lmfit),"retweets_fit",retweets$text,retweets$no)
  colnames(retweets_fit) <- c("date", "number", "action","text","no")
  
  fit <- rbind(favorites_fit, retweets_fit)
 fit$date <- lapply(fit$date, as.character)
 
 

##########
p3<-nPlot(number~no,group="action", data=fit, type="lineChart")
p3$chart(color = c('brown', 'green'))
p3$xAxis(axisLabel = 'Tweet Number')
p3$yAxis(axisLabel = 'Average Interactions (Linear)')
p3$chart(tooltipContent = "#! function(key, x, y, e){ 
               return e.point.date + ': ' + e.point.text   
} !#")

#intchart



p3$save("fit.html", cdn = FALSE)





}

