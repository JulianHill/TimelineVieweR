TwitterTimeline <- function(username,no)
{
  require(twitteR)
  require(rCharts)

  
  #authentication
  
  #data(twitCred, envir=environment())
  registerTwitterOAuth(twitCred)
  
  
  #get tweets
  tweets <- userTimeline(username,n=no)
  user <- getUser(username)
  no = length(tweets)
  df = data.frame(text = 1:no, interactions=1:no, date=1:no,imgs=1:no,ids=1:no, stringsAsFactors=FALSE)
  
  
  tweets <- twListToDF(tweets)
  

    df$text = tweets$text
    df$interactions = tweets$retweetCount+tweets$favoriteCount
    df$date = substr(tweets$created, 1, 19)  
    df$ids = tweets$id

    
 #add media
  
 
 for (i in 1:no)
 { 
   tmp = tweets$text[i]
   
   url <- sub(".*?http://(.*?) .*", "\\1", tmp)
   
   if(grepl(nchar(url),nchar(tmp))){
     
     df$imgs[i] <- " / "
   
   }else{
     
     url <- paste("http://",sub(".*?http://(.*?) .*", "\\1", tmp),sep="")
     
     url_long <- decode_short_url(url)
     
     df$imgs[i] <- url_long
   }
   
   
 }
  
 
  
  tpl <- "
  <b>Interactions:</b> {{x.interactions}}<br/>
  <b>Date:</b> {{x.date}}<br/>
  "
  
  
  d4 <- apply(df, 1, function(x){
    list(
      startDate = gsub("-", ",", as.character(x$date)),
      headline = x$text,
      #text = whisker::whisker.render(tpl, list(x = x)),
      text = paste("URL in Tweet: ",getLongURL.curl(x$imgs),"</br> Number of interactions: ", x$interactions, " </br> Date: ",x$date,sep=""),
      asset = list(media = "")
    )
  })
  
  
  #check if website exists
  if(length(user$url)>0){ 
  headl = toString(paste("Twitter Timeline of: ", user$name," </br> <a href ='",user$url,"'>Website</a>",sep=""))
  }else
  {
   headl = toString(paste("Twitter Timeline of: ", user$name,sep="")) 
    
  }
  
  
  start <- substr(tweets$created[no], 1, 8)
  start <- paste(start,"01",sep="")
  
  # Create Timeline
  m = Timeline$new()
  
 #START
 m$main(
    headline = headl ,
    type = 'default',
    text = paste(user$description,"</br> </br> Followers: ",user$followersCount," </br> Following: ",user$friendsCount,sep=""),
    startDate =  start,
    #asset = list(media = 'http://r-project.org')
    asset = list(media = user$profileImageUrl)
    
    #asset = list(media = 'https://twitter.com/ArjunaSoriano/status/456342231683592192')
    
  )
  m$config(
    font = "Merriweather-Newscycle"
  )
  names(d4) <- NULL
  m$event(d4)
  m$save('index.html')
  
  
  
  
  # Modify JS Path to use Local Assets
  x <- paste(readLines('index.html', warn = F), collapse = '\n')
  x <- gsub('https://github.com/JulianHill/TimelineVieweR/tree/master', 'compiled', x)
  writeLines(x, con = 'index.html')
  
  # Browse Page
  # browseURL('index.html')
  
  
  
}

decode_short_url <- function(url, ...) {
  # PACKAGES #
  require(RCurl)
  
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) )
    if(inherits(x, 'try-error') | length(grep(".*Location: (\\S+).*", x))<1) {
      return(u)
    } else {
      return(gsub(".*Location: (\\S+).*", "\\1", x))
    }
  }
  
  # MAIN #
  # return decoded URLs
  urls <- c(url, ...)
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}

getLongURL.curl <- function(shorturl){
  # uses curl statement to get expanded url from t.co links (or any link)
  # loop through until there's no location attribute... that's the long link.
  newurl <- shorturl
  url <- ""
  while(url != newurl){
    data <- system(paste0("curl -I ", newurl), intern=T)
    if(sum(grepl("location: ", tolower(data))) == 0){
      url <- newurl
    }else{
      data <- subset(data, tolower(substring(data, 1, 9))=="location:")
      stringurl <- substring(data[1], 11, nchar(data[1])-1)
      # sometimes the location data isn't a url.
      if(substring(stringurl, 1, 4)=="http"){ 
        newurl <- stringurl
      }else{
        url <- newurl
      }
    }
  }
  return(newurl)
}
