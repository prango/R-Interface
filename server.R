shinyServer(function(input, output) {
  
  #############################################
  # Chunk - 1 - Authenticate with twitter API
  #############################################
  
  library(twitteR)
  library(ROAuth)
  library(plyr)
  library(stringr)
  library(ggplot2)
  library(sentiment)
  library(wordcloud)
  library(RColorBrewer)
  library(stringdist)
  library(rJava)
  library(wordnet)
  setDict("C:/Users/Home/Documents/R/win-library/3.1/wordnet/dict")
  
  ## Windows users need to get this file
  #download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
  
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL = "https://api.twitter.com/oauth/access_token"
  authURL = "https://api.twitter.com/oauth/authorize"
  
  consumerKey = "GpquxeqnMGTpfJkFkvn1L0Smp"
  consumerSecret = "HkiUxDdGx9DAMpIfEELHgpgf8flpq9A9RRHAVe0HbmvLLm43zq"
  Cred <- OAuthFactory$new(consumerKey=consumerKey,
                           consumerSecret=consumerSecret,
                           requestURL=requestURL,
                           accessURL=accessURL, 
                           authURL=authURL)
  
  setup_twitter_oauth(consumerKey, consumerSecret, "2705605963-jiWpfYPBhoBUd5U8VW7NWy8c47Jrn5TsnWfvAIo", "bBHzW9mylhFyAJ76U3rJqYfoLy6ktNF5wwbcx4A04mMjI")
  # save(Cred, file="twitter authentication.Rdata")
  
  
  ## Future use
  
  #load("twitter authentication.Rdata")
  
  
  ############################################################
  # Chunk  - 2 - Twitter Scrape  #Rangers #Athletics #MLB  
  ############################################################
  output$plot1 <- renderPlot({ 
    
    gp<-input$graph
    if(gp =="1"){
    HashTag <-input$text
    HashTag <- paste('#',HashTag)
    # toString(input$date)
    Rangers.list <- searchTwitter(HashTag,n=input$tweets,since = toString(input$date))  
    Rangers.df = twListToDF(Rangers.list)  
    write.csv(Rangers.df, file='E:/Rproject/RangersTweets.csv', row.names=F)
    
    
    
    
    ###############################
    #Chunk -3- Sentiment Function     
    ###############################
    
    
    score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
    {  
      require(plyr)  
      require(stringr)       
      
      # we got a vector of sentences. plyr will handle a list  
      # or a vector as an "l" for us  
      # we want a simple array ("a") of scores back, so we use   
      # "l" + "a" + "ply" = "laply":  
      
      scores = laply(sentences, function(sentence, pos.words, neg.words) {  
        
        # clean up sentences with R's regex-driven global substitute, gsub():  
        
        sentence = gsub('[[:punct:]]', '', sentence)  
        
        sentence = gsub('[[:cntrl:]]', '', sentence)  
        
        sentence = gsub('\\d+', '', sentence)  
        
        # and convert to lower case:  
        
        sentence = tolower(sentence)  
        
        # split into words. str_split is in the stringr package  
        
        word.list = str_split(sentence, '\\s+')  
        
        # sometimes a list() is one level of hierarchy too much  
        
        words = unlist(word.list)  
        
        # compare our words to the dictionaries of positive & negative terms  
        
        pos.matches = match(words, pos.words)  
        neg.matches = match(words, neg.words)  
        
        # match() returns the position of the matched term or NA  
        # we just want a TRUE/FALSE:  
        
        pos.matches = !is.na(pos.matches)  
        
        neg.matches = !is.na(neg.matches)  
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
        
        score = sum(pos.matches) - sum(neg.matches)  
        
        return(score)  
        
      }, pos.words, neg.words, .progress=.progress )  
      scores.df = data.frame(score=scores, text=sentences)  
      return(scores.df)  
    } 
    
    
    ############################################
    #Chunk - 4 - Scoring Tweets & Adding a column      
    ############################################
    
    #Load sentiment word lists
    hu.liu.pos = scan('E:/Rproject/positive-words.txt', what='character', comment.char=';')
    hu.liu.neg = scan('E:/Rproject/negative-words.txt', what='character', comment.char=';')
    
    #Add words to list
    pos.words = c(hu.liu.pos, 'upgrade')
    neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical')
    
    #Import 3 csv
    DatasetRangers <- read.csv("E:/Rproject/RangersTweets.csv")
    DatasetRangers$text<-as.factor(DatasetRangers$text)
    
    
    
    
    #Score all tweets 
    Rangers.scores = score.sentiment(DatasetRangers$text, pos.words,neg.words, .progress='text')
    
    
    path<-"E:/Rproject/"
    
    
    write.csv(Rangers.scores,file=paste(path,"RangersScores.csv",sep=""),row.names=TRUE)
    
    
    Rangers.scores$Team = 'Rangers'
    
    
    ############################# 
    #Chunk -5- Visualizing         
    #############################
    
    #hist(Rangers.scores$score)
    qplot(Rangers.scores$score)
  }
  
  
  #################################
  #Chunk -6- Comparing 3 data sets                
  #################################
  
  
  
  ####################################################
  #Chunk -7- Classification by emotions and polarity                
  ####################################################
  
  
  
  # Get the text
  else if(gp =="2"){
    HashTag <- input$text
    HashTag <- paste('#',HashTag)
    Rangers.list <- searchTwitter(HashTag, n= input$tweets,since =toString(input$date))  
    Rangers_txt = sapply(Rangers.list, function(x) x$getText())
    
    # Prepare text for the analysis
    Rangers_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Rangers_txt)
    Rangers_txt = gsub("@\\w+", "", Rangers_txt)
    Rangers_txt = gsub("[[:punct:]]", "", Rangers_txt)
    Rangers_txt = gsub("[[:digit:]]", "", Rangers_txt)
    Rangers_txt = gsub("http\\w+", "", Rangers_txt)
    Rangers_txt = gsub("[ \t]{2,}", "", Rangers_txt)
    Rangers_txt = gsub("^\\s+|\\s+$", "", Rangers_txt)
    
    try.error = function(x)
      
    {  
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    
    # lower case using try.error with sapply 
    Rangers_txt = sapply(Rangers_txt, try.error)
    
    # remove NAs in Rangers_txt
    Rangers_txt = Rangers_txt[!is.na(Rangers_txt)]
    names(Rangers_txt) = NULL
    
    #classify emotion
    class_emo = classify_emotion(Rangers_txt, algorithm="bayes", prior=1.0)
    #get emotion best fit
    emotion = class_emo[,7]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(Rangers_txt, algorithm="bayes")
    
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(text=Rangers_txt, emotion=emotion,
                         polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    
    # plot distribution of emotions
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets", 
           title = "Sentiment Analysis of Tweets about Rangers\n(classification by emotion)",
           plot.title = element_text(size=12))
  
  # plot distribution of polarity
  }
  else{
    
    HashTag <- input$text
    HashTag <- paste('#',HashTag)
    Rangers.list <- searchTwitter(HashTag, n= input$tweets,since =toString(input$date))  
    Rangers_txt = sapply(Rangers.list, function(x) x$getText())
    
    # Prepare text for the analysis
    Rangers_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Rangers_txt)
    Rangers_txt = gsub("@\\w+", "", Rangers_txt)
    Rangers_txt = gsub("[[:punct:]]", "", Rangers_txt)
    Rangers_txt = gsub("[[:digit:]]", "", Rangers_txt)
    Rangers_txt = gsub("http\\w+", "", Rangers_txt)
    Rangers_txt = gsub("[ \t]{2,}", "", Rangers_txt)
    Rangers_txt = gsub("^\\s+|\\s+$", "", Rangers_txt)
    
    try.error = function(x)
      
    {  
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    
    # lower case using try.error with sapply 
    Rangers_txt = sapply(Rangers_txt, try.error)
    
    # remove NAs in Rangers_txt
    
    Rangers_txt = Rangers_txt[!is.na(Rangers_txt)]
    names(Rangers_txt) = NULL
    #classify emotion
    class_emo = classify_emotion(Rangers_txt, algorithm="bayes", prior=1.0)
    #get emotion best fit
    
    emotion = class_emo[,7]
    # substitute NA's by "unknown"
    
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(Rangers_txt, algorithm="bayes")
    
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(text=Rangers_txt, emotion=emotion,
                         polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    # plot distribution of polarity
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets",
           title = "Sentiment Analysis of Tweets about Rangers\n(classification by polarity)",
           plot.title = element_text(size=12))
    
    
  }
  
  }) 

  
  output$tb <- renderTable({
  
    HashTag <-input$text
    HashTag <- paste('#',HashTag)
    # toString(input$date)
    Ranger.list <- searchTwitter(HashTag,n=input$tweets,since = toString(input$date))  
    Ranger.df = twListToDF(Ranger.list)  
  length(Ranger.df)
    write.csv(Ranger.df, file='E:/Rproject/RangerTweets.csv', row.names=F)
    
    category <- toString(input$text2)
  syn <- ""
  strwith <- ""
  endwith <- ""
  synsets<-""
  filter <- getTermFilter("ExactMatchFilter",category, TRUE)
   terms <- getIndexTerms("NOUN", 1, filter)
    syn <- getSynonyms(terms[[1]])
  
  filter1 <- getTermFilter("StartsWithFilter",category, TRUE)
  terms1 <- getIndexTerms("NOUN", 5, filter)
  strwith<-sapply(terms, getLemma)
  
  filter2 <- getTermFilter("EndsWithFilter",category, TRUE)
  terms2 <- getIndexTerms("NOUN", 5, filter)
  endwith<-sapply(terms, getLemma)
 
  filter3 <- getTermFilter("ExactMatchFilter",category, TRUE)
  terms3 <- getIndexTerms("NOUN", 1, filter)
  related <- getSynsets(terms[[1]])
  synsets <-  sapply(related, getWord)
  
  caegory <-paste(category,toString(syn))
  category<-paste(category,toString(strwith))
  category <-paste(category,toString(endwith))
  category <- paste(category,toString(synsets))
  
  ###############################
    #Chunk -3- Sentiment Function     
    ###############################
    
    
    score.sentiment = function(sentences,screen_name,Ranger.df,category, pos.words, neg.words, .progress='none')  
    {  
      require(plyr)  
      require(stringr)       
      
      # we got a vector of sentences. plyr will handle a list  
      # or a vector as an "l" for us  
      # we want a simple array ("a") of scores back, so we use   
      # "l" + "a" + "ply" = "laply":  
      
      scores = laply(sentences, function(sentence, pos.words, neg.words) {  
        
        # clean up sentences with R's regex-driven global substitute, gsub():  
        
        sentence = gsub('[[:punct:]]', '', sentence)  
        
        sentence = gsub('[[:cntrl:]]', '', sentence)  
        
        sentence = gsub('\\d+', '', sentence)  
        
        # and convert to lower case:  
        
        sentence = tolower(sentence)  
        
        # split into words. str_split is in the stringr package  
        
        word.list = str_split(sentence, '\\s+')  
        
        # sometimes a list() is one level of hierarchy too much  
        
        words = unlist(word.list)  
        
        # compare our words to the dictionaries of positive & negative terms  
        
        pos.matches = match(words, pos.words)  
        neg.matches = match(words, neg.words)  
        
        # match() returns the position of the matched term or NA  
        # we just want a TRUE/FALSE:  
        
        pos.matches = !is.na(pos.matches)  
        
        neg.matches = !is.na(neg.matches)  
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
        
        score = sum(pos.matches) - sum(neg.matches)  
        
        return(score)  
        
      }, pos.words, neg.words, .progress=.progress )
      
      
     regexp <- "<U\\+([[:digit:]]+)([[:alpha:]])>"
     sentences = gsub(pattern = regexp, '', sentences)
     regexp <- "<U\\+([[:digit:]]+)>"
     sentences = gsub(pattern = regexp, '', sentences)
     
     scoreTable <- data.frame(a = numeric())
     length(Ranger.df)
     j=1
     for(i in 1:nrow(Ranger.df) ) {
       
       
       row <- Ranger.df[i,]
       rdmTweets<-""
       rdmTweets <- userTimeline(row$screenName,10)
      if(length(rdmTweets)!=1){
       twt <- ""
      twt<- paste(twt,toString(rdmTweets))
      sc<- stringdist(category,twt,method ="lv")
       scoreTable <- rbind(scoreTable, data.frame(a = sc))
      }
      else{
        scoreTable <- rbind(scoreTable, data.frame(a = 10000))
      }
      # do stuff with row
     }
        scores.df = data.frame(score=scores,PersonName=screen_name,tweet=sentences,sortBy=scoreTable$a) 
    attach(scores.df)
     scores.df <-scores.df[order(sortBy),]
      return(scores.df)  
    } 
    
    
    ############################################
    #Chunk - 4 - Scoring Tweets & Adding a column      
    ############################################
    
    #Load sentiment word lists
    hu.liu.pos = scan('E:/Rproject/positive-words.txt', what='character', comment.char=';')
    hu.liu.neg = scan('E:/Rproject/negative-words.txt', what='character', comment.char=';')
    
    #Add words to list
    pos.words = c(hu.liu.pos, 'upgrade')
    neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical')
    
    #Import 3 csv
    DatasetRanger <- read.csv("E:/Rproject/RangerTweets.csv")
    DatasetRanger$text<-as.factor(DatasetRanger$text)
    
    
    
    
    #Score all tweets 
    Ranger.scores = score.sentiment(DatasetRanger$text,DatasetRanger$screenName,Ranger.df,category,pos.words,neg.words, .progress='text')
  
  Ranger.scores  
  
  
  }) 
}
)