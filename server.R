
# This is the server logic for a Shiny web application.
# Server.R part of text analytics app in R

library(shiny)

shinyServer(function(input, output) {

#Get input from user using either Custom Text, Twitter or File upload
  get_text<-reactive({  
    #Get the text
    if(input$typeInput=='Custom Text')
    {
      some_txt<-input$text
      some_txt<-get_sentences(some_txt)
      some_txt<-some_txt[[1]]
      return (some_txt)
    }
    #Search twitter 
    else if (input$typeInput=='Search Twitter')
    {
      return(twiter())
    }
    #Upload file
    else if (input$typeInput=='Upload File')
    {
      return(file_upload())
    }
  })
  
 #Fuction to get tweets from twitter
  twiter <- eventReactive(input$button, {
    tweet_term<-input$text
    #Twitter Authorization
    requestURL = "https://api.twitter.com/oauth/request_token"
    accessURL = "https://api.twitter.com/oauth/access_token"
    authURL = "https://api.twitter.com/oauth/authorize"
    consumerKey = "nqhZAqkUAr3onlXSTgY8HLEza"
    consumerSecret = "ASK2gSxhDE4iNqbd66Oq2Z3KDZj7bUpruXLKWy0afsvLn50KGj"
    accessToken = "1950518124-Uhs8osbQ95bYzeXPWCMnxKvjVBiuSkSl41NjW2L"
    accessSecret = "bSgAVaCD8ZHoZ5fGLFQI20snH9EuAad5sZPY6rxGJ6E2l"
    setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessSecret)
    #Get sample tweets
    N=input$tweetnum  # tweets to request from each query
    #Get tweets
    tweets_data=searchTwitter(tweet_term,lang="en",n=N,resultType="recent")
    #extract text only
    tweets= sapply(tweets_data, function(x) x$getText())
    #Return tweets
    return(tweets)
  })
  
  #Funtion to upload a file and extract text out of it
  file_upload <- eventReactive(input$choice, {
    inFile <- input$file
    req(inFile)
    f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    #f<<- read.table('C:\\TEST\\test.csv', sep = ',',header = T)
    f<-data.frame(f)
    vars <- names(f)
    # Update select input immediately after clicking on the action button. 
    #updateSelectInput(session, "columns","Select Columns", choices = vars)
    colum<-input$columns
    f_sub <- f[colum] #subsetting takes place here
    s_txt<-paste(unlist(f),collapse =".")
    return(s_txt)
  })
  
  #Function to clean the text
  clean_text<- function (some_txt)
  {  
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    # replace emotioncs with words
    if(input$emoticon==TRUE){some_txt = replace_emoticon(some_txt)}
    # Replace grades like 'A+'
    if(input$grading==TRUE){some_txt = replace_grade(some_txt)}
    # Replace ratings 
    if(input$rating==TRUE){some_txt = replace_rating(some_txt)}
    # define "tolower error handling" function 
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
    some_txt = sapply(some_txt, try.error)
    # remove NAs in some_txt
    some_txt = some_txt[!is.na(some_txt)]
    names(some_txt) = NULL
    myCorpus = Corpus(VectorSource(some_txt))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    #Add words to be excluded from the list of stop words here
    exceptions   <- c("not","nor","neither","never")
    my_stopwords <- setdiff(stopwords("en"), exceptions)
    myCorpus = tm_map(myCorpus, removeWords,my_stopwords)
    some_txt_clean<-as.character(unlist(sapply(myCorpus, `[`, "content")))
    return(some_txt_clean)
  }
  
  #Function to create word cloud
  create_wordcloud<-function(some_txt)
  {
    myCorpus = Corpus(VectorSource(some_txt))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = input$ngram[1], max = input$ngram[2]))
    tdm.bigram = TermDocumentMatrix(myCorpus, control = list(tokenize = BigramTokenizer))
    freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
    freq.df = data.frame(word=names(freq), freq=freq)
    return (freq.df)
  }
  
  ############################### Topic Modelling AND WORD FREQUENCIES#######################################
  topic_model<-function(final_txt)
  {
  #Create DTM
  myCorpus = Corpus(VectorSource(final_txt))
  DTM = DocumentTermMatrix(myCorpus)
  rowTotals <- apply(DTM , 1, sum) #Find the sum of words in each Document
  DTM   <- DTM[rowTotals> 0, ] 
  #Compute word frequencies
  freq <- sort(colSums(as.matrix(DTM)), decreasing=TRUE) 
  wf <- data.frame(word=names(freq), freq=freq)   
  #Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  #Number of topics
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(DTM,as.numeric(input$k))
  #Get top 5 words for each topic
  top_terms <-as.data.frame(terms(ldaOut,10))
  return(top_terms)
  }
  
  #################################### Actual code part of the app###############################################
  
  # React to submit button
observeEvent(input$button,{
  #Get the text in sentences format and make a copy
  org_txt=get_text()
  #org_txt=get_sentences('I am so ;, happy. I am not 65 happy. I AM SAD. Beantown is amazing. I have zero ideas. I am :-(')[[1]]
  org_txt_copy=org_txt
  #Clean the text   
  cleaned_txt<-clean_text(org_txt)
  
  ############################### Sentiment Analysis at the sentense level #######################################
  #Identify sentiment in the sentences and then create data frame 
  sen<-sentiment(cleaned_txt)
  sen_by<-sentiment_by(cleaned_txt)
  sen<-as.data.frame(sen)
  sen$sentence<-cleaned_txt
  sen=within(sen,{sentiment_pol=ifelse(sentiment>0,"positive","negative")})
  sen=within(sen,{sentiment_pol=ifelse(sentiment==0,"neutral",sentiment_pol)})
  
  #Split up sentences into positive, neutral and negative data frames
  pos_sen<-subset.data.frame(sen,sen$sentiment_pol=='positive',select=element_id:sentiment_pol)
  neg_sen<-subset.data.frame(sen,sen$sentiment_pol=='negative',select=element_id:sentiment_pol)
  neutral_sen<-subset.data.frame(sen,sen$sentiment_pol=='neutral',select=element_id:sentiment_pol)
  
  ########################################## plotting part of the code #####################################
  
 output$sentences<- renderTable({
   #Show the senteces and the number of total sentences
   text.data <- as.data.frame(cbind(org_txt,cleaned_txt))
   colnames(text.data) <- c("Sentences in Input Text", "Senteces after cleanup") 
   text.data
  })
  
  #plot distribution of sentense level polarity with text
  output$sent_polarity_plot_txt<-
    renderPlot(width = 1000, height = 1000,{
      b<-ggplot(sen,aes(element_id,sentiment,fill = sentiment_pol))+geom_bar(stat="identity",position=position_dodge(),color="grey",width=.25) + guides(fill=FALSE)+  coord_flip()+
        scale_fill_hue (c=100, l=80) +scale_fill_manual(values=c( "pink","white", "green"))
      labs(x="Sentence Number", y="Sentiment Score", panel.background = element_rect(fill = "white")) 
      #add text label
      b +  geom_text(aes(label = sentence),hjust=0.02, vjust=0.5,nudge_y=ifelse(sen$sentiment>0,-0.3,0.1))
    })
  
  #Plot the distribution of complete text overtime
  output$overall_sent_plot<-
    renderPlot(width = 1000, height = 500,{
      plot(sen_by)+ ylim(-100, 100) + geom_hline(yintercept = 0) +labs(x="Emotions ", y="Sentiment Score") 
    })
  
  #Plot the word cloud
  output$wordcloud_plot<-
    renderPlot(width = 1000, height = 500,{
      #wordcloud(names(word_list), word_list, scale=c(6,0.5),min.freq = 1, max.words=500,colors=pal2,random.color =T )
      pal=brewer.pal(8,"Dark2")
      freq.df=create_wordcloud(cleaned_txt)
      wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
    })
  
  #Plot the positive word cloud
  output$wordcloud_pos_plot<-
    renderPlot(width = 1000, height = 500,{
      pal=brewer.pal(8,"Dark2")
      freq.df=create_wordcloud(pos_sen$sentence)
      wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
      })
  
  #Plot the negative word cloud
  output$wordcloud_neg_plot<-
    renderPlot(width = 1000, height = 500,{
      pal=brewer.pal(8,"Dark2")
      freq.df=create_wordcloud(neg_sen$sentence)
      wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
    })
  
  #Print word frequencies in ggplot
  output$freq_plot<-
    renderPlot(width = 1000, height = 1000,{ 
      freq.df=create_wordcloud(cleaned_txt)
      ggplot(head(freq.df,25), aes(reorder(word,freq), freq)) +   
        geom_bar(stat="identity",fill='#EB6866') + coord_flip() + 
        xlab("Ngrams") + ylab("Frequency") +
        ggtitle("Top 25 most frequent terms")
    })
  
  #Print positive word frequencies  in ggplot
  output$freq_pos_plot<-
    renderPlot(width = 1000, height = 1000,{ 
      freq.df=create_wordcloud(pos_sen$sentence)
      ggplot(head(freq.df,25), aes(reorder(word,freq), freq)) +   
        geom_bar(stat="identity",fill='#EB6866') + coord_flip() + 
        xlab("Ngrams") + ylab("Frequency") +
        ggtitle("Top 25 most frequent terms")
    })
  
  #Print negative word frequencies  in ggplot
  output$freq_neg_plot<-
    renderPlot(width = 1000, height = 1000,{ 
      freq.df=create_wordcloud(neg_sen$sentence)
      ggplot(head(freq.df,25), aes(reorder(word,freq), freq)) +   
        geom_bar(stat="identity",fill='#EB6866') + coord_flip() + 
        xlab("Ngrams") + ylab("Frequency") +
        ggtitle("Top 25 most frequent terms")
    })
  
  #Print Topics as graph in ggplot
  output$topic_plot <- renderTable({
    top_terms<-topic_model(cleaned_txt)
    top_terms
  })
  
  #Plot the output HTML
  output$html_txt<-
    renderUI({
      set.seed(2)
      highlight(sen_by,org_txt,open=T)
    })
  
    })
  
})











