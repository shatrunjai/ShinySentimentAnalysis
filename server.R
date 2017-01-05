#########################################################################################
# Description: R code to create a shiny app for sentiment analysis   ####################
# Author: Shatrunjai P Singh                                         ####################
# Date: 12/17/2016                                                   ####################
# File: server.R file                                               #####################
#########################################################################################
#Supress warnings
options(warn=0)

#To install packages if they are not installed
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#install.packages('rsconnect')
#install.packages("lazyeval")
#install.packages("SnowballC")
#install.packages("devtools")
#install.packages("C:/sentiment_0.2.tar.gz", repos = NULL, type="source")
#install.packages("C:/Rstem_0.4-0.tar.gz", repos = NULL, type="source") #This worked!!
#install.packages('DT')
#if (!library("pacman")) install.packages("pacman")
#update.packages(ask = FALSE)
#install.packages("devtools")

install_github("aloth/sentiment/sentiment")
library(sentiment)
library(devtools)
usePackage("twitteR")
usePackage("reshape2")
usePackage("sentimentr")
usePackage("plyr")
usePackage("ggplot2")
usePackage("lazyeval")
usePackage("wordcloud")
usePackage("RColorBrewer")
usePackage("ggplot2")
usePackage("SnowballC")
usePackage("devtools")
usePackage("tm")
usePackage("shiny")
usePackage("shinythemes")
usePackage("rsconnect")
usePackage("NLP")
usePackage("openNLP")
usePackage("reshape2")
usePackage("RColorBrewer")
usePackage("plotly")
usePackage("topicmodels")
usePackage("tidytext")
usePackage("DT")
usePackage("sentimentr")
usePackage("dplyr")


# install.packages("Rstem",repos = "http://www.omegahat.org/R", type="source")
# #install_github("omegahat/Rstem")
# install.packages("twitteR")
# install.packages("reshape2")
# install.packages("sentimentr")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("lazyeval")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("SnowballC")
# install.packages("devtools")
# install.packages("tm")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("rsconnect")
# install.packages("NLP")
# install.packages("openNLP")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# install.packages("plotly")
# install.packages("topicmodels")
# install.packages("tidytext")
# install.packages("DT")
# install.packages("sentimentr")
# install.packages("dplyr")
 

 
# # required pakacges
# library(twitteR)
# library(reshape2)
# library(sentimentr)
# library(plyr)
# library(ggplot2)
# library(lazyeval)
# library(wordcloud)
# library(RColorBrewer)
# library(ggplot2)
# library(SnowballC)
# library(devtools)
# library(tm)
# library(shiny)
# library(shinythemes)
# library(rsconnect)
# library(NLP)
# library(openNLP)
# library(reshape2)
# library(RColorBrewer)
# library(plotly)
# library(topicmodels)
# library(tidytext)
# library(DT)
# library(sentimentr)
# library(dplyr)
# #library("pacman")
# library(sentiment)
# pacman::p_load_gh(c("trinker/stansent", "trinker/sentimentr"))
# pacman::p_load(dplyr)


# Define server logic required to perform sentiment analysis
shinyServer<-function(input, output, session)
{
  
  info <- eventReactive(input$choice, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    # Changes in read.table 
    #f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    f<<- read.table('C:\\TEST\\test.csv', sep = ',',header = T)
    f<-data.frame(f)
    vars <- names(f)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    colum<-input$columns
    #f_sub <- f[colum] #subsetting takes place here
    s_txt<-paste(unlist(f),collapse =".")
    return(s_txt)
    #return(f)
  })
  
  
  #Get text from user
  get_text<-reactive({  
    #Get the text
    if(input$typeInput=='Custom Text')
    {
      some_txt<-input$text
      return (some_txt)
    }
    else if (input$typeInput=='Upload File')
    {
      return(info())
    }
  })
  
  some_txt= "This is not a bad day :-) . This is not a great review ;(! I am not happy at all :-(."
  #some_txt= "I had a peanut butter sandwich for breakfast.I like to eat almonds, peanuts and walnuts. My neighbor got a little dog yesterday.Cats and dogs are mortal enemies.You mustn’t feed peanuts to your dog."
  
  observeEvent(input$button,{
    some_txt=get_text()
    sentences<-get_sentences(some_txt)
    sentences<-sentences[[1]]
    some_txt<-sentences
    
    ################################# Code to make word cloud #########################
    text <-strsplit(as.character(some_txt), " ")
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,c(stopwords("SMART"), "the", "they", "I", "him", "and", "but", "her"))
    myDTM = TermDocumentMatrix(myCorpus,control = list(wordLengths=c(0,Inf)))
    m = as.matrix(myDTM)
    word_list<-sort(rowSums(m), decreasing = TRUE)
    pal2 <- brewer.pal(8,"Dark2")
    
    ############################### Text Cleanup Code #######################################
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    #replace emotioncs with words
    some_txt = replace_emoticon(some_txt)
    #Replace grades like 'A+'
    some_txt = replace_grade(some_txt)
    #Replace ratings 
    some_txt = replace_rating(some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # Remove spaces
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
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
    final_txt=some_txt
    
    ############################### Sentiment Analysis at the word level #######################################
    # # classify emotion
    class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0,verbose=T)
    #Convert the reultant dataset into numeric, take average of all sentences and make it ready for plotting
    x<-data.frame(melt(class_emo))
    x<-subset(x, Var2!="BEST_FIT")
    x$value<-as.numeric(as.character(x$value))
    emotion_names<-aggregate(x[,3],list(x$Var2), mean)

    # classify polarity
    class_pol = classify_polarity(some_txt, algorithm="bayes")
    #Convert the resultant dataset into numeric, take average of all sentences and make it ready for plotting
    y<-data.frame(melt(class_pol))
    y<-subset(y, Var2!="BEST_FIT")
    y$value<-as.numeric(as.character(y$value))
    polar_names<-aggregate(y[,3],list(y$Var2), mean)
    
    ############################### Sentiment Analysis at the sentense level #######################################
    #Create data frame and then plot in ggplot
    sen<-sentiment(sentences)
    sen_by<-sentiment_by(sentences)
    sen<-as.data.frame(sen)
    sen$sentence<-sentences
    sen=within(sen,{sentiment_pol=ifelse(sentiment>0,"positive","negative")})
    ############################### Topic Modelling AND WORD FREQUENCIES#######################################
    #Preprocessing
    
    #Create DTM
    myCorpus = Corpus(VectorSource(final_txt))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,c(stopwords("SMART"), "the", "they", "I", "him", "and", "but", "her"))
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
    k<-2
    #k <- ifelse(is.numeric(input$k):=F,2,as.numeric(input$k))
    #Run LDA using Gibbs sampling
    ldaOut <-LDA(DTM,k)
    #Get top 5 words for each topic
    top_terms <-as.data.frame(terms(ldaOut,10))
    
    
    ################################### Plot the data ###########################################
    #Show the senteces and the number of total sentences
    
    
    text.data <- as.data.frame(some_txt)
    text.data$ProcessedText<-final_txt
    colnames(text.data) <- c("Sentences Analyzed in Text","Sentence after processing")
    
    output$sentences<- renderTable({
      text.data
    })
    
    output$text2<-renderText({
      some_txt
    })
    
    # # plot distribution of emotions
    output$emotion_plot<-
      renderPlot(width = 1000, height = 500,{
        ggplot(data=emotion_names, aes(x=emotion_names$Group.1,y=emotion_names$x,fill=emotion_names$Group.1)) + geom_bar(stat="identity",position=position_dodge(),width=.5,color="grey") + guides(fill=FALSE)+ coord_flip() +
          scale_fill_hue (c=45, l=80) +
          labs(x="emotion categories", y="Emotion Average score")

          })

        #plot distribution of polarity
        output$polarity_plot<-
          renderPlot(width = 1000, height = 500,{
            ggplot(data=polar_names, aes(x=polar_names$Group.1,y=polar_names$x,fill=polar_names$Group.1)) + geom_bar(stat="identity",position=position_dodge(),color="grey",width=.25) + guides(fill=FALSE)+  coord_flip()+
              scale_fill_hue (c=100, l=80) +
              labs(x="Polarity categories", y="Polarity Average score", panel.background = element_rect(fill = "white"))
          })

        
        #plot distribution of sentense level polarity
        output$sent_polarity_plot<-
          renderPlot(width = 1000, height = 1000,{
            b<-ggplot(sen,aes(element_id,sentiment,fill = sentiment_pol))+geom_bar(stat="identity",position=position_dodge(),color="grey",width=.25) + guides(fill=FALSE)+  coord_flip()+
              scale_fill_hue (c=100, l=80) +scale_fill_manual(values=c( "pink", "green"))
            labs(x="Sentence Number", y="Sentiment Score", panel.background = element_rect(fill = "white")) 
            b 
          })
        
        output$sent_polarity_plot_txt<-
          renderPlot(width = 1000, height = 1000,{
            b<-ggplot(sen,aes(element_id,sentiment,fill = sentiment_pol))+geom_bar(stat="identity",position=position_dodge(),color="grey",width=.25) + guides(fill=FALSE)+  coord_flip()+
              scale_fill_hue (c=100, l=80) +scale_fill_manual(values=c( "pink", "green"))
            labs(x="Sentence Number", y="Sentiment Score", panel.background = element_rect(fill = "white")) 
            #add text label
            b +  geom_text(aes(label = sentence),hjust=0.02, vjust=0.5,nudge_y=ifelse(sen$sentiment>0,-0.3,0.1))
          })
        
        #Print Topics as graph in ggplot
        output$topic_plot <- renderTable({
          top_terms
        })
        
        #Print word frequencies  in ggplot
        output$freq_plot<-
          renderPlot(width = 1000, height = 1000,{    
            p <- ggplot(subset(wf, freq>1), aes(word, freq))    
            p <- p + geom_bar(stat="identity",fill='#EB6866')   
            p <- p + theme(axis.text.x=element_text(angle=0, hjust=1))+ coord_flip()   
            p
          })
        
        #Plot the distribution of complete text overtime
        output$overall_sent_plot<-
          renderPlot(width = 1000, height = 500,{
            plot(sen_by)+ ylim(-25, 25)
          })
        
        #Plot the word cloud
        output$wordcloud_plot<-
          renderPlot(width = 1000, height = 500,{
            wordcloud(names(word_list), word_list, scale=c(6,0.5),
                      min.freq = 1, max.words=500,colors=pal2,random.color =T )
          })
        
        # #Plot the output HTML
         output$html_txt<-
           renderUI({
             set.seed(2)
             highlight(sen_by,sentences,open=T)
           })

        #Show the rows in the input excel file
         output$table_display <- renderTable({
           head(f,n=25)
         })
      })
    
  }
  
  
  
  
  