#########################################################################################
# Description: R code to create a shiny app for sentiment analysis   ####################
# Author: Shatrunjai P Singh                                         ####################
# Date: 12/17/2016                                                   ####################
# File: ui.R file                                                   #####################
#########################################################################################
#Supress warnings
#options(warn=0)


# install.packages("devtools")
# install.packages('DT')
# install.packages("devtools")
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
# install.packages("sentimentr")
# install.packages("dplyr")
# install.packages("RWeka")
# install.packages("reshape2")

# library(devtools)
#devtools::install_github("aloth/sentiment/sentiment")
#devtools::install_github("shatrunjai/Omegahat-Rstem")

# required pakacges
library(devtools)
library(twitteR)
library(reshape2)
library(sentimentr)
library(plyr)
library(ggplot2)
library(lazyeval)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(devtools)
library(tm)
library(shiny)
library(shinythemes)
library(rsconnect)
library(NLP)
library(openNLP)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(topicmodels)
library(tidytext)
library(DT)
library(sentimentr)
library(dplyr)
library(RWeka)
library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI
(
  fluidPage
  (theme=shinytheme("journal"),
    
    
    # Application title
    titlePanel(title=" ",windowTitle="Sentiment Analysis"),
    h1(strong("Sentiment Analysis App by Manulife Text Analytics Core"), align = "center", style="color:#EB6864"),
    h3(strong("Author: Shatrunjai Singh"), align = "center", style="color:#EB6866"),
    br(),
    br(),
    sidebarPanel
    (
      radioButtons("typeInput", "Upload File, Search Twitter or Input Custom Text",10,choices = c("Custom Text","Search Twitter","Upload File"),selected="Custom Text"),
      
      #Selector for file upload
      fileInput('file', 'Choose info-file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
      tags$hr(),
      checkboxInput('header', 'First Row Has Column Names', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   ''),
      
      actionButton("choice", "Analyze Uploaded File"),
      
      selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
      tags$hr(),
      h4("Options"),
      checkboxInput("emoticon",label ="Replace Emotiocons", value = TRUE),
      checkboxInput("rating",label ="Replace Ratings", value = TRUE),
      checkboxInput("grading",label ="Replace Grading", value = TRUE),
      sliderInput("tweetnum", label = h4("Number of Tweets to get"), min = 1,max = 250, value = 50),
      sliderInput("k", label = h4("Number of topics"), min = 2,max = 15, value = 2),
      sliderInput("ngram", label = h4("N-gram range"), min = 1,max = 10, value = c(1, 2)),
      tags$hr(),
      h4("Input Custom text if not uploading file"),
      #The conditional panel is triggered by the preceding checkbox
      conditionalPanel(
        condition="input$typeInput ==Custom Text",tags$textarea(id="text", rows=10, cols=50,".")),
      tags$hr(),
      actionButton("button", "Submit")
      
    ),
    
    #Plots output   
    mainPanel(
      h4(textOutput("Results of sentiment Analysis")),
      tabsetPanel(
         tabPanel("Sentences Analyzed in the Text",uiOutput("sentences")),
         tabPanel("Overall text sentiment with labels",plotOutput("sent_polarity_plot_txt")),
         tabPanel("Overall text sentiment",plotOutput("overall_sent_plot")),
         tabPanel("Wordcloud All",plotOutput("wordcloud_plot")), 
         tabPanel("Wordcloud Positive",plotOutput("wordcloud_pos_plot")), 
         tabPanel("Wordcloud Negative",plotOutput("wordcloud_neg_plot")), 
         tabPanel("Word Frequencies All",plotOutput("freq_plot")),
         tabPanel("Word Frequencies Positive",plotOutput("freq_pos_plot")),
         tabPanel("Word Frequencies Negative",plotOutput("freq_neg_plot")),
         tabPanel("Key topics identified in data",tableOutput("topic_plot")),
         tabPanel("Click to open the document in a new window?",uiOutput("html_txt")),
         tabPanel("Excel Uploaded Data",tableOutput("table_display"))
      )
    )
    
  )
)

## Sharing on LAN details
## shiny::runApp('Z:/Advanced Analytics/99 - Users/04 - Text Analytics/02 - Projects/02 - Text Minning/my_app8',host="0.0.0.0",port=5050)
## http://10.44.90.33:5050/ 