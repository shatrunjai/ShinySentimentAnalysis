# ShinySentimentAnalysis
Shiny app for text analytics


################################################################################################################
# Description: R code to create a shiny app for text analytics including sentiment analysis ####################
# Author: Shatrunjai P Singh                                                                ####################
# Date: 12/17/2016                                                                          ####################
# File: server.R file  and ui.R file                                                        ####################
################################################################################################################
To run this app:
  a) Open R, and
  b) Type in the following commands:

        install.packages("shiny")
        library(shiny)
        runGitHub( "ShinySentimentAnalysis", "shatrunjai") 
 
################################################################################################################ 
        
Here I use shiny to develop an app which can do the following:
- Perform sentiment analysis on words,
- Perform sentiment analysis at sentense level,
- Get sentiment across a document
- Creat word frequencies
- Create a word cloud
- Mark positive and negative lines within text


I use sentimentr, worldcloud, topicmodeller and tm packages for this analysis.

