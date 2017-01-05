#########################################################################################
# Description: R code to create a shiny app for sentiment analysis   ####################
# Author: Shatrunjai P Singh                                         ####################
# Date: 12/17/2016                                                   ####################
# File: ui.R file                                                   #####################
#########################################################################################

library(shiny)
library(shinythemes)
library(DT)

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
      radioButtons("typeInput", "Upload File or Input Custom Text",10,choices = c("Custom Text","Upload File"),selected="Custom Text"),
      
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
      h4("Options for text cleanup"),
      checkboxInput("emoticon",label ="Replace Emotiocons", value = TRUE),
      checkboxInput("rating",label ="Replace Ratings", value = TRUE),
      checkboxInput("grades",label ="Replace Grading", value = TRUE),
      #textInput("k", label = h3("Number of topics"), value = "2"),
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
        tabPanel("Emotion Plot by words",plotOutput("emotion_plot")),
        #tabPanel("Polarity Plot by words",plotOutput("polarity_plot")),
        tabPanel("Polarity Plot by sentence",plotOutput("sent_polarity_plot")),
        tabPanel("Overall text sentiment with labels",plotOutput("sent_polarity_plot_txt")),
        tabPanel("Overall text sentiment",plotOutput("overall_sent_plot")),
        tabPanel("Wordcloud",plotOutput("wordcloud_plot")), 
        tabPanel("Word Frequencies",plotOutput("freq_plot")),
        tabPanel("Key topics identified in data",tableOutput("topic_plot")),
        tabPanel("Click to open the document in a new window?",uiOutput("html_txt")),
        tabPanel("Excel Uploaded Data",tableOutput("table_display"))
      )
    )
    
  )
)

###############################