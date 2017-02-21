#########################################################################################
# Description: R code to create a shiny app for sentiment analysis   ####################
# Author: Shatrunjai P Singh                                         ####################
# Date: 12/17/2016                                                   ####################
# File: ui.R file                                                   #####################
#########################################################################################
#Supress warnings
options(warn=0)
options(shiny.maxRequestSize=30*1024^2)

Install packages
install.packages(devtools)
install.packages(twitteR)
install.packages(reshape2)
install.packages(sentimentr)
install.packages(plyr)
install.packages(ggplot2)
install.packages(lazyeval)
install.packages(wordcloud)
install.packages(RColorBrewer)
install.packages(ggplot2)
install.packages(SnowballC)
install.packages(devtools)
install.packages(tm)
install.packages(shiny)
install.packages(shinythemes)
install.packages(rsconnect)
install.packages(NLP)
install.packages(openNLP)
install.packages(reshape2)
install.packages(RColorBrewer)
install.packages(plotly)
install.packages(topicmodels)
install.packages(tidytext)
install.packages(DT)
install.packages(sentimentr)
install.packages(dplyr)
install.packages(RWeka)
install.packages(shiny)
install.packages(shinythemes)
install.packages(stringr)
install.packages(RedditExtractoR)
install.packages(scales)
install.packages(qdap)
install.packages(plotly)
install.packages(magrittr)


#devtools::install_github("aloth/sentiment/sentiment")  #Not used
#devtools::install_github("shatrunjai/Omegahat-Rstem")  #Not used

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
library(stringr)
# library(jsonlite)
# library(Rfacebook)
library(RedditExtractoR)
library(scales)
library(qdap)
library(plotly)
library(magrittr)
library(shinydashboard)

header <- dashboardHeader(
  title = "Sentiment Analysis",
  tags$li(a(# href = "",
            img(src = 'company_logo.png',
                title = "", 
                height = "30px"
                ),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "menu_dashboard", icon = icon("dashboard")),
    menuItem("Charts", icon = icon("bar-chart"),
             menuSubItem("Sentiment", tabName = "menu_sentiment"),
             menuSubItem("Wordcloud", tabName = "menu_wordcloud"),
             menuSubItem("Word Frequencies", tabName = "menu_freq")
    ),
    menuItem("Export", tabName = "menu_export", icon = icon("download")),
    br(), br(), br(),
    absolutePanel(
      bottom = 10,
      left = 10,
      draggable = F,
      width='100%',
      height='auto',
      a("Author:"),
      br(),
      a("Shatrunjai Singh, Yesheng Liu")
    )  
  )
)
body <- dashboardBody(
  tags$head(tags$style(HTML('
      .main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 20px;
                            }
                            '))),
  tabItems(
    tabItem(tabName = "menu_dashboard",
            fluidRow(
              column(width = 3,
                     box(title = "Inputs",
                         status = "warning",
                         width = NULL,
                         selectInput(
                           inputId = "typeInput",
                           label = "Select Option:",
                           choices = c("Custom Text",
                                       "Search Twitter",
                                       # "Search Facebook",
                                       # "Search Glassdoor",
                                       "Search Reddit",
                                       "Upload File"),
                           selected = "Search Twitter"
                         ),
                         conditionalPanel(
                           condition = "input.typeInput == 'Custom Text'",
                           h5(strong("Input Custom text:")),
                           tags$style(type = "text/css", 
                                      "textarea {width:100%}"
                           ),
                           tags$textarea(id = "custom.text.text", 
                                         rows=10, 
                                         cols=50,
                                         ""
                           )
                         ),
                         conditionalPanel(
                           condition = "input.typeInput == 'Search Twitter'",
                           textInput(
                             inputId = "twitter.text",
                             label = "Search by keywords:",
                             value = ""
                           ),
                           sliderInput(
                             inputId = "twitter.num", 
                             label = h5(strong("Number of tweets:")), 
                             min = 1,
                             max = 250, 
                             value = 50
                           )
                         ),
                         conditionalPanel(
                           condition = "input.typeInput == 'Search Reddit'",
                           textInput(
                             inputId = "reddit.text",
                             label = "Search by keywords:",
                             value = ""
                           ),
                           sliderInput(
                             inputId = "reddit.num",
                             label = h5(strong("Number of posts:")),
                             min = 1,
                             max = 250,
                             value = 50
                           )
                         ),
                         conditionalPanel(
                           condition = "input.typeInput == 'Upload File'",
                           #Selector for file upload
                           fileInput(
                             inputId = "files", 
                             label = "Browse info-file to upload:",
                             accept = c('text/csv',
                                        'text/comma-separated-values',
                                        'text/tab-separated-values',
                                        'text/plain',
                                        '.csv',
                                        '.tsv'
                             )
                           ),
                           selectInput(
                             inputId = "columns",
                             label = "Select Columns:", 
                             choices = NULL          # no choices before uploading
                           ),
                           h5(span(style="color:red", strong("Please set up following options before uploading"))),
                           checkboxInput(
                             inputId = 'header', 
                             label = 'First Row Has Column Names', 
                             value = TRUE
                           ),
                           radioButtons(
                             inputId = "sep", 
                             label = "Separator:",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ','
                           ),
                           radioButtons(
                             inputId = "quote",
                             label = "Quote:",
                             choices = c("None" = "",
                                         "Double Quote"='"',
                                         "Single Quote"="'"),
                             selected = ""
                           )
                         ),
                         h5(strong("Options:")),
                         checkboxInput(
                           inputId = "emoticon",
                           label ="Replace Emoticons", 
                           value = TRUE
                         ),
                         checkboxInput(
                           inputId = "rating",
                           label = "Replace Ratings", 
                           value = TRUE
                         ),
                         # checkboxInput(
                         #   inputId = "grading",
                         #   label ="Replace Grading",
                         #   value = FALSE
                         # ),
                         sliderInput(inputId = "ngram", 
                                     label = h5(strong("N-gram range:")), 
                                     min = 1,
                                     max = 10, 
                                     value = c(1, 2)
                         ),
                         actionButton(
                           inputId = "button",
                           label = "Submit"
                         )
                     )
              ),
              column(width = 9,
                     box(title = "Sentences Analyzed in the Text",
                         status = "success",
                         width = NULL,
                         dataTableOutput("sentences")
                     )
              )
            )
    ),
    tabItem(tabName = "menu_sentiment",
            fluidRow(
              column(width = 4,
                     box(title = "Overall text sentiment with labels",
                         status = "success",
                         width = NULL,
                         uiOutput("sent_polarity_plot_txt")
                     )
              ),
              column(width = 4,
                     box(title = "Overall text sentiment polarity",
                         status = "success",
                         width = NULL,
                         uiOutput("sent_polarity_pie")
                     )
              ),
              column(width = 4,
                     box(title = "Overall text sentiment",
                         status = "success",
                         width = NULL,
                         uiOutput("overall_sent_plot")
                     )
              )
            )
    ),
    tabItem(tabName = "menu_wordcloud",
            fluidRow(
              column(width = 9,
                     box(title = "Wordcloud",
                         status = "success",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         uiOutput("wordcloud_plot")
                     ),
                     box(title = "Key topics identified in data",
                         status = "success",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         dataTableOutput("topic_plot")
                     )
              ),
              column(width = 3,
                     box(title = "Options",
                         status = "warning",
                         width = NULL,
                         sliderInput(inputId = "k", 
                                     label = h5(strong("Number of topics:")), 
                                     min = 2,
                                     max = 15, 
                                     value = 2
                         )
                     )
              )
            )
    ),
    tabItem(tabName = "menu_freq",
            fluidRow(
              column(width = 9,
                     box(title = "Word Frequencies All",
                         status = "success",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         uiOutput("freq_plot")
                     ),
                     box(title = "Word Frequencies Positive",
                         status = "success",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         uiOutput("freq_pos_plot")
                     ),
                     box(title = "Word Frequencies Negative",
                         status = "success",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         uiOutput("freq_neg_plot")
                     )
              ),
              column(width = 3,
                     box(title = "Options",
                         status = "warning",
                         width = NULL,
                         sliderInput(inputId = "count.frequency.words",
                                     label = h5(strong("Number of Words:")),
                                     min = 1,
                                     max = 100,
                                     value = 25
                         )
                     )
              )
            )
    ),
    tabItem(tabName = "menu_export",
            fluidRow(
              column(width = 10,
                     box(title = "Detailed Information",
                         status = "success",
                         width = NULL,
                         dataTableOutput("table_display")
                     )
              ),
              column(width = 2,
                     box(title = "Export",
                         status = "warning",
                         width = NULL,
                         downloadButton(outputId = "button.exportcsv",
                                        label = "Download csv")
                     )
              )
            )
    )
  )
)
dashboardPage(skin = "green",
              header, sidebar, body)
