# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(shiny)
library(quanteda)
library(readtext)

# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")

sotu <- read.csv("state-union-2001-2022.csv")


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("State of the Union Text Analyzer"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Both Analyses")),
           sliderInput(inputId = "wordamounts",
                       label = "Amount of Words to Show",
                       min = 1,
                       max = 20,
                       value = 10)
    ),
    
    # replace with your widget
    column(3,
           p(em("Both Analyses")),
           radioButtons(inputId = "choosewords", 
                        label = "Choose Words to Include", 
                        choices = c("All Words" = "allwords",
                                    "No Stop Words" = "stopwords"), 
                        selected = "allwords")
    ),
    
    # replace with your widget
    column(3,
           p(em("Word Frequency Analysis")),
           selectInput(inputId = "presidents", 
                       label = "Select a President",
                       choices = c("George W. Bush" = "bush",
                                   "Barack Obama" = "obama",
                                   "Donald J. Trump" = "trump",
                                   "Joseph R. Biden" = "biden"),
                       selected = "bush"),
           checkboxInput(inputId = "facetbyyear",
                         label = strong("Facet by Year"),
                         value = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           p(em("Sentiment Analysis")),
           selectInput(inputId = "sentiments", 
                       label = "Choose a Sentiment",
                       choices = c("Positive" = "positive",
                                   "Negative" = "negative",
                                   "Neutral" = "neutral"),
                       selected = "positive"),
           checkboxInput(inputId = "facetbysentiment",
                         label = strong("Facet by Sentiment"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis",
                       h3("Word Frequency Analysis"),
                       plotOutput("wfanalysis"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Sentiment Analysis", 
                       h3("Sentiment Analysis"),
                       plotOutput("sentimentan"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
 
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # tokenization
  sotu_bypres <- reactive ({
    sotu_bypres <- sotu %>%
      filter(president == input$presidents)
  })
  
  tidy_sotu <- unnest_tokens(sotu_bypres(), word, message)
  
  sotu_freqs <- tidy_sotu() %>%
    count(word)
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  sotu_bypres <- reactive {(
    sotu_bypres <- sotu %>% 
      filter(president == "George W. Bush") %>% 
      pull(message)
  )}
  
  sotu_corpus <- reactive ({
    sotu_corpus <- corpus(sotu_bypres())
  })
  
  sotu_toks <- reactive ({
    sotu_toks <- tokens(sotu_corpus(), remove_punct = TRUE)
  })

  sotu_dfm <- reactive ({
    sotu_dfm <- dfm(sotu_toks())
  })
  
  sotu_wt_td <- reactive ({
    sotu_wt_td <- tidy(sotu_dfm())
  })
  
  sotu_wt_idf <- reactive ({
    sotu_wt_idf <- sotu_wt_td() %>%
      bind_tf_idf(term, document, count) %>%
      arrange(desc(tf_idf)) %>%
      slice_head(n = 10)
  })
  
  output$sentimentan <- renderPlot ({
    ggplot(data = sotu_wt_idf(), aes(x = tf_idf, y = term)) +
      gg_bar(stat = "identity")
  })
  
}
# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

