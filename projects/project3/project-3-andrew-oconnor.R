# ===============================================
# Fill in the following fields
# ===============================================
# Title: State of the Union Text Analyzer
# Description: STAT 133 Project 3
# Author: Andrew O'Connor
# Date: 4/29/2022


# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(DT)
library(quanteda)

# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
sotu <- read_csv("state-union-2001-2022.csv")


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
    
    # replace with your widgets
    column(3,
           p(em("Word Trend Analysis")),
           selectInput(inputId = "pres", 
                       label = "Select a President",
                       choices = c("George W. Bush" = "George W. Bush",
                                   "Barack Obama" = "Barack Obama",
                                   "Donald J. Trump" = "Donald J. Trump",
                                   "Joseph R. Biden" = "Joseph R. Biden"),
                       selected = "Barack Obama")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Word Frequency Analysis")),
           radioButtons(inputId = "choosewords", 
                        label = "Choose Words to Include", 
                        choices = c("All Words" = "All Words",
                                    "No Stop Words" = "No Stop Words"), 
                        selected = "All Words")
    ),
    # replace with your widgets
    column(3,
            p(em("Word Frequency Analysis")),
            selectInput(inputId = "year", 
                        label = "Choose a Year",
                        choices = c("2001" = "2001",
                                    "2002" = "2002",
                                    "2003" = "2003",
                                    "2004" = "2004",
                                    "2005" = "2005",
                                    "2006" = "2006",
                                    "2007" = "2007",
                                    "2008" = "2008",
                                    "2009" = "2009",
                                    "2010" = "2010",
                                    "2011" = "2011",
                                    "2012" = "2012",
                                    "2013" = "2013",
                                    "2014" = "2014",
                                    "2015" = "2015",
                                    "2016" = "2016",
                                    "2017" = "2017",
                                    "2018" = "2018",
                                    "2019" = "2019",
                                    "2020" = "2020",
                                    "2021" = "2021",
                                    "2022" = "2022"),
                        selected = "2008")
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Trend Analysis",
                       h3("Word Trend Analysis"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Word Frequency Analysis", 
                       h3("Word Frequency Analysis"),
                       plotOutput("histogram"),
                       hr(),
                       dataTableOutput('table2'))
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
  
  
  sotu_bypres_wt <- reactive ({
    sotu_bypres_wt <- sotu %>% 
      filter(president == input$pres) %>%
      pull(message)
  })
  
  sotu_corpus_wt <- reactive ({
    sotu_corpus_wt <- corpus(sotu_bypres_wt())
  })
  
  sotu_toks_wt <- reactive ({
    sotu_toks_wt <- tokens(sotu_corpus_wt(), remove_punct = TRUE)
  })
  
  sotu_dfm_wt <- reactive ({
    sotu_dfm_wt <- dfm(sotu_toks_wt())
  })
  
  sotu_wt_td <- reactive ({
    sotu_wt_td <- tidy(sotu_dfm_wt())
  })
  
  sotu_wt_idf <- reactive ({
    sotu_wt_idf <- sotu_wt_td() %>%
      bind_tf_idf(term, document, count) %>%
      arrange(desc(tf_idf)) %>%
      slice_head(n = input$wordamounts)
  })
  
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
      ggplot(data = sotu_wt_idf(), aes(x = tf_idf, y = term)) +
        geom_bar(stat = "identity", fill = "#09BFC4") +
      ggtitle(paste("Words", "important", "to", "Speeches", "of", input$pres)) +
      theme(plot.title = element_text(size = 20)) +
      xlab("TF-IDF") +
      ylab("Term")
  })

  output$table1 <- renderDataTable({
    datatable(sotu_wt_idf() %>% select(term, count, tf_idf), colnames = c("Term", "Count", "TF-IDF"))
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  sotu_byyear_wf <- reactive ({
    sotu_byear_wf <- sotu %>%
      filter(input$year == year)
  })
  
  tidy_sotu_wf <- reactive ({
    if (input$choosewords == "All Words") {
      tidy_sotu_wf <- unnest_tokens(sotu_byyear_wf(), word, message)
    }
    else {
      tidy_sotu_wf <- unnest_tokens(sotu_byyear_wf(), word, message) %>%
        anti_join(stop_words, by = "word")
    }
  })
  
  
  sotu_freqs_wf <- reactive ({
    sotu_freqs_wf <- tidy_sotu_wf() %>%
      count(word)
  })
  
  top_words <- reactive({
    top_words <- sotu_freqs_wf() %>% 
      arrange(desc(n)) %>%
      slice_head(n = input$wordamounts)
    })
  
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = top_words(),
           aes(x = reorder(word, -n), y = n)) +
      geom_col(fill = "#f05632", width = 0.9) + 
      ggtitle(paste("Top", input$wordamounts, "Words", "Used", "in", input$year)) +
      theme(plot.title = element_text(size = 20)) +
      xlab("Word") +
      ylab("Count")
  })
  
  output$table2 <- renderDataTable({
    datatable(top_words(), colnames = c("Word", "Count"))
  })
}

# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

