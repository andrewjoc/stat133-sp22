# Stat 133, Fall 2021
# Author: Gaston Sanchez
# Description: Shiny app that computes a data table of future values, 
#     and graphs their timeline.
# Inputs:
# - present: present value (or initial amount)
# - time: number of years
# - rate: annual rate of return (in percentage!)
# Note: sample code used for lab-10

library(shiny)
library(tidyverse)

# Define UI for application that graphs a timeline
ui <- fluidPage(
  
  # Application title
  titlePanel("Future Value (compound interest)"),
  
  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      # present value
      numericInput(inputId = "present",
                   label = "Initial amount ($)",
                   min = 1,
                   value = 1000),
      # time (number of years)
      sliderInput(inputId = 'time', 
                  label = 'Time (in years)', 
                  min = 1, 
                  max = 50,
                  value = 5,
                  step = 1),
      # annual rate of return
      sliderInput(inputId = 'rate', 
                  label = 'Rate of return (in %)', 
                  min = 0, 
                  max = 20,
                  value = 5,
                  step = 0.1),
      # specify type of annuity
      radioButtons(inputId = "type",
                   label = "Type of Annuity",
                   choices = c("ordinary annuity" = "ord", "annuity due" = "due"))
    ),
    
    # Show a plot of the generated future values
    mainPanel(
      plotOutput("timeline"),
      hr(),
      h4('Future Value'),
      verbatimTextOutput('table')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # table of future values (reactive expression)
  # (notice that "rate" is divided by 100)
  future <- reactive({
    if (input$type == "ord") {
      fv <- input$pmt * ((1 + input$rate / 100)^(0:input$time)-1)/(input$rate/100)
    }
    else {
      fv <- input$pmt * ((1 + input$rate / 100)^(0:input$time)-1)/(input$rate/100) * (1 + input$rate/100)
    }
    tbl = data.frame(
      year = 0:input$time,
      amount = fv
    )
    tbl
  })
  
  # plot timeline
  # (notice that "future" is reactive)
  output$timeline <- renderPlot({
    ggplot(data = future(), aes(x = year, y = amount)) +
      geom_point() +
      geom_line() + 
      theme_minimal()
  })
  
  # show data frame
  output$table <- renderPrint({
    print(future(), print.gap = 3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
