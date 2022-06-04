# Stat 133, Fall 2021
# Author: Gaston Sanchez
# Description: Shiny app that computes a data table of simulated future values, 
#     and graphs their timeline, using random rates of return.
# Inputs:
# - present: present value (or initial amount)
# - time: number of years
# - sims: number of simulations
# - rate_mean: mean for annual rate of return
# - rate_sd: standard deviation for annual rate of return
# Details: the rates of return are generated with a normal distribution
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
      # number of simulations
      numericInput(inputId = "sims",
                   label = "Number of simulations",
                   min = 1,
                   max = 100,
                   value = 3,
                   step = 1),
      hr(),
      h4("Random Rates of Return"),
      # annual rate of return
      sliderInput(inputId = 'rate_mean', 
                  label = 'Mean', 
                  min = 0, 
                  max = 0.20,
                  value = 0.10,
                  step = 0.01),
      # annual rate of return
      sliderInput(inputId = 'rate_sd', 
                  label = 'Standard Deviation', 
                  min = 0, 
                  max = 0.20,
                  value = 0.18,
                  step = 0.01),
    ),
    
    # Show a plot of the generated future values
    mainPanel(
      plotOutput("timeline")
    )
  )
)

# Define server logic required to create table and graph timelines
server <- function(input, output) {
  
  # data table of future values (reactive expression)
  dat <- reactive({
    # initialize list to store simulations
    fv_list = as.list(1:input$sims)
    names(fv_list) = paste0("sim", 1:input$sims)
    
    # loop to compute simulations
    for (s in 1:input$sims) {
      rate = rnorm(1, mean = input$rate_mean, sd = input$rate_sd)
      fv_list[[s]] = input$present * (1 + rate)^(0:input$time)
    }
    
    # assemble data frame, adding time column
    fv_dat = data.frame(fv_list)
    fv_dat$year = 0:input$time
    
    # reshape table into "long" (or "tall") format
    # (this is the returned table)
    pivot_longer(
      fv_dat,
      cols = starts_with("sim"),
      names_to = "simulation",
      values_to = "amount")
  })
  
  # plot timeline
  # (notice that "dat" is reactive)
  output$timeline <- renderPlot({
    ggplot(data = dat(), aes(x = year, y = amount, group = simulation)) +
      geom_point(aes(color = simulation)) +
      geom_line(aes(color = simulation)) + 
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
