# Stat 133, Fall 2021
# Author: Gaston Sanchez
# Description: Shiny app that computes a data table of future values, 
#     and graphs their timeline, using a random rate of return.
# Inputs:
# - present: present value (or initial amount)
# - time: number of years
# - rate_mean: mean for annual rate of return
# - rate_sd: standard deviation for annual rate of return
# Details: the rate of return is generated with a normal distribution
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
            hr(),
            h4("Random Rate of Return"),
            # annual rate of return
            sliderInput(inputId = 'rate_mean', 
                        label = 'Mean', 
                        min = 0, 
                        max = 0.20,
                        value = 0.05,
                        step = 0.01),
            # annual rate of return
            sliderInput(inputId = 'rate_sd', 
                        label = 'Standard Deviation', 
                        min = 0, 
                        max = 0.20,
                        value = 0.05,
                        step = 0.01),
            strong("Generated Rate:"),
            tableOutput(outputId = "rate"),
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

# Define server logic required to create objects and plot
server <- function(input, output) {

    # randomly generated rate of return (reactive expression)
    # (follows a normal distribution)
    rate <- reactive({
        rate = rnorm(1, mean = input$rate_mean, sd = input$rate_sd)
        rate
    })
    
    # table of future values (reactive expression)
    # (notice that "rate" is reactive)
    future <- reactive({
      if (input$type == "ord") {
        fv <- input$present * ((1 + rate())^(0:input$time)-1)/(rate())
      }
      else {
        fv <- input$present * ((1 + rate())^(0:input$time)-1)/(rate()) * (1 + rate())
      }
        tbl = data.frame(
            year = 0:input$time,
            amount = fv
        )
        tbl
    })
    
    # display the random generated rate of return
    output$rate <- renderText(
        rate()
    )
    
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
