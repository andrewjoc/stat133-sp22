# Stat 133, Spring 2022
# Author: Gaston Sanchez
# Description: Shiny app that computes x-y coordinates of a quadratic
#     polynomial and graphs it.
# Inputs:
# - slider: range of x-axis values (minimum and maximum)
# Note: sample code used for lab-8
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)

# Define UI for application that graphs a polynomial
ui <- fluidPage(

    # Application title
    titlePanel("Graphing a polynomial"),

    # Sidebar with a slider input for x-axis range
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "slider", 
                        label = h4("Slider Range  (x-axis)"), 
                        min = -4, 
                        max = 4, 
                        value = c(-4, 4))  # notice that this is a vector
        ),

        # Show plot of polynomial
        mainPanel(
           plotOutput(outputId = "polynomial")
        )
    )
)

# Define server logic required to make the graph
server <- function(input, output) {

    output$polynomial <- renderPlot({
        # function
        poly1 <- function(x) {
            x^2 - 1
        }
        
        # quadratic polynomial with x-values based on input$slider
        x_min = input$slider[1]
        x_max = input$slider[2]
        x = seq(from = x_min, to = x_max, by = 0.1)
        y = poly1(x)
        
        # plot
        plot(x, y, type = 'l', lwd = 4, col = "#FB7215", 
             las = 1, xlim = c(-4, 4), ylim = c(-1, 15))
        abline(h = 0, v = 0, col = '#888888aa', lwd = 1.5)
        title(main = expression(paste(f(x), ' = ', x^2, -1)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
