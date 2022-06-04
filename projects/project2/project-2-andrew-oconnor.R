# ===============================================
# Fill in the following fields
# ===============================================
# Title: Client Interest and Savings Rate Calculator
# Description: 
# Author: Andrew O'Connor
# Date: April 4, 2022


# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)


# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Interest and Savings Rate Calculator"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           h4("Annual Income"),
           numericInput(inputId = "income",
                        label = "Income in USD ($)",
                        min = 1,
                        value = 50000)
    ),
    
    # Input(s) for target-amount
    column(3,
           h4("Target Amount"),
           numericInput(inputId = "target",
                        label = "Goal in USD ($)",
                        min = 1,
                        value = 1000000)
    ),
    
    # Input(s) for current-age
    column(3,
           h4("Current Age"),
           numericInput(inputId = 'age', 
                       label = 'Age in years', 
                       min = 0, 
                       value = 25)
    ),
    
    # Input(s) for rate-of-return
    column(3,
           h4("Rate of Return"),
           sliderInput(inputId = 'return_rate', 
                       label = 'Rate as a Percent', 
                       min = 0, 
                       max = 100,
                       value = 5,
                       step = 1)
    )
  ),
  
  hr(),
  h4('Savings Rate and Time to Reach Target Amount'),
  plotOutput('savings_time'),

  hr(),
  h4('Percent Contribution and Percent Growth'),
  plotOutput('contribution_growth'),
  
  hr(),
  h4('Client Summary Statistics'),
  DT::dataTableOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # savings rate, remember to divide by 100
  savings_rate <- reactive ({
    savings_rate <- seq(5,100,5)
  })
  
  # annual contribution
  annual_contribution <- reactive ({
    annual_contribution <- input$income * (savings_rate()/100)
  })
  
  # total contribution
  total_contribution <- reactive ({
    total_contribution <- annual_contribution() * num_years()
  })
  
  # number of years, t
  num_years <- reactive ({
    num_years <- log((((input$return_rate/100) * input$target) / annual_contribution()) + 1) / log(1 + (input$return_rate/100))
  })
  
  # total growth
  total_growth <- reactive ({
    total_growth <- input$target - total_contribution()
  })
  
  dat <- reactive({
    dat <- data.frame(
      Savings_Rate = savings_rate(),
      Annual_Contribution = annual_contribution(),
      Total_Contribution = round(total_contribution(), 1),
      Total_Growth = round(total_growth(), 1),
      Percent_Contrib = round((total_contribution()/input$target) * 100, 2),
      Percent_Growth = round((total_growth()/input$target) * 100, 2),
      Years_To_Target = round(num_years(), 2),
      Age_At_Target = round(input$age + num_years(), 1))
  })
  
  
  # code for plot-1
  
  output$savings_time <- renderPlot({
    
    ggplot(data = dat(), aes(x = Savings_Rate, y = Years_To_Target)) +
      geom_bar(fill = "#09BFC4", stat='identity', width = 3) + 
      theme_classic() +
      labs(x = "Savings Rate (%)",
           y = "Years to Reach Target") +
      
      # make x and y axis increment by 5 
      scale_x_continuous(n.breaks=20, limits = c(2,104), expand = c(0,0)) +
      scale_y_continuous(n.breaks=10, expand = c(0,0)) +
      
      # adjust font size
      theme(text = element_text(size = 14))
  })
  

  # code for plot-2
  
  output$contribution_growth <- renderPlot({
    
    # melt data frame to make grouping easier
    dat1 <- melt(dat() %>% select(Savings_Rate, Percent_Growth, Percent_Contrib), id.vars = "Savings_Rate") 
    
    ggplot(dat1, aes(x= Savings_Rate, y = value)) + 
      geom_bar(aes(fill = variable), position = position_dodge(), stat="identity", width = 4) + 
      theme_classic() +
      labs(x = "Savings Rate (%)",
           y = "Percent (%)") +
      scale_fill_discrete(
        labels = c("Percent Growth", "Percent Contribution")) + 
      guides(fill=guide_legend(title="")) + 
      
      # make x and y axis increment by 5
      scale_x_continuous(n.breaks=20, limits = c(2,104), expand = c(0,0)) +
      scale_y_continuous(n.breaks=10, expand = c(0,0)) +
      
      # adjust font size
      theme(legend.position="bottom",
            text = element_text(size = 15))
  })
  
  
  # code for statistics
  output$table <- DT::renderDataTable({
    # rename data frame with nicer names
    datatable(dat(),
              colnames = c("Savings Rate", "Annual Contribution", "Total Contribution", "Total Growth", "Percent Contribution", "Percent Growth", "Years To Reach Target", "Age at Target"))
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

