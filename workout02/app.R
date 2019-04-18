#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  
  titlePanel("Savings Modalities"),

fluidRow(
  column(4,
         sliderInput("initial",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500),
         sliderInput("annual",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value = 2000,
                     step = 500)
         
  ),
  column(4,
         sliderInput("return",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 5,
                     step = 0.1),
         sliderInput("growth",
                     "Growth Rate (in %)",
                     min = 0, 
                     max = 20,
                     value = 2,
                     step = 0.1)
  ),
  column(4,
         sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 20,
                     step = 1),
         selectInput("facet",
                     "Facet?",
                     choices = c("No", "Yes"))
  )
),
 
hr(),

    # Show a plot of the generated distribution
    mainPanel(
      h4("Timelines"),
      plotOutput("timePlot"),
      
      h4("Balances"),
      verbatimTextOutput("balances"),
      
      width = 15
    )
  )


 

# Define server logic required to draw plot
server <- function(input, output) {
  
  modalities <- reactive({
    initial_amount <- input$initial
    contribution <- input$annual
    years <- input$years
    return_rate <- input$return / 100
    growth_rate <- input$growth / 100
   
    future_value <- function(amount, rate, years) {
      fv <- amount * ((1 + rate)^years)
      return(fv)
    }
    
    annuity <- function(contrib, rate, years) {
      fva <- (((1 + rate)^years - 1) / rate) * contrib
      return(fva)
    }
    
    growing_annuity <- function(contrib, rate, growth, years) {
      fvga <- contrib * (((1 + rate)^years - (1 + growth)^years) / (rate - growth))
      return(fvga)
    }
    
    modalities <- data.frame(
      year = 0:years,
      no_contrib = rep(initial_amount, years + 1),
      fixed_contrib = rep(initial_amount, years + 1),
      growing_contrib = rep(initial_amount, years + 1)
    )
    
    for (y in 1:years) {
      fv <- future_value(initial_amount, return_rate, y)
      fva <- annuity(contribution, return_rate, y)
      fvga <- growing_annuity(contribution, return_rate, growth_rate, y)
      modalities$no_contrib[y + 1] <- fv
      modalities$fixed_contrib[y + 1] <- fv + fva
      modalities$growing_contrib[y + 1] <- fv + fvga
    }
    
    modalities <- melt(modalities, id.vars = "year")
    
    return(modalities)
    
  })
  
  balances <- reactive({
    initial_amount <- input$initial
    contribution <- input$annual
    years <- input$years
    return_rate <- input$return / 100
    growth_rate <- input$growth / 100
    
    future_value <- function(amount, rate, years) {
      fv <- amount * ((1 + rate)^years)
      return(fv)
    }
    
    annuity <- function(contrib, rate, years) {
      fva <- (((1 + rate)^years - 1) / rate) * contrib
      return(fva)
    }
    
    growing_annuity <- function(contrib, rate, growth, years) {
      fvga <- contrib * (((1 + rate)^years - (1 + growth)^years) / (rate - growth))
      return(fvga)
    }
    
    balances <- data.frame(
      year = 0:years,
      no_contrib = rep(initial_amount, years + 1),
      fixed_contrib = rep(initial_amount, years + 1),
      growing_contrib = rep(initial_amount, years + 1)
    )
    
    for (y in 1:years) {
      fv <- future_value(initial_amount, return_rate, y)
      fva <- annuity(contribution, return_rate, y)
      fvga <- growing_annuity(contribution, return_rate, growth_rate, y)
      balances$no_contrib[y + 1] <- fv
      balances$fixed_contrib[y + 1] <- fv + fva
      balances$growing_contrib[y + 1] <- fv + fvga
    }
    
    return(balances)
    
  })
  
  output$timePlot <- renderPlot({
    if (input$facet == "No"){
      ggplot(modalities(), aes(x = year, y = value)) +
        geom_point(aes(color = variable)) +
        geom_line(aes(color = variable)) +
        ggtitle("Three modes of investing")
    } else {
      ggplot(modalities(), aes(x = year, y = value)) +
        geom_point(aes(color = variable)) +
        geom_line(aes(color = variable)) +
        geom_area(aes(fill = variable), alpha = 0.6) +
        ggtitle("Three modes of investing") +
        facet_wrap(~ variable) +
        theme_light()
    }
    
  })
  
  output$balances <- renderPrint({
    dataset <- balances()
    dataset
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

