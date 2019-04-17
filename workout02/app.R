library(shiny)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Dynamic Comparison of Investing Modalities over Time"),
  
  fluidRow(
    column(4,
           sliderInput("initial_amount",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step = 500,
                       pre = "$"),
           sliderInput("annual_contribution",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500,
                       pre = "$")
           ),
    column(4,
           sliderInput("return_rate",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1),
           sliderInput("growth_rate",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1)
    ),
    column(4,
           sliderInput("years",
                       label = "Years",
                       min = 0,
                       max = 50,
                       value = 10,
                       step = 1),
           selectInput("facet",
                       label = "Facet?",
                       choices = list("No" = FALSE, "Yes" = TRUE),
                       selected = FALSE)
    ),
    fluidRow(
      column(10, offset = 1,
             h4("Timelines"),
             plotOutput("timeline"),
             h4("Balances"),
             verbatimTextOutput("balances")
             )
    )
  )
)

server <- function(input, output) {
  
  balances = reactive({
    #' @title Future Value Function
    #' @description Calculates future value of an investment given the initial invested amount, the annual rate of return, and number of years spanned.
    #' @param amount initial invested amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return future value of the investment
    future_value = function(amount, rate, years) {
      return (amount * (1 + rate)^years)
    }
    
    #' @title Future Value of Annuity Function
    #' @description Calculates future value of a compounded annual investment given the annual contributed amount, the annual rate of return, and number of years spanned.
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return future value of the investment
    annuity = function(contrib, rate, years) {
      return (contrib * ((1 + rate)^years - 1) / rate)
    }
    
    #' @title Future Value of Growing Annuity Function
    #' @description Calculates future value of a compounded annual investment that grows over time given the initial contributed amount, the annual rate of return, the contribution's annual rate of growth, and number of years spanned.
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years
    #' @return future value of the investment
    growing_annuity = function(contrib, rate, growth, years) {
      return (contrib * ((1 + rate)^years - (1 + growth)^years) / (rate - growth))
    }
    
    balances = data.frame()
    
    for (year in 0:input$years) {
      no_contrib = future_value(input$initial_amount, input$return_rate / 100, year)
      fixed_contrib = no_contrib + annuity(input$annual_contribution, input$return_rate / 100, year)
      growing_contrib = no_contrib + growing_annuity(input$annual_contribution, input$return_rate / 100, input$growth_rate / 100, year)
      row = data.frame("year"=year, "no_contrib"=round(no_contrib, 2), "fixed_contrib"=round(fixed_contrib, 2), "growing_contrib"=round(growing_contrib,2))
      balances = rbind(balances, row)
    }
    
    balances
  })
  
  output$timeline <- renderPlot({
    melted = melt(balances(), id.var='year')
    if (input$facet == FALSE) {
      ggplot(melted, aes(x=year, y=value, col=variable)) + geom_point() + geom_line() + ylab("value") + xlab("year") + ggtitle("Three modes of investing")
    } else {
      ggplot(melted, aes(x=year, y=value, col=variable, fill=variable)) + geom_point() + geom_line() + ylab("value") + xlab("year") + ggtitle("Three modes of investing") + facet_grid(. ~ variable) + geom_area(alpha=0.5)
    }
  })
  
  output$balances <- renderPrint({
    balances()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

