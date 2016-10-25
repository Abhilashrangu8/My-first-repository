library('shiny')
library('shinythemes')
library('plotly')
library('forecast')


shinyUI(fluidPage(

  titlePanel(h3("Margin Potential & Forecast")), 

  mainPanel(width = 12,
    fluidRow(
      selectInput("Stockoutcosts", tags$b("Industry"), choices=list("Groceries"=0.5,"Consumer Goods"=1,"Construction"=2,"Automobiles"=3,"Machinery"=4,"High Tech"=5,"Agriculture"=6), selected=2),
                                      
      plotlyOutput("main_plot", height = 300),
  
      fluidRow(
        column(6,
               sliderInput("Servicelevel", label = tags$b("Service Level in %"), min = 0, max = 99.99, value = 90),
               sliderInput("Grossmargin", label = tags$b("Gross Margin in %"), min = 0, max = 70, value = 25)),
  
        column(6, 
              sliderInput("MAPEimpr", label = tags$b("Reduction of Forecast Error (MAPE) in %"), min = 0, max = 100, value = 0))
        ), 
        #tableOutput("FCA_table")
        htmlOutput("forecastStatus")
        )
    )
  )
  )