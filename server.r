library(shiny)
library(plotly)
library(forecast)
library(tsoutliers)
library(forecast)
library(data.table)
library(polynom)
source('helper.r')



#  Data import and to transformation ts
data <- read.csv2("Test_Data.csv", sep=";",header=TRUE)
data <- ts(as.matrix(data, start = 1, frequency = 12))
data[is.na(data)] = 0
data[data<0] = 0
zerocol <- (colSums(data^2) !=0)
data <- data[,zerocol] # all the non-zero columns
x <- ts(data,start = c(2014,1), frequency = 12)
total = ts(rowSums(x), start = c(2014,1), frequency = 12)

input = y(total)

demandSelected = total
outputselect = input
###### Result visualization

shinyServer (function(input, output) {
  
 
  
  output$main_plot <- renderPlotly ({
    # Filter to the currently selected SKU
    
    
    # Calculate the forecast
    FcastSelected = outputselect$fcast
    FcastSelected = ts(FcastSelected,start = end(demandSelected)+c(0,1), frequency = 12)
    
    # Calculate yearly revenue benefit
    
    D = sum(total[length(total):(length(total)-12)])      #input$Yearlyrevenue
    
    alpha = input$Servicelevel/100
    a = as.numeric(input$Stockoutcosts)
    m = input$Grossmargin/100
    MAPE_impr = input$MAPEimpr/100
    V = 3200000
    h = 0.15
    Bi  = V*h*MAPE_impr
    
    B  = D *(1-alpha) * a * m * MAPE_impr + Bi  
    
    

    add = matrix(B/12,nrow=12)
    add = ts(add,start = end(demandSelected)+c(0,1), frequency = 12)
    
    # Generate data.frames for plot
    plotd = data.frame(Y=as.matrix(demandSelected)*m, date=as.Date(as.yearmon(time(demandSelected))))
    plotf = data.frame(Y=as.matrix(FcastSelected)*m, date=as.Date(as.yearmon(time(FcastSelected))))
    plota = data.frame(Y= as.matrix(add)+as.matrix(FcastSelected)*m, date=as.Date(as.yearmon(time(add))))
    colnames(plota)= c("Y","date")
    #Create the plot for the demand 
    p = plot_ly(plotd,x = date, y = Y, type ="scatter", name = 'Demand', mode = "Markers")
    #Add the demand for the forecast
      
    p =   add_trace(p,x = plota$date, y = plota$Y, type = 'scatter', name = 'Margin Potential', mode = "Markers")
    p =   add_trace(p,x = plotf$date, y = plotf$Y, type = 'scatter', name = 'Forecast', mode = "Markers") %>%
        layout(xaxis = list(title= "Time", size = 24), yaxis = list(title= 'Margin', size = 24))
    
  })
  
  # output$FCA_table <- renderTable ({
  # 
  #   # Calculate the best fit method and MApe
  #   MethodSelected  = outputselect$method
  #   ErrorSelected   = 100-outputselect$MAPEavg3m
  #   # Calculate margin potential
  #   D = sum(total[length(total):(length(total)-12)])      #input$Yearlyrevenue
  #   
  #   alpha = input$Servicelevel
  #   a = as.numeric(input$Stockoutcosts)
  #   m = input$Grossmargin
  #   MAPE_impr = input$MAPEimpr
  #   V = 3200000
  #   h = 0.15
  #   
  #   Bt  = D *(1-alpha) * a * m * MAPE_impr
  #   Bi  = V*h*MAPE_impr
  #   
  #   bestfitDT = data.table(MethodSelected,
  #                          ErrorSelected, round(Bt,0),round(Bi,0))
  #   names(bestfitDT) = c("Best fit Method", "FCA last 3 month", "yearly margin potential","yearly inventory value potential")
  #   bestfitDT
  # })  
  
  output$forecastStatus <- renderUI({
    
    'MethodSelected  = outputselect$method
    ErrorSelected   = 100-outputselect$MAPEavg3m
    # Calculate margin potential
    D = sum(total[length(total):(length(total)-12)])      #input$Yearlyrevenue
    
    alpha = input$Servicelevel
    a = as.numeric(input$Stockoutcosts)
    m = input$Grossmargin
    MAPE_impr = input$MAPEimpr
    V = 3200000
    h = 0.15
    
    Bt  = D *(1-alpha) * a * m * MAPE_impr
    Bi  = V*h*MAPE_impr'
    
    m = input$Grossmargin/100
    MethodSelected  = outputselect$method
    ErrorSelected   = 100-outputselect$MAPEavg3m
    Fcastmargin =sum(outputselect$fcast)*m
    # Calculate margin potential
    D = sum(total[length(total):(length(total)-12)])      #input$Yearlyrevenue
    alpha = input$Servicelevel/100
    a = as.numeric(input$Stockoutcosts)
    m = input$Grossmargin/100
    MAPE_impr = input$MAPEimpr/100
    I = 3200000
    h = 0.15
    Bi = I* h * MAPE_impr
    B  = D *(1-alpha) * a * m * MAPE_impr
    
    marginimp = ((Fcastmargin+B)-Fcastmargin)/Fcastmargin*100
    
    
    secCol <- "</td><td width='150 px' align='right'><b>"
    newRow <- "</td></tr><tr><td><b>"
    HTML(paste("<table><tr><td width='240 px'><b>Best fit Method: ",
               secCol,
               MethodSelected,secCol, "<div align = 'left'> &nbsp;</div>",
               newRow,
               "FCA last 3 months: ",
               secCol,
               format(round(ErrorSelected,2),nsmall=0,big.mark=".",decimal.mark=","), secCol, "<div align = 'left'> &nbsp; %</div>",
               newRow,
               "Yearly margin potential: ",
               secCol,
               format(round(B, 0), nsmall=0, big.mark=".", decimal.mark=","),
               secCol, "<div align = 'left'> &nbsp; Euro</div>",
               newRow,
               "Yearly margin improvement: ",
               secCol,
               format(round(marginimp, 2), nsmall=0, big.mark=".", decimal.mark=","),
               secCol, "<div align = 'left'> &nbsp; %</div>",
               newRow,
               "Yearly inventory value potential: ",
               secCol,
               format(round(Bi, 0), nsmall=0, big.mark=".", decimal.mark=","), secCol, "<div align = 'left'> &nbsp; Euro</div>",
               "</b></td></tr></table>"))
  })
})

