#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)
library(mgcv)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
# reactive    
  dataSource<-reactive({
     data<-read.csv("Resources/sledzie.csv",stringsAsFactors = FALSE,na.strings = "?")
     data[complete.cases(data),c("X","length","cfin1","cfin2","totaln","sst","sal")]
     
   })
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    data<-dataSource()
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    plot<-ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+xlab("Numer obserwacji")+ylab("Długość śledzia")
    plot<-plot+ggtitle(paste("Długość śledzia względem zmiennej",input$var))
    
    #cfin1","cfin2","totaln","sst","sal"
    if(input$var=="cfin1")
    {
      plot+geom_line(aes(x=X,y=cfin1),color="green")
    }
    else if (input$var=="cfin2")
    {
      plot+geom_line(aes(x=X,y=cfin2),color="green")
    }
    else if (input$var=="totaln")
    {
      plot+geom_line(aes(x=X,y=totaln),color="green")
    }
    else if (input$var=="sst")
    {
      plot+geom_line(aes(x=X,y=sst),color="green")
    }
    else if (input$var=="sal")
    {
      plot+geom_line(aes(x=X,y=sal),color="green")
    }
    else
    {
      plot+geom_line(aes(x=X,y=cfin1),color="green")
    }

  })
  output$distPlot2 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    data=dataSource()
    length<-length(data$X)
    from<-round(length*(input$range[1]/100))
    to<-round(length*(input$range[2]/100))
    data<-data[from:to,]
    
    # draw the histogram with the specified number of bins
    plot<-ggplot(data=data,aes(x=X,y=length))+ggtitle("Zmiany długości śledzia w analizowanym okresie")+xlab("Numer obserwacji")+ylab("Długość śledzia")
    plot+geom_line()+geom_smooth(color="green")
  })
  output$text1 <- renderText({ 
    paste("You have selected", input$var)
  })
  output$text2 <- renderText({ 
    data=dataSource()
    length<-length(data$X)
    from<-round(length*(input$range[1]/100))
    to<-round(length*(input$range[2]/100))

    paste("You have chosen a range that goes from",
          from, "to", to)
  })
  output$summary <- renderPrint({ 
    data=dataSource()
    length<-length(data$X)
    from<-round(length*(input$range[1]/100))
    to<-round(length*(input$range[2]/100))
    data<-data[from:to,]
    data=tbl_df(data)
    summary(data)
  })
  
})