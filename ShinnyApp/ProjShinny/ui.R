#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Przyczyny stopniowego zmniejszania się długości śledzi oceanicznych wyławianych w Europie"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel
    (
    h2("Parametry"),
       # sliderInput("bins",
       #             "Liczba kubełków:",
       #             min = 1,
       #             max = 50,
       #             value = 30),
      selectInput("var", 
                label = "Wybierz zmienną",
                choices = c("cfin1","cfin2","totaln","sst","sal"),
                selected = "cfin1"),
      sliderInput("range", 
                label = "Zakres dla numerów obserwacji:",
                min = 0, max = 100, value = c(0, 100))
    # textOutput("text1"),
    # textOutput("text2")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot2"),
      plotOutput("distPlot"),
      h2("Podstawowe statystyki zbioru danych"),
      verbatimTextOutput("summary")
    )
  )
))
