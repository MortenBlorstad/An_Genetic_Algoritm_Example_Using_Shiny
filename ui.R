#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fillPage(theme = shinytheme("superhero"),
                 tags$head(
                   tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Sonsie One|Cabin:400,700');
      
      h1{
        font-family: 'Sonsie One', cursive;
        font-weight: 500;
        line-height: 1.1;
      }

    "))
                 ),
  
  # Application title
  titlePanel(h1("Gene Game - Survival of the Fittest")),
  
  
    
    # Show a plot of the generated distribution
    
        plotOutput("plot1", click = "plot1_click", height = "80%"),
        h2(textOutput("currentTime")),
        h2(textOutput("Generation"))
    # view underlying data (dubugging)
        #,dataTableOutput("dd")
    
  )
)
