#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

shinyUI <- fluidPage(
  titlePanel("The next word predicter"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId="Text", label = "Please enter your text here: ", value =""),
      radioButtons("TypeText","Select type of tekst:",c("General","Twitter","Blogs","News"))
      ),
    mainPanel(
      htmlOutput("html1"),
      htmlOutput("html2")
      )
    )
  )
  
