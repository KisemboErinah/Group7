library(shiny)
library(shinydashboard)

shinyServer(
  function(input,output){
    #.....................................shows the index pictures on the interface when no data is being imported................................
    output$picto <- renderUI({
      
        h2(" statistical,graphical analysis system",tags$img(src='d.jpg',width=200,height=200),tags$img(src='dddd.jpg',width=200,height=200),tags$img(src='d1.jpg',width=200,height=200))
      
      
    })
    #.......shows images in the help..........
    output$pics <- renderUI({
      
      h2("Do you have any questions about our system???",tags$img(src='pp.jpg',width=200,height=200),tags$img(src='pppp.jpg',width=200,height=200))
      
      
    })
  }
)