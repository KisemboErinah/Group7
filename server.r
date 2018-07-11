options(shiny.maxRequestSize=1000*1024^2)
library(shiny)
library(shinydashboard)
library(tm)
library(ggplot2)
library(RColorBrewer)
#library(reshape2)
#library(plotly)
library(leaflet)
library(htmltools)

shinyServer(
  
  function(input,output){
    
    
    #.................................putting images on the home page....................................
    output$picto <- renderUI({
      
      h2(" statistical,graphical analysis system",tags$img(src='d.jpg',width=200,height=200),tags$img(src='dddd.jpg',width=200,height=200),tags$img(src='d1.jpg',width=200,height=200))
      
      
    })
    
    
    #..............................shows images in the help..........
    output$pics <- renderUI({
      
      h2("Do you have any questions about our system???",tags$img(src='pp.jpg',width=200,height=200),tags$img(src='pppp.jpg',width=200,height=200))
      
      
    })
    
    #........................displays the schema table with the survey questions when loaded.........
    output$input_file <- renderTable({
      file_to_read = input$file
      if(is.null(file_to_read)){
        return()
      }
      read.csv(file_to_read$datapath , sep = input$sep, header = input$header)
    })
    
    #...............uploadeds the public table with the survey data.......
    my_data2<- reactive({
      files<- input$file2
      if(is.null(files)){
        return()
      }
      read.csv(files$datapath, sep = ",", header = T,stringsAsFactors = F)
    })
    output$out2 <- renderTable({
      if(is.null(my_data2())){
        return()
      }
      input$file2
    })
    
    #......getting column data from the public table......
    output$first <- renderPlot({
      options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
      ted <- my_data2()[,7]
      corp <- Corpus(VectorSource(ted))
      
      
      #..data cleaning
      corp <- tm_map(corp, tolower)
      corp <- tm_map(corp, removePunctuation)
      corp <- tm_map(corp, removeNumbers)
      corp <- tm_map(corp, removeWords, stopwords("english"))
      corp <- tm_map(corp, removeWords, c("beng", "edd", "mba", "gymnasium", "german", "without", "degree", "etc", "meng", "phd", "school", "american", "earning", "education", "realschule", "study", "formal", "completed", "high"))
      corp <- tm_map(corp, stripWhitespace)
      
      
      #term document matrix
      dtm <- TermDocumentMatrix(corp)
      
      # pie chart
      findFreqTerms(dtm, lowfreq = 2)
      termFrequency <- rowSums(as.matrix(dtm))
      termFrequency
      barplot(termFrequency,las = 2, col = rainbow(24))
    })
    
    tutu <- reactive({
      factor(my_data2()$FormalEducation)
      
    })
    tutu2 <- reactive({
      levels(tutu())
    })
    output$tinx <- renderUI({
      selectInput("formaleducation","choose education",choices = tutu2())
      
    })
    act <- reactive({
      tik <- subset(my_data2(),FormalEducation == input$formaleducation)
      
      
      options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
      datam<-melt(tik[,8],id.vars ="UndergradMajor")
      
      
      
      corp <- Corpus(VectorSource(datam))
      
      
      #..data cleaning
      corp <- tm_map(corp, tolower)
      corp <- tm_map(corp, removePunctuation)
      corp <- tm_map(corp, removeNumbers)
      corp <- tm_map(corp, removeWords, stopwords("english"))
      
      corp <- tm_map(corp, stripWhitespace)
      
      
      #term document matrix
      dtm <- TermDocumentMatrix(corp)
      
      # pie chart
      findFreqTerms(dtm, lowfreq = 2)
      termFrequency <- rowSums(as.matrix(dtm))
      termFrequency
      barplot(termFrequency,las = 2, col = rainbow(24))
      
      
    })
    output$this <-renderPlot({
      act()
    })
  }
)