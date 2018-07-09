options(shiny.maxRequestSize=1000*1024^2)
library(shiny)
library(shinydashboard)
library(tm)

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
      read.table(file_to_read$datapath , sep = input$sep, header = input$header)
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
  }
)