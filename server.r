options(shiny.maxRequestSize=1000*1024^2)
library(shiny)
library(shinydashboard)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
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
      
      h2("Do you need any directions about our system???",tags$img(src='pp.jpg',width=200,height=200),tags$img(src='pppp.jpg',width=200,height=200))
      
      
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
    
    #.....lists the different data columns in order....
    tutu <- reactive({
      factor(colnames(my_data2()))
      
    })
    tutu2 <- reactive({
      levels(tutu())
    })
    output$tinx <- renderUI({
      selectInput("column","choose Data Column",choices = tutu2())
      
    })
    
    
    #new render
    tutu3 <- reactive({
      factor(colnames(my_data2()))
      
    })
    tutu4 <- reactive({
      levels(tutu3())
    })
    output$colmn <- renderUI({
      selectInput("colmn","choose Data Column",choices = tutu4())
      
    })  
    #......matches the data column education....
    
    tuti <- reactive({
      if(colnames(my_data2()[7]) == input$colmn){
        tik <- subset(my_data2(), colnames(my_data2()[7]) == input$colmn)
        factor(tik[,7])}
      else if(colnames(my_data2()[75]) == input$colmn){
        tik <- subset(my_data2(), colnames(my_data2()[75]) == input$colmn)
        factor(tik[,75])
      }
      else if(colnames(my_data2()[83]) == input$colmn){
        tik <- subset(my_data2(), colnames(my_data2()[83]) == input$colmn)
        factor(tik[,83])
      }
      else if(colnames(my_data2()[80]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[80]) == input$colmn)
        factor(tik[,80]) 
      }
      else if(colnames(my_data2()[98]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[98]) == input$colmn)
        factor(tik[,98]) 
      }
      else if(colnames(my_data2()[102]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[102]) == input$colmn)
        factor(tik[,102]) 
      }
      else if(colnames(my_data2()[104]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[104]) == input$colmn)
        factor(tik[,104]) 
      }
      else if(colnames(my_data2()[106]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[106]) == input$colmn)
        factor(tik[,106]) 
      }
      else if(colnames(my_data2()[126]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[126]) == input$colmn)
        factor(tik[,126]) 
      }
      else if(colnames(my_data2()[13]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[13]) == input$colmn)
        factor(tik[,13]) 
      }
      else if(colnames(my_data2()[14]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[14]) == input$colmn)
        factor(tik[,14]) 
      }
      else if(colnames(my_data2()[11]) == input$colmn){
        tik <- subset(my_data2(),colnames(my_data2()[11]) == input$colmn)
        factor(tik[,11]) 
      }
    })
    tuti2 <- reactive({
      levels(tuti())
    })
    output$tin <- renderUI({
      
      selectInput("formaleducation","choose education",choices = tuti2())
    })
    
    # draws the different bar graphs
    act <- reactive({
      if(colnames(my_data2()[7]) == input$colmn){
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
      }
      
      else if(colnames(my_data2()[83]) == input$colmn){
        tik <- subset(my_data2(),AdsAgreeDisagree1 == input$formaleducation)
        
        ggplot(tik, aes(x= as.factor(tik[,84]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=AdsAgreeDisagree2), width = 0.2,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the responses to the AdsAgreeDisagree1 relate to the ones to AdsAgreeDisagree2 by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[80]) == input$colmn){
        tik <- subset(my_data2(),AdBlocker == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,81]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=AdBlockerDisable), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to the AdBlocker relate to the responses to AdBlockerDisable by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[98]) == input$colmn){
        tik <- subset(my_data2(),EthicsChoice == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,100]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=EthicsResponsible), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to the EthicsReport is related to EthicsResponsible by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[102]) == input$colmn){
        tik <- subset(my_data2(), StackOverflowRecommend == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,103]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=StackOverflowVisit), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to the StackOverflowRecommend relate to the responses to StackOverflowVisit the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[104]) == input$colmn){
        tik <- subset(my_data2(),StackOverflowHasAccount == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,105]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=StackOverflowVisit), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to StackOverflowHasAccount relate to the responses to StackoverflowVisit by
                                                                                                                                                                                                                                                                                                                                                                                      the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[106]) == input$colmn){
        tik <- subset(my_data2(),StackOverflowJobs == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,108]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=StackOverflowJobsRecommend), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                               label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to StackOverflowJobs relate to the responses to StackOverflowJobsRecommend by the developers"))+ 
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[126]) == input$colmn){
        tik <- subset(my_data2(),Dependents == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,116]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=HoursComputer), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to Dependents relate to the responses to HoursComputer by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[13]) == input$colmn){
        tik <- subset(my_data2(),JobSatisfaction == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,16]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=JobSearchStatus), width = 0.2,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to JobSatisfaction relate to the responses JobSearchStatus by the developers"))+
          xlab("Responses")+ylab("Total") 
        
      }
      else if(colnames(my_data2()[14]) == input$colmn){
        tik <- subset(my_data2(),CareerSatisfaction == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,15]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=HopeFiveYears), width = 0.2,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the responses to CareerSatisfaction relate to the responses to HopeFiveYears by the developers"))+
          xlab("Responses")+ylab("Total") 
        
      }
      else if(colnames(my_data2()[11]) == input$colmn){
        tik <- subset(my_data2(),YearsCoding == input$formaleducation)
        
        
        ggplot(tik, aes(x= as.factor(tik[,13]),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=JobSatisfaction), width = 0.2,position = "dodge") + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the responses to YearsCoding relate to the responses to JobStatisfaction by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      
    })
    output$this <-renderPlot({
      act()
    })
    
    #....bar chart showing the Formal Education of the developres 
    
    output$first <- renderPlot({
      if(colnames(my_data2()[7]) == input$column){
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
        df= (termFrequency/sum(termFrequency))*100
        tx <- barplot(df,las = 2, col = rainbow(24),xlab = "FormalEducation", ylab = "count (%)",main = "A bar chart showing th formal Education levels of the Developers", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
        }
      
      else 
        
        if(colnames(my_data2()[75]) == input$column){
          options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
          
          
          ted1 <- my_data2()[,75]
          
          corp <- Corpus(VectorSource(ted1))
          
          
          #..data cleaning
          corp <- tm_map(corp, tolower)
          corp <- tm_map(corp, removePunctuation)
          corp <- tm_map(corp, removeNumbers)
          corp <- tm_map(corp, removeWords, stopwords("english"))
          
          corp <- tm_map(corp, stripWhitespace)
          
          
          
          #term document matrix
          dtm <- TermDocumentMatrix(corp,control = list(wordLengths=c(1,Inf)))
          dtm
          # pie chart
          
          termFrequency <- rowSums(as.matrix(dtm))
          df <- (termFrequency/sum(termFrequency))*100
          tx <- barplot(termFrequency, col = rainbow(12), xlab = "Operating System", ylab = "count (%)", main = "A bar chart showing the Operating systems used by the developers", width = 0.5, space = 1, legend.text = TRUE)
          text(tx, 0, round(df, 1), cex=1, pos=3)
        }
      
      else if(colnames(my_data2()[83]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[83]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsAgreeDisagree1),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AdsAgreeDisagree1)), width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the responses to the AdsAgreeDisagree1 Question by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[115]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[115]) == input$column)
        ggplot(tik, aes(x= as.factor(WakeTime),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=WakeTime), width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the responses to the wake up time of the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[80]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[80]) == input$column)
        ggplot(tik, aes(x= as.factor(AdBlocker),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AdBlocker)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the reponses to the AdBlocker Question by the Devlopers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[81]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[81]) == input$column)
        ggplot(tik, aes(x= as.factor(AdBlockerDisable),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AdBlockerDisable)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing responses to the Adblocker Question by the Developers"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[5]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[5]) == input$column)
        ggplot(tik, aes(x= as.factor(Student),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(Student)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the Students and non Students"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[3]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[3]) == input$column)
        ggplot(tik, aes(x= as.factor(OpenSource),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(OpenSource)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the reponses to the OpenSource Question by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[2]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[2]) == input$column)
        ggplot(tik, aes(x= as.factor(Hobby),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(Hobby)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the responses to the Hobby Question by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[8]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[8]) == input$column)
        ggplot(tik, aes(x= as.factor(UndergradMajor),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(UndergradMajor)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                              label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing  the Under graduation Major of the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[11]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[11]) == input$column)
        ggplot(tik, aes(x= as.factor(YearsCoding),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(YearsCoding)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the years spent coding by the developers"))+
          xlab("Years")+ylab("Total")
        
      }
      else if(colnames(my_data2()[13]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[13]) == input$column)
        ggplot(tik, aes(x= as.factor(JobSatisfaction),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(JobSatisfaction)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing how the devlopers are satisfied by their current jobs"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[14]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[14]) == input$column)
        ggplot(tik, aes(x= as.factor(CareerSatisfaction),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(CareerSatisfaction)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing how the developers are satisfied by their careers"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[15]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[15]) == input$column)
        ggplot(tik, aes(x= as.factor(HopeFiveYears),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(HopeFiveYears)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing what the developers hope to accomplish in yht next five years"))+
          xlab("Responses")+ylab("Total")
        
      } 
      else if(colnames(my_data2()[16]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[16]) == input$column)
        ggplot(tik, aes(x= as.factor(JobSearchStatus),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(JobSearchStatus)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the Job searching status of the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[17]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[17]) == input$column)
        ggplot(tik, aes(x= as.factor(LastNewJob),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(LastNewJob)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing When the developers last got a new job"))+
          xlab("Responses")+ylab("Total")
      }
      else if(colnames(my_data2()[51]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[51]) == input$column)
        ggplot(tik, aes(x= as.factor(UpdateCV),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(UpdateCV)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing why the developers last changed thier CVs"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[52]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[52]) == input$column)
        ggplot(tik, aes(x= as.factor(Currency),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(Currency)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the currecy type earned by the developers"))+
          xlab("Curreny")+ylab("Total")
        
      }
      else if(colnames(my_data2()[58]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[58]) == input$column)
        ggplot(tik, aes(x= as.factor(TimeFullyProductive),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(TimeFullyProductive)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how long a new developer can fully become productive to a new team"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[61]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[61]) == input$column)
        ggplot(tik, aes(x= as.factor(TimeAfterBootcamp),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(TimeAfterBootcamp)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how long it took the developers to get a job after bootcamp"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[76]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[76]) == input$column)
        ggplot(tik, aes(x= as.factor(NumberMonitors),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(NumberMonitors)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                              label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the number of monitors used by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[79]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[79]) == input$column)
        ggplot(tik, aes(x= as.factor(CheckInCode),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(CheckInCode)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the number of times the delopers have committed in their code"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[98]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[98]) == input$column)
        ggplot(tik, aes(x= as.factor(EthicsChoice),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(EthicsChoice)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                          label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing whether  developers can write unethical code or not"))+
          xlab("Response")+ylab("Total")
        
      }
      else if(colnames(my_data2()[100]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[100]) == input$column)
        ggplot(tik, aes(x= as.factor(EthicsResponsible),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(EthicsResponsible)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing who is responsible for the ethics of the code"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[103]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[103]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowVisit),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(StackOverflowVisit)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing how frequently the developers visit StackOverflow"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[104]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[104]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowHasAccount),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(StackOverflowHasAccount)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                                label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing whether the developers have an account on StackOverflow or not"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[105]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[105]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowParticipate),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(StackOverflowParticipate)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing how frequently the developersparticipte on stackOverflow"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[106]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[106]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowJobs),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(StackOverflowJobs)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing whether the developers visit the StackOverflow jobs or not"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[109]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[109]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowConsiderMember),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(StackOverflowConsiderMember)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing whether the developers consider themselves as part of StackOverflow"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[110]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[110]) == input$column)
        ggplot(tik, aes(x= as.factor(HypotheticalTools1),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(HypotheticalTools1)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the level to which developers are interested in the peer mentoring system"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[111]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[111]) == input$column)
        ggplot(tik, aes(x= as.factor(HypotheticalTools2),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(HypotheticalTools2)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the level to which the developres like a privete platform for peaple new to coding"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[112]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[112]) == input$column)
        ggplot(tik, aes(x= as.factor(HypotheticalTools3),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(HypotheticalTools3)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing to what level the developers would like a programming oriented blog platform"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[113]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[113]) == input$column)
        ggplot(tik, aes(x= as.factor(HypotheticalTools4),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(HypotheticalTools4)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing to what level the devlopers are interested in the employer/ job review sysytem"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[114]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[114]) == input$column)
        ggplot(tik, aes(x= as.factor(HypotheticalTools5),y = prop.table(..count..) * 100)) + geom_bar(aes(fill = factor(HypotheticalTools5)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing to what level the developers are interested in an area with Q&A related to career growth"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[116]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[116]) == input$column)
        ggplot(tik, aes(x= as.factor(HoursComputer),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(HoursComputer)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the number of hours spent on the computer by the developers"))+
          xlab("Time")+ylab("Total")
        
      }
      else if(colnames(my_data2()[117]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[117]) == input$column)
        ggplot(tik, aes(x= as.factor(HoursOutside),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(HoursOutside)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                          label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing the number of hours spent outside by the developers"))+
          xlab("Time")+ylab("Total")
        
      }
      else if(colnames(my_data2()[118]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[118]) == input$column)
        ggplot(tik, aes(x= as.factor(SkipMeals),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(SkipMeals)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how frequently the developers skip meals to code"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[120]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[120]) == input$column)
        ggplot(tik, aes(x= as.factor(Exercise),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(Exercise)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal() + ggtitle(paste("A bar graph showing how freqeuntly the developres exercise"))+
          xlab("Age groups")+ylab("Total")
        
      }
      else if(colnames(my_data2()[121]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[121]) == input$column)
        ggplot(tik, aes(x= as.factor(Gender),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(Gender)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                               label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing Gender of the developers"))+
          xlab("Gender")+ylab("Total")
        
      }
      else if(colnames(my_data2()[126]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[126]) == input$column)
        ggplot(tik, aes(x= as.factor(Dependents),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(Dependents)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the developers with dependents"))+
          xlab("Responses")+ylab("Total")
        
      }else if(colnames(my_data2()[127]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[127]) == input$column)
        ggplot(tik, aes(x= as.factor(MilitaryUS),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(MilitaryUS)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the developers that are part of the US army"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[128]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[128]) == input$column)
        ggplot(tik, aes(x= as.factor(SurveyTooLong),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(SurveyTooLong)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the developers' responses to the length of the survey"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[129]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[129]) == input$column)
        ggplot(tik, aes(x= as.factor(SurveyEasy),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(SurveyEasy)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the developers' responses to structure of the survey"))+
          xlab("Age groups")+ylab("Total")
        
      }
      else if(colnames(my_data2()[125]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[125]) == input$column)
        ggplot(tik, aes(x= as.factor(Age),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(Age)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing Age groups of the developers"))+
          xlab("Age groups")+ylab("Total")
        
      }
      else if(colnames(my_data2()[84]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[84]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsAgreeDisagree2),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(AdsAgreeDisagree2)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the reponses to the AdsAgreeDisagree2 question by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[85]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[85]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsAgreeDisagree3),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AdsAgreeDisagree3)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the responses to the AdsAgreeDisAgree3 Question by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[99]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[99]) == input$column)
        ggplot(tik, aes(x= as.factor(EthicsReport),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(EthicsReport)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                          label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the developers can report unethical code"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[101]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[101]) == input$column)
        ggplot(tik, aes(x= as.factor(EthicalImplications),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(EthicalImplications)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the developers think that unethical code has implications"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[94]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[94]) == input$column)
        ggplot(tik, aes(x= as.factor(AIDangerous),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AIDangerous)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                        label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the most dangerous aspect of AI basing on the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[95]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[95]) == input$column)
        ggplot(tik, aes(x= as.factor(AIInteresting),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AIInteresting)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the most interesting aspect of AI  basing on the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[96]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[96]) == input$column)
        ggplot(tik, aes(x= as.factor(AIResponsible),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AIResponsible)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                            label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing who should be responsible to the AI effects basing on the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[97]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[97]) == input$column)
        ggplot(tik, aes(x= as.factor(AIFuture),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AIFuture)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                  label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing  the future of AI basing on the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[102]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[102]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowRecommend),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(StackOverflowRecommend)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                              label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how the developers are likely to recommend people to use StackOverflow"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[108]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[108]) == input$column)
        ggplot(tik, aes(x= as.factor(StackOverflowJobsRecommend),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(StackOverflowJobsRecommend)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                                       label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing how likely developers are to recommend people tomStackOverflow jobs"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[6]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[6]) == input$column)
        ggplot(tik, aes(x= as.factor(Employment),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(Employment)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the employment type of the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[63]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[63]) == input$column)
        ggplot(tik, aes(x= as.factor(AgreeDisagree1),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AgreeDisagree1)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                              label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5)  + theme_minimal()+ ggtitle(paste("A bar graph showing whether the developers feel kinship to other developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[64]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[64]) == input$column)
        ggplot(tik, aes(x= as.factor(AgreeDisagree2),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(AgreeDisagree2)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                              label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 7) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the developers think they are competing with thier peers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[65]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[65]) == input$column)
        ggplot(tik, aes(x= as.factor(AgreeDisagree3),y = prop.table(..count..) * 100)) + geom_bar(aes(fill= factor(AgreeDisagree3)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                               label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the developers think are not good at programming as thier peers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[122]) == input$column){
        tik <- subset(my_data2(),colnames(my_data2()[122]) == input$column)
        ggplot(tik, aes(x= as.factor(SexualOrientation),y = prop.table(..count..) * 100)) + geom_bar(aes(fill=factor(SexualOrientation)),width = 0.1,position = "dodge") + coord_flip() + theme(aspect.ratio = 1/5) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the sexualorientation of the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[18]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[18]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob1),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob1))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the company the developers are working in is"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[19]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[19]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob2),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob2))) + coord_flip() +  theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the financial status of the company is"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[20]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[20]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob3),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob3))) + coord_flip() +  theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                    label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of imporatnce the department the developers are working with is"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[21]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[21]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob4),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob4))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the language and framework the developers are working with"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[22]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[22]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob5),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob5))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the benefits of the job are to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[23]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[23]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob6),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob6))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the levelof importance the company culture is to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[24]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[24]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob7),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob7))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the opportunity to work at home is to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[25]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[25]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob8),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob8))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the opportunity for proffesional development is to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[26]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[26]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob9),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob9))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                   label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the diversity of the company is to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[27]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[27]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessJob10),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessJob10))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the impact of the service/product is to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[28]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[28]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits1),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits1))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the salary at a new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[29]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[29]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits2),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits2))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the shares at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[30]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[30]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits3),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits3))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the health insurance at the ne job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[31]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[31]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits4),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits4))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the parental leave at the new job to the developers "))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[32]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[32]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits5),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits5))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the fittness benefits at the new job to the developers "))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[33]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[33]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits6),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits6))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the retirement saving at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[34]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[34]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits7),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits7))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importace the company meals at new job are to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[35]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[35]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits8),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits8))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the the level of importance of the office equipment allownce at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[36]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[36]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits9),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits9))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                             label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the child care benefit at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[37]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[37]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits10),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits10))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                               label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance ofthe transportation benefit at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[38]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[38]) == input$column)
        ggplot(tik, aes(x= as.factor(AssessBenefits11),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AssessBenefits11))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                               label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of the conference/education budget at the new job to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[39]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[39]) == input$column)
        ggplot(tik, aes(x= as.factor(JobContactPriorities1),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobContactPriorities1))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of preference by the developers to be contacted on telephone cell"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[40]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[40]) == input$column)
        ggplot(tik, aes(x= as.factor(JobContactPriorities2),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobContactPriorities2))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of preference by the developers to be contacted on the private address email"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[41]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[41]) == input$column)
        ggplot(tik, aes(x= as.factor(JobContactPriorities3),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobContactPriorities3))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of preference by the developers to be contacted on the work address email"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[42]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[42]) == input$column)
        ggplot(tik, aes(x= as.factor(JobContactPriorities4),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobContactPriorities4))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of preference by the developers to be contacted on job site"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[43]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[43]) == input$column)
        ggplot(tik, aes(x= as.factor(JobContactPriorities5),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobContactPriorities5))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                         label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of preference by the developers to be contacted on a social media site"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[44]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[44]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities1),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities1))) + coord_flip()  + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                      label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the details of company the developer is to be working in being included in the job contact message"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[45]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[45]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities2),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities2))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance the department the devloper is to work with being included in the job contact message."))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[46]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[46]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities3),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities3))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of including the specificcs as to why the developer is a good fit for the job in the job contact message"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[47]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[47]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities4),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities4))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of including the technologies the developer is to work with in job contact message"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[48]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[48]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities5),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities5))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of including the company compensation range to the developers in the job contact message"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[49]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[49]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities6),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities6))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importanc eof  including the company hiring process in the job contact message to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[50]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[50]) == input$column)
        ggplot(tik, aes(x= as.factor(JobEmailPriorities7),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(JobEmailPriorities7))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                                     label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of importance of including the details on the company product development process in the job contact message"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[87]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[87]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities1),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities1))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of relevence the online adverts to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[88]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[88]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities2),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities2))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of honesty in the goals of the online adverts to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[89]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[89]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities3),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities3))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level of imporatnce of information in the online adverts to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[90]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[90]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities4),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities4))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing the level to which the online adverts seem to be trustworthy to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[91]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[91]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities5),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities5))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the advert is from the company liked by the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[92]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[92]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities6),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities6))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the on-line adverts offer something of value to the developers"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[93]) == input$column){
        
        tik <- subset(my_data2(),colnames(my_data2()[93]) == input$column)
        ggplot(tik, aes(x= as.factor(AdsPriorities7),y = prop.table(..count..) * 100)) + geom_bar(width = 0.2,aes(fill=factor(AdsPriorities7))) + coord_flip() + theme(aspect.ratio = 1/2) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                                                                                                                                                                                                           label = paste0(round(prop.table(..count..) * 100, digits = 2), '%')), stat = 'count', position = position_dodge(.9),size = 5) + theme_minimal()+ ggtitle(paste("A bar graph showing whether the on-line adverts avoid fluffy or vague language"))+
          xlab("Responses")+ylab("Total")
        
      }
      else if(colnames(my_data2()[54]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        ted1 <- my_data2()[,54]
        
        corp <- Corpus(VectorSource(ted1))
        
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("united", "federation"))
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=100)
        
        df <- (termFrequency/sum(termFrequency))*100
       tx <-  barplot(df, las=2, col=rainbow(20), xlab = "salary type", ylab = "count (%)" ,main = "A bar graph showing the salary types of the developers", width = 0.5, space = 1, legend.text = TRUE)
       text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if( colnames(my_data2()[78]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        ted <- my_data2()[,78]
        corp <- Corpus(VectorSource(ted))
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("gitsubversion","version", "file","files","copying","network","shares","control","gitzip","use","gitcopying","gitteam","gitmercurial","shareszip","team"))
        
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="foundation", replacement="teamfoundationversioncontrol")
        cleanset <- tm_map(cleanset, gsub, pattern="dont", replacement="noversioncontrol")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        
        
        
        
        
        
        
        
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
        
        tx <-barplot(df, las=2, col=rainbow(20), xlab = "version control technique", ylab = "count",main = "A bar chart showing the version control techniques used by the developers", width = 0.5, space = 1,legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
        
      }
      else if(colnames(my_data2()[119]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted4 <- my_data2()[,119]
        corp <- Corpus(VectorSource(ted4))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("ergonomic", "mouse","desk","mat","fatiguerelieving","braces","wristhand","matwristhand","deskwristhand","mousewristhand","matergonomic","deskergonomic","deskfatiguerelieving"))
        
        #replacing words
        
        cleanset <- tm_map(cleanset, gsub, pattern="floor", replacement="floormat")
        
        cleanset <- tm_map(cleanset, gsub, pattern="supports", replacement="wristsupports")
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
        
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "ergonomic devices", ylab = "count (%)", main = "A bar chart showing the Egornomic devices used by the developers", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[52]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        ted5 <- my_data2()[,52]
        corp <- Corpus(VectorSource(ted5))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("british", "sterling","dollars", "rupees","reais","oty","yuan","renminbi","sek","kroner", "krone","rubles","francs"))
        
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="pounds", replacement="BritishPoundsSterling")
        cleanset <- tm_map(cleanset, gsub, pattern="indian", replacement="IndianRupees")
        cleanset <- tm_map(cleanset, gsub, pattern="australian", replacement="AustralianDollars")
        cleanset <- tm_map(cleanset, gsub, pattern="brazilian", replacement="BrazilianReais")
        cleanset <- tm_map(cleanset, gsub, pattern="polish", replacement="PolishOty")
        cleanset <- tm_map(cleanset, gsub, pattern="swedish", replacement="SwedishKroner")
        cleanset <- tm_map(cleanset, gsub, pattern="chinese", replacement="ChineseYuan")
        cleanset <- tm_map(cleanset, gsub, pattern="russian", replacement="RussianRubles")
        cleanset <- tm_map(cleanset, gsub, pattern="canadian", replacement="canadianDollars")
        cleanset <- tm_map(cleanset, gsub, pattern="swiss", replacement="swissFrancs")
        
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=500)
        
        df <- (termFrequency/sum(termFrequency))*100
        tx <-barplot(df, las=2, col=rainbow(20), xlab = "currency", ylab = "count (%)", main = "A bar chart showing the currency symbols of the salary of the developers", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[4]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        ted <- my_data2()[,4]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("united", "federation"))
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
        
        tx <-barplot(df, las=2, col=rainbow(20), xlab = "country", ylab = "count (%)", main = "A bar chart showing the diffrent countries with the developers that participated in the survey", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[73]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        ted <- my_data2()[,73]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("angularnet","core","nodejsreact","corenodejs","angularnodejs","netcorexamarin","corexamarin","djangonodejsreact","angularnodejsreact","corenodejsreact","angulardjandonodejsreact"))
        #replacing words
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
        
       tx <-  barplot(df, las=2, col=rainbow(20), xlab = "framework desired next year", ylab = "count (%)",main = "A bar chart showing the framework the developers prefer to work with nextyear", width = 0.5, space = 1, legend.text = TRUE)
       text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[72]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted6 <- my_data2()[,72]
        corp <- Corpus(VectorSource(ted6))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("angularnodejs","nodejsreact","corenodejs","corenodejsreact","angularreact","angularnet","djangonodejsreact","angulardjangonodejsreact","angularnodejsreact"))
        #replacing words
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
        
       tx<- barplot(df, las=2, col=rainbow(20), xlab = "framework worked with", ylab = "count (%)", main = "A bar chart showing the frameworks the developers are working with",width = 0.5, legend.text = TRUE,space = 1)
        text(tx, 0, round(df, 1), cex=1, pos=3)
        }
      else if(colnames(my_data2()[57]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted <- my_data2()[,57]
        corp <- Corpus(VectorSource(ted))
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("etc", "jiraoffice","confluencegoogle","slacktrello","confluencejiraslack","etcslack","confluencejiraoffice","slackother","irc","etcother","sites","tool","google","jiraslack","proprietary","software","suite","confluencejiraslacktrello","hangoutschat","hangoutschatoffice","facebookgoogle","hangoutschatjiraoffice","facebooktrelloother","overflow","github","etcslackother","etcslacktrello","facebooktrelloother","etcslacktrelloother","microsoft","productivity",""))
        
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="chat", replacement="hipchat")
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="system", replacement="otherchatsystem")
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="wiki", replacement="wikitool")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        
        df <- (termFrequency/sum(termFrequency))*100
        
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "Communication Tools", ylab = "count",main = "A bar graph showing the Communication tools used by the developers", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[82]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        ted <- my_data2()[,82]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("asked","visiting","website","adblocking","causing","display","software","ads","viewing","wanted","content","contentthe","iti","websitethe","view","websitei","contenti","adsthe","interesting","access"))
        #replacing words
        
        cleanset <- tm_map(cleanset, gsub, pattern="specific", replacement="specificadvert")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <-(termFrequency/sum(termFrequency))*100
        
       tx <- barplot(df, las=2, col=rainbow(20), xlab = "adsblocking reasons", ylab = "count (%)",main = "A bar graph showing the reasons why developers use Ad Blocking software", width = 0.5, space = 1, legend.text = TRUE)
       text(tx, 0, round(df, 1), cex=1, pos=3)
       }
      else if(colnames(my_data2()[59]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        ted <- my_data2()[,59]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("participated","formal","framework","language","new","taking","tool","without","contributed","software","source","certification","completed","course","industry","program","inperson","onthejob","online","hackerrank","participated","contributed","programming","taken","training","codechat","coding","received","topcoder","developer","development","mooc","coursecontributed","contributed","mooccompleted","mooctaken","developmentparticipated","mcpdreceived","mooctaught","moocreceived","bootcampcompleted","developmentcontributed","mooccontributed","developmenttaught","bootcampreceived","mooctaken","bootcamptaken","developmentcompleted","bootcamptaken","moocparticipated","codechef","hackathoncontributed","bootcamptaught","topcoderparticipated","topcodercontributed","mcpdparticipated","bootcamptaught","developmentreceived","mcpdtaught","participated"))
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="open", replacement="opensource")
        cleanset <- tm_map(cleanset, gsub, pattern="taught", replacement="selftaught")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        
        df <- (termFrequency/sum(termFrequency))*100
        tx <-barplot(df, las=2, col=rainbow(20), xlab = "EducationTypes", ylab = "count (%)", main = "A bar chart showing the Education Types used by the developers to learn coding",width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[82]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted <- my_data2()[,56]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=500)
        df <- (termFrequency/sum(termFrequency))*100
        tx <-barplot(df, las=2, col=rainbow(20), xlab = "currency symbol", ylab = "count (%)", main = "A bar chart showing the Currency symbols of the salaris of the developers",width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[124]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        ted <- my_data2()[,124]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("african","descent","white","asian","eastern","latinolatina"))
        #replacing words
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        df <- (termFrequency/sum(termFrequency))*100
         
       tx <- barplot(df, las=2, col=rainbow(20), xlab = "race ethinicity", ylab = "count (%)", main = "A bar chart showing the Race Ethinicity of the developers",width = 0.5, space = 1,legend.text = TRUE)
       text(tx, 0, round(df, 1), cex=1, pos=3)
       }
      else if(colnames(my_data2()[77]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted <- my_data2()[,77]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("agilescrum","kanbanscrum","aka","iso","methodologies","standard","agileextreme","xpscrum","xpformal","kanbanpair","agileformal","xppair","agilepair","methodologiespair","xpkanbanscrum","software","programming","agilekanbanscrum","methodologiesscrum","kanbanpair","ieee","methodologieshanbanpair","agilekanbanpair","programmingscrum","methodologieskanbanpair","agilekanban","formal","xpkanbanpair"))
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="open", replacement="opensource")
        cleanset <- tm_map(cleanset, gsub, pattern="taught", replacement="selftaught")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        
        df <- (termFrequency/sum(termFrequency))*100
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "methodologies", ylab = "count (%)", main = "A bar chart showing the software development methodologies used by the developers",width = 0.5, space = 1, legend.text = TRUE )
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      
      else if(colnames(my_data2()[82]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        tts <- read.csv("survey_results_public.csv",header=TRUE,",")
        
        ted <- tts[,82]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("etc", "google","microsoft","suite","github","confluenceoffice","etcslack","etcother","confluencegoogle","facebookgoogle","jiraoffice","slacktrello","etcslacktrello","facebookother","hangoutschatoffice","enterpriseother","trelloother","facebookoffice","slackother","etcslackother","jiraother","hangoutschatslack","confluencejira","etctrello","tool","confluencehipchatjira","hangoutschatjira","confluencejiraslack","confluencejiraoffice","confluenceother","etcslack","enterprisetrelloother","etcslacktrelloother","hangoutschat","confluenceother","etcslackstack","confluencejiraslack","jiraslacktrello","confluenceslack","hangoutschatjiraoffice","jiraslack","causing","display","software","ads","viewing","wanted","content","contentthe","iti","websitethe","specific","view","websitei","contenti","adsthe","interesting","asked","adblocking","access","website"))
        #replacing words
        cleanset <- tm_map(cleanset, gsub, pattern="disable", replacement="askedtodisable")
        cleanset <- tm_map(cleanset, gsub, pattern="visiting", replacement="specificadvert")
        cleanset <- tm_map(cleanset, gsub, pattern="issues", replacement="adblockingissues")
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        
        df <- (termFrequency/sum(termFrequency))*100
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "hackathon reasons", ylab = "count (%)", main = "A bar chart showing the reasons why the developers participated in Hackathon", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[86]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted <- my_data2()[,86]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("advertisement","advertising","clicking","going","online","researched","website","advertisementsaw","advertisementstopped","access","advertisementfree","advertisingfree","adpaid","adstopped","advertisingpaid","without"))
        #replacing words
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=1000)
        
        df <- (termFrequency/sum(termFrequency))*100
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "ads actions", ylab = "count (%)",main = "A bar chart showing the actions developers have taken to stop on_line Ads", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      else if(colnames(my_data2()[60]) == input$column){
        options(header=FALSE, stringsAsFactors = FALSE, FileEncoding="latin1")
        
        
        
        ted <- my_data2()[,60]
        corp <- Corpus(VectorSource(ted))
        
        #..data cleaning
        corp <- tm_map(corp, tolower)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, removeNumbers)
        cleanset <- tm_map(corp, removeWords, stopwords("english"))
        
        cleanset <- tm_map(cleanset, stripWhitespace)
        
        #remove unwanted words from the graph
        cleanset <- tm_map(cleanset, removeWords, c("andor","publisherquestions","apress","channels","book","etc","forums","irc","official","overflow","overflowonline","similar","stack","standards","technologya","technologyquestions","overflowthe","system","computer","engineering","family","friends","network","peers","publishera","science","technologya","veersed","etcthe","technology","chat","company","rooms","set","wikis","etcinternal","developer","listservs","online","software","versed","bookquestions","technologythe"))
        
        
        
        #build term document matrix
        dtm<-TermDocumentMatrix(cleanset, control=list(minWordLength=c(1,Inf)))
        
        #bar plot
        termFrequency<-rowSums(as.matrix(dtm))
        termFrequency<-subset(termFrequency, termFrequency>=5000)
        df <- (termFrequency/sum(termFrequency))*100
        
        tx <- barplot(df, las=2, col=rainbow(20), xlab = "self taught types", ylab = "count (%)", main = "A bar chart showing the resources used by developers to teach themselves coding", width = 0.5, space = 1, legend.text = TRUE)
        text(tx, 0, round(df, 1), cex=1, pos=3)
      }
      
    })
    summa <- reactive({
      
      if(colnames(my_data2()[93])== input$column)
      {print("This bar graph shows the extent to which advertisements avoid fluffy or vague language where 1 shows greatest importance while 7 shows least importance.
             Most of the developers didn't answer this question(N.A), 15.4% of the developers suggested that advertisements slightly avoid fluffy or vague 
             While only 3.64% agreed that advertisements strongly avoid fluffy or vague language.") 
      }
      else if(colnames(my_data2()[92]) == input$column)
      {print("This bar graph shows the extent to which advertisements offer something of value like a free trial, in order of importnce where 1 is most important and 7 least important.
             Most of the developers didn't answer the survey question(N.A), 19.85% said the advertisements slightly offer something of value while 3.15% said the advertisements strongly offer something of value.")}   
      else if (colnames(my_data2()[91]) == input$column)
      {print("This bar graph shows the extent to which advertsiments came from the companies liked by the developers.
             Most developers didnt answer this question. 10.23% 0f those who answered confesed that advertisements rarely come 
             from companies they like while 5.65% of the developers confensed that advertisements come from companies they like.")}
      else if(colnames(my_data2()[90]) == input$column)
      {print("This bar graph shows the extent to which advertisements seem to be trustworthy, in order of importnce where 1 is most important and 7 least important.
             Most of the developers didn't answer the survey question(N.A), 5.12% said the advertisements rarely seem trust worthy while 8.42% agreed that most of the advertisements 
             seem to be trustworthy.")}    
      else if(colnames(my_data2()[89]) == input$column)
      {print("This bar graph shows the extent to which advertisements provide useful information, in order of importnce where 1 is most important and 7 least important.
             Most of the developers didn't answer the survey question(N.A), 2.64% said the advertisements rarely provide useful information while 8.65% said the advertisements commonly provide useful information.")}   
      else if(colnames(my_data2()[88]) == input$column)
      {print("This bar graph shows the extent to which advertisements are honest about their goal, in order of importnce where 1 is most important and 7 least important.
             Most of the developers didn't answer the survey question(N.A), 4.8% said the advertisements are rarely honest about their goals 
             while 7.92% said the advertisements arecommonly honest about their goals.")}   
      else if(colnames(my_data2()[87]) == input$column)
      {print("This bar graph shows the extent to which online advertisements are relevant to the developers, in order of importnce where 1 is most important and 7 least important.
             Most of the developers didn't answer the survey question(N.A), 3.14% said the online advertisements are rarely relevant while 23.74% said the online advertisements are commonly relevant to them")}   
      else if(colnames(my_data2()[80]) == input$column)
      {print("This bar graph shows the percentage of the developers who have any adblocking software installed on any of the 
             computers they use regularly, some developers didn't answer this question. 55.65% said they do not have any adblocking
             software installed on the computers they use regularly while 17.34% agreed that they have adblocking software installed on the computers they use regularly.")}   
      else if(colnames(my_data2()[81]) == input$column)
      {print("This bar graph shows the extent to which the developers who had adblocking software had disabled them in the past month for any reasons or even 
             temporarily for a specific website. all those that didnt have any adblocking software didn't answer this questions.
             39.25% agreed that they had disenabled their adblocking software in the past month and 13.36% hadnt disenabled their
             adblocking software at all in the past month.")}   
      else if(colnames(my_data2()[63]) == input$column)
      {print("This bar graph shows the extent to which the developers agree or disagree with the statement
             'i feel a sense of kinship  or connection with other developers.',  only 24.65% didnt answer this(N.A),
             8.9% strongly disagreed, 11.79% strongly agreed, 10.59% somewhat disagreed,30.22% somewhat agreed 
             and 13.85% neither agreed nor disagreed.")}
      else if(colnames(my_data2()[64]) == input$column)
      {print("This bar graph shows the extent to which the developers agree or disagree with the statement
             'I think of myself as competing with my peers',only 24.73% didnt answer this(N.A),
             7.2% strongly disagreed, 12.22% strongly agreed, 10.08% somewhat disagreed,27.9% somewhat agreed 
             and 17.87% neither agreed nor disagreed.")}
      else if(colnames(my_data2()[65]) == input$column)
      {print("This bar graph shows the extent to which the developers agree or disagree with the statement
             'I am as good at programming as most of my peers',  only 24.72% didnt answer this(N.A),
             6.96% strongly disagreed, 14.22% strongly agreed, 14.81% somewhat disagreed,16.93% somewhat agreed 
             and 22.35% neither agreed nor disagreed.")}
      else if(colnames(my_data2()[125]) == input$column)
      {print("This bar chart shows the ages of the developers at stack overflow. the ranges are;below 18 years old,
             between 18-24 years,between 25-34 years, between 35-44 years,between 45-54 years, between 55-64 years
             and 65 years or older.34.68% prefered not to answer. 1.66% were below 18 years,15.43% were between 18-24 years,
             32.13% were the majority, between 25-34 years, 11.6% were between 35-44 years, 3.35% were between 45-54 years,
             0.97% were between 55-64 years and only 0.18% were 65 or older.")}
      else if(colnames(my_data2()[96]) == input$column)
      {print("This bar chart shows the developers' response on whose responsibility it should be to primarily
             consider the ramifications of the increasingly advanced AI technology.33.6% did not respond,
             31.7% suggested that the developers or the people creating the AI, 1.01% suggested that prominent industry
             leaders, 12% said no body and 18.47% suggested that a governmental or other regulatory body should.")}
      else if(colnames(my_data2()[28]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'salary and or bonuses' was ranked as follows; 46.12% of the developers ranked it number 1,
             7.45% as 2, 3.8% as 3, 2.42% as 4, 1.66% as 5, 1.23% as 6,  0.86% as 7, 0.67% as 8,
             0.53% as 9, 0.5% as 10 and 0.43% as 11 while 34.33% didn't rank this aspect.")}
      
      else if(colnames(my_data2()[37]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'ex company provided transportation, public transit allowance' was ranked as follows; 0.97% of the developers ranked it number 1,
             3.28% as 2, 4.55% as 3, 5.71% as 4, 6.63% as 5, 7.42% as 6,  7.98% as 7, 8.11% as 8,
             8.1% as 9, 6.65% as 10 and 6.26% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[38]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'conference or education budget' was ranked as follows; 2.4% of the developers ranked it number 1,
             5.52% as 2, 7.13% as 3, 8.07% as 4, 8.39% as 5, 7.67% as 6,  7.14% as 7, 6.46% as 8,
             5.56% as 9, 4.08% as 10 and 3.26% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[29]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'stock options and shares' was ranked as follows; 2.1% of the developers ranked it number 1,
             9.15% as 2, 6.37% as 3, 6.63% as 4, 5.67% as 5, 5.81% as 6,  5.88% as 7, 6.14% as 8,
             6.11% as 9, 5.04% as 10 and 6.76% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[30]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Health insurance' was ranked as follows; 5.66% of the developers ranked it number 1,
             15.87% as 2, 10.5% as 3, 7.7% as 4, 5.82% as 5, 4.66% as 6,  3.91% as 7, 3.37% as 8,
             3.21% as 9, 2.36% as 10 and 2.6% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[31]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Parental leave' was ranked as follows; 1.33% of the developers ranked it number 1,
             2.57% as 2, 3.64% as 3, 4.39% as 4, 4.99% as 5, 5.39% as 6,  5.86% as 7, 6.54% as 8,
             7.72% as 9, 13.95% as 10 and 9.28% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[32]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Fitness or wellness benefit' was ranked as follows; 0.97% of the developers ranked it number 1,
             2.38% as 2, 3.9% as 3, 5.28% as 4, 6.6% as 5, 7.7% as 6,  8.03% as 7, 8.35% as 8,
             8.51% as 9, 6.63% as 10 and 7.31% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[33]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Retirement or pension savings match' was ranked as follows; 1.4% of the developers ranked it number 1,
             7.1% as 2, 10.35% as 3, 7.9% as 4, 6.74% as 5, 6.1% as 6,  5.87% as 7, 5.85% as 8,
             5.72% as 9, 4.38% as 10 and 4.27% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[34]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Company provided meals or snacks' was ranked as follows; 0.95% of the developers ranked it nummber 1,
             3.03% as 2, 4.68% as 3, 5.77% as 4, 6.74% as 5, 7.17% as 6,  7.66% as 7, 7.74% as 8,
             7.44% as 9, 6.4% as 10 and 8.09% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[35]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'computer or office equipment allowance' was ranked as follows; 3.07% of the developers ranked it number 1,
             7.32% as 2, 7.58% as 3, 7.69% as 4, 7.81% as 5, 7.37% as 6,  6.78% as 7, 5.97% as 8,
             4.95% as 9, 3.95% as 10 and 3.17% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[36]) == input$column)
      {print("There are 11 aspects of the job benefits package which the developers were asked to rank
             according to the most important ranked 1 and the least important ranked as 11. The aspect of 
             'Child care benefit' was ranked as follows; 0.71% of the developers ranked it number 1,
             2% as 2, 3.18% as 3, 4.1% as 4, 4.62% as 5, 5.15% as 6,  5.68% as 7, 6.45% as 8,
             7.82% as 9, 11.72% as 10 and 14.23% as 11 while 34.33% didn't rank this aspect.")}
      else if(colnames(my_data2()[18]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The industry a developer would be working in' 
             was ranked as follows; 5.04% ranked it as very important(1), 3.92% ranked it as 2, 4.12% ranked
             it as 3, 4.69% ranked it as 4, 5.74% ranked it as 5, 6.92% as 6, 8.28% as 7, 9.47% as 8, 
             10.32% as 9 and 9.27% as 10. Generally the percentage increased with decreasing importance. ")}
      else if(colnames(my_data2()[27]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'How widely used or impactful the product or service
             the developer would be working on is'  was ranked as follows;
             4.43% ranked it as very important(1), 4.48% ranked it as 2, 5.03% ranked
             it as 3, 5.93% ranked it as 4, 7.01% ranked it as 5, 8.05% as 6, 8.83% as 7, 9.22% as 8, 
             8.67% as 9 and 6.2% as 10. Generally the percentage increased with decreasing importance. ")}
      else if(colnames(my_data2()[19]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'Financial performance or funding of the company or organisation'
             was ranked as follows; 2.33% ranked it as very important(1), 3.16% ranked it as 2, 3.99% ranked
             it as 3, 4.93% ranked it as 4, 6.4% ranked it as 5, 7.94% as 6, 9.21% as 7, 10% as 8, 
             10.28% as 9 and 9.52% as 10. Generally the percentage increased with decreasing importance. ")}
      else if(colnames(my_data2()[20]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The specific department or team the developer would be working on'
             was ranked as follows; 3.74% ranked it as very important(1), 5.26% ranked it as 2, 6.07% ranked
             it as 3, 6.72% ranked it as 4, 7.48% ranked it as 5, 7.99% as 6, 8.33% as 7, 8.42% as 8, 
             7.95% as 9 and 5.83% as 10. Generally the percentage increased with decreasing importance. ")}
      else if(colnames(my_data2()[21]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The languages, frameworks and other technology the
             developer would love to work with'
             was ranked as follows; 11.72% ranked it as very important(1), 11.49% ranked it as 2, 10.38% ranked
             it as 3, 8.62% ranked it as 4, 6.97% ranked it as 5, 5.54% as 6, 4.43% as 7, 3.54%as 8, 
             2.89% as 9 and 2.17% as 10. Generally the percentage increased with increasing importance.
             This aspect was very important to the developers.")}
      else if(colnames(my_data2()[22]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The compensation and benefits offered'
             was ranked as follows; 12.41% ranked it as very important(1), 12.21% ranked it as 2, 10.45% ranked
             it as 3, 8.48% ranked it as 4, 6.72% ranked it as 5, 5.17% as 6, 4.18% as 7, 3.37% as 8, 
             2.86% as 9 and 1.93% as 10. Generally the percentage increased with increasing importance.
             This aspect was very important to the developers.")}
      else if(colnames(my_data2()[23]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The office or company culture'
             was ranked as follows; 9.22% ranked it as very important(1), 9.36% ranked it as 2, 9.53% ranked
             it as 3, 9.46% ranked it as 4, 8.42% ranked it as 5, 7.01% as 6, 5.3% as 7, 4.12% as 8, 
             3.33% as 9 and 2% as 10. Generally the percentage increased with increasing importance.
             This aspect was important to the developers.")}
      else if(colnames(my_data2()[24]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'The opportunity to work home or remotely'
             was ranked as follows; 6.95% ranked it as very important(1), 5.91% ranked it as 2, 6.06% ranked
             it as 3, 6.59% ranked it as 4, 6.73% ranked it as 5, 6.69% as 6, 6.68% as 7, 6.43% as 8, 
             7.01% as 9 and 8.44% as 10. Generally the percentage wasrelatively equal all over. 
             The aspect was not so important to the developers. ")}
      else if(colnames(my_data2()[25]) == input$column)
      {print("The developers were asked to asses a job opportunity by ranking 10 aspects inorder of their
             importance, 1 being the most important and 10 being the least important. 32.24% of the developers 
             didn't rank all these aspects. The aspect of 'Opportunities for professional development'
             was ranked as follows; 10.82% ranked it as very important(1), 10.05% ranked it as 2, 9.73% ranked
             it as 3, 9.06% ranked it as 4, 7.9% ranked it as 5, 6.31% as 6, 5.08% as 7, 4.08% as 8, 
             2.96% as 9 and 1.78% as 10. Generally the percentage increased with increasing importance.
             This aspect was important to the developers.")}
      else if(colnames(my_data2()[83]) == input$column)
      {print("The developers were asked to choose the extent to which they agree or disagree to the particular
             statements, the statement of 'Online advertising can be valuable when it is relevant to me', 
             was replied to as follows; 8.9% strongly disagreed, 11.79% strongly agreed, 10.59% somewhat
             disagreed, 30.22% somewhat agreed while 13.85% neither agreed or disagreed. 24.65% didn't
             reply this question. The developers mostly agreed to this statement")} 
      else if(colnames(my_data2()[84]) == input$column)
      {print("The developers were asked to choose the extent to which they agree or disagree to the particular
             statements, the statement of 'The developer enjoys seeing online advertisements from the company they like', 
             was replied to as follows; 7.2% strongly disagreed, 12.22% strongly agreed, 10.08% somewhat
             disagreed, 27.9% somewhat agreed while 17.87% neither agreed or disagreed. 24.73% didn't
             reply this question. The developers mostly agreed to this statement.")}
      else if(colnames(my_data2()[85]) == input$column)
      {print("The developers were asked to hoose the extent to which they agree or disagree to the particular
             statements, the statement of 'The developers fundamentally dislike the concept of advertising', 
             was replied to as follows; 6.96% strongly disagreed, 14.22% strongly agreed, 14.81% somewhat
             disagreed, 16.93% somewhat agreed while 22.35% neither agreed or disagreed. 24.72% didn't
             reply this question")}
      else if(colnames(my_data2()[94]) == input$column)
      {print("The developers were asked to give their views on the different questions about artificial
             intelligence.the developers gave their views on the most dangerous aspect of the increasingly
             advanced AI technology as follows; 18.23% said algorithms making important decisions was the 
             most dangerous, 17.85% said artificial intelligence surpassing human intelligence wasthe most
             dangerous, 15.13% said evolving definitions of 'faireness' in algorithms versus human was the
             most dangerous, 12.64% said the increasing automation of jobs was the most dangerous, while 
             36.15% did not reply.")}
      else if(colnames(my_data2()[97]) == input$column)
      {print("The developers were asked to give their views on the different questions about artificial
             intelligence.the developers gave their views on the overall takeon the future of AI as follows;
             the majority of the developers(51.36%) were excited about the responsibilities more than 
             worried about the dangers, 13.42% were worried about the dangers more than excited about 
             the possibilities, 5.75% said they didnt care about it or haven't thought about it. 
             29.46% did not reply. ")}
      else if(colnames(my_data2()[95]) == input$column)
      {print("The developers were asked to give their views on the different questions about artificial
             intelligence.the developers gave their views on the most exciting aspect of the increasingily
             advanced AI technology as follows; 15.56% said algorithms making important decisions was the 
             most exciting, 15.41% said artificial intelligence surpassing human intelligence wasthe most
             exciting, 8.2% said evolving definitions of 'faireness' in algorithms versus human was the
             most exciting, 26.95% said the increasing automation of jobs was the most exciting, while 
             33.88% did not reply.")}
      
      })
    output$tek <- renderText({
      summa()
    })
    
  }
)