library(shiny)
library(shinydashboard)
library(tm)

shinyUI(
  dashboardPage(skin = ("red"),
    dashboardHeader(title = ("STACK OVERFLOW")),
    dashboardSidebar(sidebarMenu(
      sidebarSearchForm(textId = "sea", buttonId = "cotton",
                        label = "Search..."),
      
      menuItem("Analysis Dashboard",  icon = icon("dashboard")),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Columns", tabName = "plots", icon = icon("line-chart")),
      menuSubItem("View", tabName = "wid"),
      
      menuSubItem("Graphs", tabName = "gets"),
      
      menuItem("General Insights", tabName = "maps", icon = icon("list")),
        
      menuItem("Help", tabName = "help", icon = icon("question")),
      fileInput("file2","upload survey data here"),
      fileInput("file","Import survey data file"),
      radioButtons("sep","seperator",choices = c(comma = ',')),
      checkboxInput("header","Header?")
      )),
    dashboardBody(
      tags$head(tags$style(HTML('
      box {
                                width:100px;
                                border:1px solid red;
                                }
                                '))),
      
      tabItems(
        # First tab content................................dashboard items begins from here....................................................
        tabItem(tabName = "home",
                fluidRow(
                  box(width=100,title="The Stack Overflow Developers' Survey Analysis System.",background="red",solidHeader=F,status="success",uiOutput("picto"),tableOutput("out2")),
                     
                  
                  box(width=100,solidHeader = T,status = "success",
                      title = "The Stack Overflow Developers' Survey Analysis System.",
                      helpText("SYSTEM DESCRIPTION.")
                      
                      
                      )
                  )
                
                ),
       
         tabItem(tabName = "help",
                fluidRow(
                  box(width=100,title="The Stack Overflow Developers' Survey Analysis System ", background="red", solidHeader=T), uiOutput("pics")),
                
                box(width=120, solidHeader = T,
                    title = "User Manual",background="red",
                    helpText("GUIDELINES")
                    
                    
                )
                ),
        
        tabItem(tabName = "wid",
                
                  uiOutput("input_file") ),
        
        tabItem(tabName = "gets",
                fluidRow(
                box(width=100,title="A bar graph showing the formal education levels of the survey participants", plotOutput("first") ))
                
        )
        )
    )
  )
  
)