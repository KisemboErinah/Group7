library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
shinyUI(
  dashboardPage(skin = ("red"),
    dashboardHeader(title = h3(("STACK OVERFLOW"),style="background-color:#2E8B57;height:60%")),
    dashboardSidebar(sidebarMenu(
      sidebarSearchForm(textId = "sea", buttonId = "cotton",
                        label = "Search..."),
      
      menuItem("Analysis Dashboard",  icon = icon("dashboard")),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Columns", tabName = "plots", icon = icon("line-chart")),
      menuSubItem("Charts", tabName = "widgets"),
      
      menuSubItem("Graphs", tabName = "widgets"),
      
      menuItem("General Insights", tabName = "maps", icon = icon("list")),
        
      menuItem("Help", tabName = "help", icon = icon("question")),
      fileInput("file","Import survey data file"),
      fileInput("file2","Import suvey questions")
      
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
                  box(width=100,title="The Stack Overflow Developers' Survey Analysis System.",background="red",solidHeader=F,status="success",uiOutput("picto")),
                     
                  
                  box(width=100,solidHeader = T,status = "success",
                      title = "The Stack Overflow Developers' Survey Analysis System.",
                      helpText("STACK OVERFLOW DATA ANALYSIS TOOLS DESCRIPTION"),
                      br(),
                      helpText("Data analysis involves extracting meaning and insights from raw data. It involves methods and algorithms that examine, clean, transform and model the data to obtain conclusions.
Users can investigate the stack overflow survey results and learn some interesting things about programmers.
                               "),
                      br(),
                      helpText("Stack overflow dataset analysis
The developer's survey results are organized into two tables:
.	The survey_results_schema.csv contains the main survey results, one respondent per row and one column per question.
.	The survey_results_schema.csv contains each column name from the main results along with the question text corresponding to that column.
"),
                      br(),
                      helpText("The ggplot2 visualization charts are used to ensure that they: "),
                      br(),
                      helpText(".	Convey the right information without distorting facts"),
                      br(),
                      helpText(".	Are simple and elegant.Users should not think much in order to get the meaning of the plots."),
                      br(),
                      helpText("The following are some of the objectives of constructing plots and the various kinds of charts associated with these objectives:"),
                      br(),
                      helpText("1.	Correlation "),
                      br(),
                      helpText("Used to establish a relationship or connection between the variables being analyzed."),
                      br(),
                      helpText(".	Scatter plot"),
                      br(),
                      helpText("Has a horizontal axis and a vertical axis. It contains a line of best fit which is a straight line drawn through the most frequently used plot for data analysis. Used when you want to understand the relationship between two variables. "),
                      br(),
                      helpText("2.	Distribution "),
                      br(),
                      helpText("When you have lots of data points and you want to study where and how the data points are distributed, the following plots are used;"),
                      br(),
                      helpText(".	Histogram"),
                      
                      br(),
                      helpText("A histogram is a display of statistical information that uses rectangles to show the frequency of data items in successive numeric intervals of equal size. The independent variable is plotted along the horizontal axis while the dependent variable is plotted along the vertical axis."),
                      
                      br(),
                      helpText("3.	Composition "),
                      
                      br(),
                      helpText("Used to show quantitative descriptions of the parts of a given variable conveying relative information."),
                      
                      br(),
                      helpText(".	Pie chart"),
                      
                      br(),
                      helpText("This is a circular statistical graphic divide into slices to illustrate numeric proportion. The arc length of each slice is proportional to the quantity it represents."),
                      
                      br(),
                      helpText(".	Bar chart"),
                      
                      br(),
                      helpText("Uses bars to show comparisons between categories of data. It has two axes; one axis will describe the types of categories being compared and the other will have numerical values that represent the values of data.")
                      
                      
                      
                      
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
                )
              
        )
    )
  )
  
)