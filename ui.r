library(shiny)
library(shinydashboard)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
#library(plotly)
library(leaflet)
library(htmltools)
library(plotrix)
library(wordcloud)
library(grDevices)
library(splitstackshape)
shinyUI(
  dashboardPage(skin = ("red"),
                dashboardHeader(title = ("STACK OVERFLOW")),
                dashboardSidebar(sidebarMenu(
                  sidebarSearchForm(textId = "sea", buttonId = "cotton",
                                    label = "Search..."),
                  
                  menuItem("Analysis Dashboard",  icon = icon("dashboard")),
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Data Columns", tabName = "plots", icon = icon("line-chart")),
                  menuSubItem("View survey questions", tabName = "wid"),
                  
                  menuSubItem("Relations", tabName = "gets"),
                  
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
                                  helpText
                                  ("The ggplot2 visualization charts are used to ensure that they: "),
                                  br(),
                                  helpText(".	Convey the right information without distorting facts"),
                                  br(),
                                  helpText(".	Are simple and elegant.Users should not think much in order to get the meaning of the plots."),
                                  br(),
                                  helpText("The following are some of the objectives of constructing plots and the various kinds of charts associated with these objectives:"),
                                  br(),
                                  
                                  
                                  
                                  
                                  helpText("3.	word cloud"),
                                  
                                  br(),
                                  helpText("Word cloud explanation:
                                           Used when the raw data is text. 
                                           Used when you want to show a visualisation format to highlight important textual data (database, dataset) the bigger and bolder it appears in the word cloud.
                                           Advantages of word cloud.
                                           It reveals the essential. Brand names pop, keywords float to the surface.
                                           They delight and provide emotional connection. Both the creation and observation of a word cloud help to provide an overall sense of the text.
                                           They are fast. Poring over text to develop themes from research takes time.
                                           They are engaging. They generate interest among the audience.
                                           They are simple to understand, easy to create, casual and visually appealing.
                                           "),
                                  
                                  br(),
                                  helpText(".	Pie chart"),
                                  
                                  br(),
                                  helpText("This is a circular statistical graphic divide into slices to illustrate numeric proportion. The arc length of each slice is proportional to the quantity it represents. Why we used pie charts;
                                           .	A pie chart presents data in a simple and easy to understand picture.
                                           .	It is an effective communication tool for even uninformed audience because they represent data visually as a fraction part of a whole.
                                           .	Readers/ audience see the data comparisons at a glance enabling them to understand information quickly. This removes the need for users to examine and measure underlying numbers themselves.
                                           .	You can emphasize pieces of data in the pie circle to emphasize points you want to make.
                                           .	Display relative portions of multiple classes of data.
                                           .	Size of the circle can be made proportional to the total quality it represents.
                                           .	Summarise a large data set in visual form.
                                           .	A pie chart is visually simpler than other types of graphs.
                                           .	Permit a visual check of the reasonableness or accuracy of calculations.
                                           .	Require minimal additional explanation.
                                           .	Are easily understood due to wide spread use in business and media.
                                           "),
                                  
                                  br(),
                                  helpText(".	Bar chart"),
                                  
                                  br(),
                                  helpText("Uses bars to show comparisons between categories of data. It has two axes; one axis will describe the types of categories being compared and the other will have numerical values that represent the values of data.
                                           Why we used bar charts;
                                           .	Show each data category in a frequency distribution.
                                           .	Display relative numbers or proportions of multiple categories.
                                           .	Summarise a large data set in visual form, easily interpretable form.
                                           .	Clarify trends better than tables.
                                           .	Estimate key values at a glance and accurately.
                                           .	Permit a visual check of the accuracy and reasonableness of calculations.
                                           .	To be easily understood due to wide spread use in business and media.
                                           .	Accessible to a wide audience.
                                           "),
                                  br(),
                                  helpText("Other visualisation tools and why we did not use them."),
                                  
                                  br(),
                                  helpText("Box Plot"),
                                  
                                  br(),
                                  helpText(".	Doesn't retain exact values and details of the distribution results 
                                           .	Only shows a simple summary of the distribution of results so that in case detailed analysis of data is required, other visualisation tools are required.
                                           .	Original data is not clearly shown.
                                           .	Mean and mode can't be defined.
                                           .	Used for only numeric data. 
                                           "),
                                  br(),
                                  helpText("sentimental analysis"),
                                  br(),
                                  helpText(" It is a measurement of positive and negative language.
                                           It is a way to evaluate written or spoken language to determine if the expression is favourable, unfavourable or neutral.
                                           Why we didn't use sentimental analysis;
                                           It is complex. Some statements can't easily be categorised as positive, negative or neutral.
                                           It is ambiguous.
                                           It is complicated.
                                           ")
                                  
                                  
                                  )
                                  )
                            
                              ),
                    
                    
                    tabItem(tabName = "help",
                            fluidRow(
                              box(width=100,title="THE STACK OVERFLOW DEVELOPERS' SURVEY RESULTS ANALYSIS SYSTEM(SODSAS)", background="red", solidHeader=T), uiOutput("pics"),
                              
                              box(width=120, solidHeader = T,length=50,
                                  title = "User Manual",
                                  helpText("The SODSAS displays the details of the analysis of the stack overflow developers'survey results.
                                           "),
                                  br(),
                                  helpText("The system uses the home, help,general insights,charts and graph buttons to display the analysis details to the user."),
                                  
                                  br(),
                                  helpText("HOME button enables the user to see an overview about data analysis in R."),
                                  
                                           br(),
                                  helpText("CHARTS button enables the user to view the charts generated from the selected data columns."),
                                  
                                           br(),
                                  helpText("GRAPHS button enables the user to view the graphs gerated from the selected data columns."),
                                  
                                  br(),
                                  helpText(" GENERAL INSIGHTS button enables the user to see the overall conclusions generated by the development team on the datasets.
                                          "),
                                  br(),
                                  helpText("  The systems uses the datacolumns drop down to enable the user to select the data column/s whose analysis he would like to see."),
                                  br(),
                                  helpText(" HELP button gives a user a help manual to maximumly utilise the system.."),
                                  
                                  br(),
                                  helpText("The user can also browse into the survey results and survey results schema tables to view the original data.")
                                  
                                  
                                           
                                          
                                           
                                          
                                          
                                           
                                  
                                  
                                  )
                              )
                            
                            ),
                    
                    tabItem(tabName = "wid",
                            
                            uiOutput("input_file") ),
                    
                    
                    
                    tabItem(tabName = "plots",
                            fluidRow(
                              box(uiOutput("tinx")),
                              box(width=150, title ="GRAPH DESCRIPTION",
                            textOutput("tek")),
                             box(width=200,title = "Visualizations",
                              plotOutput("first"),plotOutput("second") )
                            
                             
                    ) 
                            
                    ),
                    tabItem(tabName = "gets",
                            fluidRow(
                              box(uiOutput("colmn")),
                              box(uiOutput("tin")),
                              box(width=150,title ="GRAPH DESCRIPTION",
                                  textOutput("tek2")),
                              
                              box(width=150,title = "Visualizations",
                              plotOutput("this")  
                            ))
                            
                    ),
                    tabItem(tabName = "maps",
                            fluidRow(
                              box(width=100,title="THE STACK OVERFLOW DEVELOPERS' SURVEY RESULTS ANALYSIS SYSTEM(SODSAS)", background="red", solidHeader=T)),
                              
                              box(width=120, solidHeader = T,length=50,
                                  title = "GENERAL INSIGHTS FROM THE DEVELOPERS",
                                  helpText("Adblocker; Most of the developers (55.56%) have adblocking software installed on their computers.
                                           "),
                                  br(),
                                  helpText("Adblocker disabled; most of the developers (39.25%) that had adblocking software had disabled it at least once in the past month."),
                                  
                                  br(),
                                  helpText("AdsAgreeDisagree1; A good number of the developers (30.22%+11.79%) agreed that online advertisements can be valuable when relevant to them."),
                                  
                                  br(),
                                  helpText("AdsAgreeDisagree2; Most developers (27.9%+12.22%) agreed that they like advertisements from the companies they like."),
                                  
                                  br(),
                                  helpText("AdsAgreeDisagree3; The developers that agreed that they fundamentally dislike the whole concept of advertising were almost equal to those who disagreed."),
                                  
                                  br(),
                                  helpText(" AdsPriorities1; Advertisements are so relevant to developers
                                           "),
                                  br(),
                                  helpText("AdsPriorities2; Most of the online advertisements are not honest about their goals."),
                                  br(),
                                  helpText(" AdsPriorities3; Advertisements truly provided useful information to the developers."),
                                  
                                  br(),
                                  helpText("AdsPriorities4; the online advertisements do not seem to be trustworthy."),
                                  
                                  br(),
                                  helpText("AdsPriorities5; Most advertisements do not come from companies liked by the developers."),
                                  
                                  br(),
                                  helpText("AdsPriorities6; Advertisements do not offer something of value to the developers."),
                                  br(),
                                  helpText("AdsPriorities7 Advertisements do not avoid fluffy or vague language at all."),
                                  
                                  br(),
                                  helpText("Age; The dominant age of the developers is between 25 to 34 years old."),
                                  
                                  br(),
                                  helpText("AgreeDisagree1; Most developers (14.85%) neither agreed nor disagreed with the statement that 'they feel kinship or connection to other developers'."),
                                  br(),
                                  helpText("AgreeDisagree2; Most developers think of themselves as competing with their peers."),
                                  
                                  br(),
                                  helpText("AgreeDisagree3; The developers are not as good at programming as their peers."),
                                  
                                  br(),
                                  helpText("AIDangerous; The most dangerous aspect about the increasingly advanced artificial intelligence technology is 'Algorithms making important decisions.'"),
                                  
                                  br(),
                                  helpText("AIInteresting; The most interesting aspect about the increasingly advanced artificial intelligence technology is 'Increasing automation of jobs."),
                                  
                                  br(),
                                  helpText("AIFuture; Most developers were excited more about the possibilities than worried about the dangers of Artificial Intelligence."),
                                  
                                  br(),
                                  helpText("AIResponsible; The developers (31.7%) suggested that the developers or people creating the AI should be responsible to consider the ramifications on the increasingly advanced AI technology."),
                                  
                                  br(),
                                  helpText("AssesBenefits1; The developers ranked the aspect of salary and bonuses as so important."),
                                  
                                  br(),
                                  helpText("AssesBenefits2; The aspect of stock options and shares was also important to the developers."),
                                  
                                  br(),
                                  helpText("AssesBenefits3; The developers were interested in Health insurance."),
                                  
                                  br(),
                                  helpText("AssesBenefits4; Parental leave wasn't important to the developers."),
                                  
                                  br(),
                                  helpText("AssesBenefits5; The developers were averagely interested in the fitness and wellness benefit."),
                                  
                                  br(),
                                  helpText("AssesBenefits6; The developers were slightly interested in retirement or pension savings."),
                                  
                                  br(),
                                  helpText("AssesBenefits7; The developers were not so interested in the company giving meals or snacks."),
                                  
                                  br(),
                                  helpText("AssesBenefits8; the aspect of computer/office equipment was less considered by the developers."),
                                  
                                  br(),
                                  helpText("AssesBenefits9; The developers were so interested in the childcare benefit when getting a new job."),
                                  
                                  br(),
                                  helpText("AssesBenefits10; the aspect of transportation was slightly important to the developers."), 
                                  
                                  br(),
                                  helpText("AssesBenefits11; The conference or education budget was averagely important to the developers."),
                                  
                                  br(),
                                  helpText("AssesJob1; Most of the developers do not mind the company or industry they work for."),
                                  
                                  br(),
                                  helpText("AssesJob2; The developers did not consider the aspect of the financial performance or funding of the company as so important."),
                                  
                                  br(),
                                  helpText("AssesJob3; The department in which the developers would work wasn't so important while assessing a job opportunity."),
                                  
                                  br(),
                                  helpText("AssesJob4; The languages, frameworks and other technology the developers would work with was the most important aspect while assessing a job opportunity."),
                                  
                                  br(),
                                  helpText("AssesJob5; The compensation and benefits offered by the job was so important to the developers."),
                                  
                                  br(),
                                  helpText("AssesJob6; The office environment or company culture wasn't that important to the developers."),
                                  
                                  br(),
                                  helpText("AssesJob7; Developers were less concerned about the opportunity to work home or remotely."),
                                  
                                  br(),
                                  helpText("AssesJob8; The developers were so interested in opportunities for professional development when assessing a new job opportunity."),
                                  
                                  br(),
                                  helpText("AssesJob9; The diversity of the company or organization wasn't important to the developers."),
                                  
                                  br(),
                                  helpText("AssesJob10; How widely used or impactful the product of the developer would be was just averagely considered by the developers."),
                                  
                                  br(),
                                  helpText("Career satisfaction: most developers are moderately satisfied within their careers. But overall, the majority is satisfied with their careers."),
                                  
                                  br(),
                                  helpText("Check In code; Most developers prefer to check in multiple times a day."),
                                  
                                  br(),
                                  helpText("Country: The United States contributes the highest number of developers followed by India"),
                                  
                                  br(),
                                  helpText("Currency: Most developers make use of the US dollar as their currency, probably because most of them are US citizens as seen earlier"),
                                  
                                  br(),
                                  helpText("Dependents; Nearly half of the respondents don't have children they care for."),
                                  
                                  br(),
                                  helpText("Employment; A vast majority of the respondents are full time employees"),
                                  br(),
                                  helpText("Ergonomic devices: The keyboard is the most commonly used device by developers to stay comfortable while working, while the floormat is the least commonly used."),
                                  
                                  br(),
                                  helpText("Ethical Implications; Over half of the developers think unethical code has implications"), 
                                  
                                  br(),
                                  helpText("Ethics choice: Most of the developers cannot write code that they consider unethical. Ethical situations can be complicated, and about a third say that it would depend on the situation. "),
                                  
                                  br(),
                                  helpText("Ethics report; About a third of the developers can only report unethical code depending on a particular situation."),
                                  
                                  br(),
                                  helpText("Ethics responsible; the survey reveals that the upper management is the one most responsible for the unethical code."),
                                  
                                  br(),
                                  helpText("Formal education; about three-quarters of developers have the equivalent of a bachelor's degree or higher. It is not that rare to find accomplished professional developers who have not completed a degree."),
                                  
                                  br(),
                                  helpText("Exercise; Nearly a third of developers don't regularly exercise, with those who exercise almost every day being the least in number. "),
                                  br(),
                                  helpText("Hobby; Over 80 percent of the developers enjoy coding and do it as a hobby. Other interests or responsibilities outside of software don't seem to reduce developers' interest in coding as a hobby."),
                                  
                                  br(),
                                  helpText("Hope five years; They're an ambitious lot. Most developers hope to be working in a more specialized role than their current roles in the next five years, with the least number expecting to retire."),
                                  br(),
                                  helpText("Hours computer; over a third of the developers spend 9-12 hours on their desktops and laptops.. They are followed by those who spend 5-8 hours"),
                                  
                                  br(),
                                  helpText("Hours outside; Developers get outside for recreation, commuting, or other reasons. Most developers spend 1-2 hours outside, followed by those who spend 35-59 minutes. "),
                                  
                                  br(),
                                  helpText("Hypothetical tools 1; Very few developers are interested in the peer mentoring system"),
                                  
                                  br(),
                                  helpText("Hypothetical tools 2; Most developers are not interested in creating a new platform for people new to coding.")
                                  
                                  
                                  )
                              )
                            
                    )
                    
                  )
                  )
                  )
  
