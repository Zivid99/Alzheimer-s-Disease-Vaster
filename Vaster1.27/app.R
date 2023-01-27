library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(rlang)
library(rsconnect)
library(RCurl)
library(ggplot2)
library(markdown)
library(shinythemes)
library(data.table)

library(randomForest)
library(flexdashboard)


#linki <- getURL("https://raw.githubusercontent.com/Zivid99/Alzheimer-s-Disease-Vaster/main/oasis0.csv")
#data = read.csv(text = linki)
#setwd('E:/deskop/Vaster-AD/Vaster-AD/')
data = read.csv('oasis0.csv')

# clean CDR
FinalData= data['CDR'>1, c('gender','Age','Educ','SES','MMSE','eTIV','nWBV','ASF','DementedStatus')]
# choose DementedStatus == 1
dement = subset(FinalData,DementedStatus>0)

# modeling
FinalData$DementedStatus <- as.character(FinalData$DementedStatus)
FinalData$DementedStatus <- as.factor(FinalData$DementedStatus)
model = randomForest(DementedStatus ~ gender + Age + Educ + SES + MMSE + eTIV + nWBV + ASF,
                     data = FinalData,
                     ntree = 500,
                     mtry = 3,
                     importance = TRUE)

# analysis
vis_cols = c('No','ID','Gender','Age','Educ','SES','MMSE','CDR','eTIV','nWBV','ASF','DementedStatus')
data
length(vis_cols)
FinalData = filter(data,'CDR' > 0) 
dataID = data.frame(data$ID)
Count_Gender = FinalData %>% group_by(gender)%>%summarise(count = n_distinct(No))
Count_Age = FinalData %>% group_by(Age)%>%summarise(count = n_distinct(No))
Count_Educ = FinalData %>% group_by(Educ)%>%summarise(count = n_distinct(No))
Count_SES = FinalData %>% group_by(SES)%>%summarise(count = n_distinct(No))
Count_MMSE = FinalData %>% group_by(MMSE)%>%summarise(count = n_distinct(No))
Count_CDR = FinalData %>% group_by(CDR)%>%summarise(count = n_distinct(No))
Count_eTIV = FinalData %>% group_by(eTIV)%>%summarise(count = n_distinct(No))
Count_nWBV = FinalData %>% group_by(nWBV)%>%summarise(count = n_distinct(No))
Count_ASF = FinalData %>% group_by(ASF)%>%summarise(count = n_distinct(No))
Count_DementedStatus = FinalData %>% group_by(DementedStatus)%>%summarise(count = n_distinct(No))


# ui
ui <- dashboardPage(
  title = "Alzheimer's Disease Detector",
  dashboardHeader(title = span("Alzheimer's Disease Detector", style = "font-family: Tahoma, Helvetica, sans-serif; font-weight: bold; font-size: 100%;")),
  
  dashboardSidebar(sidebarMenu(
    HTML(paste0(
      "<br>",
      "<a href='https://github.com/Zivid99' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='images.png' width = '186'></a>",
      "<br>",
      "<p style = 'text-align: center;'><small>YOUR LOYALTY DOCTOR</small></p>",
      "<br>"
    )),
    menuItem("About", tabName = "Introduction", icon = icon("home")),
    menuItem("AD Tables", tabName = "table", icon = icon("table")),    
    menuItem("Descriptive Analysis", tabName = "dashboard", icon = icon("chart-line")),
    menuItem("EDA", tabName = "Match", icon = icon("map")),
    menuItem("Analysis of model", tabName = "Modeling", icon = icon("user")),
    menuItem("Detection", tabName = "Variable", icon = icon("calendar")),
    menuItem("Source Code", tabName = "releases",icon = icon("tasks"), badgeColor = "red"),
    
    HTML(paste0(
      "<br><br><br><br><br><br><br><br><br>",
      "<table style='margin-left:auto; margin-right:auto;'>",
      "<tr>",
      "<td style='padding: 5px;'><a href='https://www.facebook.com' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.youtube.com' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.twitter.com' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.instagram.com' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://www.flickr.com' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
      "</tr>",
      "</table>",
      "<br>"),
      
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://github.com/Zivid99' target='_blank'>Vaster.com</a> - <script>document.write(yyyy);</script></small></p>")
      ))
    
  )), 
  
  
  dashboardBody(
    tags$head(tags$style(HTML(".main-sidebar { font-size: 16px; }"))),
    tabItems(
      tabItem(tabName = "Introduction",
              fluidRow(
                box(title = tags$b("What is AD?"), 
                    status = "primary",
                    width = 6,
                    height = 480,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "p1.jpg", height = 380, width = 550),
                    align = "center"),
                
                column(width =6,
                       box(title = tags$p("Alzheimer's Disease Centre", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                           width = 12,height = 480,
                           background = "light-blue",
                           br(),
                           img(src = "p2.jpg", height = 200, width = 550),
                           br(),
                           br(),
                           tags$p("Alzheimer's disease is the most common form of dementia and manifests itself as a decline in the ability to remember, think, act and perform daily activities. The application of machine learning for the detection of AD can detect the disease early and provide help for early treatment.",
                                  style = "font-size: 150%;"),
                           align = "justify"),),
                
                column(
                  #title = tags$b("AD Process"), 
                  # status = "primary",
                  width = 12,
                  height = 300,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  br(),
                  img(src = "ad.png", height = 250, width = 1230),
                  align = "center"),
              )),
      
      tabItem(tabName = "table", 
              dataTableOutput("speciesDataTable")
      ),
      
      tabItem(tabName = "dashboard",
              h5("Overview of Distribution of Alzheimer's Disease Patient Features"),
              Info<- sidebarLayout(
                sidebarPanel( width = 4, style = "font-size: 90%;", style = "color:blue",
                              radioButtons("q", "Please select your desire exploratory descriptive analysis: ", 
                                           list("Gender" = "a1", "Age" = "a2", "Educ" = "a3", "SES" = "a4","MMSE" = "a5", "eTIV" = "a6",
                                                "nWBV" = "a7", "ASF" = "a8"))
                ),
                box(title = tags$b("Object Features - Histogram"), 
                    status = "primary",
                    width = 10,
                    height = 600,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    plotOutput("plot1"),
                    plotOutput("plot2"),
                    align = "center"),
              )),
      
      
      tabItem(tabName = "Variable",
              fluidRow(
                box(
                  # title = tags$b("Random Forest"), 
                  # status = "primary",
                  width = 15,
                  height = 1250,
                  #collapsible = TRUE,
                  #solidHeader = TRUE,
                  # br(),
                  theme = shinytheme("superhero"),
                  headerPanel('Random Forest'),
                  
                  sidebarPanel(
                    #fileInput('file','Upload'),
                    #helpText('Select'),
                    HTML("<h5> Input Parameters</h5>"),
                    selectInput('gender','gender:',choices = c(0, 1)),
                    sliderInput("Age", 'Age:',min = 50, max = 99, value = 78),
                    sliderInput("Edu", 'Education:',min = 0, max = 5, value = 3),
                    sliderInput('SES', 'SES:', min = 0, max = 5, value =3),
                    sliderInput('MMSE', 'MMSE:', min = 15, max = 30, value =15),
                    sliderInput('eTIV', 'eTIV:', min = 1120, max = 1999, value =1401),
                    sliderInput('nWBV', 'nWBV:', min = 0.640, max = 0.850, value =0.703),
                    sliderInput('ABF', 'ABF:', min = 0.880, max = 1.570, value =1.253),
                    submitButton("Submit")
                  ),
                  mainPanel(
                    tags$label(h3('Status/Output')),
                    verbatimTextOutput('contents'),
                    tableOutput('tabledata'),
                    gaugeOutput('Scale')
                  )
                  
                ))),
      
      tabItem(tabName = "Match",
              fluidRow(
                box(title = tags$b("Distribution Plot of Variables"), 
                    status = "primary",
                    width = 6,
                    height = 500,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "dis.png", height = 400, width = 600),
                    align = "center"),
                
                column(width =6,
                       box(title = tags$b("Classification of variables for men and women"), 
                           status = "primary",
                           width = 20,
                           height = 500,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "pair.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("Age distribution on different CDR scores"), 
                           status = "primary",
                           width = 20,
                           height = 450,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "age.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("Age distribution of men and women on different CDR scores"), 
                           status = "primary",
                           width = 20,
                           height = 450,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "age1.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("Correlation Heatmap"), 
                           status = "primary",
                           width = 20,
                           height = 450,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "corr.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("Sccart chart of CDR Scores by MMSE"), 
                           status = "primary",
                           width = 20,
                           height = 450,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "mmse.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =12,
                       box(title = tags$b("Box Plot of Variables"), 
                           status = "primary",
                           width = 20,
                           height = 550,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "box.png", height = 500, width = 1200),
                           br(),
                           align = "justify"),),
                
              )),
      
      tabItem(tabName = "Modeling",
              fluidRow(
                box(title = tags$b("Logistic Regression & Linear Discriminant Analysis"), 
                    # status = "primary",
                    width = 6,
                    height = 500,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "linear.png", height = 400, width = 600),
                    align = "center"),
                
                column(width =6,
                       box(title = tags$b("Tree Based Classifiers"), 
                           status = "success",
                           width = 20,
                           height = 500,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "Tree.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("BaselineModel"), 
                           #        status = "warming",
                           width = 20,
                           height = 500,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "bayes.png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
                
                column(width =6,
                       box(title = tags$b("Neural Networks(MLPC) & SVM & Naive Bayes"), 
                           #      status = "warming",
                           width = 20,
                           height = 500,
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           img(src = "Neural Networks(MLPC).png", height = 400, width = 600),
                           br(),
                           align = "justify"),),
              )),
      
      tabItem(tabName = "Match1",
              fluidRow(
                box(title = tags$b("Predictive Model Results using Random Forest Algorithm"),
                    status = "primary",
                    width = 12,
                    height = 700,
                    collapsible = TRUE,
                    solidHeader = TRUE,),
              )
      ),
      
      tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
    )
  ),
)


server <- function(input, output) {
  # output 1
  output$plot1<- renderPlot({
    if(input$q=='a1'){
      Count_Gender %>%
        ggplot(aes(x = gender,y = count)) + geom_bar(stat = 'identity',fill=13)
    }
    else if(input$q=='a2'){
      Count_Age %>% 
        ggplot( aes(x = Age, y = count)) + geom_bar(stat = 'identity',fill=2)
      # +coord_flip()
    }
    else if(input$q=='a3'){
      Count_Educ %>%
        ggplot( aes(x = Educ, y = count)) + geom_bar(stat = 'identity',fill=3)
    }
    else if(input$q=='a4'){
      Count_SES %>%
        ggplot( aes(x = SES,y = count)) + geom_bar(stat = 'identity',fill=4)
    }
    else if(input$q=='a5'){
      Count_MMSE %>%
        ggplot(aes(x = MMSE,y = count)) + geom_bar(stat = 'identity',fill=5)
    }
    else if(input$q=='a6'){
      Count_eTIV%>% 
        ggplot(aes(x =eTIV,y = count)) + geom_bar(stat = 'identity',fill=6)
    }
    else if(input$q=='a7'){
      Count_nWBV %>% 
        ggplot( aes(x = nWBV,y = count)) + geom_bar(stat = 'identity',fill=7)
    }
    else if(input$q=='a8'){
      Count_ASF %>% 
        ggplot( aes(x = ASF,y = count)) + geom_bar(stat = 'identity',fill=8)
    }
  })
  
  
  
  # output 2
  output$speciesDataTable <- renderDataTable(FinalData[,c(2:11)])
  
  # output 3 classification
  datasetInput <- reactive({
    df <- data.frame(
      Name = c('gender','Age','Educ','SES','MMSE','CDR','eTIV','nWBV','ASF','DementedStatus'),
      value = as.character(c(input$gender,
                             input$Age,
                             input$Educ,
                             input$SES,
                             input$MMSE,
                             input$CDR,
                             input$eTIV,
                             input$nBWV,
                             input$ASF)),
      stringsAsFactors = FALSE)
    #FinalData <- 'FinalData'
    #data(FinalData)
    #df <- rbind(df, FinalData)
    input <- t(df)
    write.table(input, 'input.csv', sep=' ', quote = FALSE, row.name = FALSE, col.name = FALSE)
    write.table(input, 'input4.csv', sep=' ', quote = FALSE, row.name = FALSE, col.name = FALSE)
    #test <- read.csv(paste('input.csv',sep=' '), header = TRUE)
    test <- read.csv('input.csv',sep=' ', header = TRUE)
    Output <<- data.frame(Prediction = predict(model,test),round(predict(model, test, type='prob'), 3))
    print(Output)
  })
  
  my_plot <- reactive({
    gauge(datasetInput())
  })
  
  output$Scale <- renderGauge({
    my_plot()
  })
  
  output$contents <- renderPrint({
    test <- read.csv('input.csv',sep=' ', header = TRUE)
    if(predict(model,test)==0){
      'You do not have Alzheimer Disease'
    } else{
      'You are sespected to have Alzheimer Disease'
    }
  })
  
#    if(Output$Prediction==0){
#    output$contents <- renderPrint({
#      'You do not have Alzheimer Disease'
#    })}
#  else{
#    output$contents <- renderPrint({
 #     'You are sespected to have Alzheimer Disease'
 #   })
#  } 
  
  output$tabledata <- renderTable({
    datasetInput()
  })
  
  
}

shinyApp(ui=ui, server=server)