######################################################
#                                                    #
#Author: Amin Sherzad, Database Manager - AFG, REACH #
#                                                    #
######################################################

library(shiny)
library(fontawesome)

#Default vectors for UI inputSelects
indicators <- vector()
indicators_label <- vector()

shinyUI(fluidPage(
  tabsetPanel(
    #Tab title
    tabPanel("Translation process UI", fluid = TRUE,
       tags$head(
         tags$style(HTML('#translated{background-color:orange}'))
       ),
       sidebarLayout(
         sidebarPanel(width = 6, style = "margin-top: 20px",
                      #Accept works only on browswer not in R built in html displayer
                      fileInput("raw_df", "Upload your raw data(xlsx)", accept = c(".xlsx")),
                      fileInput("tool", "Upload respective tool/questionnaire(xlsx)", accept = c(".xlsx")),
                      #List of other specify columns/variables
                      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 300px",
                                # List of indicator names          
                                checkboxGroupInput("show_vars", "Other specify list", choiceValues = indicators,
                                                   choiceNames = indicators_label)
                      ),
                      #Just a controller
                      uiOutput("reader"),
                      
                      actionButton("extract_translation", "Extract translations", icon("rocket")),
                      downloadButton("download_translation", "Download translations"),
                      hr(),
                      fileInput("translated", "Upload your translated file(xlsx)", accept = c(".xlsx")),
                      actionButton("replace_translation", "Replace translations", icon("wrench")),
                      downloadButton("dwn_clean_data", "Download Cleaned Data"),
         ),
         mainPanel(width = 6, style = "margin-top: 20px",
           textOutput("msgArea")
         ),
         
    )
  ),
  tabPanel("Log creator", fluid = TRUE,
           
           sidebarLayout(
             sidebarPanel(width = 6, style = "margin-top: 20px",
                          #Accept works only on browswer not in R built in html displayer
                          fileInput("modified_df", "Upload modified data(xlsx)", accept = c(".xlsx")),
                          fileInput("original_df", "Upload original data(xlsx)", accept = c(".xlsx")),
                          selectInput('meta_cols','Select Meta columns', choice = c("") , multiple=TRUE, selectize=TRUE),
                          hr(),
                          actionButton("run_logger", "Run logger", icon("rocket")),
                          downloadButton("download_logs", "Download Logs"),
                          
             ),
             mainPanel(width = 6, style = "margin-top: 20px",
               textOutput("textArea")
             ),
             
           )
           
           ),
  tabPanel("Check cleaning log", fluid = TRUE,
           
           sidebarLayout(
             sidebarPanel(width = 6, style = "margin-top: 20px",
                          fileInput("cleaning_log", "Upload Cleaning log(xlsx)", accept = c(".xlsx")),
                          fileInput("cl_raw_df", "Upload Raw Data(xlsx)", accept = c(".xlsx")),
                          actionButton("run_check", "Run Check", icon("rocket")),
                          downloadButton("download_result", "Download Result"),
                          
             ),
             mainPanel(width = 6, style = "margin-top: 20px",
                       textOutput("ccl_text_area")
             ),
             
           )
           
  )
  
    )
  )
)

#runApp(host="192.168.0.123",port=2020, launch.browser = TRUE)
