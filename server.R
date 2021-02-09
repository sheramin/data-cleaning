######################################################
#                                                    #
#Author: Amin Sherzad, Database Manager - AFG, REACH #
#                                                    #
######################################################

library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(rio)
source("./functions/translation.R")

shinyServer(function(input, output, session){

  #Global options - Permit file upload upto 50 MBs
  options(shiny.maxRequestSize=50*1024^2)
  
  ################################################# Translation part start #################################################
  global_clean_data <- data.frame()
  global_translations <- data.frame()
  
  #Return clean data
  clened_data_react <- reactive({
    cleaned_data <- import_list("output/cleaned_data.xlsx")
  })
  #Return translations 
  translation_react <- reactive({
    translation <- read_excel("output/to_translate.xlsx")
  })

  #Read raw data
  get_raw_data <- reactive({
    req(input$raw_df, file.exists(input$raw_df$datapath))
    read_excel(input$raw_df$datapath)
  })
  
  #Read tool/questionnaire
  get_tool <- reactive({
    req(input$tool, file.exists(input$tool$datapath))
    read_excel(input$tool$datapath)
  })

  #Read translated file
  get_translateds <- reactive({
    req(input$translated, file.exists(input$translated$datapath))
    read_excel(input$translated$datapath)
  })
  
  output$reader <- renderDataTable({
    tool <- get_tool()
    tool <- tool %>% select(type, name) %>% filter(type == "text")
    #Update other specify list
    updateCheckboxGroupInput(session, "show_vars", "Other specify list", 
                             choiceValues = tool$name,
                             choiceNames = tool$name,
                             selected = tool$name
                             )
  })

  #Extracting translation scripts run...
  observeEvent(input$extract_translation, {
    # loads data
    tryCatch({
      ##Test inputs
      #raw_df <- read_excel("./input/AFG2002_JMMI_November_2020_-_all_versions_-_False_-_2020-11-15-04-19-30.xlsx")
      #tool <- read_excel("./input/AFG2002_JMMI_November_2020.xlsx")
      
      #Get raw data
      raw_df <- get_raw_data()
      #Get the tool and select question names which are open-ended(need translation)
      tool <- get_tool()
      tool <- tool %>% select(type, name) %>% filter(name %in% input$show_vars)
      tool <- tool %>% select(type, name) %>% filter(name %in% names(raw_df))
      text_fields <- tool$name
      text_fields <- as.data.frame(text_fields)
      #To replace the uuid with script readable format
      names(raw_df)[names(raw_df) == "uuid"] <- "_uuid"
      names(raw_df)[names(raw_df) == "UUID"] <- "_uuid"
      names(raw_df)[names(raw_df) == "Uuid"] <- "_uuid"
      names(raw_df)[names(raw_df) == "_UUID"] <- "_uuid"
      #Send the raw data with list of questions (which may have values need review/translation) to extract the un-translated values
      showNotification(paste("Extraction is running...Will notify you when it's ready for download!"), duration = 10, type = "message")
      for_translation <- get_translations(raw_df, text_fields)
      write.xlsx(for_translation, "output/to_translate.xlsx")
      #Success message
      Sys.sleep(5)
      showNotification(paste("Extraction is done!"), duration = 10, type = "message")
    }, error = function(e){
      showNotification(paste("Error: Plese check, both tool and data must match!"), duration = 10, type = "error")
    })
  })
  
  #Replacing translation scripts run...
  observeEvent(input$replace_translation, {
    # loads data
    tryCatch({
      #Get raw data
      raw_df <- get_raw_data()
      #To replace the uuid with script readable format
      names(raw_df)[names(raw_df) == "uuid"] <- "_uuid"
      names(raw_df)[names(raw_df) == "UUID"] <- "_uuid"
      names(raw_df)[names(raw_df) == "Uuid"] <- "_uuid"
      names(raw_df)[names(raw_df) == "_UUID"] <- "_uuid"
      uuid <- "_uuid"
      #Gets the translated file from input directory
      translated <- get_translateds()
      #Befor set the translations, Responsible can harmonize it in a column may call "harmonized_value" then replace it instead of translated value.
      showNotification(paste("Replacing is running... Will notify you once it's done!"), duration = 10, type = "message")
      cleaned_df <- set_translation(raw_df, translated, uuid)
      #Iterates on all data set and looks for any non-ascii value
      showNotification(paste("Check is running..."), duration = 10, type = "message")
      check_result <- check(cleaned_df, uuid)
      #Write all 3 outputs in a single excel file, separate sheets
      data_sheets <- list("Clean data" = cleaned_df, "Cleaning logs" = translated, "Left un-translated" = check_result)
      write.xlsx(data_sheets, "output/cleaned_data.xlsx")
      #Success message
      Sys.sleep(5)
      showNotification(paste("Replacing is done. Clean data is ready for Download!"), duration = 5, type = "message")
    }, error = function(e){
      showNotification(paste("Error: Cannot replace! please check your files."), duration = 10, type = "error")
    }, warning=function(w) {
      #warning message
    }, finally={
      #Final action
    })
    
  })
  
  output$download_translation <- downloadHandler(
    filename = function(){
      paste("translation", "xlsx", sep = ".")
    },
    content = function(file){
      write.xlsx(translation_react(), file)
    }
  )

  output$dwn_clean_data <- downloadHandler(
    
    filename = function(){
      paste("cleaned_data", "xlsx", sep = ".")
    },
    
    content = function(file){
      write.xlsx(clened_data_react(), file)
      #Remove all files from server
      file.remove("output/cleaned_data.xlsx")
      file.remove("output/to_translate.xlsx")
    }
  )
  ################################################# Translation part end #################################################
  
  ################################################# Log creator start #################################################
  #Display user manual
  log_creator_text <- "You’ve a big dataset and you or someone else changed something in it?
    And you have to understand what has been changed?
    No worries! Change Log creator App is here to help you! You need to two things 1) your modified dataset 
    2) your original data set. The app gives you space to upload these two datasets, 
    it compares both files cell by cell and provide you the changes with their unique ID, question name, previous value 
    and new/changed value. You also have this opportunity to select extra or meta columns, e.g: province, district or any other column
    which the data belongs to.
    Important: your datasets must have a unique ID column which exactly named “_uuid”.
    Note: Both datasets must match in rows, columns and header! Otherwise, it won’t operate.
  "
  output$textArea <- renderText({
    log_creator_text
  })
  
  #Return change log
  change_log_react <- reactive({
    change_log <- import_list("output/change_log.xlsx")
  })
  
  #Read modified data
  get_modified_data <- reactive({
    req(input$modified_df, file.exists(input$modified_df$datapath))
    read_excel(input$modified_df$datapath)
  })
  
  #Read original data
  get_original_data <- reactive({
    req(input$original_df, file.exists(input$original_df$datapath))
    read_excel(input$original_df$datapath)
  })
  
  #List columns for meta column selection
  observe({
    #Load test data
    #modi_data <- read_excel("./input/AFG2002_JMMI_November_2020_mod.xlsx", guess_max = 10000)
    modi_data <- get_modified_data()
    write_excel_csv(modi_data, "input/modi_data.csv")
    modi_data <- read.csv("input/modi_data.csv", na.strings = "", encoding="UTF-8")
    
    #Update meta column list
    updateSelectInput(session, "meta_cols",
                      choices = names(get_modified_data()),
                      selected = NULL)
    
    #Update meta column list
    updateSelectInput(session, "uuid_col",
                      choices = names(modi_data),
                      selected = NULL)
  })
  
  observeEvent(input$run_logger, {
  showNotification(paste("Working on it... Will notify you once it's done!"), duration = 3, type = "message")
  
  #source("./functions/translation.R")
    modi_data <- read.csv("input/modi_data.csv", na.strings = "", encoding="UTF-8")
    
    #orig_data <- read_excel("./input/AFG2002_JMMI_November_2020_raw_data.xlsx", guess_max = 10000)
    orig_data <- get_original_data()
    write_excel_csv(orig_data, "input/orig_data.csv")
    orig_data <- read.csv("input/orig_data.csv", na.strings = "", encoding="UTF-8")
    
  tryCatch({
    #Check the length and hight of datasets
    check_result <- check.data(modi_data, orig_data)
    if (length(check_result) > 0) {
      showNotification(paste(check_result, "Try again when you fixed it."), duration = 5, type = "error")
    }else{
      myLogs <- create.log(modi_data, orig_data, df_uuid = input$uuid_col, c(input$meta_cols))
      write.xlsx(myLogs, "output/change_log.xlsx")
      #Success message
      Sys.sleep(2)
      showNotification(paste("Your change log is ready! Click on download button."), duration = 5, type = "message")
    }
  }, error = function(e){
    showNotification(paste("Error: Cannot Log! please check your files."), duration = 10, type = "error")
  })
  
  
  })
  
  #Download change log
  output$download_logs <- downloadHandler(
    
    filename = function(){
      paste("change_log", "xlsx", sep = ".")
    },
    
    content = function(file){
      write.xlsx(change_log_react(), file)
      #Remove all files from server
      Sys.sleep(1)
      file.remove("output/change_log.xlsx")
      file.remove("input/modi_data.csv")
      file.remove("input/orig_data.csv")
    }
  )
  
  ################################################# Log creator start #################################################
  ################################################# Check cleaning log - start #################################################
  uuid_notice <- "Your unique ID column in data must be named [_uuid] and your uniqe ID column in your cleaning log must be named [uuid] otherwise, the app won't work!
  This app is for cross checking cleaning log with raw dataset. In case, a Non-technical member of team has made a cleaning log or someone has modifed something in cleaing log
  and you want make sure that everything changed correctly! This app helps to find and differences - if any, in cleaning log against raw dataset.
  But, you MUST consider the unique ID column naming!
  "
  output$ccl_text_area <- renderText({
    uuid_notice
  })
  
  #Read raw data
  get_cl_raw_data <- reactive({
    req(input$cl_raw_df, file.exists(input$cl_raw_df$datapath))
    read_excel(input$cl_raw_df$datapath)
  })
  
  #Read raw data
  get_cleaning_log <- reactive({
    req(input$cleaning_log, file.exists(input$cleaning_log$datapath))
    read_excel(input$cleaning_log$datapath)
  })
  
  #Return check result
  check_result_react <- reactive({
    change_log <- import_list("output/check_result.xlsx")
  })
  
  #Read cleaning log - test
  #cleaning_log <- read_excel("./input/cleaning logs/AFG_ERM10_HEAT_TOOL_052020_Cleaning_log.xlsx")
  #cl_raw_df <- read_excel("./input/AFG_ERM10_HEAT_TOOL_052020_Raw_data.xlsx")
  
  observeEvent(input$run_check, {
    tryCatch({
      showNotification(paste("Working on it... Will notify you once it's done!"), duration = 5, type = "message")
      
      cleaning_log <- get_cleaning_log()
      cl_raw_df <- get_cl_raw_data()
      
      cleaning_log$old.value[is.na(cleaning_log$old.value)] <- "NA"
      cl_raw_df[is.na(cl_raw_df)] <- "NA"
      
      check_res <- check.cleaning.log(cleaning_log, cl_raw_df)
      write.xlsx(check_res, "output/check_result.xlsx")
      
      Sys.sleep(3)
      showNotification(paste("Done! please click on download button to get the result."), duration = 5, type = "message")
      
    }, error = function(e){
      showNotification(paste("Error: Cannot Log! please check your files."), duration = 5, type = "error")
    })
    
  })
  
  #Download check result
  output$download_result <- downloadHandler(
    
    filename = function(){
      paste("check_result", "xlsx", sep = ".")
    },
    
    content = function(file){
      write.xlsx(check_result_react(), file)
      #Remove all files from server
      Sys.sleep(1)
      file.remove("output/check_result.xlsx")
    }
  )
  
})

