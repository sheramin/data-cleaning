######################################################
#                                                    #
#Author: Amin Sherzad, Database Manager - AFG, REACH #
#                                                    #
######################################################

#Check for each variable, if there is anything to translate, collects and extracts it for translation
get_translations <- function(rawDF, textFieldsDF, uuid_ = "_uuid"){
  
  # Create empty vectors
  question <- vector()
  orginal_value <- vector()
  translated_value <- NA
  uuid <- vector()
  #Iterates for each given columns
  for (j in 1:nrow(textFieldsDF)) {
      #Extracts values
      for (rowi in 1:nrow(rawDF)){
        value_raw <- rawDF[rowi, paste(textFieldsDF[j,])]
        if(!is.na(value_raw)){
          # append values to vectors
          question <- c(question, paste(textFieldsDF[j,]))
          orginal_value <- c(orginal_value, as.character(value_raw))
          uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
        }
      }
    # Feedback
    cat("\014")
    print (paste("Getting un-translated column", j, "of", nrow(textFieldsDF)))
  }
  
  forTranslation <- data.frame(question, orginal_value, translated_value, uuid)
  return(forTranslation)
  
}

#Receives the translation and raw data set then replace all translated values with orginal values
set_translation <- function(rawDF, translations, uuid_ = "_uuid"){
  for (i in 1:length(rawDF)) {
    for (j in 1:length(translations$question)) {
      var_name <- unique(translations$question[j])
      
      for (r in 1:as.numeric(count(rawDF[i]))) {
        #Compares the values and uuid
        if (paste(var_name) == paste(translations[j, ][1]) & paste(rawDF[r, uuid_]) == paste(translations[j, ][4])) {
          rawDF[r,var_name] <- translations[j, ][3]
        }
      }
      # Feedback
      cat("\014")
      print (paste("Replacing translation", j, "of", length(translations$question)))
    }
    break()
  }
  return(rawDF)
}

#Iterates on all data set and looks for any non-ascii value
check <- function(rawDF, uuid_){
  # Create empty vectors
  question <- NA
  orginal_value <- NA
  translated_value <- NA
  uuid <- NA
  
  for (j in 1:length(rawDF)) {
    #Gets the name of each variable
    #if current column has any non-ascii value, then iterate on it
    value_count <- count(rawDF[which(grepl("[^\u0001-\u007F]+", rawDF[[j]])),])
    
    if(value_count > 0){
      #iterates for each value 
      for (rowi in 1:nrow(rawDF)){
        value_raw <- rawDF[rowi, j]
        if(!is.na(value_raw)){
          # append values to vectors
          question <- c(question, names(rawDF[j]))
          orginal_value <- c(orginal_value, as.character(value_raw))
          uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
        }
      }
    }
    # Feedback
    cat("\014")
    print (paste("Checking column", j, "of", length(rawDF)))
  }
  
  forTranslation <- data.frame(question, orginal_value, translated_value, uuid)
  return(forTranslation)
  
}


#Create log
#For meta columns, there could be a multi select column from data header
create.log <- function(modiDf, origDf, uuid = "X_uuid", metaCols = vector()){
  #Initiazling variables
  uuid <- vector() 
  question.name <- vector()
  old.value <- vector()
  new.value <- vector()
  
  for (c in 1:length(origDf)) {
    for (r in 1:nrow(origDf)) {
      if (as.character(origDf[[r, c]]) != as.character(modiDf[[r, c]])) {
        # append values to vectors
        uuid <- c(uuid, as.character(modiDf[r, "X_uuid"]))
        question.name <- c(question.name, colnames(modiDf[c]))
        old.value <- c(old.value, as.character(origDf[[r, c]]))
        new.value <- c(new.value, as.character(modiDf[[r, c]]))
      }
    }
    # Feedback
    #cat("\014")
    #print (paste("Loggin column ", c, "of", length(origDf)))
  }
  
  if(length(uuid) > 0){
    #Filter meta columns only for logged rows
    extra_cols <- origDf %>% filter(X_uuid %in% uuid) %>% select(uuid = X_uuid, metaCols)
    #Create data frame for grabbed logs
    logs <- data.frame(uuid, question.name, old.value, new.value)
    logs <- logs %>% left_join(extra_cols, by = c("uuid"))
    logs <- logs %>% select(metaCols, uuid, question.name, old.value, new.value)
    return(logs)
  }else{
    #print("Nothing to log!")
    return(NULL)
  }
  
}

#Cross check both datasets in lenght, width and column names.
check.data <- function(modiDF, origDF){
  if (length(modiDF) != length(origDF)) {
    return("Lenght of datasets does not match!")
  }
  if (nrow(modiDF) != nrow(origDF)) {
    return("Datasets have different number of rows!")
  }
  res <- which(names(modiDF) != names(origDF))
  if(length(res) > 0){
    return("Headers of datasets do not match!")
  }
}

#Check the cleaning or manual cleaning logs with raw data if it's logged correctly or not
check.cleaning.log <- function(cleaning_log, cl_raw_df){
  uuid_ <- vector()
  question_name <- vector()
  value_in_data <- vector()
  value_in_cleaning_log <- vector()
  length(value_in_cleaning_log)
  #Check if old.value matches in raw data and cleaning log
  for (i in 1:nrow(cleaning_log)) {
    df_row_i <- cl_raw_df %>% filter(`_uuid` == cleaning_log$uuid[i])
    df_val_i <- as.character(df_row_i[, cleaning_log$question.name[i]])
    
    cl_row_i <- cleaning_log %>% filter(uuid == cleaning_log$uuid[i]) %>% filter(question.name == cleaning_log$question.name[i])
    cl_val_i <- as.character(cl_row_i[, "old.value"])
    
    if (df_val_i != cl_val_i) {
      uuid_ <- c(uuid_, as.character(cleaning_log$uuid[i]))
      question_name <- c(question_name, as.character(cleaning_log$question.name[i]))
      value_in_data <- c(value_in_data, df_val_i)
      value_in_cleaning_log <- c(value_in_cleaning_log, cl_val_i)
      #print(paste(cl_val_i, "is not matching with data!"))
    }
  }
  check_result <- data.frame(uuid_, question_name, value_in_data, value_in_cleaning_log)
  return(check_result)
}




