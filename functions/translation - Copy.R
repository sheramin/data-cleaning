######################################################
#                                                    #
#Author: Amin Sherzad, Database Manager - AFG, REACH #
#                                                    #
######################################################


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


