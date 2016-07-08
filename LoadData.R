LoadData <- function(config){
  
  #Getting rid of "FileSource" argument - just loop over elements of "Database" and "InputFiles" fields
  datalist <- CheckNames(config)
  datalist <- LoadDataFromFiles(config, datalist)
  if (length(config$DatabaseTables) >0) {
    datalist <- LoadDataFromDatabase(config, datalist)
  }
  datalist <- ApplyNamesetCut(config, datalist)
  datalist <- TrimDataTable(config, datalist)
  return(datalist)
  
}

CheckNames <- function(config) {
  names1 <- names(config$DatabaseTables)
  names2 <- names(config$InputFiles)
  requiredNames <- c("Hierarchy_Table", "Alias_Table", "Personal_Alias_Table", "Data_Table")
  
  test <- (requiredNames %in% names1) | (requiredNames %in% names2)
  if (!all(test)) {
    stop(paste('Error in LoadData: Required input ', requiredNames[!test], 
               ' is not present in config$Database or config$InputFiles', 
               sep='', collapse='\n'))
  }
  
  test <- names1 %in% names2
  if (any(test)) {
    stop(paste('Error in LoadData: Required input ', names1[test], 
               ' is present in both config$Database and config$InputFiles', 
               sep='', collapse='\n'))
  }
  
  return (sapply(requiredNames, function(x) data.frame()))
}

LoadDataFromFiles <- function(config, datalist) {
    
    #For now, assume all is csv.  Can generalize later (if exist Hierarchy_File, load it, else pass.  Each table should be grabbed from file or DB though)
    #No reason nameset_id column should be named differently in hierarchy and alias tables. 
    tables <- names(config$InputFiles)
    for (currTable in tables) {
      datalist[[currTable]] <- read.csv(config$InputFiles[[currTable]], stringsAsFactors=FALSE)
    }
      
    return(datalist)    
}

LoadDataFromDatabase <- function(config, datalist) {
  
  conn <- odbcConnect(config$DatabaseName)
  
  tables <- names(config$DatabaseTables)
  for (currTable in tables) {
    datalist[[currTable]] <- sqlQuery(conn, config$DatabaseTables[[currTable]])
  }
  odbcClose(conn)
  
  return(datalist)
}


ApplyNamesetCut <- function(config, datalist) {
  cut <- datalist$Hierarchy_Table$hierarchy_name_set_id == config$Nameset_ID
  cut[is.na(cut)] = FALSE
  datalist$Hierarchy_Table <- datalist$Hierarchy_Table[cut,]
  
  cut <- datalist$Alias_Table$nameset_id == config$Nameset_ID
  cut[is.na(cut)] = FALSE
  datalist$Alias_Table <- datalist$Alias_Table[cut,]
  
  cut <- datalist$Personal_Alias_Table$nameset_id == config$Nameset_ID
  cut[is.na(cut)] = FALSE
  datalist$Personal_Alias_Table <- datalist$Personal_Alias_Table[cut,]
  
  return(datalist)
}

TrimDataTable <- function(config, datalist) {
  empties <- rowSums(is.na(datalist$Data_Table[,config$Data_File_Columns, drop=FALSE]) | 
                       datalist$Data_Table[,config$Data_File_Columns, drop=FALSE] == "") == length(config$Data_File_Columns)
  datalist$Data_Table <- datalist$Data_Table[!empties, , drop=FALSE]
  
  return(datalist)
}
