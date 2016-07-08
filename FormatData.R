FormatData <- function(data, config) {
  
  
  data$Data_Table <- AddDotNames(data$Data_Table, config)
  
  data <- LowerCaseAllAndTrimWhitespace(data, config$Data_File_Columns)
  data <- AddParentIDToAliasTables(data)
  
  data <- SetupFullAliasTable(data)
  data <- CheckAndSetupIDColumns(data, config)
  
  data$exit_flag <- FALSE
  data$stop_searching <- TRUE
  return(data)
}

ApplyDictionaryReplacements <- function(dotname){
  toreplace <- c("e", "w", "s", "n", "ne", "nw", "se", "sw", "c", "zone", "city", "town", "woreda", "special woreda", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  replacewith <- c("east", "west", "south", "north", "northeast", "northwest", "southeast", "southwest", "central", "", "", "", "", "", 
                   "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
  for (currReplacement in 1:length(toreplace)){
    dotname <- gsub(
      pattern=paste("(^|[[:punct:][:space:]])", toreplace[currReplacement], "([[:punct:][:space:]]|$)",sep="", collapse=""), 
      replacement=paste("\\1", replacewith[currReplacement], "\\2",sep="", collapse=""),
      x = dotname)
  }
  
  
  return(dotname)
}


CheckAndSetupIDColumns <- function(data, config) {
  nIDCols = max(str_count(data$Data_Table$dot_name, ':'))
  
  data$IDCols = strsplit(paste('id', as.character(0:nIDCols), sep='', collapse=','), ',')[[1]]
  data$Data_Table[, data$IDCols[!(data$IDCols %in% names(data$Data_Table))]] <- NA
  
  data$NEmptyCols = rowSums(is.na(data$Data_Table[, config$Data_File_Columns, drop=FALSE])) + 
    rowSums(apply(data$Data_Table[,config$Data_File_Columns, drop=FALSE], 2, 
                  function (x) str_count(x, pattern="[:alpha:]"))==0)
  data$Data_Table$matched_all <- rowSums(is.na(data$Data_Table[, data$IDCols, drop=FALSE])) <= data$NEmptyCols
  data$unmatched = which(!(data$Data_Table[,'matched_all']))

  return(data)
}


AddDotNames <- function(Data_Table, config) {
  #To do: handle case where user puts colon at end of default prefix or where default prefix is empty
  
  if (is.element('dot_name', tolower(names(Data_Table)))) {
    Data_Table$dot_name <- paste(config$Default_Prefix, Data_Table$dot_name, sep=':')
  } else{
    Data_Table$dot_name <- apply(Data_Table[, config$Data_File_Columns, drop=FALSE], 1, paste, collapse=':')
    Data_Table$dot_name <- paste(config$Default_Prefix, Data_Table$dot_name, sep=':')    
  }  
  
  #strip any leading or trailing colons
  Data_Table$dot_name <- gsub(':+$', '', Data_Table$dot_name)
  Data_Table$dot_name <- gsub('^:+', '', Data_Table$dot_name)
  
  return(Data_Table)
}

LowerCaseAllAndTrimWhitespace <- function(data, columns) {
  
  trimlower <- function (x) gsub("^\\s+|\\s+$", "", tolower(x))
    
  data$Hierarchy_Table$name <- trimlower(data$Hierarchy_Table$name)
  data$Hierarchy_Table$dot_name <- trimlower(data$Hierarchy_Table$dot_name)
  
  data$Alias_Table$alias <- trimlower(data$Alias_Table$alias)
  data$Alias_Table$alias_dot_name <- trimlower(data$Alias_Table$alias_dot_name)
  
  data$Personal_Alias_Table$alias <-          trimlower(data$Personal_Alias_Table$alias)
  data$Personal_Alias_Table$alias_dot_name <- trimlower(data$Personal_Alias_Table$alias_dot_name)
  
  data$Data_Table$dot_name <- trimlower(data$Data_Table$dot_name)
  for (col in seq_along(columns)) {
    data$Data_Table[,columns[col]] <- trimlower(data$Data_Table[,columns[col]])
  }
  
  return(data)
  
}

AddParentIDToAliasTables <- function(data) {
  data$Alias_Table$parent_id <- -999999*rep(1, nrow(data$Alias_Table))
  data$Personal_Alias_Table$parent_id <- -999999*rep(1, nrow(data$Personal_Alias_Table))
  
  matchPos = match(data$Alias_Table$hierarchy_name_id, data$Hierarchy_Table$id)
  mycut = !is.na(matchPos)
  data$Alias_Table[mycut,'parent_id'] = data$Hierarchy_Table[matchPos[mycut], 'parentid']
  
  matchPos = match(data$Personal_Alias_Table$hierarchy_name_id, data$Hierarchy_Table$id)
  mycut = !is.na(matchPos)
  data$Personal_Alias_Table[mycut,'parent_id'] = data$Hierarchy_Table[matchPos[mycut], 'parentid']
  
  return(data)
}

SetupFullAliasTable <- function(data) {
#  data$Personal_Alias_Table[,"valid_from_date"] <- as.Date.character(data$Personal_Alias_Table[,"valid_from_date"], format="%m/%d/%Y")
#  data$Personal_Alias_Table[,"valid_to_date"] <- as.Date.character(data$Personal_Alias_Table[,"valid_to_date"], format="%m/%d/%Y")
#  data$Personal_Alias_Table[,"last_upddate"] <- as.Date.character(data$Personal_Alias_Table[,"last_upddate"], format="%m/%d/%Y")
  tmpframe1 <- data.frame(alias = data$Alias_Table$alias, 
                          nameset_id = data$Alias_Table$nameset_id, 
                          hierarchy_name_id = data$Alias_Table$hierarchy_name_id, 
                          alias_dot_name = data$Alias_Table$alias_dot_name, 
                          parent_id = data$Alias_Table$parent_id, 
                          row.names = NULL, stringsAsFactors = FALSE)
  
  tmpframe2 <- data.frame(alias = data$Personal_Alias_Table$alias, 
                          nameset_id = data$Personal_Alias_Table$nameset_id, 
                          hierarchy_name_id = data$Personal_Alias_Table$hierarchy_name_id, 
                          alias_dot_name = data$Personal_Alias_Table$alias_dot_name, 
                          parent_id = data$Personal_Alias_Table$parent_id, 
                          row.names = NULL, stringsAsFactors = FALSE)
  
  tmpframe3 <- data.frame(alias = data$Hierarchy_Table$name, 
                          nameset_id = data$Hierarchy_Table$hierarchy_name_set_id, 
                          hierarchy_name_id = data$Hierarchy_Table$id, 
                          alias_dot_name = data$Hierarchy_Table$dot_name, 
                          parent_id = data$Hierarchy_Table$parentid, 
                          row.names = NULL, stringsAsFactors = FALSE)  
  
  data$Full_Alias_Table <- rbind(tmpframe1, tmpframe2)
  data$Full_Alias_Table <- rbind(data$Full_Alias_Table, tmpframe3)
  
  #Now cut to uniques
  data$Full_Alias_Table <- unique(data$Full_Alias_Table)
  
  data$Full_Alias_Table <- data$Full_Alias_Table[
    order(str_count(data$Full_Alias_Table$alias_dot_name, ':')),]
  
  return(data)
}

StripPrefixAndSplitHID <- function(hids){
  tmpID <- gsub('^root\\.\\d+\\.', '', hids)
  tmpID <- strsplit(tmpID, '\\.')
  return(tmpID)
  
}