QueryUserForFuzzyMatch <- function(data, possibleMatchDataDotName, possibleMatchHNID, add=TRUE) {
  
  if (length(possibleMatchHNID) == 1) {
    data <- QueryUserForSingleFuzzyMatch(data, possibleMatchDataDotName, unlist(possibleMatchHNID), add)
  } else if (length(possibleMatchHNID) >1){
    data <- QueryUserForMultipleFuzzyMatch(data, possibleMatchDataDotName, possibleMatchHNID, add)
  }
return(data)  
  
}

CheckUserAcceptanceMultipleMatch <- function(possibleMatchDataDotName, newDotNames, add) {

  printline <- paste("Multiple possible matches found\n", "Data Entry:           ", 
                     possibleMatchDataDotName, "\nPlease select from alias matches:", sep = "", collapse = "")
  for (currName in seq_along(newDotNames)) {
    printline <- paste(printline, "\n(", as.character(currName), ")", newDotNames[[currName]], sep="", collapse="")
  }
  printline <- paste(printline, "\n(n) Reject all matches and move on", sep="", collapse="")
  printline <- paste(printline, "\n(c) Reject all matches and keep searching", sep="", collapse="")
  printline <- paste(printline, "\n(q) Exit and write current output\n", sep="", collapse="")
  if (!add){
    printline<-paste(printline, ' Note: entry will not be added to alias table\n', sep='\n', collapse='\n')
  }
  
  repeat {
    writeLines(printline)
    uInput <- readline()
    if (tolower(uInput) == "n") {
      uInput <- length(newDotNames)+1
    } 
    if (tolower(uInput) == "c") {
      uInput <- length(newDotNames)+2
    } 
    if (tolower(uInput) == "q") {
      uInput <- NA
      return(uInput)
    }
    
    uInput <- as.numeric(uInput)
    if (uInput %in% 1: (length(newDotNames)+2)) {
      return(uInput)
    } 
    print(paste("Did not recognize input ", as.character(uInput), sep='', collapse=''))
  }
  
  
}

CheckAdditionOfAliasEntry <- function(name, add) {
  if (!add) return(FALSE)
  
  printline <- paste("Entry ", name, " has been accepted for the data table.\n",
                     "However, with multiple matches, adding this entry to the personal alias table\n", 
                     "may cause problems in future matching. \n ", 
                     "Please confirm (y) or reject (n) adding this alias to your personal alias table\n",  sep='', collapse='')

  repeat {
    writeLines(printline)
    uInput <- tolower(readline())
    if (uInput == "y") {
      print("Entry will be added to your personal alias table")
      return(TRUE)
    } else if (uInput == "n") {
      print("Entry will not be added to your personal alias table")
      return(FALSE)
    } 
    print(paste("Did not recognize input ", uInput, sep='', collapse=''))
  }    
    
}

QueryUserForMultipleFuzzyMatch <- function(data, possibleMatchDataDotName, possibleMatchHNID, add) {
  #This function needs a serious refactor
  newDotNames <- lapply(possibleMatchHNID, function(x) HNIDToDotName(data, x))
  fullHNIDs <- lapply(possibleMatchHNID, function(x) RetrieveFullHNID(data, x))
  
  selection <- CheckUserAcceptanceMultipleMatch(possibleMatchDataDotName, newDotNames, add)
  if (is.na(selection)) {
    data$exit_flag <- TRUE
    return(data)
  }
  if (selection == length(newDotNames)+2) {
    data$stop_searching <- FALSE
    return(data)
  }
  if (selection %in% 1:(length(newDotNames))) {
    data <- FillDataIDColumns(data, which(data$Data_Table$dot_name == possibleMatchDataDotName), 
                              fullHNIDs[selection])
    addFlag <- CheckAdditionOfAliasEntry(newDotNames[[selection]], add)
    if(addFlag) {
      data <- AddEntryToPersonalAliasTable(data, possibleMatchDataDotName, fullHNIDs[[selection]])
    } 
  }
    
  return(data)   
  
}
CheckUserAcceptanceSingleMatch <- function(possibleMatchDataDotName, newdotname, add) {

  printline <- paste("Possible match found\n", "Data Entry:           ", possibleMatchDataDotName, "\nAlias Match:          ", newdotname,  "\n(y) accept \n(n) reject and move on \n(c) reject and keep searching \n(q) exit and write current output", sep="", collapse="")
  if (!add){
    printline<-paste(printline, ' Note: entry will not be added to alias table\n', sep='\n', collapse='\n')
  }
  
  repeat {
    writeLines(printline)
    uInput <- tolower(readline())
    if (uInput == "n") {
      return(0)
    } else if (uInput == "y") {
      return(1)
    } else if (uInput == "q") {
      return(2)
    } else if (uInput == "c") {
      return(3)
    }
    print(paste("Did not recognize input ", uInput, sep='', collapse=''))
  }

}

QueryUserForSingleFuzzyMatch <- function(data, possibleMatchDataDotName, possibleMatchHNID, add) {
  newdotname <- HNIDToDotName(data, possibleMatchHNID)
  fullHNID <- RetrieveFullHNID(data, possibleMatchHNID)
  
  accepted <- CheckUserAcceptanceSingleMatch(possibleMatchDataDotName, newdotname, add)
  if (accepted == 1) {
    data <- FillDataIDColumns(data, which(data$Data_Table$dot_name == possibleMatchDataDotName), 
                              list(fullHNID))
    if(add){
      data <- AddEntryToPersonalAliasTable(data, possibleMatchDataDotName, fullHNID)
    }
  } else if (accepted == 2) {
    data$exit_flag <- TRUE
  } else if (accepted == 3) {
    data$stop_searching <- FALSE
  }
  
  return(data)   
}

DotNameToHNID <- function(data, dotName) {
  HNID <- unlist(StripPrefixAndSplitHID(data$Hierarchy_Table$hid[
                                        data$Hierarchy_Table$dot_name == dotName]))
  if (length(HNID) == 0) {
    HNID <- RetrieveFullHNID(data, data$Full_Alias_Table$hierarchy_name_id[
                             data$Full_Alias_Table$alias_dot_name == dotName])   
  }
  return(HNID)
  
}

HNIDToDotName <- function(data, HNID) {
    HNID <- paste(HNID[!is.na(HNID)], collapse='.')
    data_dot_name <- data$Hierarchy_Table$dot_name[grep(paste(HNID, '$', sep='', collapse=''), data$Hierarchy_Table$hid)]
    return(data_dot_name)
    
}

AddEntryToPersonalAliasTable <- function(data, dotName, HNID) {
  
  splitName <- strsplit(dotName, ':')
  lastName <- splitName[[1]][length(splitName[[1]])]
  UHIDs = unique(data$Hierarchy_Table$hierarchy_name_set_id)
  for (currHierID in seq_along(UHIDs)) {
    thisUHID <- UHIDs[currHierID]
    newRow <- data.frame(id='', alias=lastName, 
                         valid_from_date="1990-01-01", 
                         valid_to_date="9999-12-31", 
                         nameset_id= thisUHID, 
                         hierarchy_name_id= HNID[length(HNID)], 
                         admin_level=str_count(dotName, ':')-1, 
                         alias_dot_name = dotName, 
                         last_upddate=Sys.Date(), 
                         last_userid=Sys.info()['user'], message='', parent_id=HNID[length(HNID)-1])
  
  
    data$Personal_Alias_Table <- rbind(data$Personal_Alias_Table, newRow)
    data$Full_Alias_Table     <- rbind(data$Full_Alias_Table    , newRow[, c('alias', 'nameset_id', 'hierarchy_name_id', 'alias_dot_name', 'parent_id')])
  }
  return(data)
  
}

GetEntriesContainingHNID <- function(data, HNID) {
  HNID <- paste(HNID[!is.na(HNID)], collapse='.')
  matchedEntries <- grep(HNID, data$Hierarchy_Table$hid, value=FALSE)
  return(matchedEntries)
}

RetrieveFullHNID <- function(data, HNID) {
  HNID <- paste(HNID[!is.na(HNID)], collapse='.')
  fullHNID <- data$Hierarchy_Table$hid[grep(paste(HNID, '$', sep='', collapse=''), data$Hierarchy_Table$hid)]
  fullHNID <- unlist(StripPrefixAndSplitHID(fullHNID))
  return(fullHNID)
}