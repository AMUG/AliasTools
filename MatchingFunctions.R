Exact_Hierarchy_Match <- function(data) {
  #Deprecated by merging hierarchy and alias tables
  matched <- MyMatch(data$Data_Table$dot_name[data$unmatched], 
                     data$Hierarchy_Table$dot_name)
  
  matchedIDs <- StripPrefixAndSplitHID(data$Hierarchy_Table[matched$Pos, 'hid'])
  
  return(FillDataIDColumns(data, data$unmatched[matched$Rows], matchedIDs))  
  
}

Exact_Alias_Match <- function(data) {
  #Deprecated.  Top down match does everything this one does, faster (the retrieve full hnid function is slow)
  matched <- MyMatch(data$Data_Table$dot_name[data$unmatched], 
                     data$Full_Alias_Table$alias_dot_name)
    
  matchedIDs <- lapply(data$Full_Alias_Table$hierarchy_name_id[matched$Pos], 
                       function(x) RetrieveFullHNID(data, x))
  
  return(FillDataIDColumns(data, data$unmatched[matched$Rows], matchedIDs))  
  
}

Exact_TopDown_Match <- function(data) {  
    
  Unique_DotNames <- unique(data$Data_Table$dot_name[data$unmatched])
  
  Unique_HNIDs <- lapply(strsplit(Unique_DotNames, ':'), 
                        Recursive_TopDown_Match, 
                        strsplit(data$Full_Alias_Table$alias_dot_name, ':'), 
                        data$Full_Alias_Table$hierarchy_name_id, 
                        data$Full_Alias_Table$parent_id)
 
  matched <- MyMatch(data$Data_Table$dot_name[data$unmatched], 
                     Unique_DotNames)
  
  return(FillDataIDColumns(data, data$unmatched[matched$Rows], Unique_HNIDs[matched$Pos]))
}

Recursive_TopDown_Match <- function(dot_name, aliases, ID, parent) {
  these_aliases <- aliases
  these_IDs <- ID
  hnid <- rep(NA, length(dot_name))
  level <- 1
  
  repeat{
    test <- dot_name[level]
    aliases_thislevel <- unlist(lapply(these_aliases, function(x) x[level]))
    alias_pos <- match(test, aliases_thislevel)
    hnid[level] <- these_IDs[alias_pos]
    
    if (is.na(hnid[level]) || level == length(dot_name)) {
      break
    }
    
    mycut <- parent == hnid[level]
    these_aliases <- aliases[mycut]
    these_IDs <- ID[mycut]
    level <- level+1
  }  
  return(hnid)
}

Match_wo_Spaces_Punc <- function(data) {
  
  Unique_DotNames <- unique(data$Data_Table$dot_name[data$unmatched])
  Orig_HNIDs <- data$Data_Table[match(Unique_DotNames, data$Data_Table$dot_name),data$IDCols]  
    
  Unique_HNIDs <- lapply(lapply(strsplit(Unique_DotNames, ':'),   
                               function(x) gsub('[[:punct:][:space:]]', '', x)),
                        Recursive_TopDown_Match, 
                        lapply(strsplit(data$Full_Alias_Table$alias_dot_name, ':'), 
                               function(x) gsub('[[:punct:][:space:]]', '', x)), 
                        data$Full_Alias_Table$hierarchy_name_id, 
                        data$Full_Alias_Table$parent_id)

  #Can we trim this to an apply, rather than a loop?  Maybe not, bc it's updating "data" for each entry
  for(currMatch in seq_len(length(Unique_HNIDs))) {
    if (data$exit_flag) {
      break
    }
    
    if ( sum(is.na(Orig_HNIDs[currMatch,])) > sum(is.na(Unique_HNIDs[[currMatch]])) ) {
       data<- QueryUserForFuzzyMatch(data, Unique_DotNames[currMatch], Unique_HNIDs[currMatch]) 
    }
  }
     
  return(data)
  
}

Match_wo_Doubled_Consonants <- function(data) {

  Unique_DotNames <- unique(data$Data_Table$dot_name[data$unmatched])
  Orig_HNIDs <- data$Data_Table[match(Unique_DotNames, data$Data_Table$dot_name),data$IDCols]  
  
  Unique_HNIDs <- lapply(lapply(strsplit(Unique_DotNames, ':'),   
                               function(x) gsub('([(b-df-hj-np-tv-z)])\\1+', '\\1', x)),
                        Recursive_TopDown_Match, 
                        lapply(strsplit(data$Full_Alias_Table$alias_dot_name, ':'),
                               function(x) gsub('([(b-df-hj-np-tvxz)])\\1+', '\\1', x)), 
                        data$Full_Alias_Table$hierarchy_name_id, 
                        data$Full_Alias_Table$parent_id)
  
  for(currMatch in seq_len(length(Unique_HNIDs))) {
    if (data$exit_flag) {
      break
    }
    
    if ( sum(is.na(Orig_HNIDs[currMatch,])) > sum(is.na(Unique_HNIDs[[currMatch]])) ) {
      data<- QueryUserForFuzzyMatch(data, Unique_DotNames[currMatch], Unique_HNIDs[currMatch]) 
    }
  }
    
  return(data)
  
}

Match_Lowest_Level <- function(data) {
    
  Unique_DotNames <- unique(data$Data_Table$dot_name[data$unmatched])
  Unique_LowestNames <- unlist(lapply(strsplit(Unique_DotNames, ':'), function(x) x[length(x)]))
  Alias_LowestNames <- tolower(data$Full_Alias_Table$alias)
  Alias_Levels <-  str_count(data$Full_Alias_Table$alias_dot_name, ':') 
  for (currName in seq_along(Unique_LowestNames)) {
    if (data$exit_flag) {
      break
    }
    DotName_Level <- str_count(Unique_DotNames[currName], ':')
    level_cut <- Alias_Levels == DotName_Level
    matchPos <- which(Alias_LowestNames %in% Unique_LowestNames[currName])
    matchPos <- matchPos[which(level_cut[matchPos])]
    if (length(matchPos)>=1) {
      data<- QueryUserForFuzzyMatch(data, Unique_DotNames[currName], as.list(data$Full_Alias_Table$hierarchy_name_id[matchPos])) 
    }
  }
  return(data)
  
}

Fuzzy_Matching <- function(data) {
  Unique_DotNames <- unique(data$Data_Table$dot_name[data$unmatched])
  Orig_HNIDs <- data$Data_Table[match(Unique_DotNames, data$Data_Table$dot_name),data$IDCols]

  replaced_dotnames <- ApplyDictionaryReplacements(data$Full_Alias_Table$alias_dot_name)
  
  for (currName in seq_along(Unique_DotNames)) {
    if (data$exit_flag) {
      break
    }
    
    levelCut <- str_count(data$Full_Alias_Table$alias_dot_name, ':') == 
                str_count(Unique_DotNames[currName], ':')
    dotnames2check <- data$Full_Alias_Table$alias_dot_name[levelCut]
    dotnames2check2 <- replaced_dotnames[levelCut]
    dotnames2check2[is.na(dotnames2check2)] <- ApplyDictionaryReplacements(dotnames2check[is.na(dotnames2check2)])
    
    matchDists <- stringdist(ApplyDictionaryReplacements(Unique_DotNames[currName]), dotnames2check2)
    repeat {
      data$stop_searching<-TRUE
      data <- QueryUserForFuzzyMatch(data, Unique_DotNames[currName], 
                unique(
                  lapply(dotnames2check[which(matchDists==min(matchDists))], 
                  function(x) DotNameToHNID(data, x))))
      if (data$stop_searching) {
        break
      }
      matchDists[which(matchDists==min(matchDists))] <- Inf
    }
  }
  
  return(data)
}

FillDataIDColumns <- function(data, matchedRows, matchedIDs) {
  for (col in seq_along(data$IDCols)){
    data$Data_Table[matchedRows, data$IDCols[col]] <- 
      as.numeric( unlist( lapply( matchedIDs, function(x) x[col] )))
  }
  return(data)
}

#Simple function to return both rows in arg1 
#that were successfully matched and 
#the position of the match in arg2
MyMatch <- function(x, y) {
  matched <- list(Pos = NULL, Rows = NULL)
  matched$Pos <- match(x, y)
  matched$Rows<- which      (!is.na(matched$Pos))
  matched$Pos <- matched$Pos[matched$Rows]
  return(matched)
}