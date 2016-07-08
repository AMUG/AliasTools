MatchData <- function(config, data) {
  
  #Beginning of code to implement ID matching at all levels of dot name, not just lowest  

  
  
  
  #Going to attach everything to data table by default, pulling multiple outputs in R can be rough
  #Three exact matching methods -> Exact Hierarchy Dot Name, Exact Alias Dot Name, Exact Alias Top-Down Match
  #only match previously unmatched rows
  #May be unnecessary - all dot names in hierarchy table are also in alias table... not sure if this is unique to namesetID =4
  #Performance may be moderately improved after Exact_TopDownMatch - further matching functions
  #could potentially cut the list down to only elements containing the lowest successful match
  fnlist = list(Exact_TopDown_Match, Match_wo_Spaces_Punc, 
                Match_wo_Doubled_Consonants, Match_Lowest_Level, Fuzzy_Matching)
  stringlist = list( "top down matching", "matching w/o spaces and punctuation", 
                    "matching w/o doubled consonants", "matching only lowest level name", "string distance matching")

  for (currFn in seq_along(fnlist)){
    print.noquote(paste("Trying ", stringlist[[currFn]], ".", sep="", collapse=""))
    
    data <- fnlist[[currFn]](data)
    
    data$Data_Table$matched_all <- rowSums(is.na(data$Data_Table[, data$IDCols, drop=FALSE])) <= data$NEmptyCols
    data$unmatched = which(!(data$Data_Table[,'matched_all']))
    UserMessaging(data)
    
    
    SaveOutput(config, data)
    if (length(data$unmatched)==0 || data$exit_flag){
      break
    }
  }
  return(data)
}


UserMessaging <- function(data) 
{
  print.noquote(paste(  as.character(sum(data$Data_Table$matched_all)), " of ", 
                        as.character(length(data$Data_Table$matched_all)), 
                        " entries matched so far.", sep="", collapse=""))
  tmp <- unique(data.frame(name = data$Data_Table$dot_name, matched = data$Data_Table$matched_all))
  print.noquote(paste(  as.character(sum(tmp$matched)), " of ", 
                        as.character(length(tmp$matched)), 
                        " unique entries matched so far.", sep="", collapse=""))  
}
  
  