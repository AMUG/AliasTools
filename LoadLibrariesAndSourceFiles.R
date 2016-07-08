LoadLibrariesAndSourceFiles <- function() {
  source('LoadData.R')
  source('MatchData.R')
  source('SaveOutput.R')
  debugSource('MatchingFunctions.R')
  source('FormatData.R')
  source('QueryUserForFuzzyMatch.R')
  library(RJSONIO)
  library(sqldf)
  library(stringr)
  library(RODBC)
  library(stringdist)
}
  
  
  
  