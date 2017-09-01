LoadLibrariesAndSourceFiles <- function() {
  source('LoadData.R')
  debugSource('MatchData.R')
  source('SaveOutput.R')
  debugSource('MatchingFunctions.R')
  debugSource('FormatData.R')
  source('QueryUserForFuzzyMatch.R')
  library(RJSONIO)
  library(sqldf)
  library(stringr)
  library(RODBC)
  library(stringdist)
}
  
  
  
  