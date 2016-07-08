AliasMatch_Main <- function(ConfigFile){
  #Load all required libraries, source required files
  source('LoadLibrariesAndSourceFiles.R')
  LoadLibrariesAndSourceFiles()

  
  #Load Configuration file and data files
  config <- fromJSON(ConfigFile)
  config$Data_File_Columns <- gsub(" ", ".", config$Data_File_Columns)
  
  data <- LoadData(config)
  data <- FormatData(data, config)
  
  
  data <- MatchData(config, data)
  #PostProcessData()
  SaveOutput(config, data)
  
  
  
  
  
}
