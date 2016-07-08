SaveOutput <- function (config, output) {
  
  write.csv(output$Data_Table, config$Output['SaveFile'], row.names=FALSE)
  write.csv(output$Personal_Alias_Table, config$Output['Personal_Alias_File'], row.names=FALSE)
  
}