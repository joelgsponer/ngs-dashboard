options(shiny.trace=FALSE)

#Load the dependencies
library(shiny)
source('setUp.r')


#This is the server code
shinyServer(function(input, output, session){
writeLog("Server started")
updateModifyFields(session_ = session
  ,fields = c('sample.modify.primarykey', 'run.modify.primarykey','vcffile.modify.primarykey')
  ,tables=  c('tbl_samples','tbl_run','tbl_files')
)
source("source/tab_sample/main.R", local = T)
source("source/tab_run/main.R", local = T)
source("source/tab_fileupload/main.R", local = T)
source("source/tab_QC/main.R", local = T)
source("source/tab_database/main.R", local = T)
source("source/tab_help/main.R", local = T)
})
