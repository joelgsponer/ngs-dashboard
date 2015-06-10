#Libraries
require('RJSONIO')
require('dplyr')
require('devtools')
require('RCurl')
require('googleVis')
require('Hmisc')
require('MASS')
require('shiny')
require('RSQLite')
require('sqldf')
require('zoo')
require('plyr')
require('random')
require('rmarkdown')

#Load custom function
util_sourceFolder <- function(path, lazy=T){
  cat("#Loading files in folder:",path,"\n")
  for(file in list.files(path)){
    tryCatch({
      cat("#Loading",file,"|")
      source(paste(path, file, sep = "/"))
      cat("OK.\n")
    }, error = function(e){
      if(lazy){
        print(e)
      }else{
        stop(e)
      }      
    })
  }
}
util_sourceFolder("source/")
 
#Load configuration
source("config.R")

#Define database
require(sqldf)
db <- dbConnect(SQLite(), dbname = "data/ngs.sqlite")

#Database
tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_samples
	(ID INTEGER PRIMARY KEY AUTOINCREMENT
  ,ngsfacilityid TEXT
	)")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})

source("ui_elements/welcome.r", chdir = TRUE)
log.verbose <- T
log.debug <- ""

