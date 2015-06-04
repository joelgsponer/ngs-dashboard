cat(getwd())

#Laod libraries
checkPkg <- function(pkg){
  if(!is.element(pkg, installed.packages()[,1]))
    {
   cat("#!",pkg,"not found - trying to install")
   install.packages(pkg, repos="http://cran.us.r-project.org")
  }else {cat("#!",pkg,"library already installed\n")}
}

checkPkg('RJSONIO')
checkPkg('dplyr')
checkPkg('devtools')
checkPkg('RCurl')
checkPkg('googleVis')
checkPkg('Hmisc')
checkPkg('MASS')
checkPkg('shiny')
checkPkg('RSQLite')
checkPkg('sqldf')
checkPkg('zoo')
checkPkg('doParallel')
checkPkg('plyr')
checkPkg('random')
checkPkg('rmarkdown')
checkPkg('gridExtra')

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
require('doParallel')
require('plyr')
require('random')
require('shinysky')
require('rmarkdown')
require('gridExtra')



#Load custom function
util_sourceFolder <- function(path, lazy=F){
  cat("#Laoding files in folder:",path,"\n")
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

#load waRRior
source_https <- function(url, ...) {
  # load package
  require(RCurl)
 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
 
 
source_https(
   "https://raw.githubusercontent.com/joelgsponer/waRRior/master/bootstrap.R"
)

#Load configuration
source("config.R")

#Define database
require(sqldf)
db <- dbConnect(SQLite(), dbname = "data/ngs.sqlite")

#Table creation
tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_samples
	(ID INTEGER PRIMARY KEY AUTOINCREMENT
  ,ngsfacilityid TEXT
	)")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})

tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_run
  (ID INTEGER PRIMARY KEY AUTOINCREMENT
  )")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})

tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_files
  (ID INTEGER PRIMARY KEY AUTOINCREMENT
  )")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})


tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_researchers
  (ID INTEGER PRIMARY KEY AUTOINCREMENT
   ,primarykey TEXT
   ,username TEXT
  )")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})

tryCatch({
  dbSendQuery(conn = db, 
  "CREATE TABLE tbl_operators
  (ID INTEGER PRIMARY KEY AUTOINCREMENT
   ,primarykey TEXT
   ,username TEXT
  )")
}, error = function(e){cat("#!Table creation failed, maybe the table already exists.", "\n")})


source("ui_elements/welcome.r", chdir = TRUE)
log.verbose <- T
log.debug <- ""

