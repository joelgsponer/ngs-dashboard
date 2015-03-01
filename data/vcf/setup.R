#sources
source('~/.webCGH')
source('/var/shiny-server/www/webCGH/functions/filemanipulation.R')
source('/var/shiny-server/www/webCGH/functions/database.R')


# libraries
require(MASS)
require(shinysky)
require(shiny)
require(RSQLite)
require(sqldf)
require(dplyr)
require(RSQLite.extfuns)
require(zoo)
library(devtools)
#source_gist('510c8bed5698356ef583') #miniPlot fncCGHMiniPlot()


#conntect to database
db <- src_sqlite(paste(config_DATABASE,"webCGH.sqlite", sep = ""))

#helpers
printline <- function()cat("\n",rep("*",50), "\n","\n")

returnListValueByName <- function(input, name){
  x <- c()
  for(item in input) x <- c(x, as.character(item[name]))
  return(x)
}
