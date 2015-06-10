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
checkPkg('plyr')
checkPkg('random')
checkPkg('rmarkdown')
