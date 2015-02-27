# NGS Dashboard 
##Introduction
This App was initially created by me in February 2015.

#####[If you encounter any problems using this app, do not hesitate to contact me.(click)](mailto:jgsponer@gmail.com)

##Usage
###Technical information
The whole application is written in R, Specifically using the shiny package ([for more information click here](http://shiny.rstudio.com/). This creates kind of a Node.js based webserver which is capable to broadcast the application on the network to be accessible via a browser to a client (depending on the version it does not work with Internet Explorer!). The server can be accessed via ip-adress:port (eg. [workhorse.dyn.uhbs.ch:3838](workhorse.dyn.uhbs.ch:3838)

###Requirements

####Client:
* Web browser (preferabely not Internet explorer)
* Network connection to the server

####Server:
* An open port
* R
* shiny package
* some additional packages:
  - ...

If you need help installing R packages please refer to the R documentation. In General ```install.packages("nameOfPackage")``` will do the trick.

###Starting up the webserver
The file runApp.R contains the necessary information to start the webserver (defaults to prot 3288). It can directely be invoked from the commandline using 

```
cd "Path to pathostat2"
"C:\Program Files\R\R-x.x.x\\bin\\Rscript" "runApp.r"
```
where x.x.x has to be replaced with the R version installed.

In the initial setup I provided a cmd file runPathostat that will start the server (which you can find on the Desktop).

###Data input
This should be fairly easy to understand. just follow the procedure in the browser or refer to the document that is avaiable in the "Prozesslandkarte"

###Data storage


