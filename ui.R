library(shiny)
library(shinysky)
source('setUp.r') 

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  singleton(tags$head(tags$script(src = "message-handler.js"))),
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    '))),
  # Application title
  headerPanel("NGS Dashboard"),

  sidebarPanel(
	  conditionalPanel(
	 	  condition = "input.navigation==1"      
        ,selectInput("sample.modify.primarykey", "Select sample to modifiy, leave 'NEW SAMPLE' for new registration.", NA)
    )
	  ,conditionalPanel(
      condition = "input.navigation==2"
        ,selectInput("run.modify.primarykey", "Select sample to modifiy", NA)
    )
	  ,conditionalPanel(
      condition = "input.navigation==3" 
        ,verbatimTextOutput("vcffilesuploaded")
        ,verbatimTextOutput("coveragefilesuploaded")
        ,selectInput("vcffile.modify.primarykey", "Select sample to modifiy", NA)
    )
    ,conditionalPanel(
      condition = "input.navigation==4"          
	      ,HTML("QC")
	  ) 
    ,conditionalPanel(
      condition = "input.navigation==5"
        ,selectInput("table", "Select the table you want to modifiy", c("---","tbl_samples", "tbl_run", "tbl_files", "tbl_researchers","tbl_operators"))          
        ,textInput("deleteID", "Delete Record by ID", NA)
        ,actionButton("deleteRecord", "Delete Record"), helpText("Record will only show as deleted after clicking on refresh in your browser")
        ,actionButton("createBackup", "Backup database"), helpText("Click here to create a backup of the database")
        ,HTML('<br><h4>Quick report</h4>')
        ,radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE)
        ,downloadButton('downloadReport')
    )  
    ,conditionalPanel(
      condition = "input.navigation==6"          
        ,HTML("Help")
    )           
  ),
   

  mainPanel(
    tabsetPanel( 
       tabPanel("1. Register sample"
        ,HTML('<br><h4>To be filled out by the researcher.</h4>')
        ,HTML('<h3>1.</h3>')
        ,uiOutput("form_sample")
        ,helpText("Please dont't forget to save in the Database tab.")
        ,HTML('<h3>2.</h3>')
        ,HTML("<h4>Click here to register your sample:</h4>")
        ,actionButton("registerSample", "Save")
        ,HTML("<font color='red'>Wait for the success Pop-up, else all your input will be lost!</font><br><br>") 
        ,helpText("Records will only show up in the database after clicking on refresh in your browser")
        ,value = 1
      )		
      ,tabPanel("2. Run information"
        ,HTML('<br><h4>To be filled out by the operator.</h4>')
        ,HTML('<h3>1.</h3>')
        ,uiOutput("form_run")
        ,HTML("<h4>Click here to register your run:</h4>")
        ,HTML('<h3>2.</h3>')
        ,actionButton("registerRun", "Save")
        ,HTML("<font color='red'>Wait for the success Pop-up, else all your input will be lost!</font><br><br>") 
        ,helpText("Records will only show up in the database after clicking on refresh in your browser")
        ,value = 2
      )
      ,tabPanel("3. File upload"
        ,HTML('<br><h4>To be filled out by the operator.</h4>')
        ,HTML("<font color='red'>Please note, files with the same filename can only be uploaded once - please change the filename if necessary!</font><br>") 
        ,HTML('<h3>1.</h3>')
        ,fileInput("vcffiles", "VCF file to upload:", multiple = F)
        ,helpText("Please choose the VCF files to upload")
        ,HTML('<h3>2.</h3>')
        ,fileInput("coveragefiles", "Coverage file to upload:", multiple = F)
        ,helpText("Please choose the coverage files to upload")
        ,HTML('<h3>3.</h3>')
        ,uiOutput("form_fileupload")
        ,actionButton("uploadfiles", "Save")
        ,HTML("<font color='red'>Wait for the success Pop-up, else all your input will be lost!</font><br><br>") 
        ,helpText("Records will only show up in the database after clicking on refresh in your browser")
        ,value = 3
      )
      ,tabPanel("Realtime QC"
        ,htmlOutput("googleVisTest")
        ,value = 4
      )
      ,tabPanel("Database management"
        ,checkboxInput("show_tbl_samples", "Show tbl_samples", F)
        ,dataTableOutput("dataTableTblSamples")
        ,checkboxInput("show_tbl_run", "Show tbl_run", F)
        ,dataTableOutput("dataTableTblRun")
        ,checkboxInput("show_tbl_files", "Show tbl_files", F)
        ,dataTableOutput("dataTableTblVCF")
        ,checkboxInput("show_tbl_researchers", "Show tbl_researchers", F)
        ,dataTableOutput("dataTableTblResearchers")
        ,textInput("newresearcher.name", "Name of the new researcher:", NA)
        ,actionButton("db.add.researcher", "Add researcher"), helpText("Record will only show after clicking on refresh in your browser")
        ,checkboxInput("show_tbl_operators", "Show tbl_operators", F)
        ,dataTableOutput("dataTableTblOperators")
        ,textInput("newoperator.name", "Name of the new operator:", NA)
        ,actionButton("db.add.operator", "Add operator"), helpText("Record will only show after clicking on refresh in your browser")       
        ,value = 5
      )
      ,tabPanel("Help"
        ,HTML(readLines('README.md'))
        ,h4('Debug console:')
        ,verbatimTextOutput("log.verbose")
        ,value = 6
      )
      ,id="navigation"
    )
  )  
))
