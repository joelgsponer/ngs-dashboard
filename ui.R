library(shiny)
library(shinysky)
source('setUp.r') 

shinyUI(fluidPage(
  includeCSS("www/custom.css"),
  singleton(tags$head(tags$script(src = "message-handler.js"))),

  # Application title
  headerPanel("NGS Dashboard"),

  sidebarPanel(
	  conditionalPanel(
	 	  condition = "input.navigation==1"      
        ,HTML("<h4>Click here to register your sample:</h4>")
        ,actionButton("registerSample", "Register sample"), helpText("Record will only show after clicking on refresh in your browser")
	  )
	  ,conditionalPanel(
      condition = "input.navigation==2"
        ,HTML("<h4>Click here to register your run:</h4>")
        ,actionButton("registerRun", "Register run"), helpText("Record will only show after clicking on refresh in your browser")
	  )
	  ,conditionalPanel(
      condition = "input.navigation==3" 
        ,HTML("<font color='red'>Please note, files with the same filename can only be uploaded once - please change the filename if necessary!</font><br><br>") 
        ,fileInput("vcffiles", "VCF file to upload:", multiple = F)
        ,helpText("Please choose the VCF files to upload")
        ,verbatimTextOutput("vcffilesuploaded")
        ,actionButton("uploadvcf", "Upload")
        ,helpText("Changes to the database will only show after clicking on refresh in your browser")         
    )
    ,conditionalPanel(
      condition = "input.navigation==4"          
	      ,HTML("QC")
	  ) 
    ,conditionalPanel(
      condition = "input.navigation==5"
        ,selectInput("table", "Select the table you want to modifiy", c("---","tbl_samples", "tbl_run", "tbl_vcf", "tbl_researchers","tbl_operators"))          
        ,textInput("deleteID", "Delete Record by ID", NA)
        ,actionButton("deleteRecord", "Delete Record"), helpText("Record will only show as deleted after clicking on refresh in your browser")
    )  
    ,conditionalPanel(
      condition = "input.navigation==6"          
        ,HTML("Help")
    )           
  ),
   

  mainPanel(
    tabsetPanel( 
       tabPanel("1. Register sample"
        ,uiOutput("form_sample")
        ,helpText("Please dont't forget to save in the Database tab.")
        ,value = 1
      )		
      ,tabPanel("2. Run information"
        ,uiOutput("form_run")
        ,value = 2
      )
      ,tabPanel("3. VCF upload"
        ,uiOutput("form_vcfupload")
        ,value = 3
      )
      ,tabPanel("Realtime QC"
        ,value = 4
      )
      ,tabPanel("Database management"
        ,checkboxInput("show_tbl_samples", "Show tbl_samples", F)
        ,dataTableOutput("dataTableTblSamples")
        ,checkboxInput("show_tbl_run", "Show tbl_run", F)
        ,dataTableOutput("dataTableTblRun")
        ,checkboxInput("show_tbl_vcf", "Show tbl_vcf", F)
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
        ,verbatimTextOutput("log.verbose")
        ,value = 6
      )
      ,id="navigation"
    )
  )  
))
