options(shiny.trace=FALSE)

#Load the dependencies
library(shiny)
library(shinysky)
source('setUp.r')

#This is the server code
shinyServer(function(input, output, session){
writeLog("Server started")
#############
#Tab samples#
#############
#UI
output$form_sample <- renderUI({
  fncCreateUI('ui_elements/form_sample/',db = db,verbose = T)
})

#Buttons
registerSample <- observe({
  if(input$registerSample != 0) {
    isolate({
      disableActionButton("registerSample",session)
      writeLog("Button registerSample pressed")
      writeLog(input$form_sample.studyid)
      mess <- fncUItoDB('ui_elements/form_sample/'
        ,db = db
        ,table = "tbl_samples"
        ,dat = input
        ,primarykey.create = T
        ,check.fields = T
      )
      if(mess$error == 0 & mess$result) fncResetUI(session = session,'ui_elements/form_sample/', verbose = T)
      session$sendCustomMessage(
         type = 'testmessage'
        ,message = mess
      )
      enableActionButton("registerSample",session)
    })
  }
})

#Database management - buttons
observe({
  if(input$sample.review.btn != 0) {
    isolate({
      fncUpdateUI(path = 'ui_elements/form_sample/'
        ,db=db
        ,table='tbl_samples'
        ,primarykey.label='primarykey'
        ,primarykey.value=input$sample.review.primarykey
        ,session = session
      )      
    })
  }
})

#####################
#Tab Run information#
#####################
#UI
output$form_run <- renderUI({
  dbAddField(db, "tbl_run", field = "ngsfacilityid", type = "TEXT")
  fncCreateUI('ui_elements/form_run/',db = db,verbose = T)
})
#Buttons
registerRun <- observe({
  if(input$registerRun != 0) {
    isolate({
      disableActionButton("registerRun",session)
      session$sendCustomMessage(type='testmessage',message=paste("SELECT primarykey FROM tbl_samples WHERE ngsfacilityid ='",input$form_run.ngsfacilityid,"'", sep = ""))
      writeLog("Button registerRun pressed")
      mess <- fncUItoDB('ui_elements/form_run/'
        ,db = db
        ,table = "tbl_run"
        ,dat = input
        ,primarykey.create = F
        ,primarykey.val = dbGetQuery(db,paste("SELECT primarykey FROM tbl_samples WHERE ngsfacilityid ='",input$form_run.ngsfacilityid,"'", sep = ""))
        ,check.fields = T
      )
      if(mess$error == 0 & mess$result)fncResetUI(session = session,db = db,'ui_elements/form_run/', verbose = T)
      session$sendCustomMessage(
        type = 'testmessage',
        message = mess
      )
      enableActionButton("registerRun",session)
    })
  }
})


################
#Tab VCF uplaod#
################
#UI
output$form_fileupload <- renderUI({
  fncCreateUI('ui_elements/form_fileupload/',db = db, verbose = T)
})

#VCF
#File upload
observe({
  if(input$uploadfiles != 0){
    writeLog("Button uploadvcf pressed")
    isolate({
      disableActionButton("uploadfiles",session) 
      tryCatch({
          vcffile <- input$vcffiles
          coveragefile <- input$coveragefiles

          writeLog(paste(
            "VCF upload\n",
            "Name:", vcffile[,"name"],"\n",
            "Path:", vcffile[,"datapath"],"\n",
            "Destination:",paste(getwd(),config_VCFSTORAGE,vcffile[,"name"], sep = ""),"\n"
            ))
          writeLog(paste(
            "Coverage file upload\n",
            "Name:", coveragefile[,"name"],"\n",
            "Path:", coveragefile[,"datapath"],"\n",
            "Destination:",paste(getwd(),config_COVERAGESTORAGE,coveragefile[,"name"], sep = ""),"\n"
            ))

          dbAddField(db, "tbl_files", field = "vcffile", type = "TEXT")
          dbAddField(db, "tbl_files", field = "coveragefile", type = "TEXT")

          if(  !(dbMatchingRecord(db, "tbl_files", vcffile$name, "vcffile", verbose = F))
             & !(dbMatchingRecord(db, "tbl_files", coveragefile$name, "coveragefile", verbose = F))
            ){
            file.copy(vcffile$datapath, paste(getwd(),config_VCFSTORAGE,vcffile[,"name"], sep = ""))
            file.copy(coveragefile[,"datapath"], paste(getwd(),config_COVERAGESTORAGE,coveragefile[,"name"], sep = ""))
            #cmd <- paste('echo "vcf analysis"')
            #print(cmd)
            #system(cmd, wait = F)
            mess <- fncUItoDB('ui_elements/form_fileupload/'
              ,db = db
              ,table = "tbl_files"
              ,dat = input
              ,primarykey.create = F
              ,primarykey.val = dbGetQuery(db,paste("SELECT primarykey FROM tbl_run WHERE ngsfacilityid ='",input$form_fileupload.ngsfacilityid,"'", sep = ""))
              ,check.fields = F
            )            
            dbInsertInto(db, "tbl_files", "vcffile",  vcffile$name, type = "TEXT", verbose = T, c(mess$primarykey.label, mess$primarykey.val))  
            dbInsertInto(db, "tbl_files", "coveragefile",  coveragefile$name, type = "TEXT", verbose = T, c(mess$primarykey.label, mess$primarykey.val))              
            dbInsertInto(db, "tbl_files", "timestamp",format(Sys.time()), type = "TEXT", verbose = T, c(mess$primarykey.label, mess$primarykey.val))
            

          if(mess$error == 0 & mess$result) fncResetUI(session,'ui_elements/form_fileupload/', db = db,verbose = T)
              session$sendCustomMessage(
                type = 'testmessage',
                message = "SUCCESS:  You succesfully uploaded your files"
            )
          }else{
            if(dbMatchingRecord(db, "tbl_files", vcffile$name, "vcffile", verbose = F)) x <- "VCF"
            if(dbMatchingRecord(db, "tbl_files", coveragefile$name, "coveragefile", verbose = F)) x <-"coverage"
            session$sendCustomMessage(
              type = 'testmessage',
              message = sprintf("WARNING:  There is already a %s file with the same name in the database, please rename your file and try again.",x)
            )
          }
        
      }, error = function(e){
         cat("!Error while uploading VCF file\n")
         writeLog("!Error while uploading VCF file")
         writeLog(as.character(e))
         print(vcffile)
         print(e)
         session$sendCustomMessage(
           type = 'testmessage',
           message = sprintf("ERROR: %s - Please report to jgsponer@gmail.com", as.character(e))
         )
      })
      enableActionButton("uploadfiles",session)
    })
  }
})

vcffilesuploaded <- reactive({
  if(is.null(input$vcffiles)){
    return(NULL)
  }else{                  
    return(input$vcffiles[,1])
  }
})
#Print upladed files
output$vcffilesuploaded <- renderPrint({
  statusUpload <- vcffilesuploaded()
  cat("#Uploaded VCF files:\n ")
  if(!(is.null(statusUpload))){
    cat(paste("-->", statusUpload, "\n", sep = ""))
  }else{
    cat('!No VCF files uploaded yet.\n')
  }
})


coveragefilesuploaded <- reactive({
  if(is.null(input$coveragefiles)){
    return(NULL)
  }else{                  
    return(input$coveragefiles[,1])
  }
})
#Print upladed files
output$coveragefilesuploaded <- renderPrint({
  statusUpload <- coveragefilesuploaded()
  cat("#Uploaded coverage files:\n ")
  if(!(is.null(statusUpload))){
    cat(paste("-->", statusUpload, "\n", sep = ""))
  }else{
    cat('!No coverage files uploaded yet.\n')
  }
})


##########
#QC Panel#
##########
output$googleVisTest <-renderGvis({
df=data.frame(date=seq(1,100), 
              "QC"=runif(100,0,100)
              )
              
Bar <- gvisAreaChart(df
  ,options=list(
    series="[
       {color:'green'}
    ]"
    ,curveType="function"
    ,gvis.editor="Edit me!"
    ,explorer="{ actions: ['dragToZoom', 'rightClickToReset'] }"
  )
)
})

##############
#Tab database#
##############
#data tables

output$dataTableTblSamples <- renderDataTable({
  if(input$show_tbl_samples == T) dbGetRecords(db, "tbl_samples")
})
output$dataTableTblRun <- renderDataTable({
  if(input$show_tbl_run == T) dbGetRecords(db, "tbl_run")
})
output$dataTableTblVCF <- renderDataTable({
  if(input$show_tbl_files == T) dbGetRecords(db, "tbl_files")
})
output$dataTableTblResearchers <- renderDataTable({
  if(input$show_tbl_researchers == T) dbGetRecords(db, "tbl_researchers")
})
observe({
  if(input$db.add.researcher != 0){
    isolate({
      primarykey.val <- fncCreateUniquePrimarykey(db,"tbl_researchers")
      dbInsertInto(db, "tbl_researchers", "primarykey",  primarykey.val, type = "TEXT", verbose = T)    
      dbInsertInto(db, "tbl_researchers", "username",  input$newresearcher.name, type = "TEXT", where = c("primarykey", primarykey.val))
      updateTextInput(session,'newresearcher.name', 'Name of new researcher',NA)
      writeLog("New researcher added.")
      session$sendCustomMessage(
        type = 'testmessage',
        message = "The new researcher has been added." 
      )
    }) 
  }
})
output$dataTableTblOperators <- renderDataTable({
  if(input$show_tbl_operators == T) dbGetRecords(db, "tbl_operators")
})
observe({
  if(input$db.add.operator != 0){
    isolate({
      primarykey.val <- fncCreateUniquePrimarykey(db,"tbl_operators")
      dbInsertInto(db, "tbl_operators", "primarykey",  primarykey.val, type = "TEXT", verbose = T)    
      dbInsertInto(db, "tbl_operators", "username",  input$newoperator.name, type = "TEXT", where = c("primarykey", primarykey.val))
      updateTextInput(session,'newoperator.name', 'Name of new operator',NA)
      writeLog("New operator added.")
      session$sendCustomMessage(
        type = 'testmessage',
        message = "The new operator has been added." 
      )
    }) 
  }
})
#Database management - buttons
observe({
  if(input$deleteRecord != 0) {
    isolate({
      dbDeleteRecord(db, input$table, "ID", input$deleteID, operator = "=")
      updateTextInput(session,'deleteID','Delete Record by ID',NA)
      session$sendCustomMessage(
        type = 'testmessage',
        message = "You deleted the record." 
      )
    })
  }
})
##########
#Tab help#
##########
currentLog <- reactive(log.debug)
output$log.verbose <- renderPrint({
  if(log.verbose == T){
    cat(currentLog())
  }
})
})
         