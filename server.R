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
      writeLog("Button registerSample pressed")
      mess <- fncUItoDB('ui_elements/form_sample/', db = db, table = "tbl_samples",dat = input, primarykey.create = T)
      if(mess$error == 0) fncResetUI(session = session,'ui_elements/form_sample/', verbose = T)
      session$sendCustomMessage(
        type = 'testmessage',
        message = mess
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
      session$sendCustomMessage(type='testmessage',message=paste("SELECT primarykey FROM tbl_samples WHERE ngsfacilityid ='",input$form_run.ngsfacilityid,"'", sep = ""))
      writeLog("Button registerRun pressed")
      mess <- fncUItoDB('ui_elements/form_run/'
        ,db = db
        ,table = "tbl_run"
        ,dat = input
        ,primarykey.create = F
        ,primarykey.val = dbGetQuery(db,paste("SELECT primarykey FROM tbl_samples WHERE ngsfacilityid ='",input$form_run.ngsfacilityid,"'", sep = ""))
      )
      if(mess$error == 0)fncResetUI(session = session,db = db,'ui_elements/form_run/', verbose = T)
      session$sendCustomMessage(
        type = 'testmessage',
        message = mess
      )
    })
  }
})


################
#Tab VCF uplaod#
################
#UI
output$form_vcfupload <- renderUI({
  fncCreateUI('ui_elements/form_vcfupload/',db = db, verbose = T)
})
#File upload
uploadvcf <- observe({
  if(input$uploadvcf != 0){
    writeLog("Button uploadvcf pressed")
    isolate({ 
      tryCatch({
          vcffile <- input$vcffiles
          destination <- paste(getwd(),config_VCFSTORAGE,vcffile[,"name"], sep = "")
          writeLog(paste(
            "VCF upload\n",
            "Name:", vcffile[,"name"],"\n",
            "Path:", vcffile[,"datapath"],"\n",
            "Destination:",destination,"\n"
            ))
          dbAddField(db, "tbl_vcf", field = "vcffile", type = "TEXT")
          if(!(dbMatchingRecord(db, "tbl_vcf", vcffile$name, "vcffile", verbose = F))){
            file.copy(vcffile$datapath, destination)
            #cmd <- paste('echo "vcf analysis"')
            #print(cmd)
            #system(cmd, wait = F)
            mess <- fncUItoDB('ui_elements/form_vcfupload/'
              ,db = db
              ,table = "tbl_vcf"
              ,dat = input
              ,primarykey.create = F
              ,primarykey.val = dbGetQuery(db,paste("SELECT primarykey FROM tbl_run WHERE ngsfacilityid ='",input$form_vcfupload.ngsfacilityid,"'", sep = ""))
            )            
            dbInsertInto(db, "tbl_vcf", "vcffile",  vcffile$name, type = "TEXT", verbose = T, c(mess$primarykey.label, mess$primarykey.val))  
            dbInsertInto(db, "tbl_vcf", "timestamp",format(Sys.time()), type = "TEXT", verbose = T, c(mess$primarykey.label, mess$primarykey.val))
            if(mess$error == 0) fncResetUI(session,'ui_elements/form_vcfupload/', db = db,verbose = T)
              session$sendCustomMessage(
                type = 'testmessage',
                message = "SUCCESS:  You succesfully uploaded your VCF file."
            )
          }else{
            session$sendCustomMessage(
              type = 'testmessage',
              message = "WARNING:  There is already a VCF file with the same name in the database, please rename your file and try again."
            )
          }
        
      }, error = function(e){
         cat("!Error while uploading VCF file\n")
         writeLog("!Error while uploading VCF file")
         writeLog(as.character(e))
         print(vcffile)
         print(e)
      })
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
  if(input$show_tbl_vcf == T) dbGetRecords(db, "tbl_vcf")
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
         