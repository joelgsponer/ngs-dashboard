options(shiny.trace=FALSE)

#Load the dependencies
library(shiny)
library(shinysky)
source('setUp.r')


#This is the server code
shinyServer(function(input, output, session){
writeLog("Server started")
updateModifyFields(session_ = session
  ,fields = c('sample.modify.primarykey', 'run.modify.primarykey','vcffile.modify.primarykey')
  ,tables=  c('tbl_samples','tbl_run','tbl_files')
)
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
      if(input$sample.modify.primarykey == 'NEW RECORD'){
         #if(dbMatchingRecord(db, tbl_samples, field = "ngsfacilityid", value = input$) == F)
         mess <- fncUItoDB('ui_elements/form_sample/'
           ,db = db
           ,table = "tbl_samples"
           ,dat = input
           ,primarykey.create = T
           ,check.fields = T
         )
      }else{
        mess <- fncUItoDB('ui_elements/form_sample/'
          ,db = db
          ,table = 'tbl_samples'
          ,dat = input
          ,primarykey.create = F
          ,check.fields = F
          ,primarykey.label = 'primarykey'
          ,primarykey.val = currentSample
        )
      }
      if(mess$error == 0 & mess$result) fncResetUI(session = session,'ui_elements/form_sample/', verbose = T)
      session$sendCustomMessage(
         type = 'testmessage'
        ,message = mess$message
      )
      enableActionButton("registerSample",session)
    })
  }
})

#Database management - dropdown
observe({
  writeLog(paste("sample.modify.primarykey:",input$sample.modify.primarykey))
  if(input$sample.modify.primarykey != 'NEW RECORD' & !(is.na(input$sample.modify.primarykey))  & input$sample.modify.primarykey != 'NA'){
    isolate({
      mess <- fncUpdateUI(path = 'ui_elements/form_sample/'
        ,db=db
        ,table='tbl_samples'
        ,primarykey.label='ngsfacilityid'
        ,primarykey.value=input$sample.modify.primarykey
        ,session = session
      )   
      tryCatch({
        assign("currentSample", mess[['primarykey.val']], envir = .GlobalEnv)
        writeLog(paste("Current sample:",currentSample))
      }, error = function(e){writeLog(e)})
    })
   }else{
      fncCreateUI('ui_elements/form_sample/',db = db,verbose = T)    
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
        message = mess$message
      )
      enableActionButton("registerRun",session)
    })
  }
})

observe({
  writeLog(paste("run.modify.primarykey:",input$run.modify.primarykey))
  if(input$run.modify.primarykey != 'NEW RECORD' & !(is.na(input$run.modify.primarykey))  & input$run.modify.primarykey != 'NA'){
    isolate({
      mess <- fncUpdateUI(path = 'ui_elements/form_run/'
        ,db=db
        ,table='tbl_run'
        ,primarykey.label='ngsfacilityid'
        ,primarykey.value=input$run.modify.primarykey
        ,session = session
      )   
      tryCatch({
        assign("currentSample", mess[['primarykey.val']], envir = .GlobalEnv)
        writeLog(paste("Current sample:",currentSample))
      }, error = function(e){writeLog(e)})
    })
   }else{
      fncCreateUI('ui_elements/form_run/',db = db,verbose = T)    
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
            "Destination:",paste(getwd(),config_VCFSTORAGE,vcffile[,"name"], sep = "/"),"\n"
            ))
          writeLog(paste(
            "Coverage file upload\n",
            "Name:", coveragefile[,"name"],"\n",
            "Path:", coveragefile[,"datapath"],"\n",
            "Destination:",paste(getwd(),config_COVERAGESTORAGE,coveragefile[,"name"], sep = "/"),"\n"
            ))

          dbAddField(db, "tbl_files", field = "vcffile", type = "TEXT")
          dbAddField(db, "tbl_files", field = "coveragefile", type = "TEXT")

          if(  !(dbMatchingRecord(db, "tbl_files", vcffile$name, "vcffile", verbose = F))
             & !(dbMatchingRecord(db, "tbl_files", coveragefile$name, "coveragefile", verbose = F))
            ){
            file.copy(vcffile$datapath, paste(getwd(),config_VCFSTORAGE,vcffile[,"name"], sep = "/"))
            file.copy(coveragefile[,"datapath"], paste(getwd(),config_COVERAGESTORAGE,coveragefile[,"name"], sep = "/"))
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
                message = "You succesfully uploaded your files"
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
#Print uploaded files
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
#Print uploaded files
output$coveragefilesuploaded <- renderPrint({
  statusUpload <- coveragefilesuploaded()
  cat("#Uploaded coverage files:\n ")
  if(!(is.null(statusUpload))){
    cat(paste("-->", statusUpload, "\n", sep = ""))
  }else{
    cat('!No coverage files uploaded yet.\n')
  }
})

#Database management - dropdown
observe({
  writeLog(paste("vcffile.modify.primarykey:",input$vcffile.modify.primarykey))
  if(input$vcffile.modify.primarykey != 'NEW RECORD' & !(is.na(input$vcffile.modify.primarykey))  & input$vcffile.modify.primarykey != 'NA'){
    isolate({
      mess <- fncUpdateUI(path = 'ui_elements/form_fileupload/'
        ,db=db
        ,table='tbl_files'
        ,primarykey.label='ngsfacilityid'
        ,primarykey.value=input$vcffile.modify.primarykey
        ,session = session
      )   
      tryCatch({
        assign("currentSample", mess[['primarykey.val']], envir = .GlobalEnv)
        writeLog(paste("Current sample:",currentSample))
      }, error = function(e){writeLog(e)})
    })
   }else{
      fncCreateUI('ui_elements/form_fileupload/',db = db,verbose = T)    
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
      disableActionButton('db.add.researcher', session)
      primarykey.val <- fncCreateUniquePrimarykey(db,"tbl_researchers")
      dbInsertInto(db, "tbl_researchers", "primarykey",  primarykey.val, type = "TEXT", verbose = T)    
      dbInsertInto(db, "tbl_researchers", "username",  input$newresearcher.name, type = "TEXT", where = c("primarykey", primarykey.val))
      updateTextInput(session,'newresearcher.name', 'Name of new researcher',NA)
      writeLog("New researcher added.")
      session$sendCustomMessage(
        type = 'testmessage',
        message = "The new researcher has been added." 
      )
      enableActionButton('db.add.researcher', session)
    }) 
  }
})
output$dataTableTblOperators <- renderDataTable({
  if(input$show_tbl_operators == T) dbGetRecords(db, "tbl_operators")
})
observe({
  if(input$db.add.operator != 0){
    isolate({
      disableActionButton('db.add.operator', session)
      primarykey.val <- fncCreateUniquePrimarykey(db,"tbl_operators")
      dbInsertInto(db, "tbl_operators", "primarykey",  primarykey.val, type = "TEXT", verbose = T)    
      dbInsertInto(db, "tbl_operators", "username",  input$newoperator.name, type = "TEXT", where = c("primarykey", primarykey.val))
      updateTextInput(session,'newoperator.name', 'Name of new operator',NA)
      writeLog("New operator added.")
      session$sendCustomMessage(
        type = 'testmessage',
        message = "The new operator has been added." 
      )
      enableActionButton('db.add.operator', session)
    }) 
  }
})
#Database management - buttons
observe({
  if(input$deleteRecord != 0) {
    isolate({
      disableActionButton('deleteRecord', session)
      dbDeleteRecord(db, input$table, "ID", input$deleteID, operator = "=")
      updateTextInput(session,'deleteID','Delete Record by ID',NA)
      session$sendCustomMessage(
        type = 'testmessage',
        message = "You deleted the record." 
      )
      enableActionButton('deleteRecord', session)
    })
  }
})
observe({
  if(input$createBackup != 0) {
    isolate({
      disableActionButton('createBackup', session)
      sink(config_BACKUPLOG)
      mess <- waRRior.snippets.backup_file(
        file = "data/ngs.sqlite"
        ,destination = paste(getwd(),config_DBBACKUPSTORAGE, sep = "/")
        ,identifier = "ngs"
        ,max.backup.files = config_MAXBACKUPFILES
        ,create.folders = T
        ,is.silent = T #it T supresses raising of errors, istead return a list with error information.
        ,function.id = "btn.createBackup" #Use this to identfy the function in error (or success messages if applicable) messages.
        ,verbose = T #Turn messages on and off
        ,debug = F
      )
      sink()
      writeLog(mess$message)
      session$sendCustomMessage(
        type = 'testmessage',
        message = "You succesfully crated a Backup." 
      )
      enableActionButton('createBackup', session)
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
         
