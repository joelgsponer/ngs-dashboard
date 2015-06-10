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
            

          if(mess$error == 0 & mess$result){
                  fncResetUI(session,'ui_elements/form_fileupload/', db = db,verbose = T)
                  page.refresh(session = session)
          } 
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
