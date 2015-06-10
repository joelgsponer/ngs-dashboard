##############
#Tab database#
##############
#data tables

output$dataTableTblSamples <- renderDataTable({
  if(input$show_tbl_samples == T) dbGetRecords(db, "tbl_samples")
})
output$downloadTableTblSamples <- downloadHandler(
    filename = function() {
      paste(paste('NGS_table_samples_',gsub("[:]"," ",format(Sys.time(), format = "%y-%m-%d")),".csv",sep =""))
    },
    content = function(file) {
      write.csv(dbGetRecords(db, "tbl_samples"), file)
    }
  )

output$dataTableTblRun <- renderDataTable({
  if(input$show_tbl_run == T) dbGetRecords(db, "tbl_run")
})
output$downloadTableTblRun <- downloadHandler(
    filename = function() {
      paste(paste('NGS_table_run_',gsub("[:]"," ",format(Sys.time(), format = "%y-%m-%d")),".csv",sep =""))
    },
    content = function(file) {
      write.csv(dbGetRecords(db, "tbl_run"), file)
    }
  )
output$dataTableTblVCF <- renderDataTable({
  if(input$show_tbl_files == T) dbGetRecords(db, "tbl_files")
})
output$downloadTableTblVCF <- downloadHandler(
    filename = function() {
      paste(paste('NGS_table_files_',gsub("[:]"," ",format(Sys.time(), format = "%y-%m-%d")),".csv",sep =""))
    },
    content = function(file) {
      write.csv(dbGetRecords(db, "tbl_files"), file)
    }
  )

#Researchers
output$dataTableTblResearchers <- renderDataTable({
  if(input$show_tbl_researchers == T) researchers.reactive()
})

researchers.reactive <- reactive({
        dbGetRecords(db, "tbl_researchers")
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
      page.refresh(session = session)
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
      page.refresh(session = session)
      enableActionButton('db.add.operator', session)
    }) 
  }
})
#Database management - buttons
observe({
  if(input$deleteRecord != 0) {
    isolate({
      if(input$table == "---"){
        session$sendCustomMessage(
          type = 'testmessage',
          message = "Please select a table first." 
        )
      }else{
        disableActionButton('deleteRecord', session)
        dbDeleteRecord(db, input$table, "ID", input$deleteID, operator = "=")
        updateTextInput(session,'deleteID','Delete Record by ID',NA)
        session$sendCustomMessage(
          type = 'testmessage',
          message = "You deleted the record." 
        )
        page.refresh(session = session)
        enableActionButton('deleteRecord', session)
      }
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
        ,debug = T
      )
      sink()
      writeLog(mess$message)
      if(mess$error == 1){
       writeLog(mess$e)
       session$sendCustomMessage(
          type = 'testmessage',
          message = mess
       )
      }else{
        session$sendCustomMessage(
          type = 'testmessage',
          message = "You succesfully created a Backup." 
        )
      }
      enableActionButton('createBackup', session)
    })
  }
})

samples.no.files <- reactive({
    strSQL <- 'SELECT *
               FROM   tbl_run
               WHERE  ngsfacilityid NOT IN (SELECT ngsfacilityid
                                            FROM tbl_files)'
    x <-  dbGetQuery(db, strSQL)
    if(dim(x)[1] != 0){
      x <- data.frame(
        'NGS Facility ID' = x$ngsfacilityid
        ,'Operator' = x$operator
        ,'Date of run' = format(as.Date(x$dateofrun), format = "%Y-%m-%d")
      )
      return(x)
    }else{
      return('No samples')
    }
})
samples.no.run <- reactive({
    strSQL <- 'SELECT *
               FROM   tbl_samples
               WHERE  ngsfacilityid NOT IN (SELECT ngsfacilityid
                                            FROM tbl_run)'
    x <-  dbGetQuery(db, strSQL)
    if(dim(x)[1]!= 0){
      x <- data.frame(
        'NGS Facility ID' = x$ngsfacilityid
       ,'Purpose' = x$diagnostics
       ,'Researcher' = x$researcher
       ,'Researcher ID' = x$researchersampleid
       ,'Timestamp' = format(as.Date(x$timestamp), format = "%Y-%m-%d")
      )
      return(x)
    }else{
      return('No samples')
    }
})

output$downloadReport <- downloadHandler(
    filename = function() {
      paste(paste('NGS-samples-open',gsub("[:]"," ",format(Sys.time())), sep = ' '),
        switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        )
       , sep =".")
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')

      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
