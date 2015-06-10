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
      if(mess$error == 0 & mess$result){
        fncResetUI(session = session,'ui_elements/form_sample/', verbose = T)
        page.refresh(session = session)
      }
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
#
