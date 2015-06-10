####################
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
      if(mess$error == 0 & mess$result){
              fncResetUI(session = session,db = db,'ui_elements/form_run/', verbose = T)
              page.refresh(session = session)
      }
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


