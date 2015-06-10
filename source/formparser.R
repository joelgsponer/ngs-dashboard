#Create a form from a folder with json documents describing the fields
fncCreateUI <- function(path,  verbose = F,...){
  form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]
  
  #function to replace null with NA if needed and evaluate a function that was given in the field description
  f <- function(x){
    if(is.null(x)) x <- NA
    if(grepl("function", x)){
       tmp <- eval(parse(text=x))
       x <- tmp(...)
    } 
    return(x)
  }

  require("RJSONIO")
  UIelements <- list.files(path)
  if(verbose) print("Form definitions:");print(UIelements)
  
  res <- list()
  
  for(i in UIelements){
    tryCatch({
      UIelement.definition <- fromJSON(paste(path,i,sep = ""))
      if(verbose) print("UIelement:");print(UIelement.definition)
      element <- switch(UIelement.definition$type,
                        selectInput   = selectInput( 
                           inputId   = paste(form,UIelement.definition$inputId, sep = ".")
                          ,label     = f(HTML(UIelement.definition$label))
                          ,choices   = f(UIelement.definition$choices)
                          ,selected  = f(UIelement.definition$selected)
                          ,multiple  = f(UIelement.definition$multiple)
                          ,selectize = f(UIelement.definition$selectize)
                          ,width     = f(UIelement.definition$width)
                        )
                        ,numericInput = numericInput(
                           inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                          ,min      = f(UIelement.definition$min)
                          ,max      = f(UIelement.definition$max)
                          ,step     = f(UIelement.definition$step)
                        )
                        ,textInput    = textInput(
                           inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,checkboxInput = checkboxInput(
                           inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,dateInput = dateInput(
                           inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,radioButtons = radioButtons(
                           inputId   =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label     =  f(HTML(UIelement.definition$label))
                          ,choices   =  f(UIelement.definition$choices)
                          ,selected  =  f(UIelement.definition$selected)
                          ,inline    =  f(UIelement.definition$inline)
                        )
                        ,textArea = HTML(sprintf(
                        '<label for="%s">%s</label>
                        <textarea id="%s" rows="%s" cols="%s">%s</textarea>'
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(HTML(UIelement.definition$label))
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(UIelement.definition$rows)
                        ,f(UIelement.definition$cols)
                        ,f(UIelement.definition$value)                       
                        ))
      )
    }, error = function(e){return(e)})
    helpt <- HTML(gsub("NA", "", paste(UIelement.definition$'help text',"<br>", sep = "")))
    res[[length(res)+1]] <- element
    res[[length(res)+1]] <- helpt
    res[[length(res)+1]] <- HTML("<br>")
  } 

  if(verbose)print("Response:");print(res)
  return(res)
}
#Save the Form content to a database
fncUItoDB <- function(path
  ,db
  ,table,dat
  , primarykey.add = F
  , primarykey.create = F
  , primarykey.label=NA
  , primarykey.val=NA
  , timestamp = T
  , check.fields = T
  , update=T
  , verbose = T){
  require("RJSONIO")
  if(check.fields == T){
    check <- fncCheckOptional(path,dat)
    if(check$result == F) return(check)
  }
  form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]
  UIelements <- list.files(path)
  if(verbose) print(UIelements)

  #Primarykey
  if(primarykey.add == F){
  tryCatch({
    if(is.na(primarykey.label) & is.na(primarykey.val) & primarykey.create == F){
     stop("Neither primarykey label nor primarykey value were given. 
           Please provide them or allow for automatic primary key generation (primarykey.create = T)")
    }
    if(is.na(primarykey.label)) primarykey.label <- "primarykey"
    dbAddField(db, table, field = primarykey.label, type = "TEXT")
    if(primarykey.create){
      primarykey.is.unique <- F
      while(primarykey.is.unique == F){
        #create random string
        primarykey.val <-c(randomStrings(n=1, len=10, digits=TRUE, upperalpha=TRUE,loweralpha=TRUE, unique=TRUE, check=TRUE))
        #check if random string is already present
        if(dbMatchingRecord(db, table, field = primarykey.label, value = primarykey.val) == F) primarykey.is.unique <- T
      }
    }
    if(is.na(primarykey.val)) primarykey.val <- dat[[primarykey.label]]
    if(dbMatchingRecord(db, table, field = primarykey.label, value = primarykey.val) == F) dbInsertInto(db, table, field = primarykey.label, value = primarykey.val, type = "TEXT", verbose = T)
  }, error = function(e){
       cat("!Error while creating primarykey\n")
       cat("primarykey.label:",primarykey.label, "\n")
       cat("primarykey.val:",primarykey.val, "\n")
       cat("Error message:\n")
       print(e)
       return(list(
          message ="Error while creating primarykey"
         ,error   = 1
         ,e       = e
         ,result  = FALSE
       ))
  })
  }else{
    if(is.na(primarykey.label) | is.na(primarykey.val)){
      stop("Either primarykey.label or primarykey.val were not provided. The option primarykey.add == T requires both
        to be passed as arguments so the records can be associated with a given record.")
    }
  }

  tryCatch({
    for(i in UIelements){
      UIelement.definition <- fromJSON(paste(path,i,sep = ""))      
      if(verbose) print("UIelements.definition:");print(UIelement.definition)
      inputId <- UIelement.definition$inputId
      if(inputId != primarykey.label){
        dbInsertInto(db, table, inputId,dat[[paste(form,inputId,sep = ".")]], type = "TEXT", verbose = T, c(primarykey.label, primarykey.val))
      }
    }
  }, error = function(e){
       cat("!Error while adding record to the database\n")
       cat("inputId:", UIelement.definition$inputId, "\n")
       cat("Error message:\n")
       print(e)
       return(list(
          message   = "Error while adding record to the database"
         ,UIelement = UIelement.definition$inputId
         ,error     = 1
         ,e         = e
         ,result = FALSE
        ))
  })
  tryCatch({
    if(timestamp) dbInsertInto(db, table, "timestamp",format(Sys.time()), type = "TEXT", verbose = T, c(primarykey.label, primarykey.val))
  }, error = function(e){
    cat("!Error while adding timestamp\n")
    cat("Error message:\n")
    print(e)
    return(list(
       message = "Error while adding timestamp."
      ,error   = 1
      ,e       = e
      ,result = FALSE
    ))
  })
  return(list(
     primarykey.label = primarykey.label
    ,primarykey.val   = primarykey.val
    ,message          = ifelse(update==T,"You successfully updated the record.","You successfully created a new record.")
    ,error            = 0
    ,result = TRUE
  ))
}
#Revert a form to a initial state after
fncResetUI <- function(session,path, verbose = F,...){
  form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]

  #function to replace null with NA if needed
  #function to replace null with NA if needed
  f <- function(x){
    if(is.null(x)) x <- NA
    if(grepl("function", x)){
       tmp <- eval(parse(text=x))
       x <- tmp(...)
    } 
    return(x)
  }

  require("RJSONIO")
  UIelements <- list.files(path)
  if(verbose) print("Form definitions:");print(UIelements)
  
  res <- list()
  
  for(i in UIelements){
    tryCatch({
      UIelement.definition <- fromJSON(paste(path,i,sep = ""))
      if(verbose) print("UIelement:");print(UIelement.definition)
      switch(UIelement.definition$type,
                        selectInput   = updateSelectInput(
                          session
                          ,inputId   = paste(form,UIelement.definition$inputId, sep = ".")
                          ,label     = f(HTML(UIelement.definition$label))
                          ,choices   = f(UIelement.definition$choices)
                          ,selected  = f(UIelement.definition$selected)
                        )
                        ,numericInput = updateNumericInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,textInput    = updateTextInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,checkboxInput = updateCheckboxInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = UIelement.definition$value
                        )
                        ,dateInput = updateDateInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )
                        ,radioButtons = updateRadioButtons(
                           inputId   =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label     =  f(HTML(UIelement.definition$label))
                          ,choices   =  f(UIelement.definition$choices)
                          ,selected  =  f(UIelement.definition$selected)
                          ,inline    =  f(UIelement.definition$inline)
                        )
                        ,textArea = HTML(sprintf(
                        '<label for="%s">%s</label><br>
                        <textarea id="%s" rows="%s" cols="%s">%s</textarea>'
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(HTML(UIelement.definition$label))
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(UIelement.definition$rows)
                        ,f(UIelement.definition$cols)
                        ,f(UIelement.definition$value)                       
                        ))     
      )
    },error=function(e){return(e)})
  } 
  return(list(
    message = "Form was reset successfully.",
    error = 0
  ))
}
#Create a Unique primary key, which can be used to store data in a database
fncCreateUniquePrimarykey <- function(db,table,primarykey.label = "primarykey"){
  require(random)
  primarykey.is.unique <- F
  while(primarykey.is.unique == F){
    #create random string
    primarykey.val <-c(randomStrings(n=1, len=10, digits=TRUE, upperalpha=TRUE,loweralpha=TRUE, unique=TRUE, check=TRUE))
    #check if random string is already present
    if(dbMatchingRecord(db, table, field = primarykey.label, value = primarykey.val) == F) primarykey.is.unique <- T
  }
  return(primarykey.val)
}
#Check if al required filed were filled out
fncCheckOptional <- function(path,dat, verbose =F){
  tryCatch({
    debug.list <- list()
    form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]
    require("RJSONIO")
    UIelements <- list.files(path)
    if(verbose) print("Form definitions:");print(UIelements)
    for (UIelement in UIelements){
      UIelement.definition <- fromJSON(paste(path,UIelement,sep = ""))      
      if(verbose) print("UIelements.definition:");print(UIelement.definition)
      inputId <- UIelement.definition$inputId
      val <- dat[[paste(form,inputId,sep = ".")]]
      debug.list <~ c(debug.list, list(inputId = val))       
      if(UIelement.definition$optional == F){
        if(length(val) == 0 | is.na(val) | is.null(val) | val == ""){
          return(
            list(
               message=sprintf("Not all required fields have been filled out, please check:%s",UIelement.definition$label)
              ,result = FALSE
              ,error = 0
            )
          )
        }
      }else{
          return(
            list(
               message="All nessecary fields have been filled out"
              ,result = TRUE
              ,error = 0
            )
          )
      }
    }
  }, error = function(e){
    return(list(
      message = sprintf("Error while checking required fields %s", as.character(e))
      ,error = 1
      ,result = FALSE
      ,e = e
    ))
  })
}

fncUpdateUI <- function(path
  , db
  , table
  , primarykey.label = primarykey
  , primarykey.value
  , verbose = F
  , session
  ){
  form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]
  r <- dbMatchingRecord(db, table, field = primarykey.label, value = primarykey.value, record.return = T)
  require("RJSONIO")
  UIelements <- list.files(path)
  if(verbose) print("Form definitions:");print(UIelements)
  
  res <- list()
  
  for(i in UIelements){
    tryCatch({
      UIelement.definition <- fromJSON(paste(path,i,sep = ""))
      if(verbose) print("UIelement:");print(UIelement.definition)
      writeLog(paste('Update',paste(form,UIelement.definition$inputId, sep = "."), 'to', r[1,UIelement.definition$inputId]))
      switch(UIelement.definition$type,
                        selectInput   = updateSelectInput(
                          session
                          ,inputId   = paste(form,UIelement.definition$inputId, sep = ".")
                          ,selected  = as.character(r[1,UIelement.definition$inputId])
                        )
                        ,numericInput = updateNumericInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,value    =  as.numeric(r[1,UIelement.definition$inputId])
                        )
                        ,textInput  = updateTextInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,value    =  r[1,UIelement.definition$inputId]
                        )
                        ,checkboxInput = updateCheckboxInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,value    = as.logical(r[,UIelement.definition$inputId])
                        )
                        ,dateInput  =   updateDateInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,value    =  as.Date(r[1,UIelement.definition$inputId])
                        )
                        ,radioButtons = updateRadioButtons(
                           inputId   =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,selected  =  r[,UIelement.definition$inputId]
                        )
                        ,textArea = HTML(sprintf(
                        '<label for="%s">%s</label><br>
                        <textarea id="%s" rows="%s" cols="%s">%s</textarea>'
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(HTML(UIelement.definition$label))
                        ,paste(form,UIelement.definition$inputId, sep = ".")
                        ,f(UIelement.definition$rows)
                        ,f(UIelement.definition$cols)
                        ,r[1,UIelement.definition$inputId]                       
                        ))     
      )
    },error=function(e){return(e)})
  } 
  return(list(
     primarykey.label = primarykey.label
    ,primarykey.val = tryCatch(r[,'primarykey'], error = function(e){return(e)})
    ,message = "Form was updated successfully."
    ,error = 0
  ))
}
