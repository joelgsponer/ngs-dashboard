fncCreateUI <- function(path,  verbose = F,...){
  form <- unlist(strsplit(path,"/"))[length(unlist(strsplit(path,"/")))]
  
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
                        ,radioButtons + radioButtons(
                           inputId   =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label     =  f(HTML(UIelement.definition$label))
                          ,choices   =  f(UIelement.definition$choices)
                          ,selected  =  f(UIelement.definition$selected)
                          ,inline    =  f(UIelement.definition$inline)
                        )
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


fncUItoDB <- function(path,db,table,dat, primarykey.add = F, primarykey.create = F, primarykey.label=NA,primarykey.val=NA,timestamp = T, update=T,verbose = T){
  require("RJSONIO")
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
    dbInsertInto(db, table, field = primarykey.label, value = primarykey.val, type = "TEXT", verbose = T)
  }, error = function(e){
       cat("!Error while creating primarykey\n")
       cat("primarykey.label:",primarykey.label, "\n")
       cat("primarykey.val:",primarykey.val, "\n")
       cat("Error message:\n")
       print(e)
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
  })
  tryCatch({
    if(timestamp) dbInsertInto(db, table, "timestamp",format(Sys.time()), type = "TEXT", verbose = T, c(primarykey.label, primarykey.val))
  }, error = function(e){
    cat("!Error while adding timestamp\n")
    cat("Error message:\n")
    print(e)
  })
  return(list(
     "primarykey.label" = primarykey.label
    ,"primarykey.val"   = primarykey.val
    ,"message"          = paste("You successfully created a new record.")
    ,"error"            = 0
    ))
}


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
                        ,dateInput = dateInput(
                          session
                          ,inputId  =  paste(form,UIelement.definition$inputId, sep = ".")
                          ,label    = f(HTML(UIelement.definition$label))
                          ,value    = f(UIelement.definition$value)
                        )        
      )
    },error=function(e){return(e)})
  } 
  return(list(
    message = "Form was reset successfully.",
    error = 0
  ))
}

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
