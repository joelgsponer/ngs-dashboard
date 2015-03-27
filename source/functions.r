fncCopyFiles <- function(files, where){
  ifelse(!(is.null(dim(files))), n <- dim(files)[1], n <- 1)
  for(i in seq(1, n)){
    if(!(file.exists(paste(where,files, sep = "/")))){
      file.copy(files[i,4], paste(where,files[i,1], sep = "/"))
    }
  }
}

writeLog <- function(message){
  message <- paste("[",Sys.time(),"]",message,"\n", log.debug, sep = "")
  assign("log.debug",message, envir = .GlobalEnv)
}

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class="icon-question-sign")
    )
  )
}

disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                             list(code= paste("$('#",id,"').prop('disabled',true)"
                                    ,sep="")))
}
enableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                             list(code= paste("$('#",id,"').prop('disabled',false)"
                                    ,sep="")))
} 

updateModifyFields <- function(session_, fields, tables){
  tryCatch({
    r <- function(table){
      strSQL <- sprintf('SELECT ngsfacilityid FROM %s', table)
      x <-  dbGetQuery(db, strSQL)
      return(c(x$ngsfacilityid))
    }
  }, error = function(e){
    return(list(
      message = 'Error while fetching ngsfacilitzid form table'
      ,error = 1
      ,e = e
      ))
    })
  tryCatch({
    for(field in fields){
      updateSelectInput(
         session_
        ,inputId   = field
        ,choices   = c('NEW RECORD',r(table = tables[which(fields == field)]))
        ,selected  = 'NEW RECORD'
      )
    }
  }, error = function(e){
    return(list(
       message = 'Error while updating fields with ngsfacilityid'
      ,error = 1
      ,e = e
    ))
  })
}