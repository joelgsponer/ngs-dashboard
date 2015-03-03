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