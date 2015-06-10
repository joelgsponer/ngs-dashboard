##########
#Tab help#
##########
currentLog <- reactive(log.debug)
output$log.verbose <- renderPrint({
  if(log.verbose == T){
    cat(currentLog())
  }
})

