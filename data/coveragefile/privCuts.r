                                                   
privCuts <- function(strataValue){
    cutoffs <- c()
    strataValue <- sort(strataValue)
    for(i in seq(1,length(strataValue))){
	   cutoffs <- c(cutoffs, (strataValue[i]+strataValue[i+1])/2)
    }
	return(cutoffs)
}