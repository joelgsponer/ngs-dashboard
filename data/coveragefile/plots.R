# Plot the different arrays
f_genes <- function(input, bool_filter = T, alpha = 0.5, cex = 0.8){
  
  if(bool_filter){
    fnc_filter <- function(x){
      x <- filter(x, mean_log2ratio > param_logratios_ubound | mean_log2ratio < lbound, chr < 24 )
    }
  }else { fnc_filter <- function(x) return(x)}
  
  samples <- paste(returnListValueByName(input, "year"), returnListValueByName(input, "ploidy"))
  print(samples)
  cols <- makeTransparent(rainbow(length(samples)), alpha = alpha)
  sample <- input[[1]]$probebased
  plot(fnc_filter(sample)$start,fnc_filter(sample)$mean_segment
       , cex = cex
       , pch = NA
       , ylim = c(-4,4)
       , xlab = "gene index"
       , ylab = "log2ratio"
  )
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "black")
  tryCatch(
    for(i in seq(1, length(samples))){
      sample <- input[[i]]$probebased
      points(fnc_filter(sample)$start,fnc_filter(sample)$mean_segment
             , cex = cex
             , pch = 20
             , ylim = c(-4,4)
             , xlab = "gene index"
             , ylab = "log2ratio"
             , col = cols[i]
      )
    }, error = function(e){print(e)})
  legend(0,4,samples, fill = cols, bg = "white")
  rect(-100000, lbound, 10000000, param_logratios_ubound,col = makeTransparent("grey", 1), border = NA)
  
}
