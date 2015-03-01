fnc_inspectChr <- function(x, p = T, w = 1000, movingAverageSize=200){
  par(lwd = 3)
  x <- filter(obj[[1]]$normalizedData,chr == x)
  x <- arrange(x, start)
  print(x)
  ubound <- max(x$end) + w
  lbound <- min(x$start) - w
  printline()
  plot(1,1,pch = NA, xlim = c(lbound, ubound), ylim = c(-4,4), ylab = "log2ratio", main = paste("Chr:",x$chr[1]), xlab = "position")
  abline(h = 0)

  j <- 1
  for(item in obj){
    probes <- item$normalizedData
    names(probes) <- gsub("loc.","", names(probes))
    probes <- filter(probes, chr == x$chr[1], start >= lbound, end <= ubound)
    points(probes$start,probes$log2ratio, pch = 16, col = util_makeTransparent(style$colors[j], alpha = 0.5), cex = 0.1)

    j = j + 1
  }
  j <- 1
  for(item in obj){
    probes <- item$normalizedData
    names(probes) <- gsub("loc.","", names(probes))
    probes <- filter(probes, chr == x$chr[1], start >= lbound, end <= ubound)
    vals <- probes$log2ratio
    vals[is.nan(vals)] <- 0
    points(probes$start,rollmean(vals,movingAverageSize, fill = NA) ,type = "l", col = util_makeTransparent(style$colors[j], alpha = 0.8), lwd = 3)
    j = j + 1
  }
}