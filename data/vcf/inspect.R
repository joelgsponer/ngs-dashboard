fnc_inspect <- function(x, p = T, w = 500000, movingAverageSize=10){
  load(paste(config_DATABASE, 'dbagilentprobes.Rdata', sep = ""))
  annot <- tbl_df(agilentprobes)
  annot <- filter(annot, GeneSymbol == x)
  annot <- util_parseGenomicLocation(annot$TargetID)
  
  par(lwd = 3)
  info <- g[g$GeneSymbol == x,]
  ubound <- max(annot$end, na.rm = T) + w
  lbound <- min(annot$start, na.rm = T) - w
  glimpse(info)
  printline()
  plot(1,1,pch = NA, xlim = c(lbound, ubound), ylim = c(-4,4), ylab = "log2ratio", main = paste(mean(annot$chr),":",lbound,"-", ubound, sep = ""), xlab = "position")
  abline(h = 0)
  
  rect(min(annot$start), 3.6, max(annot$end), 3.4, lwd = 5, col = "grey")
  text(mean(c(annot$start,annot$end)), 4, x, adj =0.5)
  j <- 1
  for(item in obj){
    probes <- item$normalizedData
    segments <- 
    names(probes) <- gsub("loc.","", names(probes))
    probes <- filter(probes, chr == annot$chr[1], start >= lbound, end <= ubound)
    vals <- probes$log2ratio
    vals[is.nan(vals)] <- 0
    points(probes$start,vals, pch = 20, col = util_makeTransparent(style$colors[j], alpha = 0.5))
    points(probes$start,rollmean(vals,movingAverageSize, fill = NA) ,type = "l", pch = 20, col = style$colors[j], lwd = 3)
    j = j + 1
  }
}

#fnc_inspect("PTEN")

