fnc_changesInAberations <- function(obj, g, sequence, dat = "mean_segment"){
  
  for(i in seq(1,length(sequence)-1)){
    cat("# ", obj[[sequence[i]]]$id,"vs",obj[[sequence[i+1]]]$id, "\n")
    v <- paste("delta|",sequence[i],"vs",sequence[i+1], sep = "")
    g[v] <- (g[,paste(obj[[sequence[i]]]$id, dat, sep = "|")] - g[,paste(obj[[sequence[i+1]]]$id, dat, sep = "|")])^2
  }
  
  v <- c()
  for(i in seq(1,length(sequence)-1)){
    v <- c(v,paste("delta|",sequence[i],"vs",sequence[i+1], sep = ""))
    g["maxdelta"] <- apply(g[v], 1, FUN = max, na.rm = T)
  }
  print(v)
  g <- arrange(g, maxdelta)
  return(g)
}