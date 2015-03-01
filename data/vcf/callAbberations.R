fnc_callAberationInMin1Array <- function(obj,g,dat="mean_segment", ubound = param_logratios_ubound,lbound = param_logratios_lbound, SD=2){
  
  samples <- util_returnListValueByName(obj, "id")

  for(i in samples){
    cat("# Processing",paste("\n",i,dat,sep="|"), "\n")
    
    v1 <- paste(i,"amp",sep="|")
    v2 <- paste(i,"del",sep="|")

    g[v1] <- g[,paste(i,dat,sep="|")] > ubound + SD * g[,gsub("mean","sd",paste(i,dat,sep="|"))]
    print("Amplifications:")
    print(table(g[v1]))
    g[v2] <- g[,paste(i,dat,sep="|")] < lbound - SD * g[,gsub("mean","sd",paste(i,dat,sep="|"))]
    print("Deletions:")
    print(table(g[v2]))
  }
  
  g$mean_nprobes <- apply(g[,paste(samples,"nprobes",sep="|")],1, FUN = mean, na.rm = T)
  g$anyAmp <- apply(g[,paste(samples,"amp",sep="|")],1, FUN = max, na.rm = T)
  g$anyDel <- apply(g[,paste(samples,"del",sep="|")],1, FUN = max, na.rm = T)
  g$any <- apply(g[,c("anyAmp","anyDel")],1, FUN = max, na.rm = T)
  
  
  g <- filter(g, any == 1)
  print(dim(g))
  return(g)
}