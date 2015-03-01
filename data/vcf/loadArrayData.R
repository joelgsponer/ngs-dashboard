fnc_loadArrayData <- function(obj, verbose = F){

# Load arrays

#normalized data
extension <- ".txt.normalized"

i <- 1
for(item in obj){
  print("normalized")
  load(paste(config_OUTPUT, item[['barcode']],extension, sep = ""))
  obj[[i]]$normalizedData <- tbl_df(normalizedData)
  i <- i + 1
}

# segmented data
extension <- ".txt.segmented"

i <- 1
for(item in obj){
  print("Segmented")
  load(paste(config_OUTPUT, item[['barcode']],extension, sep = ""))
  obj[[i]]$segmented <- tbl_df(segmented)
  i <- i + 1
}

#annotation data baed on biomart and ensembl
#extension <- ".txt.annotation"

#i <- 1
#for(item in obj){
#  printline()
#  load(paste(config_OUTPUT, item[['barcode']],extension, sep = ""))
#  obj[[i]]$annotation <- tbl_df(annotation)
#  i <- i + 1
#}

cat("#. get the abberated genes form the database")
g <- tbl(db,"probeAnnotated") #table of all abberated genes in all array, nothing really happens until you collect
print(g)
cat(" - done \n")
i <- 1

for(item in obj){
  g <- tbl(db,"probeAnnotated") #table of all abberated genes in all array, nothing really happens until you collect
  if(verbose)print(g)
  barcode <- paste(obj[[i]]["barcode"], ".txt", sep ="")
  if(verbose)print(barcode)
  g <- filter(g,array == barcode)
  if(verbose)print(g)
  g <- arrange(collect(g), chr, start)
  if(verbose)print(g)
  obj[[i]]$probebased<- g
  i <- i + 1
}
return(obj)

}
