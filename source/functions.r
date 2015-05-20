setClass("individual",
  ,slots = c(
     name       = "character"
    ,chr        = "list"
    ,score      = "numeric"
  )
) 

setMethod("show", "individual", function(object){
  cat("--- Individual ---\n")
  cat("Name : ",object@name,"\n")
  cat("---\n")
  for(i in seq(1,length(object@chr))){
    cat(names(object@chr)[i],": ",object@chr[[i]], "\n")
  }
  cat("---\n")
  cat("Score : ",object@score,"\n")
  cat("---\n")
})

create.individual <- function(
   name = character(0)
  ,chr
  ,score = numeric(0) 
  ,create.random.name = F
){
  newIndividual <- new("individual")
  if(create.random.name){
    require(random)
    name <-c(randomStrings(n=1, len=10, digits=TRUE, upperalpha=TRUE,loweralpha=TRUE, unique=TRUE, check=TRUE))
  }
  newIndividual@name <- name
  newIndividual@chr <- chr
  newIndividual@score <- score  
  return(newIndividual)
}

mutation   <- function(
   chr
  ,mutation.frequency = 0.01
  ,genes.class = list("integer")
  ,genes.range = list(c(0,100)) 
  ,is.silent = F #it T supresses raising of errors, istead return a list with error information.
  ,function.id = 'waRRior.machinelearning.genetic_algorithm.mutation' #Use this to identfy the function in error (or success messages if applicable) messages.
  ,simpleReturn = T
  ,verbose = T #Turn messages on and off
  ,debug = F #Turn debug messages on and off
  ,...
){
  tryCatch({
    if(!(simpleReturn))t <- Sys.time() 
    #Actual Function code
    nr_genes <- length(chr)
    waRRior.snippets.verbose(paste("number of genes:", nr_genes), verbose_ = verbose)
    if(length(genes.class) != length(genes.range)) stop("nucleotides.class and nucleotides.range must have the same length (either 1 or equal to the number of genes)")
    if(length(genes.class) == 1 & length(genes.range) == 1){
       waRRior.snippets.verbose("expanding genes.class and genes genes.range.", verbose_ = verbose)
       genes.class_ <- genes.class
       for(i in seq(2, nr_genes))genes.class <- c(genes.class, genes.class_)
       if(debug)print(genes.class)
       genes.range_ <- genes.range
       for(i in seq(2, nr_genes))genes.range <- c(genes.range, genes.range_)
       if(debug)print(genes.range)
    }
    if((length(genes.class) != 1 | length(genes.range) != 1) & (length(genes.class) != nr_genes | length(genes.range) != nr_genes)) stop("genes.class and genes.range have to be either 1 or eual to to the number of genes")
    is.mutated <- rbinom(nr_genes, 1, mutation.frequency)
    if(debug)print(is.mutated)
    new_chr <- chr
    for(i in seq(1,nr_genes)){
      if(is.mutated[i]){
        new_chr[i] <- sample(
         switch(genes.class[[i]][1],
             integer = seq(genes.range[[i]][1], genes.range[[i]][2], 1)
            ,boolean = c(F,T)
            ,float   = runif(1, genes.range[[i]][1], genes.range[[i]][2])
         )
         ,size = 1
         ,replace = T
        )
        if(debug)print(new_chr[i])
      }
    }
    if(debug)print(new_chr)
    #define response
    if(simpleReturn)return(new_chr)
    response <- list(
       result <- as.list(new_chr)
      ,message <- paste(function.id, 'success')
      ,error = 0
      ,e = NULL
      ,time.elapsed = as.numeric(difftime(Sys.time(), t), units = "secs")
    )
    #Success message
    waRRior.snippets.verbose(paste(function.id,'success'), verbose_ = verbose)
    #Return respond
    return(response)
   #Error handling
   }, error = function(e, silent = is.silent){
     #If silent supress error and return list instead
      if(silent){
       return(list(
          message = sprintf("Error during function: %s", function.id)
         ,error = 1
         ,e = e
       ))
      }else{
        stop(e)
      }
     }
  )
}

crossover <- function(
   #The chromosomes can be passed as a list.
   ...
  ,is.silent = F #it T supresses raising of errors, istead return a list with error information.
  ,function.id = 'waRRior.machinelearning.genetic_algorithm.crossover' #Use this to identfy the function in error (or success messages if applicable) messages.
  ,verbose = T #Turn messages on and off
  ,debug = T #Turn debug messages on and off
){
  tryCatch({
    t <- Sys.time() 
    #Actual Function code
    chrs <- list(...)

    
    nr_of_chrs <- length(chrs)
    waRRior.snippets.verbose(paste('nr of chromosomes:', nr_of_chrs), verbose_ = verbose)

    nr_of_genes <- length(chrs[[1]])
    
    mat <- matrix(unlist(chrs),nr_of_genes, nr_of_chrs)
    colnames(mat) <- names(chrs)
    rownames(mat) <- names(chrs[[1]])
    
    if(verbose){
    	waRRior.snippets.verbose('avaiable chromosomes:', verbose_ = verbose)
    	print(mat)  	
    }
    
    if(nr_of_genes != mean(unlist(lapply(chrs, FUN = length))))stop("chromosomes must have equal length (number of genes)")
    waRRior.snippets.verbose(paste('nr of genes:', nr_of_genes), verbose_ = verbose)
    
    
    new_chr <- apply(mat, 1, FUN = sample, size = 1) 
    if(debug) print(new_chr)
        
    #define response
    response <- list(
       result <- as.list(new_chr)
      ,message <- paste(function.id, 'success')
      ,error = 0
      ,e = NULL
      ,time.elapsed = as.numeric(difftime(Sys.time(), t), units = "secs")
    )
    
    #Success message
    waRRior.snippets.verbose(paste(function.id,'success'), verbose_ = verbose)
    
    #Return respond
    return(response)
  
   #Error handling
   }, error = function(e, silent = is.silent){
     #If silent supress error and return list instead
      if(silent){
       return(list(
          message = sprintf("Error during function: %s", function.id)
         ,error = 1
         ,e = e
       ))
      }else{
        stop(e)
      }
     }
  )
}

evaluate <- function(object, test.data){
  
  #Function code
  x <- seq(1, 100)
  prediction <- x ^ object@chr$a + object@chr$b
  
  score <- sum((test.data - prediction)^2)
  #Retrun Score
  return(score)
}

x <- seq(1,100)
test.data <- x ^ 3 + 4

#Parameters
population.size <- 10
chr.init <- list(
   a = 1
  ,b = 2
)

#Initalization
population <- list()

for(i in seq(1, population.size)){
  population <- c(population, create.individual(
     chr = mutation(chr.init
       ,mutation.frequency = 1
       ,verbose = F
       ) 
    ,create.random.name=T
    )
  )  
}

for(i in seq(1, population.size)){
  s <- evaluate(population[[i]], test.data)
  population[[i]]@score <- s
}



#Run








