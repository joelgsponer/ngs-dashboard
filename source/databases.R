#This boolean functions checks if the field is already present in the designated database and field     
dbCheckFields <- function(db, table, field){
    Fields <- dbListFields(db, table)
    if(field %in% Fields){
        return(T)
    }else{return(F)}    
}

#This function adds a field to the designated database and field if it does not yet exist (depends on dbCheckFields)
dbAddField  <- function(db, table, field, type, verbose = F){
    tryCatch({
      if(!dbCheckFields(db, table, field)){
        dbSendQuery(conn = db,paste("ALTER TABLE", table, "ADD", field, type))
      } else{if(verbose)cat("#!Field already exists.\n")}
    }, error = function(e){
         cat("!Error while adding field to table\n")
         cat("Field:", field, "\n")
         cat("Error message:\n")
         print(e)
         return(FALSE)
    })
    return(TRUE)
}

#Wrapper to insert data from a matrix into  database table
dbInsertMatrix <- function(mat, db, table,  avoidDublicates = T, addField = T, type = "TEXT", primaryKeyPresent = T){
    
    numberRecords   <- dim(mat)[1]
    numberValues    <- dim(mat)[2]
    dbFields        <- dbListFields(db, table)
    recordFields    <- colnames(mat)
    for(field in recordFields){
        if(addField == T) dbAddField(db, table, field, type = type )
        }
    dbFields        <- dbListFields(db, table)
    
    for(record in seq(1, numberRecords)){
        recordString  <- paste("INSERT INTO ", table)
        
        columns <- paste("'", as.character(colnames(mat)[1]), "'", sep = "")
        for(column in seq(2, numberValues, 1)) columns <- paste(columns, paste("'", colnames(mat)[column], "'", sep = ""), sep = ",")
        
        recordString <- paste(recordString,"(", columns, ")")
        
        recordString <-  paste(recordString, " VALUES (", sep = "")
        
        values <- paste( "'",as.character(mat[1,record]), "'", sep = "")
        for(value in seq(2, numberValues)) values <- paste(values, paste("'",mat[record, value],"'", sep = ""), sep = ",")
        recordString <- paste(recordString, values, sep = "")
        recordString  <- paste(recordString,")", sep = "")
        dbSendQuery(db,
            recordString
        )
    }
}

#Delete a record by value in a field
dbDeleteRecord <- function(db, table, field, value, operator = "="){
    dbBeginTransaction(db)
    rs <- dbSendQuery(db, paste("DELETE from", table,"WHERE",field,operator,value))
    dbClearResult(rs)
    dbCommit(db)
}

dbInsertInto<- function(db, table, field, value, type = "TEXT", verbose = F, where = NA){
  #Add field to table if it doesn't exist
  cat("#Trying to add field:", field, "\n")
  fieldadded <- dbAddField(db, table, field, type, verbose = verbose)
  if(verbose) print(fieldadded)
  
  #Insert Values into table
  tryCatch({
    if((is.na(where[1]))){
      strSQL <- paste("INSERT INTO ", table," (", field, ") VALUES ('", value,"')", sep = "")
      if(verbose) print(strSQL)   
      dbSendQuery(db, strSQL)
    }else{
      strSQL <- paste("UPDATE ", table, " SET ", field, "='", value, "' WHERE ", where[1],"='", where[2],"'", sep = "")
      if(verbose)print(strSQL)
      dbSendQuery(db, strSQL)  
    }
  }, error = function(e){
       cat("!Error while inserting values into table\n")
       cat("Table:",table,"\n")
       cat("Field:",field,"\n")
       cat("Value:",value,"\n")
       cat("Error message:")
       print(e)
  })
}


#check if record is valid
dbMatchingRecord <- function(db, table, value, field, verbose = F){
    strSQL <- paste("SELECT * from ", table, "WHERE ", field, "= ",paste("'", value, "'", sep = ""))
    if(verbose) print(strSQL)
    matchingRecords <- dbGetQuery(db, strSQL)
    if(dim(matchingRecords)[1] > 0) return(T)
    else return(F)
}


dbGetRecords    <- function(db,table){
  r <- tryCatch({
    strSQL <- paste("SELECT * from",table)
    dbGetQuery(db, strSQL)
  }, error = function(e){
       cat("!Error while fetching records from database\n")
       #cat("SQL:", strSQL,"\n")
       cat("Error message:\n")
       print(e)
       return(list(
           message = "!Error while fetching records from database"
          #,SQL     = strSQL
          ,error = 1
        ))
  })
  return(r)
}