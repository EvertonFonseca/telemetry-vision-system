box::use(RMariaDB,DBI,shiny[showNotification],shinyjs,global = ../view/global)

#' @export
newConnection <- function(){
  
  con <- NULL
  try({
    con <-
      DBI$dbConnect(
        RMariaDB$MariaDB(),
        dbname = 'system',
        username = 'root',
        password = 'ssbwarcq',
        host = '127.0.0.1',
        port = 3306
      )
    
  })
  return(con)
}

#' @export
closerDatabase <- function(con){
  DBI$dbDisconnect(con)
  return(invisible(NULL))
}

#' @export
tryResetConnection <- function(callback,con = newConnection(),is.debug = F,auto.remove =F,auto.close = TRUE) {
  
  fu <- function() {
    
    showNotification("Tentando reconectar com base de dados, aguarde um momento ...", type = "warning")
    
    shinyjs$delay(5000, {
      
      con <- newConnection()
      
      if (DBI$dbIsValid(con)) {
        
        if(is.debug)
          debug(callback)
        
         if(auto.remove)
           global$removeProgressLoader(0)

        callback(con)
      } else{
        fu()
      }})
  }
  
  if(!is.null(con))
  {
    if(!DBI$dbIsValid(con))
    {
      global$newProgressLoader()
      fu()
    }
    else{
      
      if(is.debug)
        debug(callback)
      
       if(auto.remove)
        global$removeProgressLoader(0)
      
      callback(con)
      if(auto.close){
        DBI$dbDisconnect(con)
      }
      return(invisible(NULL))
    }
  }
  else{
    global$newProgressLoader()
    fu()
  }
}

#' @export
nextSequenciaID <- function(con,table,schema = 'system'){
  
  query <- paste0("SELECT AUTO_INCREMENT AS ID FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = ? AND TABLE_SCHEMA = ?")
  return(as.integer(DBI$dbGetQuery(con,query,params = list(table,schema))$ID))
}
#' @export
insertTable <- function(con,table,obj){
  
  tryCatch({
    
    colunas <- names(obj)
    query   <- paste0("INSERT INTO ",table," (")
    values  <- "VALUES ("
    datas   <- list()
    for (i in 1:length(colunas)) {
      
      coluna <- colunas[i]
      query  <- paste0(query,coluna)
      value  <- '?'
      datas[[i]]  <- obj[[coluna]]
      values <- paste0(values,value)
      
      if(i < length(colunas) )
      {
        query   <- paste0(query,",")
        values  <- paste0(values,",")
      }
      
    }
    query <- paste0(query,") ",values,")")
    
    #Inseri dados dinamico
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result,unlist(datas))
    DBI$dbClearResult(result) 
    
  },error = function(e) {print(e)})
}

#' @export
updateTable <- function(con,table,where = '',obj){
  
  tryCatch({
    
    colunas <- names(obj)
    query   <- paste0("UPDATE ",table," SET ")
    datas   <- list()
    for (i in 1:length(colunas)) {
      
      coluna <- colunas[i]
      query  <- paste0(query,coluna,' = ?')
      datas[[i]]  <- obj[[coluna]]
      
      if(i < length(colunas) )
      {
        query   <- paste0(query,",")
      }
      
    }
    
    query <- paste0(query,ifelse(stringi$stri_isempty(where),'',paste0(' WHERE ',where)))
    
    #Inseri dados dinamico
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result,unlist(datas))
    DBI$dbClearResult(result)
    
  },error = function(e) {print(e)})
}
#' @export
deleteTable <- function(con,table,where = ''){
  
  tryCatch({
    
    query   <- paste0("DELETE FROM ",table," ")
    query <- paste0(query,ifelse(stringi$stri_isempty(where),'',paste0('WHERE ',where)))
    DBI$dbExecute(con,query)
    
  },error = function(e) {print(e)})
}

#' @export
tryTransation <- function(con,expr){
  
  DBI$dbBegin(con)
  
  tryCatch({
    
    expr()
    DBI$dbCommit(con)
    return(TRUE)
  },error = function(e){
    print(e)
    DBI$dbRollback(con)
    return(FALSE)
  })

}

#' @export
formatDataType <- function(data,format){
  
  switch (toupper(format),
          'CHARACTER'                   = as.character(data),
          'CHAR'                        = as.character(data),
          'VARCHAR'                     = as.character(data),
          'VARCHAR2'                    = as.character(data),
          'TEXT'                        = as.character(data),
          'BOOLEAN'                     = as.integer(stringr$str_replace_all(data,',','.')),
          'BOOL'                        = as.integer(stringr$str_replace_all(data,',','.')),
          'SMALLINT'                    = as.integer(stringr$str_replace_all(data,',','.')),
          'INTEGER'                     = as.integer(stringr$str_replace_all(data,',','.')),
          'INT'                         = as.integer(stringr$str_replace_all(data,',','.')),
          'DECIMAL'                     = as.numeric(stringr$str_replace_all(data,',','.')),
          'DEC'                         = as.numeric(stringr$str_replace_all(data,',','.')),
          'NUMERIC'                     = as.numeric(stringr$str_replace_all(data,',','.')),
          'NUMBER'                      = as.numeric(stringr$str_replace_all(data,',','.')),
          'REAL'                        = as.numeric(stringr$str_replace_all(data,',','.')),
          'FLOAT'                       = as.numeric(stringr$str_replace_all(data,',','.')),
          'DOUBLE PRECISION'            = as.numeric(stringr$str_replace_all(data,',','.')),
          'DATE'                        = as.POSIXct(data),
          'TIME'                        = as.POSIXct(data),
          'TIMESTAMP'                   = as.POSIXct(data),
          'CLOB'                        = as.character(data),
          'CHARACTER LARGE OBJECT'      = as.character(data),
          'BLOB'                        = as.character(data),
          'BINARY LARGE OBJECT'         = as.character(data),
          {
            data
          }
  )
}