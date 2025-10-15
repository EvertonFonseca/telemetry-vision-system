box::use(DBI,dplyr[pull],./utils[...])

#' @export
checkifExistNameSetor <- function(con,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM SETOR WHERE NAME_SETOR = ?',params = list(name)) |>  pull() > 0
  
} 
#' @export
checkifExistNameSetorEdit <- function(con,id,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM SETOR WHERE NAME_SETOR = ? AND CD_ID_SETOR != ?',params = list(name,id)) |>  pull() > 0
  
}
#' @export
checkifExistUrlSetor <- function(con,url){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM SETOR WHERE URL_SETOR = ?',params = list(url)) |>  pull() > 0
  
}

#' @export
checkifExistUrlSetorEdit <- function(con,id,url){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM SETOR WHERE URL_SETOR = ? AND CD_ID_SETOR != ?',params = list(url,id)) |>  pull() > 0
  
}

#' @export
insertNewSetor <- function(con,id,setor){
  
  .run_tx_bool(con,{
    query <- 'INSERT INTO SETOR (CD_ID_SETOR,NAME_SETOR,TEMPO_REATIVAR_SETOR,TEMPO_REATIVAR_UNIDADE_SETOR,TEMPO_PASSADO_SETOR,TEMPO_PASSADO_UNIDADE_SETOR) VALUES (?,?,?,?,?,?)'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result, c(
      as.integer(id),
      setor$NAME_SETOR,
      setor$TEMPO_REATIVAR_SETOR,
      setor$TEMPO_REATIVAR_UNIDADE_SETOR,
      setor$TEMPO_PASSADO_SETOR,
      setor$TEMPO_PASSADO_UNIDADE_SETOR
    ))
    DBI$dbClearResult(result)
    
  })
  
  return(as.integer(id))
}
#' @export
updateSetor <- function(con,setor){
  
   .run_tx_bool(con,{
    
    query <- 'UPDATE SETOR SET NAME_SETOR = ?,
                              TEMPO_REATIVAR_SETOR = ?,
                              TEMPO_REATIVAR_UNIDADE_SETOR = ?,
                              TEMPO_PASSADO_SETOR = ?,
                              TEMPO_PASSADO_UNIDADE_SETOR = ? 
                              WHERE CD_ID_SETOR = ?'
    result <-  DBI::dbSendStatement(con,query)
    DBI$dbBind(result, c(
      setor$NAME_SETOR,
      setor$TEMPO_REATIVAR_SETOR,
      setor$TEMPO_REATIVAR_UNIDADE_SETOR,
      setor$TEMPO_PASSADO_SETOR,
      setor$TEMPO_PASSADO_UNIDADE_SETOR,
      setor$CD_ID_SETOR
    ))
    DBI$dbClearResult(result)
  })
}

#' @export
selectAllSetors <- function(con){
  
  DBI$dbGetQuery(con,'SELECT * FROM SETOR')

}

#' @export
deleteSetor <- function(con,id){
  DBI::dbExecute(con,paste0('DELETE FROM SETOR WHERE CD_ID_SETOR = ',id))
}