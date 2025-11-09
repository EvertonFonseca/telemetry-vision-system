box::use(DBI,dplyr[pull,mutate],purrr[...],./camera_dao[selectCameraByComponente],./estrutura_dao[selectAllEstruturaByIdComponente],jsonlite)

#' @export
checkifExistNameObjeto <- function(con,name){
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM OBJETO WHERE NAME_OBJETO = ?',params = list(name)) |>  pull() > 0
} 

#' @export
checkifExistNameComponente <- function(con,name){
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM COMPONENTE WHERE NAME_COMPONENTE = ?',params = list(name)) |>  pull() > 0
}

#' @export
checkifExistNameObjetoEdit <- function(con,id,name){
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM OBJETO WHERE NAME_OBJETO = ? AND CD_ID_OBJETO != ?',params = list(name,id)) |>  pull() > 0
}


#' @export
insertNewObjeto <- function(con,id,objeto){

    query <- 'INSERT INTO OBJETO (CD_ID_OBJETO,NAME_OBJETO,FG_ATIVO,CD_ID_SETOR,CD_ID_OBJETO_TIPO,TIMELINE_CONTEXT_SEC) VALUES (?,?,?,?,?,?)'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result, c(
      as.integer(id),
      objeto$NAME_OBJETO,
      objeto$FG_ATIVO,
      objeto$CD_ID_SETOR,
      objeto$CD_ID_OBJETO_TIPO,
      objeto$TIMELINE_CONTEXT_SEC
    ))
    DBI$dbClearResult(result)

  return(as.integer(id))
}

#' @export
insertNewObjetoConfig <- function(con,id,objeto){

    query <- 'INSERT INTO OBJETO_CONFIG (CD_ID_OBJ_CONF,CD_ID_OBJETO) VALUES (?,?)'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result, c( 
      as.integer(id),
      objeto$CD_ID_OBJETO
    ))
    DBI$dbClearResult(result)

  return(as.integer(id))
}


#' @export
insertNewComponente <- function(con,id,objeto){

    query  <- 'INSERT INTO COMPONENTE (CD_ID_COMPONENTE,NAME_COMPONENTE,POLIGNO_COMPONENTE,CD_ID_OBJ_CONF,CD_ID_CAMERA) VALUES (?,?,?,?,?)'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result, c(
      as.integer(id),
      objeto$NAME_COMPONENTE,
      objeto$POLIGNO_COMPONENTE,
      objeto$CD_ID_OBJ_CONF,
      objeto$CD_ID_CAMERA
    ))
    DBI$dbClearResult(result)
}

#' @export
insertNewAtributo <- function(con,id,objeto){

    query  <- 'INSERT INTO ATRIBUTO (CD_ID_ATRIBUTO,NAME_ATRIBUTO,CLASSE_ATRIBUTO,FG_ATIVO,CD_ID_COMPONENTE,CD_ID_DATA) VALUES (?,?,?,?,?,?)'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result, c(
      as.integer(id),
      objeto$NAME_ATRIBUTO,
      objeto$CLASSE_ATRIBUTO,
      objeto$FG_ATIVO,
      objeto$CD_ID_COMPONENTE,
      objeto$CD_ID_DATA
    ))
    DBI$dbClearResult(result)
}

#' @export
updateObjeto <- function(con,obj){

    query <- 'UPDATE OBJETO SET NAME_OBJETO = ?,
                               CD_ID_SETOR = ?,
                               FG_ATIVO= ?
                               WHERE CD_ID_OBJETO = ?'
    result <-  DBI$dbSendStatement(con,query)
    DBI$dbBind(result,c(
      obj$NAME_OBJETO,
      obj$CD_ID_SETOR,
      obj$FG_ATIVO,
      obj$CD_ID_OBJETO
    ))
    DBI$dbClearResult(result)

}

#' @export
selectAllObjetos <- function(con,fg.ativo = c(TRUE,FALSE)){
  objetos <- DBI$dbGetQuery(con,paste0('SELECT
              o.*,
              s.NAME_SETOR,
              op.NAME_OBJETO_TIPO
              FROM OBJETO o 
              LEFT JOIN SETOR s ON s.CD_ID_SETOR = o.CD_ID_SETOR
              LEFT JOIN OBJETO_TIPO op ON op.CD_ID_OBJETO_TIPO = o.CD_ID_OBJETO_TIPO
              WHERE o.FG_ATIVO IN (',paste0(as.integer(fg.ativo),collapse = ","),')'))
  
  objetos$CONFIG <- map(seq_len(nrow(objetos)),function(i){
    selectObjetoConfig(con,objetos[i,])
  })
  objetos
}

selectObjetoConfig <- function(con,obj){
  # pega ultimo registro mais recente
  configs <- DBI$dbGetQuery(con,paste0('SELECT * FROM OBJETO_CONFIG 
                                       WHERE CD_ID_OBJETO = ',obj$CD_ID_OBJETO,
                                      " ORDER BY DT_HR_LOCAL DESC LIMIT 1"))
  
  configs$COMPONENTES <- map(seq_len(nrow(configs)),function(i){
    selectAllComponentesByObjeto(con,configs[i,])
  })
  configs
}

selectAllComponentesByObjeto <- function(con,config){

  componentes <- DBI$dbGetQuery(con,paste0('SELECT * FROM COMPONENTE WHERE CD_ID_OBJ_CONF = ',config$CD_ID_OBJ_CONF)) |> 
                 mutate(POLIGNO_COMPONENTE = map(POLIGNO_COMPONENTE,~ jsonlite$fromJSON(.x)))
  
  componentes$ESTRUTURA <- map(seq_len(nrow(componentes)),function(i){
    selectAllEstruturaByIdComponente(con,componentes$CD_ID_ESTRUTURA[i])
  })
  componentes$CAMERAS   <- map(seq_len(nrow(componentes)),function(i){
     selectCameraByComponente(con,componentes[i,])
  })
  componentes
}

#' @export
deleteObjeto <- function(con,id){
  DBI::dbExecute(con,paste0('DELETE FROM OBJETO WHERE CD_ID_OBJETO = ',id))
}

#' @export
selectTipoObjeto <- function(con){
    DBI$dbGetQuery(con,'SELECT * FROM OBJETO_TIPO')
}
