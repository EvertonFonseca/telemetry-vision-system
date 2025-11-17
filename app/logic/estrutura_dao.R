box::use(DBI,dplyr[pull],./utils[...],purrr[...])

#' @export
checkifExistNameEstrutura <- function(con,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM ESTRUTURA WHERE NAME_ESTRUTURA = ?',params = list(name)) |>  pull() > 0
  
} 
#' @export
checkifExistNameEstruturaEdit <- function(con,id,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM ESTRUTURA WHERE NAME_ESTRUTURA = ? AND CD_ID_ESTRUTURA != ?',params = list(name,id)) |>  pull() > 0
  
}
#' @export
checkifExistUrlEstrutura<- function(con,url){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM ESTRUTURA WHERE URL_ESTRUTURA = ?',params = list(url)) |>  pull() > 0
  
}

#' @export
selectAllEstrutura <- function(con){
 estruturas <- DBI$dbGetQuery(con, "SELECT * FROM ESTRUTURA")
 estruturas$CONFIGS <- map(seq_len(nrow(estruturas)),function(i){
    selectConfigByEstrutura(con,estruturas[i,])
  })
  estruturas
}

#' @export
selectAllEstruturaByIdComponente <- function(con,id){
 estruturas <- DBI$dbGetQuery(con,paste0("SELECT * FROM ESTRUTURA WHERE CD_ID_ESTRUTURA = ",id))
 estruturas$CONFIGS <- map(seq_len(nrow(estruturas)),function(i){
    selectConfigByEstrutura(con,estruturas[i,])
  })
  estruturas
}

selectConfigByEstrutura <- function(con,estrutura){
  configs <- DBI$dbGetQuery(con,paste0("SELECT * FROM ESTRUTURA_CONFIG WHERE CD_ID_ESTRUTURA = ",estrutura$CD_ID_ESTRUTURA," ORDER BY DT_HR_LOCAL DESC LIMIT 1"))

  configs$ATRIBUTOS <- map(seq_len(nrow(configs)),function(i){
    selectAllAtributosByEstruturas(con,configs[i,])
  })
  configs
}

selectAllAtributosByEstruturas <- function(con,estrutura_config){

  DBI$dbGetQuery(con,paste0('SELECT 
      a.CD_ID_ATRIBUTO,
      a.CD_ID_ESTRUTURA_CONFIG,
      a.NAME_ATRIBUTO,
      a.VALUE_ATRIBUTO,
      a.FG_ATIVO,
      a.CD_ID_DATA,
      td.NAME_DATA,
      td.R_DATA 
      FROM atributo a
      LEFT JOIN tipo_data  td ON td.CD_ID_DATA = a.CD_ID_DATA
      WHERE a.CD_ID_ESTRUTURA_CONFIG = ',estrutura_config$CD_ID_ESTRUTURA_CONFIG))
}

#' @export
selectAllTipoDados <- function(con){
  DBI$dbGetQuery(con,'SELECT * FROM TIPO_DATA')
}
