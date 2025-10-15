box::use(DBI,dplyr[pull],./utils[...])

#' @export
selelectPlotDao <- function(con,setor){
  
  DBI::dbGetQuery(con,paste0("SELECT 
                            p.CD_ID_PLOT,
                            p.TITLE_PLOT,
                            p.TEXT_X_PLOT,
                            p.TEXT_Y_PLOT,
                            p.TEXT_LEGEND_PLOT,
                            p.WIDTH_PLOT,
                            p.ORDEM_PLOT,
                            p.FLAG_ACTIVE,
                            p.CD_ID_SETOR,
                            p.TIPO_Y_DATA,
                            p.TIPO_X_DATA,
                            tp.CD_ID_TIPO,
                            tp.NAME_TIPO
                            FROM
                            plot p INNER JOIN tipo_plot tp ON tp.CD_ID_TIPO = p.CD_ID_TIPO
                            WHERE  p.CD_ID_SETOR = ? ORDER BY ORDEM_PLOT"),params = setor$CD_ID_SETOR)
  
}

#' @export
selelectAllPlots <- function(con){
  
  DBI::dbGetQuery(con,paste0("SELECT
                              p.CD_ID_PLOT,
                              p.TITLE_PLOT,
                              p.TEXT_X_PLOT,
                              p.TEXT_Y_PLOT,
                              p.TEXT_LEGEND_PLOT,
                              p.WIDTH_PLOT,
                              p.ORDEM_PLOT,
                              p.FLAG_ACTIVE,
                              p.CD_ID_SETOR,
                              p.TIPO_Y_DATA,
                              p.TIPO_X_DATA,
                              tp.CD_ID_TIPO,
                              tp.NAME_TIPO
                              FROM
                              plot p INNER JOIN tipo_plot tp ON tp.CD_ID_TIPO = p.CD_ID_TIPO
                              ORDER BY p.TITLE_PLOT"))
  
}

#' @export
selectTypesPlots <- function(con){
  DBI::dbGetQuery(con,'SELECT * FROM TIPO_PLOT')
}

selectToolPlotDao <- function(con,plot){
  
  DBI::dbGetQuery(con,"SELECT * 
                       FROM plot_tools_items pti
                       INNER JOIN plot_tools_parametro ptp ON ptp.CD_ID_PTP = pti.CD_ID_PTP
                       WHERE pti.CD_ID_TIPO = ?",params = plot$CD_ID_TIPO)
  
}

#' @export
selelectDataplots <- function(con,plot){
  
  DBI::dbGetQuery(con,paste0("SELECT 
                            *
                            FROM
                            DATAPLOT WHERE CD_ID_PLOT = ?"),params = plot$CD_ID_PLOT)
  
}

#' @export
selectToolPlotDataDao <- function(con,plot,is.active = F){
  
  if(!is.active)
     DBI::dbGetQuery(con,"SELECT *
                          FROM plot_tools pt
                          INNER JOIN plot_tools_items pti ON pti.CD_ID_PTP = pt.CD_ID_PLOT AND pti.CD_ID_TIPO = pt.CD_ID_PLOT
                          INNER JOIN plot_tools_parametro ptp ON ptp.CD_ID_PTP = pti.CD_ID_PTP
                          WHERE pt.CD_ID_PLOT =  ?",params = plot$CD_ID_PLOT)
  else{
    DBI::dbGetQuery(con,"SELECT *
                         FROM plot_tools pt
                         INNER JOIN plot_tools_items pti ON pti.CD_ID_PTP = pt.CD_ID_PLOT AND pti.CD_ID_TIPO = pt.CD_ID_PLOT
                         INNER JOIN plot_tools_parametro ptp ON ptp.CD_ID_PTP = pti.CD_ID_PTP
                         WHERE pt.CD_ID_PLOT = ? AND FLAG_ACTIVE_PT = 1",params = plot$CD_ID_PLOT)
  }
}

#' @export
selectPlotDefaultDao <- function(con,setores){
   
  DBI::dbGetQuery(con,'SELECT * FROM PLOT WHERE FLAG_ACTIVE = 1 AND CD_ID_SETOR = ? ORDER BY ORDEM_PLOT',params = list(setores))
  
}

#' @export
selelectPlotOnlyDao <- function(con,cd_id_plot){
  
  DBI::dbGetQuery(con,paste0("SELECT
                           p.CD_ID_PLOT,
                           p.TITLE_PLOT,
                           p.TEXT_X_PLOT,
                           p.TEXT_Y_PLOT,
                           p.TEXT_LEGEND_PLOT,
                           p.FLAG_TIMELINE,
                           p.FLAG_ACTIVE,
                           p.CD_ID_SETOR,
                           p.WIDTH_PLOT,
                           tp.CD_ID_TIPO,
                           tp.NAME_TIPO,
                           tp.EIXO_Y_TIPO,
                           tp.FLAG_TIPO
                           FROM
                           plot p INNER JOIN tipo_plot tp ON tp.CD_ID_TIPO = p.CD_ID_TIPO
                           WHERE  p.CD_ID_PLOT = ?"),params = list(cd_id_plot))
  
}

#' @export
isPlotAlreadyExist <- function(con,plot,setor){
  
    DBI::dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM PLOT WHERE TITLE_PLOT = ? AND CD_ID_SETOR = ?',params = list(plot,setor)) %>% pull() > 0
}

#' @export
isPlotAlreadyExistUpdate <- function(con,plot,id,setor){
   
  DBI::dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM PLOT WHERE TITLE_PLOT = ? AND CD_ID_SETOR = ? AND CD_ID_PLOT != ?',params = list(plot,setor,id)) %>% pull() > 0
}

#' @export
selectDataplotsDao <- function(con,plot){
  
    DBI::dbGetQuery(con,'SELECT * FROM DATAPLOT WHERE CD_ID_PLOT = ?',params = plot$CD_ID_PLOT)
}

#' @export
#Essa funcao buscas todos os graficos relacionando com setor
#@param Setor
readAllPlotsOfSetor <- function(con,setor){
  
  #source('control/Plot_DAO.R',encoding = 'UTF-8',local = TRUE)
  
  plots <- matrixToModalEncapsolado(DBI::dbGetQuery(con,paste0("SELECT 
                                                  p.CD_ID_PLOT,
                                                  p.TITLE_PLOT,
                                                  p.TEXT_X_PLOT,
                                                  p.TEXT_Y_PLOT,
                                                  p.TEXT_LEGEND_PLOT,
                                                  p.WIDTH_PLOT,
                                                  p.ORDEM_PLOT,
                                                  p.FLAG_ACTIVE,
                                                  p.CD_ID_SETOR,
                                                  p.TIPO_Y_DATA,
                                                  p.TIPO_X_DATA,
                                                  tp.CD_ID_TIPO,
                                                  tp.NAME_TIPO
                                                  FROM
                                                  plot p INNER JOIN tipo_plot tp ON tp.CD_ID_TIPO = p.CD_ID_TIPO
                                                  WHERE p.FLAG_ACTIVE = TRUE AND p.CD_ID_SETOR = ? ORDER BY p.ORDEM_PLOT"),params = list(setor$CD_ID_SETOR)))
  
  #busca as ferramentas de cada plot
  for(i in seq(plots)){
    
    plot       <- plots[[i]]
    plots[[i]]$dataplots <- selelectDataplots(con,plot)
    plots[[i]]$tools     <- selectToolPlotDataDao(con,plot,is.active = TRUE)
  }
  
  return(plots)
  
}

#' @export
readAllDataframePlotsOfSetor <- function(con,setor){
  
  #source('control/Plot_DAO.R',encoding = 'UTF-8',local = TRUE)
  
  DBI::dbGetQuery(con,paste0("SELECT 
                              p.CD_ID_PLOT,
                              p.TITLE_PLOT,
                              p.TEXT_X_PLOT,
                              p.TEXT_Y_PLOT,
                              p.TEXT_LEGEND_PLOT,
                              p.WIDTH_PLOT,
                              p.ORDEM_PLOT,
                              p.FLAG_ACTIVE,
                              p.CD_ID_SETOR,
                              p.TIPO_Y_DATA,
                              p.TIPO_X_DATA,
                              tp.CD_ID_TIPO,
                              tp.NAME_TIPO
                              FROM
                              plot p INNER JOIN tipo_plot tp ON tp.CD_ID_TIPO = p.CD_ID_TIPO
                              WHERE p.FLAG_ACTIVE = TRUE AND p.CD_ID_SETOR = ? ORDER BY p.ORDEM_PLOT"),params = list(setor$CD_ID_SETOR))
  
}