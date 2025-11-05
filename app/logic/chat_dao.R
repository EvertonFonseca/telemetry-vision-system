box::use(
  DBI,
  RMariaDB,
  dplyr[...],
  jsonlite,
  httr2,
  lubridate,
  ggplot2,
  tidyr,
  dbp  = ../infra/db_pool
)

`%||%` <- function(x, y) if (is.null(x)) y else x

OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY")
DEFAULT_N_REGISTROS <- 250L
DEFAULT_MODEL       <- "gpt-5-chat-latest"

# =========================================================
# Constrói versão longa dos dados após expandir DATA_OC
# (sem 'match' — pivot_longer já preserva colunas base)
# =========================================================
build_df_ctx_long <- function(df_ctx) {
  if (!nrow(df_ctx)) return(data.frame())
  
  base_cols <- c("DATA_OC", "DT_HR_LOCAL", "NAME_OBJETO", "NAME_SETOR")
  comp_cols <- setdiff(names(df_ctx), base_cols)
  if (!length(comp_cols)) return(data.frame())
  
  df_long <- tryCatch({
    tidyr::pivot_longer(
      df_ctx,
      cols = dplyr::all_of(comp_cols),
      names_to = "colname",
      values_to = "VALOR"
    )
  }, error = function(e) data.frame())
  
  if (!nrow(df_long)) return(df_long)
  
  df_long |>
    dplyr::mutate(
      colname    = toupper(.data$colname),
      COMPONENTE = sub("_.*$", "", .data$colname),
      ATRIBUTO   = sub("^[^_]+_", "", .data$colname),
      ATRIBUTO   = dplyr::if_else(.data$ATRIBUTO == .data$colname, "VALOR", .data$ATRIBUTO)
    ) |>
    dplyr::select(DT_HR_LOCAL, NAME_OBJETO, NAME_SETOR, COMPONENTE, ATRIBUTO, VALOR)
}

# =========================================================
# Normaliza filtros de data/hora gerados pelo GPT para UTC
# (use se oc.DT_HR_LOCAL é armazenado em UTC)
# =========================================================
normalize_sql_dt_to_utc <- function(sql_txt,
                                    tz_local = "America/Sao_Paulo") {
  out <- sql_txt
  
  # DATE(oc.DT_HR_LOCAL) = 'YYYY-MM-DD'
  pat_date <- "DATE\\(oc\\.DT_HR_LOCAL\\)\\s*=\\s*'([0-9]{4}-[0-9]{2}-[0-9]{2})'"
  m <- regexpr(pat_date, out, perl = TRUE)
  if (m[1] != -1) {
    full     <- regmatches(out, m)[[1]]
    date_str <- sub(pat_date, "\\1", full, perl = TRUE)
    
    dt_local <- as.POSIXct(paste0(date_str, " 00:00:00"), tz = tz_local)
    dt_utc   <- format(as.POSIXct(dt_local, tz = "UTC"), "%Y-%m-%d")
    out <- sub(pat_date, paste0("DATE(oc.DT_HR_LOCAL) = '", dt_utc, "'"), out, perl = TRUE)
  }
  
  # TIME(oc.DT_HR_LOCAL) BETWEEN 'HH:MM:SS' AND 'HH:MM:SS'
  pat_time_between <- "TIME\\(oc\\.DT_HR_LOCAL\\)\\s+BETWEEN\\s+'([0-9]{2}:[0-9]{2}:[0-9]{2})'\\s+AND\\s+'([0-9]{2}:[0-9]{2}:[0-9]{2})'"
  m2 <- regexpr(pat_time_between, out, perl = TRUE)
  if (m2[1] != -1) {
    full2 <- regmatches(out, m2)[[1]]
    t1 <- sub(pat_time_between, "\\1", full2, perl = TRUE)
    t2 <- sub(pat_time_between, "\\2", full2, perl = TRUE)
    
    to_utc_time <- function(tstr) {
      today_local <- as.POSIXct(paste0("2000-01-01 ", tstr), tz = tz_local)
      today_utc   <- as.POSIXct(today_local, tz = "UTC")
      format(today_utc, "%H:%M:%S")
    }
    
    t1_utc <- to_utc_time(t1)
    t2_utc <- to_utc_time(t2)
    out <- sub(
      pat_time_between,
      paste0("TIME(oc.DT_HR_LOCAL) BETWEEN '", t1_utc, "' AND '", t2_utc, "'"),
      out, perl = TRUE
    )
  }
  
  out
}

# =========================================================
# Consulta SQL no banco.
# Assume que DT_HR_LOCAL está em UTC no banco e converte para exibição local.
# =========================================================
run_user_sql <- function(con, sql_txt) {
  out <- DBI::dbGetQuery(con, sql_txt)
  
  if ("DT_HR_LOCAL" %in% names(out)) {
    if (!inherits(out$DT_HR_LOCAL, "POSIXct")) {
      out$DT_HR_LOCAL <- as.POSIXct(out$DT_HR_LOCAL, tz = "UTC")
    } else {
      attr(out$DT_HR_LOCAL, "tzone") <- "UTC"
    }
    out$DT_HR_LOCAL <- lubridate::with_tz(out$DT_HR_LOCAL, tzone = "America/Sao_Paulo")
  }
  
  out
}

# =========================================================
# Expande DATA_OC (JSON) em colunas wide robustas (suporta arrays)
# =========================================================
expand_data_oc <- function(df) {
  if (!nrow(df) || !"DATA_OC" %in% names(df)) return(df)
  
  rows_extra <- lapply(df$DATA_OC, function(js) {
    if (is.na(js) || !nzchar(js)) return(list())
    parsed <- tryCatch(jsonlite::fromJSON(js, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return(list())
    
    flat <- list()
    walk_obj <- function(x, prefix = NULL) {
      if (is.list(x)) {
        for (nm in names(x)) {
          new_prefix <- if (is.null(prefix)) toupper(nm) else paste0(prefix, "_", toupper(nm))
          walk_obj(x[[nm]], new_prefix)
        }
      } else {
        flat[[prefix]] <<- if (length(x) > 1) toString(x) else x
      }
    }
    walk_obj(parsed, NULL)
    flat
  })
  
  all_names <- unique(unlist(lapply(rows_extra, names)))
  if (length(all_names) == 0) return(df)
  
  extra_df <- as.data.frame(matrix(NA_character_, nrow = nrow(df), ncol = length(all_names)))
  names(extra_df) <- all_names
  
  for (i in seq_along(rows_extra)) {
    if (length(rows_extra[[i]])) {
      for (nm in names(rows_extra[[i]])) {
        extra_df[i, nm] <- as.character(rows_extra[[i]][[nm]])
      }
    }
  }
  
  # tenta converter números quando fizer sentido
  extra_df[] <- lapply(extra_df, function(col) {
    sup <- suppressWarnings(as.numeric(col))
    if (!all(is.na(sup)) && sum(!is.na(sup)) >= 1) sup else col
  })
  
  dplyr::bind_cols(df, extra_df)
}

# =========================================================
# Carrega nomes de funções mais comuns do ggplot2 no ambiente do eval
# (binder enxuto e "à prova de versão")
# =========================================================
loadNamesGgplot <- function(env){
  
  env$ggplot                     <- ggplot2::ggplot
  env$geom_bar                   <- ggplot2::geom_bar
  env$is_facet                   <- ggplot2::is_facet
  env$wrap_dims                  <- ggplot2::wrap_dims
  env$class_waiver               <- ggplot2::class_waiver
  env$scale_colour_binned        <- ggplot2::scale_colour_binned
  env$scale_x_binned             <- ggplot2::scale_x_binned
  env$position_fill              <- ggplot2::position_fill
  env$theme_set                  <- ggplot2::theme_set
  env$GeomFunction               <- ggplot2::GeomFunction
  env$annotation_raster          <- ggplot2::annotation_raster
  env$CoordRadial                <- ggplot2::CoordRadial
  env$el_def                     <- ggplot2::el_def
  env$Scale                      <- ggplot2::Scale
  env$stat_smooth                <- ggplot2::stat_smooth
  env$scale_shape_binned         <- ggplot2::scale_shape_binned
  env$StatEcdf                   <- ggplot2::StatEcdf
  env$get_element_tree           <- ggplot2::get_element_tree
  env$scale_fill_continuous      <- ggplot2::scale_fill_continuous
  env$update_geom_defaults       <- ggplot2::update_geom_defaults
  env$scale_color_continuous     <- ggplot2::scale_color_continuous
  env$GeomCrossbar               <- ggplot2::GeomCrossbar
  env$layer                      <- ggplot2::layer
  env$scale_linewidth_datetime   <- ggplot2::scale_linewidth_datetime
  env$element_geom               <- ggplot2::element_geom
  env$mean_sdl                   <- ggplot2::mean_sdl
  env$draw_key_polygon           <- ggplot2::draw_key_polygon
  env$scale_color_gradientn      <- ggplot2::scale_color_gradientn
  env$is_position                <- ggplot2::is_position
  env$geom_text                  <- ggplot2::geom_text
  env$geom_line                  <- ggplot2::geom_line
  env$theme_sub_legend           <- ggplot2::theme_sub_legend
  env$geom_polygon               <- ggplot2::geom_polygon
  env$scale_fill_hue             <- ggplot2::scale_fill_hue
  env$dup_axis                   <- ggplot2::dup_axis
  env$class_theme                <- ggplot2::class_theme
  env$scale_x_sqrt               <- ggplot2::scale_x_sqrt
  env$coord_transform            <- ggplot2::coord_transform
  env$GeomBin2d                  <- ggplot2::GeomBin2d
  env$scale_fill_brewer          <- ggplot2::scale_fill_brewer
  env$scale_discrete_identity    <- ggplot2::scale_discrete_identity
  env$mean_se                    <- ggplot2::mean_se
  env$geom_map                   <- ggplot2::geom_map
  env$derive                     <- ggplot2::derive
  env$geom_density               <- ggplot2::geom_density
  env$get_layer_data             <- ggplot2::get_layer_data
  env$GeomLinerange              <- ggplot2::GeomLinerange
  env$scale_color_manual         <- ggplot2::scale_color_manual
  env$geom_quantile              <- ggplot2::geom_quantile
  env$draw_key_text              <- ggplot2::draw_key_text
  env$draw_key_vpath             <- ggplot2::draw_key_vpath
  env$geom_freqpoly              <- ggplot2::geom_freqpoly
  env$geom_function              <- ggplot2::geom_function
  env$scale_shape_manual         <- ggplot2::scale_shape_manual
  env$GeomMap                    <- ggplot2::GeomMap
  env$GuideOld                   <- ggplot2::GuideOld
  env$geom_linerange             <- ggplot2::geom_linerange
  env$StatCount                  <- ggplot2::StatCount
  env$stat_density               <- ggplot2::stat_density
  env$scale_fill_steps           <- ggplot2::scale_fill_steps
  env$scale_colour_viridis_b     <- ggplot2::scale_colour_viridis_b
  env$coord_munch                <- ggplot2::coord_munch
  env$Layout                     <- ggplot2::Layout
  env$scale_colour_viridis_c     <- ggplot2::scale_colour_viridis_c
  env$scale_colour_viridis_d     <- ggplot2::scale_colour_viridis_d
  env$scale_colour_identity      <- ggplot2::scale_colour_identity
  env$ScaleDiscreteIdentity      <- ggplot2::ScaleDiscreteIdentity
  env$layer_scales               <- ggplot2::layer_scales
  env$scale_size_binned_area     <- ggplot2::scale_size_binned_area
  env$GeomArea                   <- ggplot2::GeomArea
  env$StatContour                <- ggplot2::StatContour
  env$geom_jitter                <- ggplot2::geom_jitter
  env$GeomRibbon                 <- ggplot2::GeomRibbon
  env$ScaleBinnedPosition        <- ggplot2::ScaleBinnedPosition
  env$class_facet                <- ggplot2::class_facet
  env$GeomDensity2d              <- ggplot2::GeomDensity2d
  env$scale_fill_datetime        <- ggplot2::scale_fill_datetime
  env$coord_map                  <- ggplot2::coord_map
  env$get_labs                   <- ggplot2::get_labs
  env$scale_colour_steps2        <- ggplot2::scale_colour_steps2
  env$coord_fixed                <- ggplot2::coord_fixed
  env$scale_colour_hue           <- ggplot2::scale_colour_hue
  env$CoordSf                    <- ggplot2::CoordSf
  env$quo_name                   <- ggplot2::quo_name
  env$update_ggplot              <- ggplot2::update_ggplot
  env$class_S3_gg                <- ggplot2::class_S3_gg
  env$scale_color_date           <- ggplot2::scale_color_date
  env$coord_polar                <- ggplot2::coord_polar
  env$GuideAxisStack             <- ggplot2::GuideAxisStack
  env$ggplotGrob                 <- ggplot2::ggplotGrob
  env$reset_theme_settings       <- ggplot2::reset_theme_settings
  env$theme_sub_axis_top         <- ggplot2::theme_sub_axis_top
  env$GuideColoursteps           <- ggplot2::GuideColoursteps
  env$scale_linetype_binned      <- ggplot2::scale_linetype_binned
  env$StatAlign                  <- ggplot2::StatAlign
  env$scale_fill_discrete        <- ggplot2::scale_fill_discrete
  env$aes_all                    <- ggplot2::aes_all
  env$is_coord                   <- ggplot2::is_coord
  env$enexpr                     <- ggplot2::enexpr
  env$combine_vars               <- ggplot2::combine_vars
  env$class_guides               <- ggplot2::class_guides
  env$summarise_layers           <- ggplot2::summarise_layers
  env$GeomRasterAnn              <- ggplot2::GeomRasterAnn
  env$stat_unique                <- ggplot2::stat_unique
  env$GeomDensity                <- ggplot2::GeomDensity
  env$is_ggproto                 <- ggplot2::is_ggproto
  env$GeomSf                     <- ggplot2::GeomSf
  env$draw_key_boxplot           <- ggplot2::draw_key_boxplot
  env$GeomPath                   <- ggplot2::GeomPath
  env$guides                     <- ggplot2::guides
  env$FacetNull                  <- ggplot2::FacetNull
  env$theme_sub_axis_x           <- ggplot2::theme_sub_axis_x
  env$mean_cl_normal             <- ggplot2::mean_cl_normal
  env$theme_sub_axis_y           <- ggplot2::theme_sub_axis_y
  env$scale_fill_grey            <- ggplot2::scale_fill_grey
  env$scale_type                 <- ggplot2::scale_type
  env$scale_linetype_discrete    <- ggplot2::scale_linetype_discrete
  env$label_context              <- ggplot2::label_context
  env$position_identity          <- ggplot2::position_identity
  env$draw_key_smooth            <- ggplot2::draw_key_smooth
  env$class_layout               <- ggplot2::class_layout
  env$scale_shape_identity       <- ggplot2::scale_shape_identity
  env$stat_bin_2d                <- ggplot2::stat_bin_2d
  env$CoordPolar                 <- ggplot2::CoordPolar
  env$geom_sf_text               <- ggplot2::geom_sf_text
  env$stat_connect               <- ggplot2::stat_connect
  env$geom_abline                <- ggplot2::geom_abline
  env$GeomLine                   <- ggplot2::GeomLine
  env$geom_tile                  <- ggplot2::geom_tile
  env$.data                      <- ggplot2::.data
  env$facet_grid                 <- ggplot2::facet_grid
  env$scale_fill_date            <- ggplot2::scale_fill_date
  env$flip_data                  <- ggplot2::flip_data
  env$stat_quantile              <- ggplot2::stat_quantile
  env$class_scales_list          <- ggplot2::class_scales_list
  env$draw_key_vline             <- ggplot2::draw_key_vline
  env$scale_color_binned         <- ggplot2::scale_color_binned
  env$element_render             <- ggplot2::element_render
  env$class_layer                <- ggplot2::class_layer
  env$scale_size_datetime        <- ggplot2::scale_size_datetime
  env$scale_color_steps          <- ggplot2::scale_color_steps
  env$margin_auto                <- ggplot2::margin_auto
  env$complete_theme             <- ggplot2::complete_theme
  env$geom_count                 <- ggplot2::geom_count
  env$make_constructor           <- ggplot2::make_constructor
  env$stat_ellipse               <- ggplot2::stat_ellipse
  env$scale_y_date               <- ggplot2::scale_y_date
  env$StatSummaryBin             <- ggplot2::StatSummaryBin
  env$scale_linewidth_date       <- ggplot2::scale_linewidth_date
  env$fill_alpha                 <- ggplot2::fill_alpha
  env$GuideCustom                <- ggplot2::GuideCustom
  env$reset_geom_defaults        <- ggplot2::reset_geom_defaults
  env$GeomAbline                 <- ggplot2::GeomAbline
  env$draw_key_linerange         <- ggplot2::draw_key_linerange
  env$scale_alpha_identity       <- ggplot2::scale_alpha_identity
  env$scale_fill_steps2          <- ggplot2::scale_fill_steps2
  env$StatConnect                <- ggplot2::StatConnect
  env$pattern_alpha              <- ggplot2::pattern_alpha
  env$calc_element               <- ggplot2::calc_element
  env$scale_discrete_manual      <- ggplot2::scale_discrete_manual
  env$scale_y_time               <- ggplot2::scale_y_time
  env$GeomBar                    <- ggplot2::GeomBar
  env$get_panel_scales           <- ggplot2::get_panel_scales
  env$is.theme                   <- ggplot2::is.theme
  env$remove_missing             <- ggplot2::remove_missing
  env$scale_colour_fermenter     <- ggplot2::scale_colour_fermenter
  env$scale_color_discrete       <- ggplot2::scale_color_discrete
  env$stat_spoke                 <- ggplot2::stat_spoke
  env$ScaleContinuousDate        <- ggplot2::ScaleContinuousDate
  env$geom_hline                 <- ggplot2::geom_hline
  env$geom_hex                   <- ggplot2::geom_hex
  env$draw_key_dotplot           <- ggplot2::draw_key_dotplot
  env$GeomText                   <- ggplot2::GeomText
  env$scale_x_continuous         <- ggplot2::scale_x_continuous
  env$xlab                       <- ggplot2::xlab
  env$get_geom_defaults          <- ggplot2::get_geom_defaults
  env$ylab                       <- ggplot2::ylab
  env$geom_density_2d_filled     <- ggplot2::geom_density_2d_filled
  env$ScaleContinuous            <- ggplot2::ScaleContinuous
  env$stat_contour_filled        <- ggplot2::stat_contour_filled
  env$guide_transform            <- ggplot2::guide_transform
  env$Position                   <- ggplot2::Position
  env$scale_colour_grey          <- ggplot2::scale_colour_grey
  env$scale_fill_ordinal         <- ggplot2::scale_fill_ordinal
  env$scale_size_date            <- ggplot2::scale_size_date
  env$panel_cols                 <- ggplot2::panel_cols
  env$guide_colorbar             <- ggplot2::guide_colorbar
  env$median_hilow               <- ggplot2::median_hilow
  env$summarise_coord            <- ggplot2::summarise_coord
  env$geom_bin_2d                <- ggplot2::geom_bin_2d
  env$StatManual                 <- ggplot2::StatManual
  env$StatUnique                 <- ggplot2::StatUnique
  env$coord_trans                <- ggplot2::coord_trans
  env$annotation_logticks        <- ggplot2::annotation_logticks
  env$geom_qq_line               <- ggplot2::geom_qq_line
  env$geom_rect                  <- ggplot2::geom_rect
  env$scale_color_grey           <- ggplot2::scale_color_grey
  env$class_gg                   <- ggplot2::class_gg
  env$layer_grob                 <- ggplot2::layer_grob
  env$lims                       <- ggplot2::lims
  env$stat_sf_coordinates        <- ggplot2::stat_sf_coordinates
  env$has_flipped_aes            <- ggplot2::has_flipped_aes
  env$update_stat_defaults       <- ggplot2::update_stat_defaults
  env$scale_size_area            <- ggplot2::scale_size_area
  env$is_waiver                  <- ggplot2::is_waiver
  env$scale_linetype             <- ggplot2::scale_linetype
  env$theme_light                <- ggplot2::theme_light
  env$syms                       <- ggplot2::syms
  env$element_grob               <- ggplot2::element_grob
  env$theme_sub_strip            <- ggplot2::theme_sub_strip
  env$CoordTrans                 <- ggplot2::CoordTrans
  env$scale_linewidth_manual     <- ggplot2::scale_linewidth_manual
  env$scale_color_viridis_b      <- ggplot2::scale_color_viridis_b
  env$GeomSmooth                 <- ggplot2::GeomSmooth
  env$scale_color_viridis_c      <- ggplot2::scale_color_viridis_c
  env$theme_sub_axis_left        <- ggplot2::theme_sub_axis_left
  env$scale_color_viridis_d      <- ggplot2::scale_color_viridis_d
  env$ggproto                    <- ggplot2::ggproto
  env$scale_alpha_date           <- ggplot2::scale_alpha_date
  env$theme_sub_plot             <- ggplot2::theme_sub_plot
  env$`%+replace%`               <- ggplot2::`%+replace%`
  env$theme_bw                   <- ggplot2::theme_bw
  env$GeomLabel                  <- ggplot2::GeomLabel
  env$class_scale                <- ggplot2::class_scale
  env$GuideNone                  <- ggplot2::GuideNone
  env$GeomBoxplot                <- ggplot2::GeomBoxplot
  env$scale_y_sqrt               <- ggplot2::scale_y_sqrt
  env$panel_rows                 <- ggplot2::panel_rows
  env$update_labels              <- ggplot2::update_labels
  env$stat_boxplot               <- ggplot2::stat_boxplot
  env$coord_radial               <- ggplot2::coord_radial
  env$Facet                      <- ggplot2::Facet
  env$render_axes                <- ggplot2::render_axes
  env$GuideLegend                <- ggplot2::GuideLegend
  env$stat_summary_2d            <- ggplot2::stat_summary_2d
  env$scale_color_distiller      <- ggplot2::scale_color_distiller
  env$StatYdensity               <- ggplot2::StatYdensity
  env$scale_linewidth_identity   <- ggplot2::scale_linewidth_identity
  env$ggtitle                    <- ggplot2::ggtitle
  env$ggplot_add                 <- ggplot2::ggplot_add
  env$StatEllipse                <- ggplot2::StatEllipse
  env$is_guide                   <- ggplot2::is_guide
  env$last_plot                  <- ggplot2::last_plot
  env$GeomTile                   <- ggplot2::GeomTile
  env$guide_bins                 <- ggplot2::guide_bins
  env$.stroke                    <- ggplot2::.stroke
  env$.pt                        <- ggplot2::.pt
  env$GuideAxisTheta             <- ggplot2::GuideAxisTheta
  env$guide_merge                <- ggplot2::guide_merge
  env$is.ggplot                  <- ggplot2::is.ggplot
  env$flipped_names              <- ggplot2::flipped_names
  env$element_text               <- ggplot2::element_text
  env$is_scale                   <- ggplot2::is_scale
  env$as_labeller                <- ggplot2::as_labeller
  env$scale_alpha_discrete       <- ggplot2::scale_alpha_discrete
  env$unit                       <- ggplot2::unit
  env$geom_pointrange            <- ggplot2::geom_pointrange
  env$aes                        <- ggplot2::aes
  env$sf_transform_xy            <- ggplot2::sf_transform_xy
  env$GeomHline                  <- ggplot2::GeomHline
  env$StatSum                    <- ggplot2::StatSum
  env$annotation_borders         <- ggplot2::annotation_borders
  env$draw_key_blank             <- ggplot2::draw_key_blank
  env$scale_linetype_identity    <- ggplot2::scale_linetype_identity
  env$StatContourFilled          <- ggplot2::StatContourFilled
  env$get_theme                  <- ggplot2::get_theme
  env$GuideAxis                  <- ggplot2::GuideAxis
  env$element_rect               <- ggplot2::element_rect
  env$scale_size_discrete        <- ggplot2::scale_size_discrete
  env$labeller                   <- ggplot2::labeller
  env$GeomContourFilled          <- ggplot2::GeomContourFilled
  env$from_theme                 <- ggplot2::from_theme
  env$CoordCartesian             <- ggplot2::CoordCartesian
  env$ScaleBinned                <- ggplot2::ScaleBinned
  env$StatBin                    <- ggplot2::StatBin
  env$is_geom                    <- ggplot2::is_geom
  env$scale_fill_identity        <- ggplot2::scale_fill_identity
  env$`%+%`                      <- ggplot2::`%+%`
  env$scale_linewidth_discrete   <- ggplot2::scale_linewidth_discrete
  env$stat_function              <- ggplot2::stat_function
  env$scale_y_discrete           <- ggplot2::scale_y_discrete
  env$scale_color_hue            <- ggplot2::scale_color_hue
  env$theme_sub_axis             <- ggplot2::theme_sub_axis
  env$class_ggplot_built         <- ggplot2::class_ggplot_built
  env$ggplot_gtable              <- ggplot2::ggplot_gtable
  env$PositionIdentity           <- ggplot2::PositionIdentity
  env$draw_key_crossbar          <- ggplot2::draw_key_crossbar
  env$is_margin                  <- ggplot2::is_margin
  env$stat_bin2d                 <- ggplot2::stat_bin2d
  env$scale_size                 <- ggplot2::scale_size
  env$benchplot                  <- ggplot2::benchplot
  env$geom_qq                    <- ggplot2::geom_qq
  env$scale_linewidth_binned     <- ggplot2::scale_linewidth_binned
  env$scale_x_discrete           <- ggplot2::scale_x_discrete
  env$summarise_layout           <- ggplot2::summarise_layout
  env$scale_colour_date          <- ggplot2::scale_colour_date
  env$GeomDotplot                <- ggplot2::GeomDotplot
  env$draw_key_label             <- ggplot2::draw_key_label
  env$GeomSpoke                  <- ggplot2::GeomSpoke
  env$guide_axis_theta           <- ggplot2::guide_axis_theta
  env$scale_color_stepsn         <- ggplot2::scale_color_stepsn
  env$draw_key_point             <- ggplot2::draw_key_point
  env$label_both                 <- ggplot2::label_both
  env$GeomRect                   <- ggplot2::GeomRect
  env$FacetGrid                  <- ggplot2::FacetGrid
  env$geom_rug                   <- ggplot2::geom_rug
  env$GeomContour                <- ggplot2::GeomContour
  env$render_strips              <- ggplot2::render_strips
  env$get_layer_grob             <- ggplot2::get_layer_grob
  env$geom_sf                    <- ggplot2::geom_sf
  env$stat_summary               <- ggplot2::stat_summary
  env$position_jitterdodge       <- ggplot2::position_jitterdodge
  env$expand_limits              <- ggplot2::expand_limits
  env$GeomCurve                  <- ggplot2::GeomCurve
  env$waiver                     <- ggplot2::waiver
  env$scale_colour_brewer        <- ggplot2::scale_colour_brewer
  env$labs                       <- ggplot2::labs
  env$geom_point                 <- ggplot2::geom_point
  env$PositionDodge2             <- ggplot2::PositionDodge2
  env$class_ggproto              <- ggplot2::class_ggproto
  env$cut_number                 <- ggplot2::cut_number
  env$GeomQuantile               <- ggplot2::GeomQuantile
  env$annotation_custom          <- ggplot2::annotation_custom
  env$stat_identity              <- ggplot2::stat_identity
  env$draw_key_rect              <- ggplot2::draw_key_rect
  env$StatQqLine                 <- ggplot2::StatQqLine
  env$theme_grey                 <- ggplot2::theme_grey
  env$xlim                       <- ggplot2::xlim
  env$class_guide                <- ggplot2::class_guide
  env$stat_bin                   <- ggplot2::stat_bin
  env$ylim                       <- ggplot2::ylim
  env$ensyms                     <- ggplot2::ensyms
  env$facet_null                 <- ggplot2::facet_null
  env$geom_crossbar              <- ggplot2::geom_crossbar
  env$scale_size_identity        <- ggplot2::scale_size_identity
  env$stat_density2d_filled      <- ggplot2::stat_density2d_filled
  env$class_zero_grob            <- ggplot2::class_zero_grob
  env$stat_sum                   <- ggplot2::stat_sum
  env$geom_density2d             <- ggplot2::geom_density2d
  env$Geom                       <- ggplot2::Geom
  env$is_layer                   <- ggplot2::is_layer
  env$borders                    <- ggplot2::borders
  env$ggplot_build               <- ggplot2::ggplot_build
  env$AxisSecondary              <- ggplot2::AxisSecondary
  env$margin_part                <- ggplot2::margin_part
  env$geom_errorbarh             <- ggplot2::geom_errorbarh
  env$theme_linedraw             <- ggplot2::theme_linedraw
  env$scale_colour_stepsn        <- ggplot2::scale_colour_stepsn
  env$guide_legend               <- ggplot2::guide_legend
  env$geom_spoke                 <- ggplot2::geom_spoke
  env$GeomPointrange             <- ggplot2::GeomPointrange
  env$is.facet                   <- ggplot2::is.facet
  env$stat_binhex                <- ggplot2::stat_binhex
  env$register_theme_elements    <- ggplot2::register_theme_elements
  env$annotate                   <- ggplot2::annotate
  env$scale_colour_distiller     <- ggplot2::scale_colour_distiller
  env$scale_shape_ordinal        <- ggplot2::scale_shape_ordinal
  env$element                    <- ggplot2::element
  env$geom_step                  <- ggplot2::geom_step
  env$is.ggproto                 <- ggplot2::is.ggproto
  env$ggplot                     <- ggplot2::ggplot
  env$ScaleContinuousIdentity    <- ggplot2::ScaleContinuousIdentity
  env$guide_axis_stack           <- ggplot2::guide_axis_stack
  env$scale_fill_stepsn          <- ggplot2::scale_fill_stepsn
  env$scale_x_log10              <- ggplot2::scale_x_log10
  env$scale_shape                <- ggplot2::scale_shape
  env$CoordTransform             <- ggplot2::CoordTransform
  env$max_width                  <- ggplot2::max_width
  env$geom_violin                <- ggplot2::geom_violin
  env$autoplot                   <- ggplot2::autoplot
  env$scale_y_log10              <- ggplot2::scale_y_log10
  env$StatSfCoordinates          <- ggplot2::StatSfCoordinates
  env$stat_align                 <- ggplot2::stat_align
  env$StatDensity                <- ggplot2::StatDensity
  env$translate_shape_string     <- ggplot2::translate_shape_string
  env$cut_interval               <- ggplot2::cut_interval
  env$scale_y_datetime           <- ggplot2::scale_y_datetime
  env$new_guide                  <- ggplot2::new_guide
  env$coord_cartesian            <- ggplot2::coord_cartesian
  env$enexprs                    <- ggplot2::enexprs
  env$label_parsed               <- ggplot2::label_parsed
  env$position_dodge             <- ggplot2::position_dodge
  env$map_data                   <- ggplot2::map_data
  env$get_alt_text               <- ggplot2::get_alt_text
  env$annotation_map             <- ggplot2::annotation_map
  env$GeomAnnotationMap          <- ggplot2::GeomAnnotationMap
  env$geom_histogram             <- ggplot2::geom_histogram
  env$scale_color_gradient       <- ggplot2::scale_color_gradient
  env$scale_color_ordinal        <- ggplot2::scale_color_ordinal
  env$theme_classic              <- ggplot2::theme_classic
  env$is_theme_element           <- ggplot2::is_theme_element
  env$stat_density_2d_filled     <- ggplot2::stat_density_2d_filled
  env$scale_shape_continuous     <- ggplot2::scale_shape_continuous
  env$draw_key_pointrange        <- ggplot2::draw_key_pointrange
  env$coord_quickmap             <- ggplot2::coord_quickmap
  env$scale_x_datetime           <- ggplot2::scale_x_datetime
  env$quos                       <- ggplot2::quos
  env$theme_dark                 <- ggplot2::theme_dark
  env$StatBin2d                  <- ggplot2::StatBin2d
  env$stat_summary2d             <- ggplot2::stat_summary2d
  env$cut_width                  <- ggplot2::cut_width
  env$theme_gray                 <- ggplot2::theme_gray
  env$get_last_plot              <- ggplot2::get_last_plot
  env$scale_color_brewer         <- ggplot2::scale_color_brewer
  env$guide_coloursteps          <- ggplot2::guide_coloursteps
  env$quickplot                  <- ggplot2::quickplot
  env$draw_key_timeseries        <- ggplot2::draw_key_timeseries
  env$coord_equal                <- ggplot2::coord_equal
  env$class_coord                <- ggplot2::class_coord
  env$scale_color_steps2         <- ggplot2::scale_color_steps2
  env$CoordFixed                 <- ggplot2::CoordFixed
  env$zeroGrob                   <- ggplot2::zeroGrob
  env$StatSummaryHex             <- ggplot2::StatSummaryHex
  env$stat_summary_hex           <- ggplot2::stat_summary_hex
  env$GeomPoint                  <- ggplot2::GeomPoint
  env$transform_position         <- ggplot2::transform_position
  env$is_mapping                 <- ggplot2::is_mapping
  env$stat_sf                    <- ggplot2::stat_sf
  env$scale_fill_manual          <- ggplot2::scale_fill_manual
  env$scale_colour_discrete      <- ggplot2::scale_colour_discrete
  env$guide_geom                 <- ggplot2::guide_geom
  env$set_last_plot              <- ggplot2::set_last_plot
  env$scale_color_gradient2      <- ggplot2::scale_color_gradient2
  env$theme_replace              <- ggplot2::theme_replace
  env$geom_bin2d                 <- ggplot2::geom_bin2d
  env$GeomCustomAnn              <- ggplot2::GeomCustomAnn
  env$set_theme                  <- ggplot2::set_theme
  env$scale_continuous_identity  <- ggplot2::scale_continuous_identity
  env$StatBindot                 <- ggplot2::StatBindot
  env$stat_qq                    <- ggplot2::stat_qq
  env$after_stat                 <- ggplot2::after_stat
  env$guide_gengrob              <- ggplot2::guide_gengrob
  env$layer_sf                   <- ggplot2::layer_sf
  env$aes_                       <- ggplot2::aes_
  env$geom_contour               <- ggplot2::geom_contour
  env$element_blank              <- ggplot2::element_blank
  env$PositionJitter             <- ggplot2::PositionJitter
  env$scale_colour_ordinal       <- ggplot2::scale_colour_ordinal
  env$margin                     <- ggplot2::margin
  env$guide_colourbar            <- ggplot2::guide_colourbar
  env$GeomErrorbarh              <- ggplot2::GeomErrorbarh
  env$mean_cl_boot               <- ggplot2::mean_cl_boot
  env$scale_color_fermenter      <- ggplot2::scale_color_fermenter
  env$scale_fill_binned          <- ggplot2::scale_fill_binned
  env$ScaleDiscretePosition      <- ggplot2::ScaleDiscretePosition
  env$FacetWrap                  <- ggplot2::FacetWrap
  env$StatBoxplot                <- ggplot2::StatBoxplot
  env$PositionFill               <- ggplot2::PositionFill
  env$stat_summary_bin           <- ggplot2::stat_summary_bin
  env$geom_smooth                <- ggplot2::geom_smooth
  env$scale_linewidth            <- ggplot2::scale_linewidth
  env$theme                      <- ggplot2::theme
  env$theme_update               <- ggplot2::theme_update
  env$max_height                 <- ggplot2::max_height
  env$scale_color_identity       <- ggplot2::scale_color_identity
  env$scale_colour_gradient2     <- ggplot2::scale_colour_gradient2
  env$scale_y_binned             <- ggplot2::scale_y_binned
  env$guide_colorsteps           <- ggplot2::guide_colorsteps
  env$scale_size_binned          <- ggplot2::scale_size_binned
  env$scale_fill_viridis_b       <- ggplot2::scale_fill_viridis_b
  env$StatBinhex                 <- ggplot2::StatBinhex
  env$scale_fill_viridis_c       <- ggplot2::scale_fill_viridis_c
  env$scale_fill_viridis_d       <- ggplot2::scale_fill_viridis_d
  env$is_theme                   <- ggplot2::is_theme
  env$expansion                  <- ggplot2::expansion
  env$layer_data                 <- ggplot2::layer_data
  env$theme_sub_panel            <- ggplot2::theme_sub_panel
  env$.expose_data               <- ggplot2::.expose_data
  env$guide_axis_logticks        <- ggplot2::guide_axis_logticks
  env$sec_axis                   <- ggplot2::sec_axis
  env$continuous_scale           <- ggplot2::continuous_scale
  env$geom_density2d_filled      <- ggplot2::geom_density2d_filled
  env$update_theme               <- ggplot2::update_theme
  env$geom_segment               <- ggplot2::geom_segment
  env$GeomPolygon                <- ggplot2::GeomPolygon
  env$GeomRaster                 <- ggplot2::GeomRaster
  env$scale_colour_continuous    <- ggplot2::scale_colour_continuous
  env$GeomBlank                  <- ggplot2::GeomBlank
  env$Stat                       <- ggplot2::Stat
  env$standardise_aes_names      <- ggplot2::standardise_aes_names
  env$theme_void                 <- ggplot2::theme_void
  env$geom_sf_label              <- ggplot2::geom_sf_label
  env$theme_get                  <- ggplot2::theme_get
  env$theme_minimal              <- ggplot2::theme_minimal
  env$scale_x_reverse            <- ggplot2::scale_x_reverse
  env$gg_dep                     <- ggplot2::gg_dep
  env$expand_scale               <- ggplot2::expand_scale
  env$position_dodge2            <- ggplot2::position_dodge2
  env$scale_alpha_continuous     <- ggplot2::scale_alpha_continuous
  env$geom_vline                 <- ggplot2::geom_vline
  env$ensym                      <- ggplot2::ensym
  env$scale_size_continuous      <- ggplot2::scale_size_continuous
  env$scale_colour_steps         <- ggplot2::scale_colour_steps
  env$old_guide                  <- ggplot2::old_guide
  env$class_rel                  <- ggplot2::class_rel
  env$class_ggplot               <- ggplot2::class_ggplot
  env$label_wrap_gen             <- ggplot2::label_wrap_gen
  env$scale_alpha                <- ggplot2::scale_alpha
  env$qplot                      <- ggplot2::qplot
  env$GeomLogticks               <- ggplot2::GeomLogticks
  env$StatSummary2d              <- ggplot2::StatSummary2d
  env$stage                      <- ggplot2::stage
  env$autolayer                  <- ggplot2::autolayer
  env$element_line               <- ggplot2::element_line
  env$enquo                      <- ggplot2::enquo
  env$theme_sub_axis_bottom      <- ggplot2::theme_sub_axis_bottom
  env$GeomVline                  <- ggplot2::GeomVline
  env$scale_colour_manual        <- ggplot2::scale_colour_manual
  env$fortify                    <- ggplot2::fortify
  env$PositionNudge              <- ggplot2::PositionNudge
  env$class_labels               <- ggplot2::class_labels
  env$GeomHex                    <- ggplot2::GeomHex
  env$guide_custom               <- ggplot2::guide_custom
  env$check_device               <- ggplot2::check_device
  env$scale_linetype_manual      <- ggplot2::scale_linetype_manual
  env$GeomRug                    <- ggplot2::GeomRug
  env$stat                       <- ggplot2::stat
  env$replace_theme              <- ggplot2::replace_theme
  env$resolution                 <- ggplot2::resolution
  env$scale_linetype_continuous  <- ggplot2::scale_linetype_continuous
  env$is_ggplot                  <- ggplot2::is_ggplot
  env$scale_alpha_ordinal        <- ggplot2::scale_alpha_ordinal
  env$element_polygon            <- ggplot2::element_polygon
  env$StatDensity2dFilled        <- ggplot2::StatDensity2dFilled
  env$sym                        <- ggplot2::sym
  env$guide_train                <- ggplot2::guide_train
  env$scale_colour_datetime      <- ggplot2::scale_colour_datetime
  env$StatDensity2d              <- ggplot2::StatDensity2d
  env$StatQq                     <- ggplot2::StatQq
  env$guide_none                 <- ggplot2::guide_none
  env$enquos                     <- ggplot2::enquos
  env$is.Coord                   <- ggplot2::is.Coord
  env$datetime_scale             <- ggplot2::datetime_scale
  env$scale_size_manual          <- ggplot2::scale_size_manual
  env$rel                        <- ggplot2::rel
  env$coord_flip                 <- ggplot2::coord_flip
  env$label_bquote               <- ggplot2::label_bquote
  env$label_value                <- ggplot2::label_value
  env$quo                        <- ggplot2::quo
  env$find_panel                 <- ggplot2::find_panel
  env$scale_color_datetime       <- ggplot2::scale_color_datetime
  env$draw_key_abline            <- ggplot2::draw_key_abline
  env$CoordFlip                  <- ggplot2::CoordFlip
  env$geom_path                  <- ggplot2::geom_path
  env$position_nudge             <- ggplot2::position_nudge
  env$stat_manual                <- ggplot2::stat_manual
  env$stat_qq_line               <- ggplot2::stat_qq_line
  env$GuideColourbar             <- ggplot2::GuideColourbar
  env$alpha                      <- ggplot2::alpha
  env$scale_colour_gradient      <- ggplot2::scale_colour_gradient
  env$GeomCol                    <- ggplot2::GeomCol
  env$get_guide_data             <- ggplot2::get_guide_data
  env$scale_colour_gradientn     <- ggplot2::scale_colour_gradientn
  env$geom_area                  <- ggplot2::geom_area
  env$aes_auto                   <- ggplot2::aes_auto
  env$StatSf                     <- ggplot2::StatSf
  env$stat_ecdf                  <- ggplot2::stat_ecdf
  env$scale_alpha_datetime       <- ggplot2::scale_alpha_datetime
  env$GeomStep                   <- ggplot2::GeomStep
  env$scale_fill_distiller       <- ggplot2::scale_fill_distiller
  env$stat_count                 <- ggplot2::stat_count
  env$scale_fill_fermenter       <- ggplot2::scale_fill_fermenter
  env$ScaleContinuousPosition    <- ggplot2::ScaleContinuousPosition
  env$CoordQuickmap              <- ggplot2::CoordQuickmap
  env$geom_label                 <- ggplot2::geom_label
  env$discrete_scale             <- ggplot2::discrete_scale
  env$StatSummary                <- ggplot2::StatSummary
  env$aes_string                 <- ggplot2::aes_string
  env$Coord                      <- ggplot2::Coord
  env$geom_boxplot               <- ggplot2::geom_boxplot
  env$scale_shape_discrete       <- ggplot2::scale_shape_discrete
  env$theme_test                 <- ggplot2::theme_test
  env$scale_size_ordinal         <- ggplot2::scale_size_ordinal
  env$geom_contour_filled        <- ggplot2::geom_contour_filled
  env$class_mapping              <- ggplot2::class_mapping
  env$scale_radius               <- ggplot2::scale_radius
  env$StatFunction               <- ggplot2::StatFunction
  env$scale_fill_gradientn       <- ggplot2::scale_fill_gradientn
  env$scale_alpha_binned         <- ggplot2::scale_alpha_binned
  env$scale_linewidth_continuous <- ggplot2::scale_linewidth_continuous
  env$theme_sub_axis_right       <- ggplot2::theme_sub_axis_right
  env$StatQuantile               <- ggplot2::StatQuantile
  env$merge_element              <- ggplot2::merge_element
  env$class_derive               <- ggplot2::class_derive
  env$gg_par                     <- ggplot2::gg_par
  env$is_stat                    <- ggplot2::is_stat
  env$stat_density2d             <- ggplot2::stat_density2d
  env$reset_stat_defaults        <- ggplot2::reset_stat_defaults
  env$ScaleContinuousDatetime    <- ggplot2::ScaleContinuousDatetime
  env$add_gg                     <- ggplot2::add_gg
  env$after_scale                <- ggplot2::after_scale
  env$PositionJitterdodge        <- ggplot2::PositionJitterdodge
  env$StatSmooth                 <- ggplot2::StatSmooth
  env$GuideBins                  <- ggplot2::GuideBins
  env$scale_x_date               <- ggplot2::scale_x_date
  env$ggsave                     <- ggplot2::ggsave
  env$as_label                   <- ggplot2::as_label
  env$aes_q                      <- ggplot2::aes_q
  env$GeomDensity2dFilled        <- ggplot2::GeomDensity2dFilled
  env$StatIdentity               <- ggplot2::StatIdentity
  env$expr                       <- ggplot2::expr
  env$stat_contour               <- ggplot2::stat_contour
  env$GuideAxisLogticks          <- ggplot2::GuideAxisLogticks
  env$scale_fill_gradient        <- ggplot2::scale_fill_gradient
  env$geom_raster                <- ggplot2::geom_raster
  env$should_stop                <- ggplot2::should_stop
  env$scale_y_reverse            <- ggplot2::scale_y_reverse
  env$geom_ribbon                <- ggplot2::geom_ribbon
  env$geom_density_2d            <- ggplot2::geom_density_2d
  env$position_stack             <- ggplot2::position_stack
  env$geom_blank                 <- ggplot2::geom_blank
  env$get_strip_labels           <- ggplot2::get_strip_labels
  env$scale_x_time               <- ggplot2::scale_x_time
  env$position_jitter            <- ggplot2::position_jitter
  env$GeomSegment                <- ggplot2::GeomSegment
  env$stat_bin_hex               <- ggplot2::stat_bin_hex
  env$geom_errorbar              <- ggplot2::geom_errorbar
  env$stat_ydensity              <- ggplot2::stat_ydensity
  env$vars                       <- ggplot2::vars
  env$scale_fill_gradient2       <- ggplot2::scale_fill_gradient2
  env$PositionDodge              <- ggplot2::PositionDodge
  env$guide_axis                 <- ggplot2::guide_axis
  env$coord_sf                   <- ggplot2::coord_sf
  env$is_guides                  <- ggplot2::is_guides
  env$stat_density_2d            <- ggplot2::stat_density_2d
  env$scale_y_continuous         <- ggplot2::scale_y_continuous   # <- já tinha, mantive
  env$binned_scale               <- ggplot2::binned_scale
  env$draw_key_path              <- ggplot2::draw_key_path
  env$PositionStack              <- ggplot2::PositionStack
  env$arrow                      <- ggplot2::arrow
  env$scale_linewidth_ordinal    <- ggplot2::scale_linewidth_ordinal
  env$geom_col                   <- ggplot2::geom_col
  env$ggproto_parent             <- ggplot2::ggproto_parent
  env$CoordMap                   <- ggplot2::CoordMap
  env$facet_wrap                 <- ggplot2::facet_wrap
  env$geom_dotplot               <- ggplot2::geom_dotplot
  env$element_point              <- ggplot2::element_point
  env$geom_curve                  <- ggplot2::geom_curve
  env$ScaleDiscrete               <- ggplot2::ScaleDiscrete
  env$GeomViolin                  <- ggplot2::GeomViolin
  env$GeomErrorbar                <- ggplot2::GeomErrorbar
  env$scale_alpha_manual          <- ggplot2::scale_alpha_manual
  env$Guide                       <- ggplot2::Guide
  env
}

# =========================================================
# GPT: gerar SQL (apenas SELECT) seguindo regras
# =========================================================
gpt_build_sql <- function(user_msg,
                          ctx    = NULL,
                          model  = DEFAULT_MODEL,
                          api_key = OPENAI_API_KEY) {
  
  if (!nzchar(api_key)) {
    return(list(sql = NULL, error = "OPENAI_API_KEY não configurada"))
  }
  
  ctx_txt <- ""
  if (!is.null(ctx)) {
    if (!is.null(ctx$last_object)) {
      ctx_txt <- paste0(ctx_txt, "- Se o usuário NÃO mencionar objeto agora, reutilize este: '", ctx$last_object, "'.\n")
    }
    if (!is.null(ctx$last_setor)) {
      ctx_txt <- paste0(ctx_txt, "- Se o usuário NÃO mencionar setor agora, reutilize este: '", ctx$last_setor, "'.\n")
    }
  }
  
  system_msg <- paste(
    "Você gera SQL para consultar telemetria industrial em MariaDB.",
    "",
    "BASE DA CONSULTA (sempre):",
    "SELECT",
    "  oc.DATA_OC,",
    "  oc.DT_HR_LOCAL,",
    "  o.NAME_OBJETO,",
    "  s.NAME_SETOR",
    "FROM objeto_contexto oc",
    "LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO",
    "LEFT JOIN setor  s ON s.CD_ID_SETOR  = o.CD_ID_SETOR",
    "",
    "REGRAS MUITO IMPORTANTES:",
    "1. Responda APENAS em JSON, exatamente assim: {\"sql\": \"SELECT ...\"}",
    "2. NÃO use ```sql, não explique, não comente.",
    "3. SEMPRE termine com: ORDER BY oc.DT_HR_LOCAL DESC LIMIT 300",
    "4. APENAS SELECT. Proibido: UPDATE, DELETE, INSERT, CREATE, DROP.",
    "",
    "FILTROS QUE VOCÊ PODE USAR (APENAS ESTES):",
    "- Se o usuário falar de uma máquina ou objeto",
    "  filtre: o.NAME_OBJETO LIKE '%<NOME_EM_MAIUSCULO>%'",
    "",
    "- Se o usuário falar de um setor",
    "  filtre: s.NAME_SETOR LIKE '%<NOME_EM_MAIUSCULO>%'",
    "",
    "- Se o usuário falar de um intervalo de horário (ex.: 'entre 06:20 e 06:40'),",
    "  filtre: TIME(oc.DT_HR_LOCAL) BETWEEN '06:20:00' AND '06:40:59'",
    "",
    "- Se o usuário falar de uma data (ex.: '2025-10-31' ou 'hoje'),",
    "  use: DATE(oc.DT_HR_LOCAL) = '2025-10-31'",
    "",
    "IMPORTANTE:",
    "- NÃO crie filtros usando JSON_EXTRACT.",
    "- NÃO filtre pelo conteúdo de oc.DATA_OC.",
    "- Apenas traga oc.DATA_OC no SELECT.",
    "",
    "CONTEXTO ATUAL (se existir):",
    ctx_txt,
    "",
    "SAÍDA:",
    "- devolva somente: {\"sql\": \"SELECT ...\"}",
    sep = "\n"
  )
  
  user_full <- paste(
    "Usuário pediu:",
    user_msg,
    "\nMonte a SQL seguindo as regras."
  )
  
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_full)
      )
    ))
  
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    return(list(sql = NULL, error = httr2::resp_body_string(resp)))
  }
  
  body    <- httr2::resp_body_json(resp)
  raw_txt <- body$choices[[1]]$message$content
  
  sql_txt <- NULL
  parsed  <- tryCatch(jsonlite::fromJSON(raw_txt), error = function(e) NULL)
  if (!is.null(parsed) && !is.null(parsed$sql)) {
    sql_txt <- parsed$sql
  }
  
  if (is.null(sql_txt)) {
    lines <- strsplit(raw_txt, "\n", fixed = TRUE)[[1]]
    cand  <- lines[grepl("^\\s*SELECT\\s", lines, ignore.case = TRUE)]
    if (length(cand)) {
      sql_txt <- paste(cand, collapse = " ")
    }
  }
  
  if (is.null(sql_txt)) {
    sql_txt <- paste(
      "SELECT oc.DATA_OC, oc.DT_HR_LOCAL, o.NAME_OBJETO, s.NAME_SETOR",
      "FROM objeto_contexto oc",
      "LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO",
      "LEFT JOIN setor s ON s.CD_ID_SETOR = o.CD_ID_SETOR",
      "ORDER BY oc.DT_HR_LOCAL DESC",
      "LIMIT 1000;"
    )
    return(list(
      sql   = sql_txt,
      error = "Modelo não retornou SELECT; usei a query base."
    ))
  }
  
  if (!grepl("^\\s*SELECT\\s", sql_txt, ignore.case = TRUE)) {
    return(list(sql = NULL, error = "Modelo não retornou um SELECT mesmo após normalização."))
  }
  if (grepl(";", sql_txt)) {
    parts   <- strsplit(sql_txt, ";", fixed = TRUE)[[1]]
    sql_txt <- paste0(trimws(parts[[1]]), ";")
  }
  
  list(sql = sql_txt, error = NULL)
}

# =========================================================
# Extrai contexto (último objeto/setor) a partir das linhas retornadas
# =========================================================
infer_ctx_from_df <- function(df, old_ctx = NULL) {
  new_ctx <- old_ctx %||% list()
  if (!is.null(df) && nrow(df)) {
    if ("NAME_OBJETO" %in% names(df)) {
      obj <- df$NAME_OBJETO
      obj <- obj[!is.na(obj) & nzchar(obj)]
      if (length(obj)) {
        tab <- sort(table(obj), decreasing = TRUE)
        new_ctx$last_object <- names(tab)[1]
      }
    }
    if ("NAME_SETOR" %in% names(df)) {
      st <- df$NAME_SETOR
      st <- st[!is.na(st) & nzchar(st)]
      if (length(st)) {
        tab2 <- sort(table(st), decreasing = TRUE)
        new_ctx$last_setor <- names(tab2)[1]
      }
    }
  }
  new_ctx
}

#' @export
gpt_build_report_html <- function(user_msg_for_report,
                                  df_ctx,
                                  ctx   = NULL,
                                  model = DEFAULT_MODEL,
                                  api_key = OPENAI_API_KEY) {
  if (!nzchar(api_key)) {
    return(list(html = NULL, error = "OPENAI_API_KEY não configurada"))
  }
  
  # Amostra de dados (máx 60 linhas) — enviada ao GPT para tabulação
  df_preview <- if (nrow(df_ctx)) {
    head_n <- min(60L, nrow(df_ctx))
    jsonlite::toJSON(df_ctx[seq_len(head_n), , drop = FALSE], auto_unbox = TRUE)
  } else {
    "[]"
  }
  
  ctx_txt <- ""
  if (!is.null(ctx)) {
    if (!is.null(ctx$last_object)) ctx_txt <- paste0(ctx_txt, "- Último objeto em foco: ", ctx$last_object, "\n")
    if (!is.null(ctx$last_setor))  ctx_txt <- paste0(ctx_txt, "- Último setor em foco: ", ctx$last_setor, "\n")
  }
  
  system_msg <- paste(
    "Você é um gerador de RELATÓRIO EXECUTIVO (Diretoria Industrial).",
    "Gere **APENAS** um documento **HTML COMPLETO** (<!DOCTYPE html> ... </html>) pronto para impressão A4, com CSS inline. Não use recursos externos.",
    "Idiomas: Português (Brasil).",
    "",
    "REGRAS ESTRUTURAIS (obrigatórias):",
    "- Página A4, margens 18mm; fontes seguras (ex: Arial, sans-serif).",
    "- Cabeçalho com Título, Data/Hora local (America/Sao_Paulo) e contexto (Objeto/Setor) se existirem.",
    "- Seções mínimas: 1) Período e contexto; 2) Sumário executivo (bullet points); 3) Situação operacional; 4) Principais evidências (tabela com até 30 linhas de amostra); 5) Recomendações.",
    "- Inclua uma **tabela HTML** usando a amostra JSON fornecida (máx 30 linhas).",
    "- Não invente colunas. Use somente as colunas da amostra.",
    "- Se o usuário pediu itens específicos (ex.: métricas, destaques, filtros), atenda no texto.",
    "- Evite gráficos por imagem; use apenas tabela e texto (o PDF será gerado a partir do HTML).",
    "- Não inclua backticks nem markdown. Saída deve ser **somente** HTML completo.",
    sep = "\n"
  )
  
  user_msg <- paste(
    "INSTRUÇÕES DO USUÁRIO (copie o tom, estrutura e requisitos):\n",
    user_msg_for_report, "\n\n",
    "CONTEXTO ATUAL:\n", ctx_txt, "\n\n",
    "AMOSTRA DOS DADOS (JSON; até 60 linhas):\n", df_preview, "\n\n",
    "Observações:\n",
    "- Use datas/horas como aparecem (não traduza valores da tabela).",
    "- Se a amostra estiver vazia, produza um relatório coerente explicando que não há linhas retornadas para o período/consulta.",
    sep = ""
  )
  
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.2,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_msg)
      )
    ))
  
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    return(list(html = NULL, error = httr2::resp_body_string(resp)))
  }
  
  body <- httr2::resp_body_json(resp)
  html <- body$choices[[1]]$message$content %||% ""
  
  # Limpa cercas acidentais (caso o modelo devolva markdown por engano)
  if (grepl("```", html, fixed = TRUE)) {
    html <- gsub("^```[a-zA-Z]*", "", html)
    html <- gsub("```\\s*$", "", html)
    html <- trimws(html)
  }
  
  # Garante que é um HTML completo
  if (!grepl("<html", html, ignore.case = TRUE)) {
    html <- paste0("<!DOCTYPE html><html><head><meta charset='utf-8'><title>Relatório</title></head><body>", html, "</body></html>")
  }
  
  list(html = html, error = NULL)
}

# =========================================================
# GPT: sintetiza resposta e (opcionalmente) devolve JSON com plot_code/title
# =========================================================
gpt_answer_from_rows <- function(user_msg,
                                 df,
                                 ctx   = NULL,
                                 model = DEFAULT_MODEL,
                                 api_key = OPENAI_API_KEY) {
  
  if (!nzchar(api_key)) {
    return(list(
      assistant_text = "⚠️ IA indisponível no momento.",
      plot_code = NULL,
      plot_title = NULL
    ))
  }
  
  df_preview <- if (nrow(df)) {
    head_n <- min(40L, nrow(df))
    jsonlite::toJSON(df[seq_len(head_n), , drop = FALSE], auto_unbox = TRUE)
  } else {
    "[]"
  }
  
  ctx_txt <- ""
  if (!is.null(ctx)) {
    if (!is.null(ctx$last_object)) ctx_txt <- paste0(ctx_txt, "- Último objeto em foco: ", ctx$last_object, "\n")
    if (!is.null(ctx$last_setor))  ctx_txt <- paste0(ctx_txt, "- Último setor em foco: ", ctx$last_setor, "\n")
  }
  
  system_msg <- paste(
    "Você é um ASSISTENTE DE OPERAÇÃO INDUSTRIAL dentro de um painel.",
    "Você recebe linhas já filtradas com: DATA_OC (JSON de componentes), DT_HR_LOCAL (timestamp), NAME_OBJETO (máquina/objeto) e NAME_SETOR.",
    "",
    "REGRAS DE INTERPRETAÇÃO:",
    "- Se o usuário pedir **nomes de máquinas / objetos / ativos / equipamentos**, responda usando APENAS os valores distintos de NAME_OBJETO que vieram nos dados.",
    "- NESSA SITUAÇÃO, NÃO use os componentes do JSON (DATA_OC) como se fossem máquinas.",
    "- Só use DATA_OC quando o usuário falar de 'componentes', 'partes', 'itens dentro do objeto', 'atributos do objeto' ou pedir estado/força/volume.",
    #"- Se existir apenas 1 objeto nos dados, diga isso claramente: 'Nos registros há apenas o objeto X'.",
    "",
    "NOMES DE COLUNAS:",
    "- Quando devolver código de gráfico (ggplot), use EXATAMENTE os nomes das colunas como aparecem nos dados.",
    "- Os campos expandidos do JSON são sempre gerados em MAIÚSCULO e com '_' (ex.: PRENSA_ESTADO, BOBINA_VOLUME, CABINE_TEMPERATURA).",
    "- Portanto, **NÃO use nomes em minúsculo** como data_oc_estado ou prensa_estado.",
    "- Se precisar criar um gráfico com componente do JSON, escreva: ggplot(df_ctx, aes(x = DT_HR_LOCAL, y = PRENSA_ESTADO)) ...",
    "",
    "ESTILO DA RESPOSTA:",
    "- Responda de forma direta sobre o que o usuário perguntou (tempo, estado, máquina, setor).",
    "- Use no máximo 4 frases quando for só explicação.",
    "- NÃO reexplicar o que é DATA_OC, nem falar de banco, SQL ou R.",
    #"- Evite começar com 'Os registros mostram...' quando a pergunta for simples; vá direto: 'Nos registros há ...'.",
    "",
    "SE O USUÁRIO PEDIR GRÁFICO:",
    "- ao final, devolva um bloco JSON assim:",
    '{ \"plot_code\": \"ggplot(df_ctx, aes(...)) + ...\", \"title\": \"...\" }',
    "- caso contrário, NÃO devolva o JSON.",
    "",
    "SE O USUÁRIO PEDIR RELATORIO ou DOCUMENTO exemplo, doc, pdf e html:",
    "- DATA_OC é atributos do objeto ou maquina",
    '- DT_HR_LOCAL é Data Hora',
    "- NAME_OBJETO é Objeto",
    "- NAME_SETOR é Setor.",
    "- Seja profissional para criar o relátorio para diretória.",
    "",
    "Contexto atual:",
    ctx_txt,
    sep = "\n"
  )
  
  user_full <- paste(
    "Pergunta original do usuário:\n", user_msg, "\n\n",
    "Dados filtrados (JSON):\n", df_preview
  )
  
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_full)
      )
    ))
  
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    return(list(
      assistant_text = paste("Erro da IA:", httr2::resp_body_string(resp)),
      plot_code = NULL,
      plot_title = NULL
    ))
  }
  
  body <- httr2::resp_body_json(resp)
  txt  <- body$choices[[1]]$message$content
  
  plot_code  <- NULL
  plot_title <- NULL
  
  # Tenta bloco ```json { ... }
  m_triple <- regexpr("```json[\\r\\n]+\\{", txt, perl = TRUE)
  if (m_triple[1] == -1) {
    m_triple <- regexpr("```[rR]?[\\r\\n]+\\{", txt, perl = TRUE)
  }
  if (m_triple[1] != -1) {
    start_json <- regexpr("\\{", substr(txt, m_triple[1], nchar(txt)), perl = TRUE)
    start_json <- m_triple[1] + start_json - 1L
    end_back  <- regexpr("```", substr(txt, start_json, nchar(txt)), fixed = TRUE)
    raw_json <- if (end_back[1] != -1) substr(txt, start_json, start_json + end_back[1] - 2L) else substr(txt, start_json, nchar(txt))
    
    parsed <- tryCatch(jsonlite::fromJSON(raw_json), error = function(e) NULL)
    if (!is.null(parsed) && !is.null(parsed$plot_code)) {
      plot_code  <- parsed$plot_code
      plot_title <- parsed$title %||% "Gráfico"
      assistant_text <- trimws(substr(txt, 1, m_triple[1] - 1L))
      return(list(
        assistant_text = assistant_text,
        plot_code      = plot_code,
        plot_title     = plot_title
      ))
    }
  }
  
  # Fallback: procurar {"plot_code": ...} diretamente
  open_idx <- regexpr("\\{[[:space:]]*\"plot_code\"", txt, perl = TRUE)
  if (open_idx[1] > 0) {
    json_part <- substr(txt, open_idx, nchar(txt))
    json_part <- sub("```.*$", "", json_part)
    json_part <- sub("\\n\\s*$", "", json_part)
    
    parsed <- tryCatch(jsonlite::fromJSON(json_part), error = function(e) NULL)
    if (!is.null(parsed) && !is.null(parsed$plot_code)) {
      plot_code  <- parsed$plot_code
      plot_title <- parsed$title %||% "Gráfico"
      assistant_text <- trimws(substr(txt, 1, open_idx[1] - 1L))
      return(list(
        assistant_text = assistant_text,
        plot_code      = plot_code,
        plot_title     = plot_title
      ))
    }
  }
  
  list(
    assistant_text = txt,
    plot_code      = NULL,
    plot_title     = NULL
  )
}

# =========================================================
# Avalia código de plot retornado usando df_ctx expandido + fallback automático caso o código quebre
# =========================================================
safe_eval_plot <- function(plot_code, df_ctx) {
  # 1) expande JSON para wide
  df_ctx <- expand_data_oc(df_ctx)
  
  # 2) versão longa genérica (para fallback)
  df_ctx_long <- build_df_ctx_long(df_ctx)
  
  # remove cercas ``` que possam vir
  if (grepl("```", plot_code, fixed = TRUE)) {
    plot_code <- gsub("^```[rR]?|```$", "", plot_code)
    plot_code <- gsub("^```json|```$", "", plot_code)
    plot_code <- trimws(plot_code)
  }
  
  # 3) ambiente controlado
  env <- new.env(parent = baseenv())
  env$df_ctx      <- df_ctx
  env$df_ctx_long <- df_ctx_long
  env$ggplot2     <- asNamespace("ggplot2")
  env$dplyr       <- asNamespace("dplyr")
  env$lubridate   <- asNamespace("lubridate")
  env$jsonlite    <- asNamespace("jsonlite")
  env$tidyr       <- asNamespace("tidyr")
  env <- loadNamesGgplot(env)
  
  # 4) tenta rodar exatamente o que veio do modelo
  plt <- tryCatch({
    eval(parse(text = plot_code), envir = env)
  }, error = function(e) {
    # -------------------------
    # FALLBACK DINÂMICO
    # -------------------------
    if (nrow(df_ctx_long)) {
      freq_attr <- df_ctx_long |>
        dplyr::count(ATRIBUTO, sort = TRUE)
      
      if (nrow(freq_attr)) {
        top_attr <- freq_attr$ATRIBUTO[1]
        df_top <- df_ctx_long |>
          dplyr::filter(ATRIBUTO == top_attr)
        
        # Se numérico → linha no tempo
        if (suppressWarnings(!all(is.na(as.numeric(df_top$VALOR))))) {
          df_top$VALOR_NUM <- suppressWarnings(as.numeric(df_top$VALOR))
          return(
            ggplot2::ggplot(df_top, ggplot2::aes(x = DT_HR_LOCAL, y = VALOR_NUM, color = COMPONENTE)) +
              ggplot2::geom_line() +
              ggplot2::labs(title = paste("Evolução de", top_attr), x = "Tempo", y = top_attr) +
              ggplot2::theme_minimal()
          )
        } else {
          # Categórico → barras
          return(
            ggplot2::ggplot(df_top, ggplot2::aes(x = VALOR, fill = COMPONENTE)) +
              ggplot2::geom_bar(position = "dodge") +
              ggplot2::labs(title = paste("Frequência de", top_attr), x = top_attr, y = "Contagem") +
              ggplot2::theme_minimal()
          )
        }
      }
    }
    
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0,
                        label = paste("Erro ao gerar gráfico:", e$message)) +
      ggplot2::theme_void()
  })
  
  plt
}

# =========================================================
# Função principal chamada pelo Shiny (agora devolve df_ctx!)
# =========================================================
#' @export
handle_user_message <- function(user_msg, ctx = NULL) {
  
  # 1) monta SQL com base no contexto
  plan <- gpt_build_sql(user_msg, ctx = ctx)
  if (!is.null(plan$error) && is.null(plan$sql)) {
    return(list(
      items = list(
        list(kind = "text", content = paste("Não consegui montar a consulta:", plan$error))
      ),
      ctx = ctx,
      df_ctx = data.frame()
    ))
  }
  
  sql_txt <- normalize_sql_dt_to_utc(plan$sql)
  
  # 2) roda a SQL no banco
  con    <- dbp$get_pool()
  df     <- NULL
  ok_db  <- TRUE
  err_db <- NULL
  tryCatch({
    df  <- run_user_sql(con, sql_txt)
  }, error = function(e) {
    ok_db <<- FALSE
    err_db <<- e$message
  })
  
  if (!ok_db) {
    return(list(
      items = list(
        list(kind = "text", content = paste("Consulta gerada, mas não consegui executar no banco:", err_db))
      ),
      ctx = ctx,
      df_ctx = data.frame()
    ))
  }
  
  # 2b) atualiza contexto com o que veio do banco
  new_ctx <- infer_ctx_from_df(df, old_ctx = ctx)
  
  # 3) pede pro GPT transformar os dados em resposta operacional
  ans <- gpt_answer_from_rows(user_msg, df, ctx = new_ctx)
  
  items <- list(list(kind = "text", content = ans$assistant_text))
  
  if (!is.null(ans$plot_code)) {
    plt <- safe_eval_plot(ans$plot_code, df %||% data.frame())
    items <- c(items, list(list(kind = "plot", title = ans$plot_title %||% "Gráfico", plot = plt)))
  }
  
  list(
    items = items,
    ctx   = new_ctx,
    df_ctx = df
  )
}
