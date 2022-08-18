#' @title Plot maps of calcCropCalendar outputs
#'
#' @description This function generates a pdf file with all the relevant maps
#' to visually check the results of calculated crop calendars.
#'
#' @param fnDT file name of the output data.table
#' @param fnPDF file name of the PDF file
#'
#' @export

plotMapCropCalendars <- function(fnDT  = NULL,
                                 fnPDF = NULL
) {

  # Read RData crop calendar file
  DT <- get(load(fnDT))

  # Create pdf and plot maps of relevant columns
  pdf(fnPDF, width = a4w, height = a4h * 0.4)

  p <- plotMap_ggplot(DT,
                      Value = "seasonality_type",
                      scale_type = "categorical",
                      color_scale = seasonality_types_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "sowing_season",
                      scale_type = "categorical")
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "sowing_month",
                      color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "sowing_doy",
                      color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "maturity_doy",
                      color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "growing_period",
                      color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                          Value = "harvest_rule",
                          scale_type = "categorical",
                          color_scale = harvest_rule_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)

  p <- plotMap_ggplot(DT,
                      Value = "harvest_reason",
                      scale_type = "categorical",
                      color_scale = harvest_reason_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)

  dev.off()
}

