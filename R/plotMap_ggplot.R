# Map gridded data from dataframe (lon, lat, Value)
plotMap_ggplot <- function (dfMap       = NULL,
                            Value       = NULL,
                            landFill    = "white",
                            xlim        = NULL,
                            ylim        = NULL,
                            mainTitle   = Value,
                            legendTitle = Value,
                            legendPos   = "bottom",
                            scale_type  = "continuous",
                            color_scale = NA,
                            ...) {
  # get world map
  baseData <- ggplot2::map_data("world")

  # set coord limits to current global land extent
  if (is.null(xlim)) { xlim <- c(-159.75, 179.75) }
  if (is.null(ylim)) { ylim <- c(-55.75, 64.75) }

  # Create the plot
  p <- ggplot2::ggplot(dfMap, ggplot2::aes(x = lon, y = lat)) + ggplot2::theme_bw()
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
  # Draw map background and borders (set landFill to desired color)
  p <- p + ggplot2::geom_polygon(data = baseData,
                                 ggplot2::aes(x = long, y = lat, group = group),
                                 colour = "black", fill = landFill, alpha = 1,
                                 linetype = 1, linewidth = 0.01)
  # Display gridded data
  p <- p + ggplot2::geom_raster(ggplot2::aes(fill = .data[[Value]]))

  if (!is.na(color_scale[1])) {
    # Set color scale
    if (scale_type == "categorical"){
      p <- p + ggplot2::scale_fill_manual(..., values = color_scale, drop = FALSE)
      # note: drop=F displays all levels
    } else {
      p <- p + ggplot2::scale_fill_gradientn(name = legendTitle, oob = scales::squish,
                                             colors = color_scale, ...)  #oob=squish to constrain scale limits if declared
    }
  }

  # Overlay map borders
  p <- p + ggplot2::geom_polygon(data = baseData,
                                 ggplot2::aes(x = long, y = lat, group = group),
                                 colour = "black", fill = "white", alpha = 0,
                                 linetype = 1, linewidth = 0.01)
  # Crop map to coordinate limits
  p <- p + ggplot2::coord_fixed(ratio = 1.3, xlim = xlim, ylim = ylim, expand = 0)
  # Display title
  p <- p + ggplot2::labs(title = paste(mainTitle, "\n", sep = ""), x = "", y = "")
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = 13))

  # legend position
  p <- p + ggplot2::theme(legend.position = legendPos)
  if (legendPos == "bottom" & scale_type == "continuous") {
    p <- p + ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 12, title.position = "top"))
  }
  return(p)
}
