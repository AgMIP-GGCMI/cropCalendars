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
  baseData <- map_data("world")
  #baseData <- map('world', add=TRUE, wrap=c(-180,180), interior = FALSE)

  # set coord limits to current global land extent
  if (is.null(xlim)) { xlim <- c(-159.75, 179.75) }
  if (is.null(ylim)) { ylim <- c(-55.75, 64.75) }

  # Create the plot
  p <- ggplot(dfMap, aes(x = lon, y = lat)) + theme_bw()
  p <- p + theme(plot.title = element_text(size = rel(1.5)))
  # Draw map background and borders (set landFill to desired color)
  p <- p + geom_polygon(data = baseData, aes(x = long, y = lat, group = group),
                        colour="black", fil = landFill, alpha = 1,
                        linetype = 1, size = 0.01)
  # Display gridded data
  p <- p + geom_raster(aes_string(group = Value, fill = Value))

  if (!is.na(color_scale[1])) {
    # Set color scale
    if (scale_type == "categorical"){
      p <- p + scale_fill_manual(..., values = color_scale, drop=FALSE)
      # note: drop=F displays all levels
    } else {
      p <- p + scale_fill_gradientn(name = legendTitle, oob=squish, colors = color_scale, ...)  #oob=squish to constrain scale limits if declared
    }
  }

  # Overlay map borders
  p <- p + geom_polygon(data = baseData, aes(x = long, y = lat, group = group),
                        colour="black", fill = "white", alpha = 0,
                        linetype = 1, size = 0.01)
  # Crop map to coordinate limits
  p <- p + coord_fixed(ratio = 1.3, xlim = xlim, ylim = ylim, expand = 0)
  # Display title
  p <- p + labs(title  =paste(mainTitle, "\n", sep = ""), x = "", y = "")
  p <- p + theme(plot.title = element_text(size = 13))

  # legend position
  p <- p + theme(legend.position = legendPos)
  if (legendPos == "bottom" & scale_type == "continuous") {
    p <- p + guides(fill = guide_colorbar(barwidth = 12, title.position = "top"))
  }
  return(p)
}
