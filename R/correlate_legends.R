## LEGEND FUNCTIONS ------------------------------------------------------------

#' @importFrom graphics par rect
#' @importFrom grDevices colorRamp rgb
#' @noRd
.legend_heat <- function(heat_col_scale = c("red", "green"),
                         legend_side = NULL,
                         legend_border_line_type = 1,
                         legend_border_line_width = 2,
                         legend_border_line_col = "black",
                         legend_text_font = 1,
                         legend_text_size = 2,
                         legend_text_col = "black",
                         margins = 0.5,
                         vars = NULL) {
  
  # CHECK ----------------------------------------------------------------------
  
  # VARIABLES
  if(is.null(vars)) {
    stop(
      "Supply the number of variables to 'vars'."
    )
  } else {
    vars <- length(vars)
  }
  
  # GRAPHICAL PARAMETERS -------------------------------------------------------
  
  # LEGEND OUTSIDE PLOT
  usr <- par("usr")
  par(xpd = NA)
  
  # PLOT LIMITS
  xmin <- usr[1]
  xmax <- usr[2]
  xrng <- xmax - xmin
  ymin <- usr[3]
  ymax <- usr[4]
  yrng <- ymax - ymin
  
  # LEGEND LOCATION ------------------------------------------------------------
  
  # COMPUTE LEGEND LOCATION - LEFT
  if(legend_side == 2) {
    legend_x <- c(
      xmin - (xrng * (vars-1)) - (0.08 * xrng * vars),
      xmin - (xrng * (vars-1)) - (0.08 * xrng * vars) - 0.14 * xrng
    )
    legend_y <- c(
      ymin + 0.2 * vars,
      ymin + (yrng * vars) + (0.075 * yrng * vars) - (0.2 * yrng)
    )
  # COMPUTE LEGEND LOCATION - RIGHT  
  } else if(legend_side == 4) {
    legend_x <- c(
      xmax + 0.1 * xrng,
      xmax + 0.24 * xrng
    )
    legend_y <- c(
      ymin + (0.2 * yrng),
      ymin + (yrng * vars) + (0.075 * yrng * vars) - (0.2 * yrng)
    )
  # LEGEND LOCATION NOT SUPPORTED
  } else {
    stop(
      "Legend must be either on the left or right of the plot."
    )
  }
  
  # LEGEND COLOUR SCALE --------------------------------------------------------
  
  # LEGEND COLOUR SCALE
  legend_col_ramp <- colorRamp(heat_col_scale)
  legend_cols <- seq(0, 1, 1 / 100)
  legend_cols <- legend_col_ramp(legend_cols)
  legend_cols <- rgb(legend_cols[, 1],
                     legend_cols[, 2],
                     legend_cols[, 3],
                     maxColorValue = 255
  )
  
  # LEGEND ---------------------------------------------------------------------
  
  # LEGEND BORDER
  rect(legend_x[1],
       legend_y[1],
       legend_x[2],
       legend_y[2],
       border = legend_border_line_col,
       lwd = legend_border_line_width,
       lty = legend_border_line_type
  )
  
  # LEGEND BOXES
  legend_box_x <- legend_x
  legend_box_y <- seq(
    legend_y[1],
    legend_y[2],
    (legend_y[2] - legend_y[1]) / 100
  )
  lapply(seq_len(100), function(z) {
    rect(legend_box_x[1],
         legend_box_y[z],
         legend_box_x[2],
         legend_box_y[z + 1],
         col = legend_cols[z],
         border = NA
    )
  })
  
  # LEGEND LABELS --------------------------------------------------------------
  
  # LEGEND LABELS 
  legend_labels <- c(-1, 0, 1)
  
  # LEGEND TEXT - Y
  legend_text_y <- c(legend_y[1],
                     legend_y[1] + 0.5 * diff(legend_y),
                     legend_y[2])
  
  # LEGEND TEXT - X
  if(legend_side == 2) {
    legend_text_x <- rep(legend_x[2] + 0.4 * diff(legend_x), 3)
  } else if(legend_side == 4) {
    legend_text_x <- rep(legend_x[2] + 0.4 * diff(legend_x), 3)
  }
  
  # LABELS
  for(i in 1:3) {
    text(legend_text_x[i],
         legend_text_y[i],
         labels = legend_labels[i],
         cex = legend_text_size,
         col = legend_text_col,
         font = legend_text_font)
  }
  
  par(xpd = FALSE)
}