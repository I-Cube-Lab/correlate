## PANEL FUNCTIONS -------------------------------------------------------------

#' @importFrom graphics plot par hist lines text rect
#' @importFrom grDevices adjustcolor
#' @importFrom stats density
#' @noRd
.panel_diag <- function(x, 
                        diag_labels = NULL,
                        diag_label_text_font = 1,
                        diag_label_text_size = 1,
                        diag_label_text_col = "black",
                        diag_label_text_pad = 0.4,
                        diag_hist_fill = "blue",
                        diag_hist_fill_alpha = 1,
                        diag_hist_border_type = 1,
                        diag_hist_border_width = 1,
                        diag_hist_border_col = "black",
                        diag_density_line_type = 1,
                        diag_density_line_width = 1,
                        diag_density_line_col = "black",
                        diag_border_fill = "white",
                        diag_border_fill_alpha = 1,
                        diag_border_line_type = 1,
                        diag_border_line_width = 1,
                        diag_border_line_col = "black") {
  
  # CHECK DATA
  if(!is.null(dim(x))) {
    x <- x[, 1]
  }
  
  # COMPUTE HISTOGRAM
  h <- hist(
    x,
    plot = FALSE
  )
  
  # EMPTY PLOT
  plot(
    1,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    xlim = range(h$breaks),
    ylim = c(0, max(h$density) + diag_label_text_pad * max(h$density)),
    xlab = "",
    ylab = "",
    bty = "n"
  )
  
  # BORDER
  rect(
    par("usr")[1],
    par("usr")[3],
    par("usr")[2],
    par("usr")[4],
    col = adjustcolor(diag_border_fill, diag_border_fill_alpha),
    border = diag_border_line_col,
    lty = diag_border_line_type,
    lwd = diag_border_line_width
  )
  
  # PLOT HISTOGRAM
  hist(
    x,
    xaxt = "n",
    yaxt = "n",
    main = "",
    col = adjustcolor(diag_hist_fill, diag_hist_fill_alpha),
    lty = diag_hist_border_type,
    lwd = diag_hist_border_width,
    border = diag_hist_border_col,
    prob = TRUE,
    add = TRUE
  )
  
  # COMPUTE DENSITY
  d <- density(
    x,
    bw = abs(diff(h$breaks[1:2]))
  )
  
  # PLOT DENSITY
  lines(
    d,
    col = diag_density_line_col,
    lty = diag_density_line_type,
    lwd = diag_density_line_width
  )
  
  # LABEL
  text(
    mean(par("usr")[1:2]),
    max(h$density) + 0.6 * diag_label_text_pad * max(h$density),
    labels = diag_labels,
    cex = diag_label_text_size,
    font = diag_label_text_font,
    col = diag_label_text_col
  )
  
}

#' @importFrom graphics plot rect par text
#' @importFrom grDevices colorRamp rgb
#' @noRd
.panel_heat <- function(p = NULL,
                        corr = NULL,
                        corr_method = "spearman",
                        heat_col_scale = c("red", "green"),
                        heat_border_fill_alpha = 1,
                        heat_border_line_type = 1,
                        heat_border_line_width = 1,
                        heat_border_line_col = "black",
                        heat_label_text_font = 1,
                        heat_label_text_size = 1,
                        heat_label_text_col = "black",
                        alpha = 0.05) {
  
  # PLOT
  plot(
    1,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    ylab = "",
    xlab = "",
    bty = "n"
  )
  
  # BORDER COLOUR
  heat_border_fill <- colorRamp(heat_col_scale)
  heat_border_fill <- rgb(heat_border_fill((corr - (-1)) / (1 - (-1)))/255)
  
  # BORDER
  if(p > alpha) {
    heat_border_fill <- "white"
  }
  rect(
    par("usr")[1],
    par("usr")[3],
    par("usr")[2],
    par("usr")[4],
    col = adjustcolor(heat_border_fill, heat_border_fill_alpha),
    border = heat_border_line_col,
    lty = heat_border_line_type,
    lwd = heat_border_line_width
  )
  
  # LABELS
  label <- paste0(
    ifelse(corr_method == "pearson", 
           paste0("r = ", round(corr, 2)), 
           paste0("rs = ", round(corr, 2))),
    "\n",
    "p = ",
    .pvalue(p)
  )
  text(
    x = 0,
    y = 0,
    labels = label,
    cex = heat_label_text_size,
    col = heat_label_text_col,
    font = heat_label_text_font
  )
  
}

#' @importFrom stats lm loess
#' @importFrom graphics plot axis lines abline rect points
#' @importFrom grDevices adjustcolor
#' @noRd
.panel_corr <- function(x,
                        corr_point_type = 16,
                        corr_point_size = 1,
                        corr_point_col = "black",
                        corr_point_col_alpha = 1,
                        corr_trend_line_method = "loess",
                        corr_trend_line_smooth = 0.75,
                        corr_trend_line_type = 1,
                        corr_trend_line_width = 1,
                        corr_trend_line_col = "red",
                        corr_axes_sides = c(1,3),
                        corr_axes_text_font = 1,
                        corr_axes_text_size = 1,
                        corr_axes_text_col = "black",
                        corr_border_fill = "white",
                        corr_border_fill_alpha = 1,
                        corr_border_line_type = 1,
                        corr_border_line_width = 1,
                        corr_border_line_col = "black") {
  
  # PLOT
  plot(
    x,
    type = "n",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )
  
  # BORDER
  rect(
    par("usr")[1],
    par("usr")[3],
    par("usr")[2],
    par("usr")[4],
    col = adjustcolor(corr_border_fill, corr_border_fill_alpha),
    border = corr_border_line_col,
    lty = corr_border_line_type,
    lwd = corr_border_line_width
  )
  
  # POINTS
  points(
    x,
    pch = corr_point_type,
    cex = corr_point_size,
    col = adjustcolor(corr_point_col, corr_point_col_alpha),
  )
  
  # AXES
  for(i in corr_axes_sides) {
    if(!is.na(i)) {
      axis(
        side = i,
        font.axis = corr_axes_text_font,
        cex.axis = corr_axes_text_size,
        col.axis = corr_axes_text_col
      )
    }
  }
  
  # LOESS REGRESSION
  if(corr_trend_line_method == "loess") {
    mod <- suppressWarnings(
      loess(
        x[, 2] ~ x[, 1],
        span = corr_trend_line_smooth
      )
    )
    lines(
      x[order(x[, 1]), 1],
      mod$fitted[order(x[, 1])],
      col = corr_trend_line_col,
      lty = corr_trend_line_type,
      lwd = corr_trend_line_width
    )
  # LINEAR REGRESSION
  } else if(corr_trend_line_method == "lm") {
    abline(
      lm(x[, 2] ~ x[, 1]),
      col = corr_trend_line_col,
      lty = corr_trend_line_type,
      lwd = corr_trend_line_width
    )
  # UNSUPPORTED METHOD
  } else {
    stop(
      "Supported trendline methods include 'loess' and 'lm' only!"
    )
  }
  
}

#' @noRd
.panel_dend <- function() {
  
}
