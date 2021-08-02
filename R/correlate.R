## VISUALISE CORRELATION MATRICES ----------------------------------------------

#' Create a correlation matrix
#'
#' @param x a matrix or data.frame containing the data to plot.
#' @param corr type of correlation to perform, set to \code{"spearman"} by
#'   default but can be changed to \code{"pearson"}.
#' @param corr_round number of decimal places to round p values and
#'   correlations, set to 2 decimal places by default.
#' @param corr_col_scale colour scale to use for the heatmap component of the
#'   correlation plot, set to \code{c("red", "green")} by default.
#' @param order column names in the order that they should appear in the plot.
#' @param upper type of plots to include in the upper portion of the panel, set
#'   to \code{"scatter"} by default. This can be changed to \code{"heatmap"} to
#'   flip the plot around the other way.
#' @param labels vector of labels in an order matching the column names of
#'   \code{x} to replace the feature labels on the histogram panels.
#' @param label_text_size size of text in diagonal panels, set to 2 by default.
#' @param label_text_font font for text on diagonal panels, set to 1 by default.
#' @param label_text_col colour for text on diagonal panels, set to
#'   \code{"black"} by default.
#' @param corr_text_size text size for p values and correlation text in heatmap
#'   panels, set to 2 by default.
#' @param corr_text_font font for p values and correlation text in heatmap
#'   panels, set to 1 by default.
#' @param corr_text_col colour for p values and correlation text in heatmap
#'   panels, set to \code{"black"} by default.
#' @param point_size numeric to control the size of points in scatterplots, set
#'   to 1 by default.
#' @param point_col colour to use for points in scatterplots, set to
#'   \code{"black"} by default.
#' @param point_type type of point to plot in scatterplots, set to 16 default.
#' @param title a title for the correlation plot, set to \code{"Correlation
#'   Matrix"} by default.
#' @param title_text_size numeric to control the size of the title text, set to
#'   1 by default.
#' @param title_text_font font to use for title text, set to 1 by default.
#' @param title_text_col colour to use for title text, set to \code{"black"} by
#'   default.
#' @param hist_col colour to use for histograms in diagonal panels, set to
#'   \code{"blue"} by default.
#' @param hist_border_col colour to use for histogram borders in diagonal
#'   panels, set to \code{"black"} by default.
#' @param hist_border_width width of histogram borders in diagonal panels, set
#'   to 1 by default.
#' @param hist_border_type type of border to use use for histograms in diagonal
#'   panels, set to 1 by default.
#' @param density_line_col colour to use for density distributions in diagonal
#'   panels, set to \code{"black"} by default.
#' @param density_line_type type of line to use for density distributions in
#'   diagonal panels, set to 1 by default.
#' @param density_line_width width of lines for density distributions in
#'   diagonal panels, set to 1 by default.
#' @param trend_line_type type of line to use for trendlines, set to 1 by
#'   default.
#' @param trend_line_col colour to use for trendlines, set to \code{"red"} by
#'   default.
#' @param trend_line_width width of trendlines, set to 1 by default.
#' @param margins vector of length 4 to control margins around each plot, set to
#'   \code{c(0.5, 0.5, 0.5, 0.5)} by default.
#' @param frame controls the amount of padding around the outside of the panel,
#'   set to 2 by default.
#' @param alpha cutoff point for p value significance, set to \code{0.05} by
#'   default.
#'
#' @return recorded plot
#'
#' @importFrom graphics par
#' @importFrom grDevices recordPlot
#' @importFrom Hmisc rcorr
#'
#' @export
plot_corr <- function(x,
                      corr = "spearman",
                      corr_round = 2,
                      corr_col_scale = c("red", "green"),
                      order = NULL,
                      upper = "scatter",
                      labels = NULL,
                      label_text_size = 2,
                      label_text_font = 1,
                      label_text_col = "black",
                      corr_text_size = 2,
                      corr_text_font = 1,
                      corr_text_col = "black",
                      point_size = 1,
                      point_col = "black",
                      point_type = 16,
                      title = "Correlation Matrix",
                      title_text_size = 1.5,
                      title_text_col = "black",
                      title_text_font = 2,
                      hist_col = "blue",
                      hist_border_col = "black",
                      hist_border_type = 1,
                      hist_border_width = 1,
                      density_line_col = "black",
                      density_line_type = 1,
                      density_line_width = 1,
                      trend_line_type = 1,
                      trend_line_col = "red",
                      trend_line_width = 1,
                      margins = 0.5,
                      frame = 2,
                      alpha = 0.05) {
  
  # PREPARE DATA ---------------------------------------------------------------
  
  # PREPARE DATA - RECODE NON-NUMERIC DATA
  x <- data.matrix(x)
  
  # ORDER
  if(!is.null(order)) {
    x <- x[, order]
  }
  
  # LABELS 
  if(!is.null(labels)) {
    colnames(x) <- labels
  }
  
  # VARIABLES
  vars <- colnames(x)
  vars_ind <- seq_along(vars)
  
  # PREPARE ARGUMENTS ----------------------------------------------------------
  
  # ARGUMENTS
  args <- as.list(environment())
  
  # REPEAT ARGUMENTS
  args <- structure(
    lapply(names(args), function(z){
      # CORRELATION
      if(grepl("^corr_text", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = sum(seq(length(vars)-1, 1, -1))
          )
        )
      # LABEL_TEXT
      } else if(grepl("^label_text", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = length(vars)
          )
        )
      # POINTS
      } else if(grepl("^point", z, ignore.case = TRUE) |
                grepl("^trend", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = sum(seq(length(vars)-1, 1, -1))
          )
        )
      # HISTOGRAMS
      } else if(grepl("^hist", z, ignore.case = TRUE) |
                grepl("density_line", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = length(vars)
          )
        )
      # OTHER
      } else {
        return(args[[z]])
      }
    }),
    names = names(args)
  )

  # UPDATE ARGUMENTS
  lapply(seq(1,length(args)), function(z){
    assign(names(args)[z], 
           args[[z]], envir = parent.frame(n = 2))
  })
  
  # CORRELATION MATRIX ---------------------------------------------------------
  
  # COMPUTE CORRELATION MATRIX
  x_corr <- rcorr(
    x,
    type = corr
  )
  
  # PREPARE GRAPHICS DEVICE ----------------------------------------------------
  
  # OUTER MARGINS - BASED ON OPTIONS
  oma <- rep(frame, 4)
  if(nzchar(title)) {
    oma[3] <- oma[3] + 3.4
  }
  if(upper == "scatter") {
    oma[4] <- oma[4] + 1
  } else {
    oma[2] <- oma[2] + 1
  }
  
  # CREATE LAYOUT
  par(mfrow = rep(length(vars), 2),
      oma = oma)
  
  # X PARAMETER
  for(z in vars_ind) {
    # X VARIABLE
    x_var <- vars[z]
    # Y PARAMETER
    for(y in vars_ind) {
      # Y PARAMETER
      y_var <- vars[y]
      # DIAGONAL PANELS
      if(z == y) {
        # OUTER MARGINS
        par(mar = rep(margins, 4))
        # HISTOGRAM
        h <- hist(
          x[, z],
          plot = FALSE
        )
        plot(
          1,
          type = "n",
          xaxt = "n",
          yaxt = "n",
          xlim = range(h$breaks),
          ylim = c(0, max(h$density) + 0.3 * max(h$density)), 
          ylab = "",
          xlab = ""
        )
        hist(
          x[, z],
          xaxt = "n",
          yaxt = "n",
          main = "",
          col = hist_col[z],
          lty = hist_border_type[z],
          lwd = hist_border_width[z],
          border = hist_border_col[z],
          prob = TRUE,
          add = TRUE
        )
        # DENSITY
        d <- density(
           x[, z],
           bw = abs(diff(h$breaks[1:2]))
        )
        # PLOT DENSITY
        lines(
          d,
          col = density_line_col[z],
          lty = density_line_type[z],
          lwd = density_line_width[z]
        )
        # TEXT
        text(
          mean(par("usr")[1:2]),
          max(h$density) + 0.15 * max(h$density),
          labels = x_var,
          cex = label_text_size[z],
          font = label_text_font[z],
          col = label_text_col[z]
        )
      # LOWER PANELS
      } else if(z > y) {
        # SCATTER
        if(upper != "scatter") {
          # CREATE SCATTER PLOT
          plot(
            x[, c(y, z)],
            xlab = "",
            ylab = "",
            xaxt = "n",
            yaxt = "n",
            pch = point_type[1],
            cex = point_size[1],
            col = point_col[1]
          )
          # X AXIS
          if(y == 1) {
            axis(
              side = 2
            )
          }
          # Y AXIS
          if(z == length(vars)) {
            axis(
              side = 1
            )
          }
          # LOESS REGRESSION
          if(corr == "spearman") {
            mod <- suppressWarnings(
              loess(x[, z] ~ x[, y])
            )
            lines(
              x[order(x[, y]), y],
              mod$fitted[order(x[, y])],
              col = trend_line_col[1],
              lty = trend_line_type[1],
              lwd = trend_line_width[1]
            )
          # LINEAR REGRESSION
          } else if(corr == "pearson") {
            abline(
              lm(x[, z] ~ x[, y]),
              col = trend_line_col[1],
              lty = trend_line_type[1],
              lwd = trend_line_width[1]
            )
          }
          # REMOVE ARGUMENTS
          point_type <- point_type[-1]
          point_size <- point_size[-1]
          point_col <- point_col[-1]
          trend_line_type <- trend_line_type[-1]
          trend_line_width <- trend_line_width[-1]
          trend_line_col <- trend_line_col[-1]
        # HEATMAP
        } else {
          # CREATE PLOT WITH CORRELATION CIRCLE
          plot(
            1,
            type = "n",
            xaxt = "n",
            yaxt = "n",
            xlim = c(-1, 1),
            ylim = c(-1, 1),
            ylab = "",
            xlab = ""
          )
          # CORRELATION
          corr_val <- round(x_corr$r[x_var, y_var], corr_round)
          # CORRELATION COLOUR
          corr_col <- colorRamp(corr_col_scale)
          corr_col <- rgb(corr_col((corr_val - (-1)) / (1 - (-1)))/255)
          # RECTANGLE
          if(x_corr$P[x_var, y_var] < alpha) {
            rect(
              par("usr")[1],
              par("usr")[3],
              par("usr")[2],
              par("usr")[4],
              col = corr_col
            )
          }
          # LABELS
          corr_label <- paste0(
            ifelse(corr == "pearson", 
                   paste0("r = ", round(x_corr$r[z, y], 2)), 
                   paste0("rs = ", round(x_corr$r[z, y], 2))),
            "\n",
            "p = ",
            .pvalue(x_corr$P[x_var, y_var])
          )
          text(
            x = 0,
            y = 0,
            labels = corr_label,
            cex = corr_text_size[1],
            col = corr_text_col[1],
            font = corr_text_font[1]
          )
          # REMOVE ARGUMENTS
          corr_text_size <- corr_text_size[-1]
          corr_text_col <- corr_text_col[-1]
          corr_text_font <- corr_text_font[-1]
        }
      # UPPER PANELS
      } else if(z < y) {
        # SCATTER
        if(upper == "scatter") {
          # CREATE SCATTER PLOT
          plot(
            x[, c(y, z)], 
            xlab = "",
            ylab = "",
            xaxt = "n",
            yaxt = "n",
            pch = point_type[1],
            cex = point_size[1],
            col = point_col[1]
          )
          # X AXIS
          if(z == 1) {
            axis(
              side = 3
            )
          }
          # Y AXIS
          if(y == length(vars)) {
            axis(
              side = 4
            )
          }
          # LOESS REGRESSION
          if(corr == "spearman") {
            mod <- suppressWarnings(
              loess(x[, z] ~ x[, y])
            )
            lines(
              x[order(x[, y]), y],
              mod$fitted[order(x[, y])],
              col = trend_line_col[1],
              lty = trend_line_type[1],
              lwd = trend_line_width[1]
            )
            # LINEAR REGRESSION
          } else if(corr == "pearson") {
            abline(
              lm(x[, z] ~ x[, y]),
              col = trend_line_col[1],
              lty = trend_line_type[1],
              lwd = trend_line_width[1]
            )
          }
          # REMOVE ARGUMENTS
          point_type <- point_type[-1]
          point_size <- point_size[-1]
          point_col <- point_col[-1]
          trend_line_type <- trend_line_type[-1]
          trend_line_width <- trend_line_width[-1]
          trend_line_col <- trend_line_col[-1]
          # HEATMAP
        } else {
          # CREATE PLOT WITH CORRELATION CIRCLE
          plot(
            1,
            type = "n",
            xaxt = "n",
            yaxt = "n",
            xlim = c(-1, 1),
            ylim = c(-1, 1),
            ylab = "",
            xlab = ""
          )
          # CORRELATION
          corr_val <- round(x_corr$r[x_var, y_var], corr_round)
          # CORRELATION COLOUR
          corr_col <- colorRamp(corr_col_scale)
          corr_col <- rgb(corr_col((corr_val - (-1)) / (1 - (-1)))/255)
          # RECTANGLE
          if(x_corr$P[x_var, y_var] < alpha) {
            rect(
              par("usr")[1],
              par("usr")[3],
              par("usr")[2],
              par("usr")[4],
              col = corr_col
            )
          }
          # LABELS
          corr_label <- paste0(
            ifelse(corr == "pearson", 
                   paste0("r = ", round(x_corr$r[z, y], 2)), 
                   paste0("rs = ", round(x_corr$r[z, y], 2))),
            "\n",
            "p = ",
            .pvalue(x_corr$P[x_var, y_var])
          )
          text(
            x = 0,
            y = 0,
            labels = corr_label,
            cex = corr_text_size[1],
            col = corr_text_col[1],
            font = corr_text_font[1]
          )
          # REMOVE ARGUMENTS
          corr_text_size <- corr_text_size[-1]
          corr_text_col <- corr_text_col[-1]
          corr_text_font <- corr_text_font[-1]
        }
      }
    }
  }
  
  # HEATMAP KEY
  
  # TITLE
  if(nzchar(title)) {
    mtext(
      title,
      line = 2.6,
      font = title_text_font,
      cex = title_text_size,
      col = title_text_col,
      outer = TRUE
    )
  }
  
  # RETURN DATA
  return(
    recordPlot()
  )
}

#' Format P value as character string
#' @noRd
.pvalue <- function(x,
                    round = 2) {
  
  for(i in seq_along(x)) {
    if(x[i] < 0.0001) {
      x[1] <- "<0.001"
    } else if(x[i] < 0.001) {
      x[i] <- "<0.001"
    } else if(x[i] < 0.01) {
      x[i] <- "<0.01"
    } else {
      x[i] <- as.character(round(x[i], round))
    }
  }
  return(x)
}
