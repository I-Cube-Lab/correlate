#' @importFrom graphics par
#' @importFrom grDevices recordPlot
#' @importFrom plotrix draw.circle
#' @export
plot_corr <- function(x,
                      corr = "spearman",
                      corr_round = 2,
                      corr_col_scale = c("red", "blue"),
                      corr_border_col = "black",
                      corr_border_type = 1,
                      corr_border_width = 1,
                      label_text_size = 2,
                      label_text_font = 1,
                      label_text_col = "black",
                      point_size = 1,
                      point_col = "black",
                      point_type = ".",
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
                      margins = 0.5,
                      frame = 2,
                      p = 0.05) {
  
  
  # ONLY DISPLAY IF P<0.05
  
  # PREPARE DATA - RECODE NON-NUMERIC DATA
  x <- data.matrix(x)
  
  # VARIABLES
  vars <- colnames(x)
  vars_ind <- seq_along(vars)
  
  # PREPARE ARGUMENTS
  args <- as.list(environment())
  
  # REPEAT ARGUMENTS
  args <- structure(
    lapply(names(args), function(z){
      # CORRELATION
      if(grepl("^corr_border", z, ignore.case = TRUE)) {
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
      } else if(grepl("^point", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = sum(seq(length(vars)-1, 1, -1))
          )
        )
      # HISTOGRAMS
      } else if(grepl("^hist", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = length(vars)
          )
        )
      # DENSITY
      } else if(grepl("^density_line", z, ignore.case = TRUE)) {
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
  
  # COMPUTE CORRELATION MATRIX
  x_corr <- cor(x,
                method = corr)
  
  # OUTER MARGINS
  oma <- rep(frame, 4)
  if(nzchar(title)) {
    oma[3] <- oma[3] + 3.4
  }
  oma[4] <- oma[4] + 1
  
  # CREATE LAYOUT
  par(mfrow = rep(length(vars), 2),
      oma = oma)
  
  # X PARAMETER
  lapply(vars_ind, function(z){
    # X VARIABLE
    x_var <- vars[z]
    # Y PARAMETER
    lapply(vars_ind, function(y) {
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
        hist(
          x[, z],
          xaxt = "n",
          yaxt = "n",
          main = "",
          col = hist_col[z],
          lty = hist_border_type[z],
          lwd = hist_border_width[z],
          border = hist_border_col[z],
          ylim = c(0, max(h$density) + 0.3 * max(h$density)),
          prob = TRUE
        )
        # DENSITY
        d <- density(
           x[, z]
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
        # OUTER MARGINS
        # par(oma = rep(0.5, 4))
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
        corr <- round(x_corr[x_var, y_var], corr_round)
        # CORRELATION COLOUR
        corr_col <- colorRamp(corr_col_scale)
        corr_col <- corr_col((corr - (-1)) / (1 - (-1)))
        # DRAW CIRCLE
        draw.circle(
          0,
          0,
          radius = abs(corr)/2,
          col = corr_col,
          border = corr_border_col[1],
          lty = corr_border_type[1],
          lwd = corr_border_width[1]
        )
        # REMOVE ARGUMENTS
        assign("corr_border_col", 
               corr_border_col[-1], 
               envir = parent.frame(4))
        assign("corr_border_type", 
               corr_border_type[-1], 
               envir = parent.frame(4))
        assign("corr_border_width", 
               corr_border_width[-1], 
               envir = parent.frame(4))
      # UPPER PANELS
      } else if(z < y) {
        # CREATE SCATTER PLOT
        plot(
          x[, c(z,y)],
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
            side = 3,
          )
        }
        # Y AXIS
        if(y == length(vars)) {
          axis(
            side = 4
          )
        }
        # REMOVE ARGUMENTS
        assign("point_type", 
               point_type[-1], 
               envir = parent.frame(4))
        assign("point_size", 
               point_size[-1], 
               envir = parent.frame(4))
        assign("point_col", 
               point_col[-1], 
               envir = parent.frame(4))
      }
    })
  })
  
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
  
  recordPlot()
}