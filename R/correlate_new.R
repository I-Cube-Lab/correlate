## NEW CORRELATE PLOTS ---------------------------------------------------------

#' @importFrom grDevices recordPlot
#' @importFrom Hmisc rcorr
#' @export
plot_corr_new <- function(x,
                          order = NULL,
                          upper = "scatter",
                          alpha = 0.05,
                          corr_method = "pearson",
                          corr_round = 2,
                          corr_point_size = 1,
                          corr_point_fill = "white",
                          corr_point_fill_alpha = 1,
                          corr_point_col = "black",
                          corr_point_col_alpha = 1,
                          corr_point_type = 16,
                          corr_trend_line_method = "loess",
                          corr_trend_line_smooth = 0.75,
                          corr_trend_line_type = 1,
                          corr_trend_line_col = "red",
                          corr_trend_line_width = 1,
                          corr_border_fill = "white",
                          corr_border_fill_alpha = 1,
                          corr_border_line_type = 1,
                          corr_border_line_width = 1,
                          corr_border_line_col = "black",
                          corr_axes_text_font = 1,
                          corr_axes_text_size = 1,
                          corr_axes_text_col = "black",
                          heat_col_scale = c("red", "green"),
                          heat_text_font = 1,
                          heat_text_size = 1,
                          heat_text_col = "black",
                          heat_border_fill_alpha = 1,
                          heat_border_line_type = 1,
                          heat_border_line_width = 1,
                          heat_border_line_col = "black",
                          diag_labels = NA,
                          diag_label_text_font = 1,
                          diag_label_text_size = 1,
                          diag_label_text_col = "black",
                          diag_label_text_pad = 0.3,
                          diag_hist_fill = "blue",
                          diag_hist_fill_alpha = 1,
                          diag_hist_border_col = "black",
                          diag_hist_border_type = 1,
                          diag_hist_border_width = 1,
                          diag_density_line_col = "black",
                          diag_density_line_type = 1,
                          diag_density_line_width = 1,
                          diag_border_fill = "white",
                          diag_border_fill_alpha = 1,
                          diag_border_line_type = 1,
                          diag_border_line_width = 1,
                          diag_border_line_col = "black",
                          title = "Correlation Matrix",
                          title_text_font = 2,
                          title_text_size = 1.5,
                          title_text_col ="black",
                          margins = c(0.5, 0.5, 0.5, 0.5),
                          outer_margins = 2,
                          legend_border_line_type = 1,
                          legend_border_line_width = 2,
                          legend_border_line_col = "black",
                          legend_text_font = 1,
                          legend_text_size = 2,
                          legend_text_col = "black") {
  
  # RESET GRAPHICAL PARAMETERS -------------------------------------------------
  
  old_pars <- par(c("mfrow","oma", "mar"))
  on.exit({
    par(old_pars)
  })
  
  # PREPARE DATA ---------------------------------------------------------------
  
  # PREPARE DATA - RECODE NON-NUMERIC DATA
  x <- data.matrix(x)
  
  # LABELS 
  diag_labels <- rep(diag_labels, length.out = ncol(x))
  colnames(x)[!is.na(diag_labels)] <- diag_labels[!is.na(diag_labels)]
  diag_labels <- colnames(x)
  
  # ORDER
  if(!is.null(order)) {
    x <- x[, order]
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
      # HEATMAP LABEL TEXT
      if(grepl("^heat_text", z, ignore.case = TRUE) |
         grepl("^heat_border", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = sum(seq(length(vars)-1, 1, -1))
          )
        )
      # SCATTER POINTS, TRENDLINE & BORDER
      } else if(grepl("^corr_point", z, ignore.case = TRUE) |
                grepl("^corr_trend", z, ignore.case = TRUE) |
                grepl("^corr_border", z, ignore.case = TRUE)) {
        return(
          rep(
            args[[z]],
            length.out = sum(seq(length(vars)-1, 1, -1))
          )
        )
      # DIAGONAL PANELS - HISTOGRAMS, DENSITY & LABELS
      } else if(grepl("^diag", z, ignore.case = TRUE)) {
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
  
  # REMOVE X FROM ARGUMENTS
  args <- args[!names(args) %in% "x"]
  
  diag_args <- purrr::transpose(
    lapply(args[names(args) %in% formalArgs(".panel_diag")], function(z){
      split(rep(z, length.out = length(vars)), 1:length(vars))
    })
  )
  heat_args <- purrr::transpose(
    lapply(args[names(args) %in% formalArgs(".panel_heat")], function(z){
      split(rep(z, length.out = sum(seq(length(vars)-1, 1, -1))), 
            1:sum(seq(length(vars)-1, 1, -1)))
    })
  )
  corr_args <- purrr::transpose(
    lapply(args[names(args) %in% formalArgs(".panel_corr")], function(z){
      split(rep(z, length.out = sum(seq(length(vars)-1, 1, -1))), 
            1:sum(seq(length(vars)-1, 1, -1)))
    })
  )
  
  # CORRELATION MATRIX ---------------------------------------------------------
  
  # COMPUTE CORRELATION MATRIX
  x_corr <- rcorr(
    x,
    type = corr_method
  )
  
  # PREPARE GRAPHICS DEVICE ----------------------------------------------------
  
  # OUTER MARGINS - BASED ON OPTIONS
  oma <- rep(outer_margins, 4)
  if(upper == "scatter") {
    # AXIS
    oma[4] <- oma[4] + 1.2
    oma[3] <- oma[3] + 1.2
    # TITLE
    if(nzchar(title)) {
      oma[3] <- oma[3] + 3.5
    }
    # LEGEND
    oma[2] <- oma[2] + 4
  } else {
    # AXIS
    oma[1] <- oma[1] + 1.2
    oma[2] <- oma[2] + 1.2
    # TITLE
    if(nzchar(title)) {
      oma[3] <- oma[3] + 1.8
    }
    # LEGEND
    oma[4] <- oma[4] + 4
  }

  # CREATE LAYOUT
  par(mfrow = rep(length(vars), 2),
      oma = oma,
      mar = rep(margins, length.out = 4))
  
  # PLOT CORRELATION MATRIX ----------------------------------------------------
  
  for(z in vars_ind) {
    x_var <- vars[z]
    for(y in vars_ind) {
      y_var <- vars[y]
      # DIAGONAL PANEL
      if(z == y) {
        do.call(".panel_diag", 
                c(
                  list("x" = x[, z]),
                  diag_args[[1]]
                )
        )
        diag_args <- diag_args[-1]
      # LOWER PANELS
      } else if(z > y) {
        # HEATMAP
        if(upper == "scatter") {
          do.call(
            ".panel_heat",
            c(
              list("p" = x_corr$P[x_var, y_var],
                   "corr" = x_corr$r[x_var, y_var]),
              heat_args[[1]]
            )
          )
          heat_args <- heat_args[-1]
        # SCATTER
        } else {
          # AXES SIDES
          corr_args[[1]][["corr_axes_sides"]] <- c(NA, NA)
          # Y AXIS
          if(y == 1) {
            corr_args[[1]][["corr_axes_sides"]][2] <- 2
          }
          # X AXIS
          if(z == length(vars)) {
            corr_args[[1]][["corr_axes_sides"]][1] <- 1
          }
          # SWAP X & Y
          do.call(
            ".panel_corr",
            c(
              list("x" = x[, c(y_var, x_var)]),
              corr_args[[1]]
            )
          )
          corr_args <- corr_args[-1]
        }
      # UPPER PANELS
      } else {
        # SCATTER
        if(upper == "scatter") {
          # AXES SIDES
          corr_args[[1]][["corr_axes_sides"]] <- c(NA, NA)
          # X AXIS
          if(z == 1) {
            corr_args[[1]][["corr_axes_sides"]][1] <- 3
          }
          # Y AXIS
          if(y == length(vars)) {
            corr_args[[1]][["corr_axes_sides"]][2] <- 4
          }
          # SWAP X & Y
          do.call(
            ".panel_corr",
            c(
              list("x" = x[, c(x_var, y_var)]),
              corr_args[[1]]
            )
          )
          corr_args <- corr_args[-1]
        # HEATMAP
        } else {
          # HEATMAP
          do.call(
            ".panel_heat",
            c(
              list("p" = x_corr$P[x_var, y_var],
                   "corr" = x_corr$r[x_var, y_var]),
              heat_args[[1]]
            )
          )
          heat_args <- heat_args[-1]
        }
      }
    }
  }
  
  # LEGENDS --------------------------------------------------------------------
  
  # COLOUR SCALE LEGEND
  do.call(
    ".legend_heat",
    c(
      list("legend_side" = ifelse(upper =="scatter", 2, 4)),
      args[names(args) %in% formalArgs(".legend_heat")]
    )
  )
  
  # TITLE ----------------------------------------------------------------------
  
  # TITLE
  if(nzchar(title)) {
    mtext(
      title,
      line = ifelse(upper == "scatter",
                    2.6,
                    0.8),
      font = title_text_font,
      cex = title_text_size,
      col = title_text_col,
      outer = TRUE
    )
  }
  
  # RETURN PLOT ----------------------------------------------------------------
  
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
