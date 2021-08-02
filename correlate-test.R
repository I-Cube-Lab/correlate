library(correlate)
library(DataEditR)
rdata <- data_edit(read_args = list(check.names = FALSE, row.names = 1))
plot_corr(rdata,
          point_type = 16, 
          upper = "heatmap",
          corr = "pearson",
          trend_line_type = 2,
          corr_col_scale = c("blue", "green", "yellow", "red"))

rdata <- data_edit("Predict 4T1 mass RF top 5 feature for corrplot.csv",
                   read_args = list(check.names = FALSE, row.names = 1))
rdata

png("test.png",
    width = 20,
    height = 20,
    units = "in",
    res = 300)
plot_corr(rdata,
          point_size = 2,
          point_type = 16,
          label_text_size = 1.2)
dev.off()

pdf("test.pdf",
    width = 20,
    height = 20)
plot_corr(rdata,
          point_size = 2,
          point_type = 16,
          label_text_size = 1.2)
dev.off()