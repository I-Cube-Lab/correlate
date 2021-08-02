## SHINY APPLICATION -----------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(
    type="text/css",
    "#corr img {max-width: 100%; width: 100%; height: auto}"
  )),
  mainPanel(
    dataInputUI("input1"),
    imageOutput("corr",
                height = "auto")
  )
)

server <- function(input, output, session) {
  
  data <- dataInputServer(
    "input1"
  )
  
  output$corr <- renderImage({
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    png(outfile, 
        width = 12, 
        height = 12,
        units = "in",
        res = 300)
    plot_corr(data())
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
}

shinyApp(ui, server)
  
