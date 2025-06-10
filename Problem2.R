library(shiny)
library(DT)

ur <- fluidPage(
    DTOutput("table")
)

server <- function(input, output) {
    track <- read.csv("https://raw.githubusercontent.com/EricBrownTTU/ISQS5346/main/pgs.csv")
    
    output$table <- renderDT({
        datatable(track)
    })
    
}