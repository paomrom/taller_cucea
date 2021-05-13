rm(list=ls())

#Librerías
library(readxl)

library(shiny)
library(shinythemes)

library(plotly)

library(dplyr)

runExample()

df <- read_excel("deuda2.xlsx")

df %>% names()
names(df)[names(df) == "aÒo"] <- "año"
names(df)[names(df) == "codigo"] <- "abreviatura"


# Interfas del usuario
ui <- navbarPage("Deuda externa mundial",
                 theme = shinytheme("united"),
                 tabPanel("Mapa",
                          h2(aling= "center", "Mapa mundial de deuda externa"),
                          h4(align = "justify","El mapa interactivo muestra la deuda extena por países de acuerdo al Banco Mundial"),
                          mainPanel(plotlyOutput("plot", width = "1200px"))
                 ),
                 tabPanel("Tabla",
                          downloadButton('downloadData', 'Download'),
                          DT::dataTableOutput("table1"))
)

# Server
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    #Set country boundaries as light grey
    l <- list(color = toRGB("#d9b779"), width = 0.5)
    #Specify map projection and options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'orthographic'),
      resolution = '100',
      showcountries = TRUE,
      countrycolor = '#d1a547',
      showocean = TRUE,
      oceancolor = '#3fa8d4',
      showlakes = TRUE,
      lakecolor = '#1fa8e0',
      showrivers = F,
      rivercolor = '#24AAEC')
    
    p <- plot_geo(df) %>%
      add_trace(z = ~deuda, color = ~deuda, colors = 'Oranges',
                text = ~pais, locations = ~abreviatura, marker = list(line = l)) %>%
      colorbar(title = 'Monto de deuda') %>%
      layout(title = '', geo = g)
    
  })
  output$table1 <- DT::renderDataTable({
    df
  }, filter='top', 
  options = list(pageLength = 10, scrollX=TRUE, autoWidth = TRUE))
  
  ## Download Buttons ----
  output$downloadData <- downloadHandler(
    filename = 'Download.csv',
    content = function(file) {
      write.csv(Data[input[["table1_rows_all"]],], file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
