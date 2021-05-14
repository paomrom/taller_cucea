rm(list=ls())

#Librerías
library(readxl)
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(DT)



df <- read_excel("inflacion.xlsx")

df %>% names()
names(df)[names(df) == "aÒo"] <- "año"
names(df)[names(df) == "codigo"] <- "abreviatura"

#dput(ranked_by_inf_top10$pais)

df %>%
    select(pais, inflacion, abreviatura, continente, region) %>%
    filter(inflacion!=0) %>% 
    arrange( -as.numeric(inflacion)) %>%
    mutate(rank = 1:n())->  ranked_by_inf


ranked_by_inf_top10<-ranked_by_inf %>%
    filter(rank<=10)%>% 
    mutate(pais=factor(pais,
                       levels=c("Argentina",  "Angola",     "Uzbekistan", "Sudan",     
                                "Surinam",    "Haiti",      "Turquia",    "Egipto",    
                                "Etiopia",    "Nigeria")))


ranked_by_inf_less<-ranked_by_inf %>%
    filter(rank>=165) %>% 
    mutate(pais=factor(pais,
                       levels=c("Emiratos Arabes Unidos", "Burkina Faso",          
                                "Brunei",                 "Oman",                  
                                "Liberia",                "Catar",                 
                                "Zimbabue",               "Kuwait",                
                                "Guinea Ecuatorial",      "Guinea Bissau")))


############### Interfas del usuario###############
ui <- navbarPage("inflacion mundial",
                 theme = shinytheme("united"),
                 tabPanel("Inicio", icon = icon("home"),
                          h4("De acuerdo con el Banxico la inflación es un fenómeno que se observa en la economía de un país y está relacionado con el aumento
                          desordenado de los precios de la mayor parte de los bienes y servicios que se comercian en sus mercados, 
                          por un periodo de tiempo prolongado.

Cuando hay inflación en una economía, es muy difícil distribuir nuestros ingresos, planear un viaje, pagar nuestras deudas o invertir
                             en algo rentable, ya que los precios, que eran una referencia para asignar nuestro dinero de la mejor 
                             manera posible, están distorsionados.", align = "justify"),
                          h4("La inflación es el incremento, a nivel general, de los precios y, la deflación, es todo lo contrario, 
                          es decir, cuando el nivel general de los precios disminuye notablemente y de forma prolongada en el tiempo.
                          Ni una ni otra es buena o mala, sino que lo ideal es encontrar una estabilidad en los precios, es decir, 
                          en el caso de que aumente el valor del dinero, aumente también el nivel de precios. Los encargados de mantener 
                             esta estabilidad, son los Bancos Centrales de los diferentes países, que dedican todos sus esfuerzos a que los precios ni sufran subidas ni bajadas significativas.", align = "justify"),
                          plotlyOutput("rank1"),
                          h4("El gráfico de barra invertido muestra el nivel de inflación o deflación, se debe puntualizar que hay países como Venezuela, Corea del Norte, Cuba entre otro no cuenta con información pública de su nivel de precio",align = "justify"),
                          h5("Este proyecto de visualización de datos se realizó con la paquetería de Shiny en lenguaje R con datos abiertos las estimaciones de gapmider del Banco Mundial.
              Está abierto a todo público, encontrará la documentación, bases de datos y scripts en el siguiente",
                             tags$a(target="_blank",href="https://github.com/paomrom/taller_cucea","repositorio de GitHub")," con licencia de código abierto. Agradecemos sus comentarios.",br(), 
                             strong("Gracias por su visita."),align = "center")),
                 
                 navbarMenu("Visualizaciones",
                            tabPanel("Gráficos",
                          h2(aling= "center", "Mapa mundial de inflacion"),
                          h4(align = "justify","El mapa interactivo muestra la inflacion por países de acuerdo a las estimaciones de gapminder"),
                          sliderInput(inputId = "rank",
                                      label = "Selecciona el nivel del rango",
                                      min = min(ranked_by_inf$rank),
                                      max = max(ranked_by_inf$rank),
                                      value = 170),
                          submitButton(text = "Create my plot!"),
                          plotOutput(outputId = "lineal"),
                          plotlyOutput("top10"),
                          plotlyOutput("less10")),
                          tabPanel("Mapa",
                plotlyOutput("plot", width = "1200px"),
                )),
                tabPanel("Descarga",icon = icon("fas fa-cloud-download-alt"),
                         downloadButton('downloadData', 'Download'),
                         DT::dataTableOutput("table1"))
)
                 

################################### Server ###########################################
server <- function(input, output) {
    
 
    output$lineal <- renderPlot({
        

    g<- ggplot(ranked_by_inf)+
        aes(x =rank, y= inflacion) +
        geom_line(size = 0.5, colour = "#112446")+
        theme_minimal()
    g
    })
    ggplot(ranked_by_inf) +
        aes(x = rank, y = inflacion) +
        geom_line() +
        theme_minimal()
    
   
    
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
            add_trace(z = ~inflacion, color = ~inflacion, colors = 'Oranges',
                      text = ~pais, locations = ~abreviatura, marker = list(line = l)) %>%
            colorbar(title = 'Inflacion') %>%
            layout(title = '', geo = g)
        
    })
    output$top10 <- renderPlotly({
        
        top<-ggplot(ranked_by_inf_top10) +
            aes(x = pais, weight=inflacion) +
            geom_bar(position = "dodge", fill = "#D08326") +
            labs(
                x = "",
                y = "",
                title = "Top 10 de países con mayor y menor inflacion en el 2019",
                caption = "Con datos de gapminder") +
            theme_minimal()+
            theme(axis.ticks.y= element_blank(),
                  axis.text.y = element_blank())
        ggplotly(top, tooltip=c("pais","inflacion"), dynamicTicks = TRUE)
    })
    
    output$rank1 <- renderPlotly({
        rank<-ggplot(ranked_by_inf, aes(rank, group = continente,
                                        fill = as.factor(continente), color = as.factor(continente))) +
            geom_tile(aes(y = inflacion/2,
                          height = inflacion,
                          width = 0.9), alpha = 0.8, color = NA) +
            scale_fill_manual(values=c("#d03f26", "#d06126", "#d08326",	"#d0a526","#d0c726"))+
            coord_flip(clip = "off", expand = FALSE) +
            scale_y_continuous(labels = scales::comma) +
            guides(color = FALSE, fill = FALSE) +
            labs(title='', x = "", y = " ") +
            scale_x_reverse()+
            theme_minimal()+
            theme(axis.ticks.y= element_blank(),
                  axis.text.y = element_blank())
        
        ggplotly(rank,tooltip=c("continente", "rank"), dynamicTicks = TRUE)
    })
    
    output$less10 <- renderPlotly({
        
        less<-ggplot(ranked_by_inf_less) +
            aes(x = pais, weight=inflacion) +
            geom_bar(position = "dodge", fill = "#D08326") +
            labs(
                x = "",
                y = "",
                title = "",
                caption = "Con datos de gapminder") +
            theme_minimal()+
            theme(axis.ticks.y= element_blank(),
                  axis.text.y = element_blank())
        ggplotly(less)
    })
    
    output$tabletop = renderDataTable({
        ranked_by_inf_top10})
    

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
