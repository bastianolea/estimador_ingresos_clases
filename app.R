library(shiny)
library(htmltools)
library(bslib)
library(shades)

# crear colores a partir de un color base
color_base = "#1FBE9A"
color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
color_detalle = color_base |> brightness(delta(-0.3)) |> saturation(delta(-0.3)) |> as.character()
color_texto = color_base |> chroma(55) |> lightness(95) |> as.character()

# # previsualizar colores
# swatch(c(color_fondo,
#        color_texto,
#        color_detalle,
#        color_principal), bg = "#181818")

ui <- fluidPage(
    title = "Estimador de ingresos por clases particulares", lang = "es",
    
    theme = bs_theme(
        bg = color_fondo,
        fg = color_texto,
        primary = color_principal,
        font_scale = 1.3,
        base_font = font_google("Pacifico")),
    
    # tamaño del texto de los sliders
    tags$style(".irs-min, .irs-max, .irs-single { font-size: 60% !important ;}"),
    
    div(style = css(max_width = "300px", 
                    margin = "auto", margin_top = "60px"),
        fluidRow(
            column(12,
                   
                   h1("Clases particulares", 
                      style = css(color = color_detalle)),
                   br(),
                   
                   sliderInput("precio",
                               label = "Precio por clase",
                               min = 3000, max = 30000, 
                               value = 18000, step = 1000,
                               ticks = F, sep = ".", pre = "$") |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("duracion",
                               label = "Duración de clases",
                               min = 1, max = 3, 
                               value = 1.5, step = 0.1,
                               ticks = F, sep = ",", post = " hrs.") |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("alumnos",
                               label = "Alumnxs",
                               min = 1, max = 20,
                               value = 5, 
                               ticks = F) |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("clases_semanales",
                               label = "Clases por alumnx a la semana",
                               min = 0, max = 3, 
                               value = 1, step = 0.1, 
                               ticks = F) |> 
                       div(style = css(margin_bottom = "30px")),
            )
        ),
        
        fluidRow(
            column(12, 
                   style = css(padding = "18px",
                               background_color = color_fondo, 
                               border = paste("3.5px", color_detalle, "solid"), 
                               margin_top = "24px",
                               padding_bottom = "0px",
                               text_align = "center",
                               color = color_texto,
                               border_radius = "6px"),
                   
                   div(style = css(font_size = "130%"),
                       div(style = css(margin_bottom = "-10px"),
                           strong("Ingreso mensual:")),
                       
                       p("$", textOutput("texto", inline = T))
                   ),
                   
                   div(style = css(font_size = "90%", 
                                   font_style = "italic",
                                   margin_top = "-16px",
                                   color = color_detalle),
                       p(textOutput("horas", inline = T), "horas semanales,",
                         textOutput("clases_por_dia", inline = T), "x día")
                   )
            )
        ),
        
        # firma
        fluidRow(
            column(12,
                   style = css(margin_top = "50px",
                               margin_bottom = "64px",
                               border_radius = "12px",
                               background_color = color_fondo |> brightness(delta(-0.025)),
                               padding = "16px",
                               padding_bottom = "0px",
                               font_size = "60%", 
                               font_family = "Tahoma",
                               opacity = "50%"),
                   markdown("App desarrollada en R por [Bastián Olea Herrera.](https://bastianolea.github.io/shiny_apps/) 
                            [Código de fuente en GitHub.](https://github.com/bastianolea/estimador_ingresos_clases)")
            )
        )
    )
)

server <- function(input, output, session) {
    
    ingreso_mensual <- reactive({
        
        clases_mes = input$clases_semanales * 4
        
        clases_totales_mes = input$alumnos * clases_mes
        
        ingreso_mensual = clases_totales_mes * input$precio
        
        return(ingreso_mensual)
    })
    
    horas_semanales <- reactive({
        (input$clases_semanales * input$alumnos) * input$duracion
    })
    
    output$texto <- renderText({
        format(ingreso_mensual(), 
               big.mark = ".", decimal.mark = ",",
               scientific = FALSE)
    })
    
    output$horas <- renderText(format(horas_semanales(), big.mark = ".", decimal.mark = ","))
    
    output$clases_por_dia <- renderText(round(horas_semanales()/5, 0))
}

shinyApp(ui = ui, server = server)