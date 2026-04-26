tab_costo_ui <- function() {
  tabItem(tabName = "tab_costo",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
              solidHeader = TRUE, width = 100,status = "info" ),
          br(),
          tags$h2("Análisis de consumo: Energía activa", style = "text-align:center; font-weight:bold;"),
          h3(strong("Información general")),
          br(),
          fluidRow(
            valueBoxOutput("bloque_box"),
            valueBoxOutput("fecha1_box"),
            valueBoxOutput("fecha2_box")
          ),
          br(),
          h3(strong("Comportamiento del consumo total de energía activa (Kw-h) diario por cada mes")),
          br(),
          h4("La siguiente gráfica muestra el total de energía activa consumida diariamente por todos los
              bloques en análisis para el mes que sea seleccionado..."),
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput("selector_mes", "Seleccione el mes a analizar:", 
                          choices = c("enero", "febrero", "marzo","abril","mayo","junio",
                                      "julio","agosto","septiembre", "octubre", "noviembre", "diciembre")),
              fluidRow(infoBoxOutput("prom1"), " Energía activa (Kw-h) promedio*:"),
              fluidRow(infoBoxOutput("max1"), " Energía activa  (Kw-h)  máxima:"),
              fluidRow(infoBoxOutput("min1"), " Energía activa (Kw-h)  mínima:"),
              fluidRow(infoBoxOutput("prom_cost"), " Costo promedio energía activa**:"),
              HTML(paste0("<h5>- La energía activa (Kw-h) promedio y su costo...<br>",
                          "* Consumo diario promedio...<br>",
                          "** Costo diario promedio...</h5>"))
            ),
            mainPanel(
              HTML("<br><br>"),
              box(plotlyOutput("comp_mes"), width = 40, status = "info")
            )
          ),
          h3(strong("Comportamiento del consumo total de energía activa (Kw-h) diario por mes y bloque")),
          br(),
          h4("La siguiente gráfica permite observar el consumo total diario..."),
          br(),
          box(plotlyOutput("por_consumo_mes_bloque"), width = 40, status = "info"),
          br(),
          h3(strong("Consumo de energía activa (Kw-h) y su costo por bloque")),
          br(),
          h4("El consumo y el costo dados a continuación para cada bloque..."),
          br(),
          fluidRow(uiOutput("valueBoxes")),
          br(),
          h3(strong("Consumo total de energía activa (Kw-h) por mes")),
          br(),
          h4("En la siguiente gráfica se observa el consumo total..."),
          br(),
          box(plotOutput("por_consumo_mes"), width = 40, status = "info"),
          br(),br(),
          h3(strong("Cálculo del costo de la energía activa total (Kw-h) consumida por mes")),
          h4("El costo de energía activa consumido cada mes..."),
          br(),
          sidebarLayout(
            sidebarPanel(
              tags$div(
                numericInput("precio", "Introduzca el precio del Kw-h:", value = 259),
                tags$style("#precio {border-color: #3A959D; border-width: 2px; border-style: solid; padding: 5px; border-radius: 5px; box-shadow: 0 0 5px #3A959D;}"),
                tags$script("$(document).ready(function() {...})")
              ),
              fluidRow(infoBoxOutput("totalact"), " Energía activa total consumida:"),
              fluidRow(infoBoxOutput("preciototalact"), " Costo total de energía activa (Kw-h):"),
              fluidRow(infoBoxOutput("promact"), " Energía activa promedio consumida:"),
              fluidRow(infoBoxOutput("preciopromact"), " Costo promedio de energía activa (Kw-h):"),
              h5("*El valor de 259 $/kwh corresponde al precio aplicado por EPM...")
            ),
            mainPanel(
              fluidRow(
                br(), br(), br(),
                fluidRow(uiOutput("valueBoxes_mes_act"))
              )
            )
          ),
          br(),
          h3(strong("Consumo total de energía activa (Kw-h) por día de la semana")),
          br(),
          h4("El consumo total registrado durante toda la ventana de observación..."),
          br(),
          box(plotOutput("por_consumo_dia"), width = 40, status = "info"),
          h4("*Esto permite observar el consumo de todos los lunes..."),
          br(),
          h3(strong("Cálculo del costo de energía activa total (Kw-h) consumida por día de la semana")),
          h4("El costo del consumo total de energía activa por día..."),
          br(),
          sidebarLayout(
            sidebarPanel(
              tags$h4("Para tener en cuenta:", style = "color: #3A959D;"),
              h5("*La conversión a dólares se realiza de acuerdo a la TRM actual...")
            ),
            mainPanel(
              fluidRow(uiOutput("valueBoxes_dia_act"))
            )
          ),
          br(),
          h3(strong("Análisis del consumo y costo promedio de energía activa por día de la semana")),
          br(),
          h4("El consumo promedio por día de la semana permite observarse..."),
          br(),
          box(plotOutput("por_prom_consumo_dia"),  status = "info",width = 40),
          br(),
          sidebarLayout(
            sidebarPanel(
              h5("* Costo promedio de consumo de energía activa por día...")
            ),
            mainPanel(
              fluidRow(uiOutput("valueBoxes_dia_act_prom"))
            )
          ),
          br(), br(),
          tags$fieldset(
            h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
            tags$legend("Comentarios"),
            tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 300)
          ),
          br(), br(),
          tags$footer(
            p(
              strong(
                HTML(
                  paste(
                    "Sistema de gestión de energia UNAL sede Medellín. <br>",
                    a("sigen_med@unal.edu.co", href = "mailto:sigen_med@unal.edu.co"), " <br>",
                    "Ext. 46408 <br>",
                    "Juan Enrique Torres Madrigal <br>",
                    a("juetorresma@unal.edu.co", href = "mailto:juetorresma@unal.edu.co")
                  )
                )
              ),
              align = "left"
            )
          )
  )
}
