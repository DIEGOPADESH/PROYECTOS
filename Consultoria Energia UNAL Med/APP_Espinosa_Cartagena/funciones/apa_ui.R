tab_apa_ui <- function() {
  tabItem(tabName = "tab_apa",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
              solidHeader = TRUE, width = 100,status = "info" ),
          tags$h2("Análisis de consumo: Energía aparente", style = "text-align:center; font-weight:bold;"),
          br(),
          h3(strong("Información general")),
          br(),
          fluidRow(
            valueBoxOutput("bloque_box4"),
            valueBoxOutput("fecha1_box4"),
            valueBoxOutput("fecha2_box4")
          ),
          br(),
          h3(strong("Comportamiento del consumo total de energía aparente (KvA-h) diario por cada mes")),
          br(),
          h4("La siguiente gráfica muestra el total de energía aparente consumida diariamente por todos los
              bloques en análisis para el mes que sea seleccionado..."),
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput("selector_mes_apa", "Seleccione el mes para ver el comportamiento:",
                          choices = c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                                      "septiembre", "octubre", "noviembre", "diciembre")),
              fluidRow(infoBoxOutput("prom3"), "Energía aparente (KvA-h) promedio:"),
              fluidRow(infoBoxOutput("max3"), "Energía aparente (KvA-h) máxima:"),
              fluidRow(infoBoxOutput("min3"), "Energía aparente (KvA-h) mínima:"),
              HTML("<h5>- La energía aparente (KvA-h) promedio, máxima y mínima...<br>* Consumo diario promedio...</h5>")
            ),
            mainPanel(
              box(plotlyOutput("comp_mes_apa"), width = 40, status = "info")
            )
          ),
          br(),
          h3(strong("Comportamiento del consumo total de energía aparente (KvA-h) diario por mes y bloque")),
          br(),
          h4("La siguiente gráfica permite observar el consumo total diario..."),
          br(),
          box(plotlyOutput("comp_mes_apa_bl"), width = 40, status = "info"),
          br(),
          h3(strong("Consumo de energía aparente (KvA-h) por bloque")),
          br(),
          h4("El consumo dado a continuación para cada bloque corresponde..."),
          br(),
          fluidRow(uiOutput("valueBoxes_apa")),
          br(),
          h3(strong("Consumo total de energía aparente (KvA-h) por mes")),
          br(),
          h4("En la siguiente gráfica se observa el consumo total..."),
          br(),
          box(plotOutput("por_consumo_mes_apa"), width = 40, status = "info"),
          br(),
          sidebarLayout(
            sidebarPanel(
              fluidRow(infoBoxOutput("totalapa"), "Energía aparente (KvA-h) total:"),
              h5("*Consumo total durante la ventana de tiempo seleccionada.")
            ),
            mainPanel()
          ),
          br(),
          h3(strong("Consumo total de energía aparente (KvA-h) por día de la semana")),
          br(),
          h4("El consumo total registrado durante toda la ventana..."),
          box(plotOutput("por_consumo_dia_apa"), width = 40, status = "info"),
          h4("*En este gráfico se calcula el consumo total..."),
          br(),
          h3(strong("Análisis del consumo promedio de energía aparente por día de la semana")),
          br(),
          h4("El consumo promedio por día de la semana permite observarse..."),
          br(),
          box(plotOutput("por_prom_consumo_dia_apa"),  status = "info",width = 40),
          br(),
          tags$fieldset(
            h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
            tags$legend("Comentarios"),
            tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
          ),
          br(),
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
