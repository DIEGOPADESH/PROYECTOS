tab_corr_ui <- function() {
  tabItem(tabName = "tab_corr",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
              solidHeader = TRUE, width = 100,status = "info" ),
          tags$h2("Análisis de corriente", style = "text-align:center; font-weight:bold;"),
          br(),
          h3(strong("Información general")),
          br(),
          fluidRow(
            valueBoxOutput("bloque_box3"),
            valueBoxOutput("fecha1_box3"),
            valueBoxOutput("fecha2_box3")
          ),
          br(),
          h3(strong("Comparación de la corriente (Amperios) en el sistema trifásico de energía")),
          br(),
          box(plotlyOutput("corrientes"), width = 40, status = "info"),
          br(),
          h5(HTML("<p> En este gráfico se puede encontrar información relacionada con: </p> <br>
              -- <strong> Variabilidad de las mediciones...</strong><br>
              -- <strong>Valores atípicos</strong>: ...<br>
              -- <strong>Corriente media y mediana...</strong>")),
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
