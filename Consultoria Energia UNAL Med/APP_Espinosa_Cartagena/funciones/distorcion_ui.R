tab_distorcion_ui <- function() {
  tabItem(tabName = "tab_distorcion",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
              solidHeader = TRUE, width = 100, status = "info"),
          br(),
          tags$h2("Análisis de distorsión armónica", style = "text-align:center; font-weight:bold;"),
          h3(strong("Información general")),
          br(),
          fluidRow(
            valueBoxOutput("bl_box"),
            valueBoxOutput("date1_box"),
            valueBoxOutput("date2_box")
          ),
          br(),
          h3(strong("Gráficos de distorsión armónica (Orden 3, Orden 5 y Orden 7)")),
          br(),
          h4("En los siguientes gráficos se puede observar el comportamiento de la distorsión armónica total de 3°, 5° y 7° Orden..."),
          br(),
          fluidRow(
            box(title = "Distorsión armónica - Orden 3",
                plotlyOutput("arm3_comp"),
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
          ),
          fluidRow(
            box(title = "Distorsión armónica - Orden 5",
                plotlyOutput("arm5_comp"),
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
          ),
          fluidRow(
            box(title = "Distorsión armónica - Orden 7",
                plotlyOutput("arm7_comp"),
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
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
