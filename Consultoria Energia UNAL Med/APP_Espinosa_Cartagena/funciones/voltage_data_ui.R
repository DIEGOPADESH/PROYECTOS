tab_voltage_data_ui <- function() {
  tabItem(tabName = "voltage_data",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), 
              align = "center", solidHeader = TRUE, width = 100, status = "info"),
          br(),
          h4(strong("Información general para filtro de datos:")),
          br(),
          sidebarLayout(
            sidebarPanel(
              HTML("<br><br><br><br><br><br><br>"),
              dateInput(inputId = "voltage_date_start", 
                        label = "Selecciona fecha inicial de los datos:",
                        value = as.Date("2023/09/29"), format = "dd/mm/yyyy", language = "es"),
              dateInput(inputId = "voltage_date_end", 
                        label = "Selecciona fecha final del los datos:",
                        value = Sys.Date(), format = "dd/mm/yyyy", language = "es"),
              HTML("<br><br><br><br><br><br><br><br>")
            ),
            mainPanel(
              tags$image(src = "Universidad-Nacional-de-Colombia-sede-Medellin.jpg", height = "500px", width = "750px")
            ) 
          ),
          h3(strong("Base de datos filtrada por fecha:")),
          DTOutput("voltage_filtered_data"),
          br(), br(),
          tags$h2("Información de importancia para el análisis de armónicos", style = "text-align:center; font-weight:bold;"),
          h3(strong("Límites de armónicos permitidos en un sistema eléctrico - Norma IEEE 519-1992")),
          br(),
          tags$image(src = "LimitesIEEE519.png", height = "350px", width = "400px", style = "text-align:center;"),
          h4("Los valores permisibles para armónicos mostrados en la imagen anterior,..."),
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
