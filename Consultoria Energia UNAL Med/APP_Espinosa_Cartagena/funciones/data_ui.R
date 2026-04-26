tab_data_ui <- function() {
  tabItem(tabName = "data",
          box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
              solidHeader = TRUE, width = 100,status = "info" ),
          br(),
          h4(strong("Información general para filtro de datos:")),
          br(),
          sidebarLayout(
            sidebarPanel(
              HTML("<br><br><br><br><br><br><br>"),
              dateInput(inputId = "fecha1_id", 
                        label = "Selecciona fecha inicial de los datos:",
                        value = as.Date("2023/03/07"), format = "dd/mm/yyyy", language = "es"),
              dateInput(inputId = "fecha2_id", 
                        label = "Selecciona fecha final del los datos:",
                        value = Sys.Date(),format = "dd/mm/yyyy", language = "es"),
              HTML("<br><br><br><br><br><br><br><br>")
            ),
            mainPanel(
              tags$image(src = "Universidad-Nacional-de-Colombia-sede-Medellin.jpg", height = "500px", width = "750px")
            ) 
          ),
          h3(strong("Base de datos filtrada por fecha:")),
          DTOutput("base_data"),
          br(),
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
