tab_voltage_debug_ui <- function() {
  tabItem(tabName = "voltage_debug",
          box(
            h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))),
            align = "center",
            solidHeader = TRUE,
            width = 100,
            status = "info"
          ),
          br(),
          h3(strong("Cargar las bases de datos")),
          br(),
          sidebarLayout(
            sidebarPanel(
              fileInput("voltage_debug_files",
                        "Sube aquí los archivos Excel",
                        multiple = TRUE,
                        accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.xlsx')),
              uiOutput("voltage_debug_selectfile")
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Base original", dataTableOutput("voltage_debug_original")),
                tabPanel("Base depurada", dataTableOutput("voltage_debug_depurada"))
              )
            )
          ),
          div(
            actionButton("voltage_debug_debug_btn", "Depurar"),
            align = "left",
            style = "color: #3A959D; background: #F0F0F0; border-color: #3A959D; border-radius: 5px; padding: 5px;",
            tags$script('
              $(document).ready(function() {
                $("#voltage_debug_debug_btn").popover({
                  title: "Depurar base de datos",
                  content: "Al hacer clic en este botón, se realizará una depuración en la base de datos de acuerdo a ciertos lineamientos",
                  placement: "bottom",
                  trigger: "hover",
                  container: "body"
                });
              });
            ')
          ),
          br(), br(),
          h3(strong("Información de la base de datos")),
          br(),
          fluidRow(
            valueBoxOutput("voltage_debug_BL"),
            valueBoxOutput("voltage_debug_inicio"),
            valueBoxOutput("voltage_debug_fin")
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
