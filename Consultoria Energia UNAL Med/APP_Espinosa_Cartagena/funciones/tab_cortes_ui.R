# archivo: tab_cortes_ui.R

tab_cortes_ui <- tabItem(
  tabName = "tab_cort",
  box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
      solidHeader = TRUE, width = 100, status = "info"),
  tags$h2("Análisis de cortes de energía", style = "text-align:center; font-weight:bold;"),
  br(),
  h5("Cargue a continuación la base de datos de registros de cortes de energía para desarrollar un breve análisis sobre esta."),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Subir base de datos:")),
      h5(HTML("<ul><li> Por como ha sido construida la aplicación para este análisis, la base de datos que cargue debe tener una estructura específica que deberá consultar en el manual de usuario en la pestaña 'Ayuda', última página. </li></ul>")),
      br(),
      fileInput("file_id", label = "Seleccione el archivo de datos a usar:")
    ),
    mainPanel(
      DTOutput("base_data_cor")
    )
  ),
  br(),
  h3(strong("Información de cortes de energía")),
  br(),
  fluidRow(
    uiOutput("infoBox_prog"),
    uiOutput("infoBox_tiempo"),
    uiOutput("infoBox_hora_prog")
  ),
  br(),
  br(),
  plotlyOutput("grafico_dif_mes"),
  br(),
  br(),
  plotlyOutput("grafico_bloque"),
  HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
  tags$fieldset(
    h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
    tags$legend("Comentarios"),
    tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
  ),
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
