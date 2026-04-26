# Sistema de gestión de energía para la Universidad Nacional de Colombia sede Medellín.


# Este aplicativo se creó con el fin de analizar los comportamientos del consumo del recurso eléctrico en los 
# diferentes bloques (edificaciones) de la UNAL sede Medellín. Es importante anotar que este es un aplicativo 
# al cual se le pueden adherir más funcionalidades de análisis de acuerdo a las necesidades que se tengan.  

# En la pestaña de Ayuda de la aplicación pueden encontrarse las funcionalidades que hasta la finalización del
# periodo 2023-1 han sido implementadas.

# Cargar paquetes necesarios para realizar gráficos, trabajar con la variable de tipo fecha y darle el aspecto
# a la interfaz.


library(stringr)
library(readxl)
library(ggplot2)
library(xts)
library(lubridate)
library(dplyr)
library(patchwork)
library(shiny)
library(shinythemes) 
library(shinydashboard)
library(ggrepel)
library(tidyverse)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(quantmod)
library(shinyBS)
library(janitor)

# Llamado a archivo de funciones auxiliares
source("Funciones_aux.R")
# Creación de interfaz de usuario.

ui <- dashboardPage(
  # Creación de la cabecera de la interfaz de usuario.
  title = " Universidad Nacional de Colombia sede Medellín Sistema de gestión de energía",
  skin = "yellow",
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 100px}"),
            tags$style(".main-header .logo {height: 100px}")
    ),
    # Se pone el logo de la UNAL y al hacer clic sobre este se accede a la plataforma de medición de datos Synergy. 
    title = tags$a(img(src = "logounal.png", height = 100, width = 200),href="http://singen.medellin.unal.edu.co:9876/User/Search.aspx")
  ),
  # Creación de los menús (titulos e iconos) para cada uno de los análisis que se proponen.
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 10px}"),
    sidebarMenu(id = "sidebarId",
                
                # Consumo de Energía
                menuItem("Consumo de Energía", icon = icon("bolt"),
                         menuSubItem("Cargue y depuración", tabName = "debug"),
                         menuSubItem("Filtrado de datos", tabName = "data"),
                         menuSubItem("Energía activa", tabName = "tab_costo"),
                         menuSubItem("Energía reactiva", tabName = "tab_react"),
                         menuSubItem("Energía aparente", tabName = "tab_apa")
                ),
                
                # Análisis de la corriente
                menuItem("Análisis de la Corriente", icon = icon("random"),
                         menuSubItem("Análisis general", tabName = "tab_corr")
                ),
                
                # Calidad de Energía
                menuItem("Calidad de Energía", icon = icon("plug-circle-check"),
                         
                         # Subgrupo Corriente
                         menuItem("Corriente", icon = icon("flash"),
                                  menuSubItem("Cargue y depuración", tabName = "debug1"),
                                  menuSubItem("Filtrar datos", tabName = "datos"),
                                  menuSubItem("Distorsión armónica", tabName = "tab_distorcion")
                         ),
                         
                         # Subgrupo Voltaje
                         menuItem("Voltaje", icon = icon("bolt"),
                                  menuSubItem("Cargue y depuración", tabName = "voltage_debug"),
                                  menuSubItem("Filtrar datos", tabName = "voltage_data"),
                                  menuSubItem("Distorsión armónica", tabName = "tab_graficos")  # Tu nueva pestaña
                         )
                ),
                
                # Cortes de Energía
                menuItem("Cortes de Energía", tabName = "tab_cort", icon = icon("power-off")),
                
                # Ayuda
                menuItem("Ayuda", tabName = "tab_help", icon = icon("question-circle")),
                
                # Créditos
                menuItem("Créditos", tabName = "tab_creditos", icon = icon("users"))
    )
  ),
  # Creación del cuerpo de la aplicación, el cual contendrá los gráficos, los espacios para entrar información y los resultados numéricos.  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Fijar el header */
    .main-header {
      position: fixed;
      top: 0;
      width: 100%;
      z-index: 9999;
    }

    /* Fijar el sidebar */
    .main-sidebar {
      position: fixed;
      top: 100px; /* Altura del header */
      height: calc(100% - 100px);
      overflow-y: auto;
    }

    /* Ajustar el contenido para que no se esconda detrás del header */
    .content-wrapper, .right-side {
      margin-top: 100px;
    }
  "))
    ),
    
    
    # Creación de la pestaña de de cargue y depuración de la base de datos:
    tabItems(tabItem(tabName = "debug",
                     box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                         solidHeader = TRUE, width = 100,status = "info" ),
                     br(),
                     h3(strong("Cargar las bases de datos")),
                     br(),
                     # Selección de los archivos Excel a analizar (uno o más archivos)
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("file",
                                   "Sube aquí los archivos Excel", 
                                   multiple = TRUE, 
                                   accept=c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv', ".xlsx")), 
                         uiOutput("selectfile"),
                       ),
                       mainPanel(
                         tabsetPanel(
                           # Panel para mostrar la base de datos original cargada:
                           # Nota: La base original es la bajada directamente de Synergy 
                           # pero con la incorporación de la variable que identifica el bloque:
                           tabPanel("Base original", dataTableOutput("table")),
                           # Panel para mostrar la base de datos original depurada:
                           tabPanel("Base depurada", dataTableOutput("table2"))
                         ) 
                       )
                     ),
                     # Botón para depuración de la base de datos:
                     # div(actionButton("buttonDebug", "Depurar"), align = "left", 
                     #     style="color: #08A3B0; border-color: #08A3B0"),
                     div(
                       actionButton("buttonDebug", "Depurar"),
                       align = "left",
                       style = "color: #3A959D; background: #F0F0F0; border-color: #3A959D; border-radius: 5px; padding: 5px;",
                       tags$script('
                       $(document).ready(function() {
                       $("#buttonDebug").popover({
                       title: "Depurar base de datos",
                       content: "Al hacer clic en este botón, se realizará una depuración en la base de datos de acuerdo a ciertos lineamientos",
                       placement: "bottom",
                       trigger: "hover",
                       container: "body"
                       });
                       });
                                   ')
                     ),
                     br(),
                     br(),
                     # Cajas de información de la base de datos depurada:
                     h3(strong("Información de la base de datos")),
                     br(),
                     fluidRow(
                       # Edificaciones o bloques a analizar:
                       valueBoxOutput("edif"),
                       # Fecha de inicio de registros de la base de datos:
                       valueBoxOutput("inicio_reg"),
                       # Fecha de fin de registros de la base de datos:
                       valueBoxOutput("fin_reg")
                     ),
                     br(),
                     # Información a pie de página
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
                     
                     
    ), 
    # Pestaña para filtrado de datos por fechas:
    tabItem(tabName = "data",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                solidHeader = TRUE, width = 100,status = "info" ),
            br(),
            h4(strong("Información general para filtro de datos:")),
            br(),
            # Selección de ventana de tiempo de interés:
            sidebarLayout(
              sidebarPanel(
                HTML("<br><br><br><br><br><br><br>"),
                # Fecha inicial del filtro:
                dateInput(inputId = "fecha1_id", 
                          label = "Selecciona fecha inicial de los datos:",
                          value = as.Date("2023/03/07"), format = "dd/mm/yyyy", language = "es"),
                # Fecha final del filtro:
                dateInput(inputId = "fecha2_id", 
                          label = "Selecciona fecha final del los datos:",
                          value = Sys.Date(),format = "dd/mm/yyyy", language = "es"),
                HTML("<br><br><br><br><br><br><br><br>")
              ),
              # Imagen colocada en la primera interfaz de introducción de información.
              mainPanel(
                tags$image(src = "Universidad-Nacional-de-Colombia-sede-Medellin.jpg", height = "500px", width = "750px")
              ) 
            ),
            # Para mostrar la base de datos filtrada:
            h3(strong("Base de datos filtrada por fecha:")),
            DTOutput("base_data"),
            br(),
            br(),
            # Información a pie de página:
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
    ),
    
    # Creación de la pestaña de comportamiento del consumo de energía activa y costos.    
    tabItem(tabName = "tab_costo",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                solidHeader = TRUE, width = 100,status = "info" ),
            br(),
            tags$h2("Análisis de consumo: Energía activa", style = "text-align:center; font-weight:bold;"),
            h3(strong("Información general")),
            br(),
            # Información del análisis:
            fluidRow(
              valueBoxOutput("bloque_box"),
              valueBoxOutput("fecha1_box"),
              valueBoxOutput("fecha2_box")
            ),
            br(),
            # Serie de comportamiento total para el mes seleccionado:
            h3(strong("Comportamiento del consumo total de energía activa (Kw-h) diario por cada mes")),
            br(),
            h4("La siguiente gráfica muestra el total de energía activa consumida diariamente por todos los
            bloques en análisis para el mes que sea seleccionado, este total corresponde a la suma de las 96 
            observaciones que se espera el sistema registre diariamente con una frecuencia de muestreo de 15 minutos."),
            br(),
            sidebarLayout(
              sidebarPanel(
                # Seleccionador del mes para ver el comportamiento del consumo de energía activa en Kwh.
                selectInput(
                  "selector_mes",              
                  label = "Seleccione el mes a analizar:", 
                  choices = c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                              "septiembre", "octubre", "noviembre", "diciembre")
                ),
                # Valores para energía activa promedio, máxima y mínima en Kwh
                fluidRow(infoBoxOutput("prom1"), " Energía activa (Kw-h) promedio*:"),
                fluidRow(infoBoxOutput("max1"), " Energía activa  (Kw-h)  máxima:"),
                fluidRow(infoBoxOutput("min1"), " Energía activa (Kw-h)  mínima:"),
                fluidRow(infoBoxOutput("prom_cost"), " Costo promedio energía activa**:"),
                HTML(paste0("<h5>- La energía activa (Kw-h) promedio y su costo se calculan para el conjunto de bloques en análisis y los días registrados del mes seleccionado. 
                            De igual manera se realiza para el valor máximo y mínimo.<br>",
                            "* Consumo diario promedio para el mes seleccionado.<br>",
                            "** Costo diario promedio para el mes seleccionado</h5>"))
              ),
              # Creación del panel con el gráfico del comportamiento de la energía activa por el mes seleccionado en Kwh.
              mainPanel(
                HTML("<br><br>"),
                box(plotlyOutput("comp_mes"), width = 40, status = "info")
              ) 
            ),
            # Serie de consumo por el mes seleccionado discriminado por bloque:
            h3(strong("Comportamiento del consumo total de energía activa (Kw-h) diario por mes y bloque")),
            br(),
            h4("La siguiente gráfica permite observar el consumo total diario de energía activa para cada bloque durante el mes seleccionado."),
            br(),
            box(plotlyOutput("por_consumo_mes_bloque"), width = 40, status = "info"),
            br(),
            # Consumo y costo por bloque durante toda la ventan de tiempo seleccionada (no es de acuerdo con el mes seleccionado).
            h3(strong("Consumo de energía activa (Kw-h) y su costo por bloque")),
            br(),
            h4("El consumo y el costo dados a continuación para cada bloque corresponden al consumo y costo totales durante toda la ventana temporal
               de análisis:"),
            br(),
            fluidRow(
              uiOutput("valueBoxes")
            ),
            br(),
            # Consumo total de energía activa consumido por de acuerdo a cada mes visto en histograma
            h3(strong("Consumo total de energía activa (Kw-h) por mes")),
            br(),
            h4("En la siguiente gráfica se observa el consumo total de energía activa para cada mes y la proporción del total consumido correspondiente."),
            br(),
            box(plotOutput("por_consumo_mes"), width = 40, status = "info"),
            br(),br(),
            # Costo del consumo por mes durante toda la ventana de tiempo seleccionada:
            h3(strong("Cálculo del costo de la energía activa total (Kw-h) consumida por mes")),
            h4("El costo de energía activa consumido cada mes durante la ventana de tiempo seleccionada es el siguiente: "),
            br(),
            # Costo de la energía activa por cada mes.
            sidebarLayout(
              sidebarPanel(
                # Introducir el precio del KWh vigente, 259 es el valor por defecto y corresponde al valor actual.
                tags$div(
                  numericInput("precio", "Introduzca el precio del Kw-h:", value = 259),
                  tags$style("#precio {
                  border-color: #3A959D;
                  border-width: 2px;
                  border-style: solid;
                  padding: 5px;
                  border-radius: 5px;
                  box-shadow: 0 0 5px #3A959D;
                  }
                             "),
                  tags$script("
                             $(document).ready(function() {
                             $('#precio').popover({
                             title: 'Precio del Kw-h',
                             content: 'Los cálculos de costos se harán de acuerdo a este valor',placement: 'right',
                             trigger: 'hover',
                             container: 'body',
                             html: true
                             });
                             });
                                         ")
                ),
                fluidRow(infoBoxOutput("totalact"), " Energía activa total consumida:"),
                fluidRow(infoBoxOutput("preciototalact"), " Costo total de energía activa (Kw-h):"),
                fluidRow(infoBoxOutput("promact"), " Energía activa promedio consumida:"),
                fluidRow(infoBoxOutput("preciopromact"), " Costo promedio de energía activa (Kw-h):"),
                h5("*El valor de 259 $/kwh corresponde al precio aplicado por EPM a la Universidad Nacional
                             por el suministro de energía y potencia para los consumos registrados entre octubre 1 de 2022
                             y marzo 31 de 2025.")
              ),
              # Costo de la energía activa en Kwh consumida para cada mes del año.
              mainPanel(
                fluidRow(
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    uiOutput("valueBoxes_mes_act")
                  )
                )
              )),
            br(),
            h3(strong("Consumo total de energía activa (Kw-h) por día de la semana")),
            br(),
            h4("El consumo total registrado durante toda la ventana de observación y diferenciado por día de la semana es el siguiente:"),
            # Grafico de barras para el consumo de energía activa en Kwh cada día de la semana.
            br(),
            box(plotOutput("por_consumo_dia"), width = 40, status = "info"),
            h4("*Esto permite observar el consumo de todos los lunes, de los martes, etc. durante la ventana temporal de análisis."),
            br(),
            h3(strong("Cálculo del costo de energía activa total (Kw-h) consumida por día de la semana")),
            h4("El costo del consumo total de energía activa por día de la semana durante la ventana de tiempo seleccionada es el siguiente: "),
            br(),
            sidebarLayout(
              sidebarPanel(
                tags$h4("Para tener en cuenta:", style = "color: #3A959D;"),
                h5("*La conversión a dólares se realiza de acuerdo a la TRM actual registrada en Yahoo Finance: "),
                tags$div(textOutput("cop_price"))
              ),
              # Costo total de la energía activa en Kwh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_act")
                )
              )
            ),
            br(),
            # Consumo promedio por día de energía activa:
            h3(strong("Análisis del consumo y costo promedio de energía activa por día de la semana")),
            br(),
            h4("El consumo promedio por día de la semana permite observarse en el siguiente gráfico: "),
            br(),
            box(plotOutput("por_prom_consumo_dia"),  status = "info",width = 40),
            br(),
            sidebarLayout(
              sidebarPanel(
                h5("* Costo promedio de consumo de energía activa por día de la semana durante la ventana de tiempo de interés
                   (costo promedio de todos los lunes, de todos los martes, etc., de la ventana de tiempo en análisis): ")
              ),
              # Costo de la energía activa en Kwh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_act_prom")
                )
              )
            ),
            br(),
            br(),
            tags$fieldset(
              h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
              tags$legend("Comentarios"),
              tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 300)
            ),
            br(),
            br(),
            # Información a pie de página.
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
    ),
    
    
    ## Pestaña energia reactiva ----
    
    # Creación de la pestaña de comportamiento del consumo de energía reactiva.
    tabItem(tabName = "tab_react",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                solidHeader = TRUE, width = 100,status = "info" ),
            tags$h2("Análisis de consumo: Energía reactiva", style = "text-align:center; font-weight:bold;"),
            br(),
            h3(strong("Información general")),
            br(),
            # Bloques y ventana de tiempo en la que se está realizando el análisis
            fluidRow(
              valueBoxOutput("bloque_box2"),
              valueBoxOutput("fecha1_box2"),
              valueBoxOutput("fecha2_box2")
            ),
            br(),
            h3(strong("Comportamiento del consumo total de energía reactiva (Kvar-h) diario por cada mes")),
            br(),
            h4("La siguiente gráfica muestra el total de energía reactiva consumida diariamente por todos los
            bloques en análisis para el mes que sea seleccionado, este total corresponde a la suma de las 96 
            observaciones que se espera el sistema registre diariamente con una frecuencia de muestreo de 15 minutos."),
            br(),
            sidebarLayout(
              sidebarPanel(
                # Seleccionador del mes para ver el comportamiento del consumo de energía reactiva en Kvarh.
                selectInput(
                  "selector_mes_react",              
                  label = "Seleccione el mes para ver el comportamiento:",               
                  choices = c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                              "septiembre", "octubre", "noviembre", "diciembre")
                ),
                # Valores para energía reactiva promedio, máxima y mínima en Kvarh.
                fluidRow(infoBoxOutput("prom2"), "Energía reactiva (Kvar-h) promedio:"),
                fluidRow(infoBoxOutput("max2"), "Energía reactiva (Kvar-h) máxima:"),
                fluidRow(infoBoxOutput("min2"), "Energía reactiva (Kvar-h) mínima:"),
                HTML(paste0("<h5>- La energía reactiva (Kvar-h) promedio, máxima y mínima se calculan para el conjunto de bloques en análisis 
                            y los días registrados del mes seleccionado.<br>",
                            "* Consumo diario promedio para el mes seleccionado.</h5>"))
              ),
              # Creación del panel con el gráfico del comportamiento de la energía reactiva por el mes seleccionado en Kvarh.
              mainPanel(
                box(plotlyOutput("comp_mes_react"), width = 40, status = "info")
              ) 
            ),
            br(),
            # Serie de consumo total por día durante el mes seleccionado:
            h3(strong("Comportamiento del consumo total de energía reactiva (Kvar-h) diario por mes y bloque")),
            br(),
            h4("La siguiente gráfica permite observar el consumo total diario de energía reactiva para cada bloque durante el mes seleccionado."),
            br(),
            box(plotlyOutput("por_consumo_mes_bloque_reac"), width = 40, status = "info"),
            br(),
            br(),
            # Consumo total por bloque durante el mes seleccionado:
            h3(strong("Consumo de energía reactiva (Kvar-h) por bloque")),
            br(),
            h4("El consumo dado a continuación para cada bloque corresponde al consumo total durante toda la ventana temporal
               de análisis:"),
            br(),
            fluidRow(
              uiOutput("valueBoxes_react")
            ),
            # Consumo total por mes de energía reactiva en gráfico de barras:
            h3(strong("Consumo total de energía reactiva (Kvar-h) por mes")),
            br(),
            h4("En la siguiente gráfica se observa el consumo total de energía reactiva para cada mes y la proporción del total consumido correspondiente."),
            br(),
            # Creación de panel para consumo total mensual de energía reactiva.
            box(plotOutput("por_consumo_mes_react"), width = 40, status = "info"),
            br(),
            
            sidebarLayout(
              sidebarPanel(
                # Introducir el precio del KWh vigente, 259 es el valor por defecto y corresponde al valor actual.
                tags$div(
                  numericInput("precio", "Introduzca el precio del Kw-h:", value = 259),
                  tags$style("#precio {
                  border-color: #3A959D;
                  border-width: 2px;
                  border-style: solid;
                  padding: 5px;
                  border-radius: 5px;
                  box-shadow: 0 0 5px #3A959D;
                  }
                             "),
                  tags$script("
                             $(document).ready(function() {
                             $('#precio').popover({
                             title: 'Precio del Kvar-h',
                             content: 'Los cálculos de costos se harán de acuerdo a este valor',placement: 'right',
                             trigger: 'hover',
                             container: 'body',
                             html: true
                             });
                             });
                                         ")
                ),
                fluidRow(infoBoxOutput("totalreact"), " Energía reactiva total consumida:"),
                fluidRow(infoBoxOutput("preciototalreact"), " Costo total de energía reactiva (Kvar-h):"),
                fluidRow(infoBoxOutput("promreact"), " Energía reactiva promedio consumida:"),
                fluidRow(infoBoxOutput("preciopromreact"), " Costo promedio de energía reactiva (Kvar-h):"),
                h5("*El valor de 259 $/kwh corresponde al precio aplicado por EPM a la Universidad Nacional
                             por el suministro de energía y potencia para los consumos registrados entre octubre 1 de 2022
                             y marzo 31 de 2025.")
              ),
              # Costo de la energía activa en Kwh consumida para cada mes del año.
              mainPanel(
                fluidRow(
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    uiOutput("valueBoxes_mes_react")
                  )
                )
              )),
            br(),
            
            
            
            sidebarLayout(
              sidebarPanel(
                fluidRow(infoBoxOutput("totalreact"), "Energía reactiva (Kvar-h) total:"),
                h5("*Consumo total durante la ventana de tiempo seleccionada.")
              ),
              mainPanel(
              )
            ),
            
            br(),
            br(),
            h3(strong("Consumo total de energía reactiva (Kvar-h) por día de la semana")),
            br(),
            h4("El consumo total registrado durante toda la ventana de observación y diferenciado por día de la semana es el siguiente:"),
            # Creación de panel para gráfico de barras del consumo de energía reactiva por día de la semana en Kvarh.
            box(plotOutput("por_consumo_dia_react"), width = 40, status = "info"),
            h4("*En este gráfico se calcula el consumo total de la energía reactiva (Kvar-h) con respecto a cada día de 
               de la semana, es decir, el consumo total de todos los lunes, martes, miercoles, jueves, viernes,
               sábados y domingos
               durante la ventana de tiempo seleccionada"),
            
            
            
            h3(strong("Cálculo del costo de energía reactiva total (Kvar-h) consumida por día de la semana")),
            h4("El costo del consumo total de energía activa por día de la semana durante la ventana de tiempo seleccionada es el siguiente: "),
            br(),
            sidebarLayout(
              sidebarPanel(
                tags$h4("Para tener en cuenta:", style = "color: #3A959D;"),
                h5("*La conversión a dólares se realiza de acuerdo a la TRM actual registrada en Yahoo Finance: "),
                tags$div(textOutput("cop_price"))
              ),
              # Costo total de la energía activa en Kwh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_react")
                )
              )
            ),
            br(),
            
            
            # Consumo promedio por día energía reactiva:
            h3(strong("Análisis del consumo promedio de energía reactiva por día de la semana")),
            br(),
            h4("El consumo promedio por día de la semana permite observarse en el siguiente gráfico: "),
            br(),
            box(plotOutput("por_prom_consumo_dia_react"),  status = "info",width = 40),
            br(),
            
            
            sidebarLayout(
              sidebarPanel(
                h5("* Costo promedio de consumo de energía reactiva por día de la semana durante la ventana de tiempo de interés
                   (costo promedio de todos los lunes, de todos los martes, etc., de la ventana de tiempo en análisis): ")
              ),
              # Costo de la energía reactiva en Kvarh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_react_prom")
                )
              )
            ),
            
            
            br(),
            br(),
            # Caja de comentarios:
            tags$fieldset(
              h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
              tags$legend("Comentarios"),
              tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
            ),
            br(),
            br(),
            # Información a pie de página.
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
    ),
    # Creación de la pestaña de comportamiento del consumo de energía aparente.
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
            bloques en análisis para el mes que sea seleccionado, este total corresponde a la suma de las 96 
            observaciones que se espera el sistema registre diariamente con una frecuencia de muestreo de 15 minutos."),
            br(),
            sidebarLayout(
              sidebarPanel(
                # Seleccionador del mes para ver el comportamiento del consumo de energía reactiva en Kvarh.
                selectInput(
                  "selector_mes_apa",
                  label = "Seleccione el mes para ver el comportamiento:",
                  choices = c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                              "septiembre", "octubre", "noviembre", "diciembre")
                ),
                # Valores para energía reactiva promedio, máxima y mínima en Kvarh.
                fluidRow(infoBoxOutput("prom3"), "Energía aparente (KvA-h) promedio:"),
                fluidRow(infoBoxOutput("max3"), "Energía aparente (KvA-h) máxima:"),
                fluidRow(infoBoxOutput("min3"), "Energía aparente (KvA-h) mínima:"),
                HTML(paste0("<h5>- La energía aparente (KvA-h) promedio, máxima y mínima se calculan para el conjunto de bloques en análisis 
                            y los días registrados del mes seleccionado.<br>",
                            "* Consumo diario promedio para el mes seleccionado.</h5>"))
              ),
              # Creación del panel con la serie del comportamiento de la energía aparente por el mes seleccionado en Kvarh.
              mainPanel(
                box(plotlyOutput("comp_mes_apa"), width = 40, status = "info")
              )
            ),
            br(),
            # Serie de consumo total diario de energía aparente por bloque:
            h3(strong("Comportamiento del consumo total de energía aparente (KvA-h) diario por mes y bloque")),
            br(),
            h4("La siguiente gráfica permite observar el consumo total diario de energía aparente para cada bloque durante el mes seleccionado."),
            br(),
            box(plotlyOutput("comp_mes_apa_bl"), width = 40, status = "info"),
            br(),
            # Consumo total por bloque durante el mes seleccionado:
            h3(strong("Consumo de energía aparente (KvA-h) por bloque")),
            br(),
            h4("El consumo dado a continuación para cada bloque corresponde al consumo total durante toda la ventana temporal
               de análisis:"),
            br(),
            fluidRow(
              uiOutput("valueBoxes_apa")
            ),
            br(),
            h3(strong("Consumo total de energía aparente (KvA-h) por mes")),
            br(),
            h4("En la siguiente gráfica se observa el consumo total de energía aparente para cada mes y la proporción del total consumido correspondiente."),
            br(),
            # Creación de panel para consumo total mensual de energía aparente por mes:
            box(plotOutput("por_consumo_mes_apa"), width = 40, status = "info"),
            br(),
            
            
            
            sidebarLayout(
              sidebarPanel(
                # Introducir el precio del KWh vigente, 259 es el valor por defecto y corresponde al valor actual.
                tags$div(
                  numericInput("precio", "Introduzca el precio del Kw-h:", value = 259),
                  tags$style("#precio {
                  border-color: #3A959D;
                  border-width: 2px;
                  border-style: solid;
                  padding: 5px;
                  border-radius: 5px;
                  box-shadow: 0 0 5px #3A959D;
                  }
                             "),
                  tags$script("
                             $(document).ready(function() {
                             $('#precio').popover({
                             title: 'Precio del KvA-h',
                             content: 'Los cálculos de costos se harán de acuerdo a este valor',placement: 'right',
                             trigger: 'hover',
                             container: 'body',
                             html: true
                             });
                             });
                                         ")
                ),
                fluidRow(infoBoxOutput("totalapa"), " Energía aparente total consumida:"),
                fluidRow(infoBoxOutput("preciototalapar"), " Costo total de energía aparente (KvA-h):"),
                fluidRow(infoBoxOutput("promapar"), " Energía aparente promedio consumida:"),
                fluidRow(infoBoxOutput("preciopromapar"), " Costo promedio de energía aparente (KvA-h):"),
                h5("*El valor de 259 $/kwh corresponde al precio aplicado por EPM a la Universidad Nacional
                             por el suministro de energía y potencia para los consumos registrados entre octubre 1 de 2022
                             y marzo 31 de 2025.")
              ),
              # Costo de la energía activa en Kwh consumida para cada mes del año.
              mainPanel(
                fluidRow(
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    uiOutput("valueBoxes_mes_apar")
                  )
                )
              )),
            br(),
            
            
            br(),
            h3(strong("Consumo total de energía aparente (KvA-h) por día de la semana")),
            br(),
            h4("El consumo total registrado durante toda la ventana de observación y diferenciado por día de la semana es el siguiente:"),
            # Creación de panel para gráfico de barras del consumo de energía reactiva por día de la semana en Kvarh.
            box(plotOutput("por_consumo_dia_apa"), width = 40, status = "info"),
            h4("*En este gráfico se calcula el consumo total de la energía aparente (KvA-h) con respecto a cada día de
               de la semana, es decir, el consumo total de todos los lunes, martes, miercoles, jueves, viernes,
               sábados y domingos
               durante la ventana de tiempo seleccionada"),
            br(),
            
            
            h3(strong("Cálculo del costo de energía aparente total (KvA-h) consumida por día de la semana")),
            h4("El costo del consumo total de energía activa por día de la semana durante la ventana de tiempo seleccionada es el siguiente: "),
            br(),
            sidebarLayout(
              sidebarPanel(
                tags$h4("Para tener en cuenta:", style = "color: #3A959D;"),
                h5("*La conversión a dólares se realiza de acuerdo a la TRM actual registrada en Yahoo Finance: "),
                tags$div(textOutput("cop_price"))
              ),
              # Costo total de la energía activa en Kwh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_apar")
                )
              )
            ),
            
            
            
            
            
            h3(strong("Análisis del consumo promedio de energía aparente por día de la semana")),
            br(),
            h4("El consumo promedio por día de la semana permite observarse en el siguiente gráfico: "),
            br(),
            box(plotOutput("por_prom_consumo_dia_apa"),  status = "info",width = 40),
            br(),
            
            
            sidebarLayout(
              sidebarPanel(
                h5("* Costo promedio de consumo de energía aparente por día de la semana durante la ventana de tiempo de interés
                   (costo promedio de todos los lunes, de todos los martes, etc., de la ventana de tiempo en análisis): ")
              ),
              # Costo de la energía reactiva en Kvarh consumida para cada día de la semana.
              mainPanel(
                fluidRow(
                  uiOutput("valueBoxes_dia_apar_prom")
                )
              )
            ),
            
            
            
            
            br(),
            tags$fieldset(
              h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
              tags$legend("Comentarios"),
              tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
            ),
            br(),
            br(),
            # Información a pie de página.
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
    ),
    # Creación de la pestaña de comparación de corriente en amperios en el sistema trifásico de energía.      
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
            # Creación de panel para grafico de boxplot de corrientes del sistema trifásico de energía.
            box(plotlyOutput("corrientes"), width = 40, status = "info"),
            br(),
            # Explicación de la información que se puede extraer del gráfico anterior.
            h5(HTML("<p> En este gráfico se puede encontrar información relacionada con: </p> <br>
              -- <strong> Variabilidad de las mediciones de corriente en amperios en cada fase y el neutro: </strong> Esto se puede 
                   evaluar observando la amplitud de los rectángulos, entre más amplios sean existe una mayor variabilidad
                   en la mediciones de la corriente para cada fase y el neutro.<br>
              -- <strong>Valores atípicos</strong>: Estos están representados por los puntos negros que están por encima de los
                   rectángulos en las respectivas fases y el neutro. Como su nombre lo indica son mediciones de
                   corriente que son anormales y están muy separadas del resto de la información. <br>
              -- <strong>Corriente media y mediana en amperios en cada fase y el neutro</strong>: La corriente media (promedio) está representada por los puntos rojos para cada
                   fase y el neutro. En el caso de la corriente mediana, representada por la recta horizontal de color negro que se encuentra
                   dentro de los rectángulos, se interpreta como el valor de la corriente bajo el cual se encuentral el 50% de la información de las mediciones de corriente para cada fase y el neutro.
                   De igual manera se interpretan los valores que toman las rectas horizontales
                   superior e inferior de los rectángulos, estas representan el 75% y 25% respectivamente.")),
            br(),
            br(),
            tags$fieldset(
              h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
              tags$legend("Comentarios"),
              tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
            ),
            br(),
            br(),
            # Información a pie de página.
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
    ),
    
    # Depuracion corriente ----
    tabItem(tabName = "debug1",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
                solidHeader = TRUE, width = 100,status = "info"),
            br(),
            h3(strong("Cargar las bases de datos")),
            br(),  
            # Selección de los archivos Excel a analizar (uno o más archivos)
            sidebarLayout(
              sidebarPanel(
                fileInput("files",
                          "Sube aquí los archivos Excel", 
                          multiple = TRUE, 
                          accept=c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv', ".xlsx")), 
                uiOutput("selectfile1")
              ),
              mainPanel(
                tabsetPanel(
                  # Panel para mostrar la base de datos original cargada:
                  # Nota: La base original es la bajada directamente de Synergy 
                  # pero con la incorporación de la variable que identifica el bloque:
                  tabPanel("Base original", dataTableOutput("original")),
                  # Panel para mostrar la base de datos original depurada:
                  tabPanel("Base depurada", dataTableOutput("depurada"))
                ) 
              )
            ),
            # Botón para depuración de la base de datos:
            # div(actionButton("buttonDebug1", "Depurar"), align = "left", 
            #     style="color: #08A3B0; border-color: #08A3B0"),
            div(
              actionButton("buttonDebug1", "Depurar"),
              align = "left",
              style = "color: #3A959D; background: #F0F0F0; border-color: #3A959D; border-radius: 5px; padding: 5px;",
              tags$script('
                       $(document).ready(function() {
                       $("#buttonDebug1").popover({
                       title: "Depurar base de datos",
                       content: "Al hacer clic en este botón, se realizará una depuración en la base de datos de acuerdo a ciertos lineamientos",
                       placement: "bottom",
                       trigger: "hover",
                       container: "body"
                       });
                       });
                                   ')
            ),
            br(),
            br(),
            # Cajas de información de la base de datos depurada:
            h3(strong("Información de la base de datos")),
            br(),
            fluidRow(
              # Edificaciones o bloques a analizar:
              valueBoxOutput("BL"),
              # Fecha de inicio de registros de la base de datos:
              valueBoxOutput("inicio_reg1"),
              # Fecha de fin de registros de la base de datos:
              valueBoxOutput("fin_reg1")
            ),
            br(),
            # Información a pie de página
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
    ), 
    
    # Filtrado corriente ----
    # Creación de pestaña para filtrado de datos por fechas:
    tabItem(tabName = "datos",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                solidHeader = TRUE, width = 100,status = "info" ),
            br(),
            h4(strong("Información general para filtro de datos:")),
            br(),
            # Selección de ventana de tiempo de interés:
            sidebarLayout(
              sidebarPanel(
                HTML("<br><br><br><br><br><br><br>"),
                # Fecha inicial del filtro:
                dateInput(inputId = "date1_id", 
                          label = "Selecciona fecha inicial de los datos:",
                          value = as.Date("2023/09/29"), format = "dd/mm/yyyy", language = "es"),
                # Fecha final del filtro:
                dateInput(inputId = "date2_id", 
                          label = "Selecciona fecha final del los datos:",
                          value = Sys.Date(),format = "dd/mm/yyyy", language = "es"),
                HTML("<br><br><br><br><br><br><br><br>")
              ),
              # Imagen colocada en la primera interfaz de introducción de información.
              mainPanel(
                tags$image(src = "Universidad-Nacional-de-Colombia-sede-Medellin.jpg", height = "500px", width = "750px")
              ) 
            ),
            # Para mostrar la base de datos filtrada:
            h3(strong("Base de datos filtrada por fecha:")),
            DTOutput("base_datos"),
            br(),
            br(),
            # Mostrar imagen Límites de la norma IEEE519
            tags$h2("Información de importancia para el análisis de armónicos", style = "text-align:center; font-weight:bold;"),
            h3(strong("Límites de armónicos permitidos en un sistema eléctrico - Norma IEEE 519-1992")),
            br(),
            tags$image(src = "LimitesIEEE519.png", height = "350px", width = "400px", style = "text-align:center;"),
            h4("Los valores permisibles para armónicos mostrados en la imagen anterior, 
               dependen de la relación de cortocircuito o impedancia relativa (Icc/IL),
               donde se define que entre mayor sea ésta, mayor será el límite permisible
               de la distorsión armónica. Para el análisis en cuestión, se trabaja bajo 
               una impedancia relativa entre 20-50, lo cual indica que el límite para 
               armónicos de orden <11 es 7%."),
            br(),
            
            # Información a pie de página:
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
    ),
    
    tabItem(tabName = "tab_distorcion",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
                solidHeader = TRUE, width = 100, status = "info"),
            br(),
            tags$h2("Análisis de distorsion armonica de corriente", style = "text-align:center; font-weight:bold;"),
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
            h4("En los siguientes gráficos se puede observar el comportamiento de la distorsión armónica total de 3°, 5° y 7° Orden en la ventana de tiempo seleccionada. La línea negra representa el límite permitido de armónicos basado en la normativa IEEE 519-1992, con un valor límite de 7%"),
            br(),
            fluidRow(
              # Gráfico de DA 3° Orden
              box(title = "Distorsión armónica - Orden 3",
                  plotlyOutput("arm3_comp"),
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
            ),
            fluidRow(
              # Gráfico de DA 5° Orden
              box(title = "Distorsión armónica - Orden 5",
                  plotlyOutput("arm5_comp"),
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
            ),
            fluidRow(
              # Gráfico de DA 7° Orden
              box(title = "Distorsión armónica - Orden 7",
                  plotlyOutput("arm7_comp"),
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)
            ),
            br(),
            
            tags$fieldset(
              h5("Ingrese aquí los comentarios que desee (máximo 500 caracteres): "),
              tags$legend("Comentarios"),
              tags$textarea(id = "comments", rows = 5, cols = 150, maxlength = 500)
            ),
            br(),
            br(),
            
            # Información a pie de página
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
    ),
    # Depuracion voltaje ----
    
    tabItem(tabName = "voltage_debug",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
                solidHeader = TRUE, width = 100, status = "info"),
            br(),
            h3(strong("Cargar las bases de datos")),
            br(),
            sidebarLayout(
              sidebarPanel(
                fileInput("voltage_debug_files",
                          "Sube aquí el archivo Excel",
                          multiple = TRUE,   # SOLO UN ARCHIVO
                          accept = c('.csv', ".xlsx")),
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
    ),
    # Filtrado voltaje ----
    
    tabItem(tabName = "voltage_data",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), 
                align = "center", solidHeader = TRUE, width = 100, status = "info"),
            br(),
            h4(strong("Información general para filtro de datos:")),
            br(),
            sidebarLayout(
              sidebarPanel(
                HTML("<br><br><br><br><br><br><br>"),
                # Fecha inicial del filtro:
                dateInput(inputId = "voltage_date_start", 
                          label = "Selecciona fecha inicial de los datos:",
                          value = as.Date("2023/09/29"), format = "dd/mm/yyyy", language = "es"),
                # Fecha final del filtro:
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
            DTOutput("voltage_debug_filtrado"),
            br(), br(),
            
            tags$h2("Información de importancia para el análisis de armónicos", 
                    style = "text-align:center; font-weight:bold;"),
            h3(strong("Límites de armónicos permitidos en un sistema eléctrico - Norma IEEE 519")),
            br(),
            br(),
            tags$image(src = "norma IEEE 519.jpg", height = "350px", width = "600px", style = "text-align:center;"),
            h4(""),
            br(),
            h3(strong("Límites de armónicos permitidos en un sistema eléctrico - Norma NTC 5001")),
            br(),
            br(),
            tags$image(src = "norma NTC 5001.jpg", height = "300px", width = "450px", style = "text-align:center;"),
            h4(""),
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
    ),
    # --------------Grafico armonicos-----------------
    tabItem(tabName = "tab_graficos",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center",
                solidHeader = TRUE, width = 100, status = "info"),
            br(),
            tags$h2("Analisis de distorsion armonica de tension (THD)", style = "text-align:center; font-weight:bold;"),
            br(),
            
            fluidRow(
              valueBoxOutput("voltage_debug_BL"),
              valueBoxOutput("voltage_debug_inicio"),
              valueBoxOutput("voltage_debug_fin")
            ),
            br(),
            fluidRow(
              box(title = "Configuración de Límites",
                  width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  
                  selectInput("normativa_select", "Seleccione la norma:",
                              choices = c("IEEE 519", "NTC 5001", "Valor personalizado")),
                  
                  conditionalPanel(
                    condition = "input.normativa_select == 'Valor personalizado'",
                    numericInput("limite_armonico", "Límite armónico individual (%):", value = 5, min = 0, max = 100, step = 0.1),
                    numericInput("limite_thd", "Límite THD total (%):", value = 5, min = 0, max = 100, step = 0.1)
                  )
              )
            ),
            h4("En los siguientes gráficos se puede observar el comportamiento de la distorsión armónica total de 3° y 5° orden o del consolidado, en la ventana de tiempo seleccionada, para el voltaje. La línea roja representa el límite permitido de armónicos basado en las normativas IEEE 519, NTC 5001 o así como valores personalizados aplicables."),
            
            br(),
            uiOutput("graficos_ui"),  # Aquí se insertarán los gráficos dinámicos
            br(),
            br(),
            
            h6("."),
            
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
    ),

    # -----------cortes de energia ----------------
    tabItem(
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
          fileInput("file_id", label = "Seleccione el archivo de datos a usar:"),
          selectInput("area_selector", "Filtrar por área:",
                      choices = c("Todas", "Minas", "Volador","Río"),
                      selected = "Todas")
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
      plotlyOutput("grafico_dif_mes",,height = "700px"),
      br(),
      br(),
      plotlyOutput("grafico_bloque",height = "1000px"),
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
    ),
    # Creación página de ayuda:
    tabItem(tabName = "tab_help",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), align = "center", 
                solidHeader = TRUE, width = 100,status = "info" ),
            tags$h2("Manual de uso", style = "text-align:center; font-weight:bold;"),
            br(),
            # Documento PDF con ayuda:
            tags$iframe(style="height:800px;width:100%", src="Manual de uso.pdf"),
            HTML("<br><br><br><br><br><br>"),
            # Información a pie de página.
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
    ),
    
    tabItem(tabName = "tab_creditos",
            box(h1(strong(HTML("Universidad Nacional de Colombia sede Medellín </p> <p> Sistema de gestión de energía"))), 
                align = "center", solidHeader = TRUE, width = 100, status = "info"),
            
            br(),
            h2("Aplicativo creado por:", style = "font-weight:bold; color:#3A959D;"),
            br(),
            
            tags$div(style = "line-height: 1.6; font-size:16px;",
                     HTML("
      <strong>Jhon Alexander Ríos García (2022 - 2)</strong><br>
      <a href='mailto:jhrios@unal.edu.co'>jhrios@unal.edu.co</a><br>
      Estadístico UNAL sede Medellín.<br><br>

      <strong>Yojan Andrés Alcaraz Pérez (2023 - 1)</strong><br>
      <a href='mailto:yalcaraz@unal.edu.co'>yalcaraz@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br><br>

      <strong>Daniela Arbeláez Montoya (2023 - 2)</strong><br>
      <a href='mailto:darbelaezm@unal.edu.co'>darbelaezm@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br><br>

      <strong>Laura Valentina Rincón Guataquira (2024 - 2)</strong><br>
      <a href='mailto:lrincong@unal.edu.co'>lrincong@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br><br>

      <strong>Jhon Sebastian Chidiak Olaya (2024 - 2)</strong><br>
      <a href='mailto:jchidiak@unal.edu.co'>jchidiak@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br><br>
      
      <strong>Juan Diego Espinosa Hernandez (2025 - 1)</strong><br>
      <a href='mailto:jespinosah@unal.edu.co'>jespinosah@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br><br>
      
      <strong>David Esteban Cartagena Mejia (2025 - 1)</strong><br>
      <a href='mailto:dcartagena@unal.edu.co'>dcartagena@unal.edu.co</a><br>
      Estudiante de Estadística UNAL sede Medellín.<br>
    ")
            )
    )
    
    
    )))
# Por defecto Shiny limita la carga de datos a archivos de 5 MB.
# Para aumentar la cantidad de datos que se pueden subir en Shiny se propone la siguiente instrucción:
options(shiny.maxRequestSize=Inf,
        shiny.autoreload.enabled = TRUE #Recarga automática al detectar cambios
)
# Ahora se pueden subir archivos de 500 MB.

# Ahora se crea el servidor de la aplicación, el cual permite realizar todas las operaciones que se muestran en la interfaz de usuario.

# Server -----

server <- function(input, output) {
  # -----------cortes------------  
  # Dataset reactivo
  # -----------cortes------------  
  # Dataset reactivo
  data_corte <- reactive({
    validate(need(input$file_id != "", "Ingrese la base de datos a analizar."))
    inFile <- input$file_id
    cortes <- read_excel(inFile$datapath, skip = 1)
    cortes <- cortes %>%
      mutate(`FECHA RESTABLECIMIENTO` = as.character(`FECHA RESTABLECIMIENTO`)) %>%
      mutate(`FECHA RESTABLECIMIENTO` = case_when(
        grepl("^[0-9]+$", `FECHA RESTABLECIMIENTO`) ~ 
          as.character(as.Date(as.numeric(`FECHA RESTABLECIMIENTO`), origin = "1899-12-30")),
        grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", `FECHA RESTABLECIMIENTO`) ~ 
          as.character(dmy(`FECHA RESTABLECIMIENTO`)),
        grepl("^\\d{4}-\\d{2}-\\d{2}$", `FECHA RESTABLECIMIENTO`) ~ 
          `FECHA RESTABLECIMIENTO`,
        grepl("(?i)no restablecimiento", `FECHA RESTABLECIMIENTO`) ~ 
          NA_character_,
        TRUE ~ NA_character_),
        `HORA DE RESTABLECIMIENTO` = as.numeric(`HORA DE RESTABLECIMIENTO`),
        `HORA DE RESTABLECIMIENTO` = as.POSIXct(`HORA DE RESTABLECIMIENTO` * 86400, origin = "1899-12-31", tz = "UTC")
      )
    cortes <- cortes[,c(1:3,6:9)]
    cortes$FECHA <- as.character(cortes$FECHA)
    cortes$`FECHA RESTABLECIMIENTO` <- as.character(cortes$`FECHA RESTABLECIMIENTO`)
    cortes$`FECHA RESTABLECIMIENTO` <- ifelse(is.na(cortes$`FECHA RESTABLECIMIENTO`), cortes$FECHA, cortes$`FECHA RESTABLECIMIENTO`)
    cortes$PROGRAMADO <- ifelse(is.na(cortes$PROGRAMADO), "No", "Sí")
    cortes$`HORA DE CORTE` <- substr(cortes$`HORA DE CORTE`, 12 ,19)
    cortes$`HORA DE RESTABLECIMIENTO` <- substr(cortes$`HORA DE RESTABLECIMIENTO`, 12 ,19)
    cortes$`HORA DE RESTABLECIMIENTO` <- ifelse(is.na(cortes$`HORA DE RESTABLECIMIENTO`), cortes$`HORA DE CORTE`, cortes$`HORA DE RESTABLECIMIENTO`)
    cortes$FH_corte <- as.POSIXct(paste0(cortes$FECHA," ",cortes$`HORA DE CORTE`),format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
    cortes$FH_rest <- as.POSIXct(paste0(cortes$`FECHA RESTABLECIMIENTO`," ",cortes$`HORA DE RESTABLECIMIENTO`),format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
    cortes$Minutos <- ifelse(as.numeric(difftime(cortes$FH_rest, cortes$FH_corte, units = "mins"))<0,0,as.numeric(difftime(cortes$FH_rest, cortes$FH_corte, units = "mins")))
    cortes$PROGRAMADO <- as.factor(cortes$PROGRAMADO)
    meses_espanol <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                       "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    num_meses <- month(cortes$FECHA, label = TRUE)
    meses <- meses_espanol[num_meses]
    cortes$Mes <- as.character(meses)
    cortes <- cortes[,c(1,11,2:7,10)]
    
    # Estandarizar nombres de campus
    cortes$CAMPUS <- tolower(cortes$CAMPUS)
    
    cortes$CAMPUS <- case_when(
      str_detect(cortes$CAMPUS, "volador") ~ "Volador",
      str_detect(cortes$CAMPUS, "roble|mina") ~ "Minas",
      str_detect(cortes$CAMPUS, "rio") ~ "Río",
      TRUE ~ "Otro"
    )
    
    if (input$area_selector == "Minas") {
      cortes <- cortes %>% filter(CAMPUS == "Minas")
    } else if (input$area_selector == "Volador") {
      cortes <- cortes %>% filter(CAMPUS == "Volador")
    } else if (input$area_selector == "Río") {
      cortes <- cortes %>% filter(CAMPUS == "Río")
    }
    
    return(cortes)
  })
  
  
  data_corte_filtrada <- reactive({
    df <- data_corte()
    bloques <- str_extract_all(df$BLOQUE, "(M-?)?(\\d\\d? ?[ABCabc]?)|(TODO EL CAMPUS|APIARIO|INGEOMINAS|PORTERIA[S]?)")
    
    bloques <- lapply(bloques, trimws)
    df_exp <- df[rep(1:nrow(df), lengths(bloques)), ]
    df_exp$Bloque_unit <- unlist(bloques)
    
    return(df_exp)
  })
  
  
  dif_prog <- reactive({
    data_corte() %>%
      group_by(PROGRAMADO) %>%
      summarise(
        horas = (sum(Minutos,na.rm=T)/60),
        dias = (sum(Minutos,na.rm=T)/60)/24
      )
  })
  
  meses_ordenados <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                       "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  dif_mes <- reactive({
    data_corte() %>%
      mutate(Mes = factor(Mes, levels = meses_ordenados)) %>%
      group_by(Mes) %>%
      summarise(horas = (sum(Minutos,na.rm=T)/60))
  })
  
  library(stringr) # For string manipulation
  library(dplyr)   # For data manipulation (e.g., filter, group_by, summarise)
  
  corte_hora_bloque <- reactive({
    df <- data_corte()
    
    extraction_pattern <- "(M-?)?(\\d\\d? ?[ABCabc]?)|(TODO EL CAMPUS|APIARIO|INGEOMINAS|PORTERIA[S]?)"
    
    bloques <- str_extract_all(df$BLOQUE, regex(extraction_pattern, ignore_case = TRUE))
    bloques <- lapply(bloques, trimws)
    bloques_u <- unlist(bloques)
    
    duracion_u <- rep(df$Minutos / 60, lengths(bloques))
    
    corte_bloque <- data.frame(Bloque = bloques_u, Duracion_min = duracion_u)
    
    # Limpieza inicial
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "-", "")    # Eliminar guiones
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, " ", "")    # Eliminar espacios
    corte_bloque$Bloque <- toupper(corte_bloque$Bloque)                      # Mayúsculas
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "PORTERIAS", "PORTERIA") # Singularizar
    
    # Agregar cero a números del 1 al 9 que NO estén precedidos por M
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "(?<!M)(\\b\\d)(?=[A-Z])", "0\\1")
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "(?<!M)(^|[^A-Z0-9])([1-9])([^A-Z0-9]|$)", "\\1 0\\2\\3")
    
    
    # Para "TODO EL CAMPUS", a pesar de str_to_title, si queremos las mayúsculas específicas:
    # Podrías hacer un reemplazo final si "Todo El Campus" no es el formato exacto que deseas
    # después de str_to_title para este caso particular.
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "TODOELCAMPUS", "Todo el campus")
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "APIARIO", "Apiario")
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "INGEOMINAS", "Ingeominas")
    corte_bloque$Bloque <- str_replace_all(corte_bloque$Bloque, "PORTERIA", "Porteria")
    
    corte_bloque %>%
      filter(!is.na(Bloque) & Bloque != "" & Bloque != "0") %>%
      group_by(Bloque) %>%
      summarise(Horas = round(sum(Duracion_min), 2))
  })
  
  
  
  
  # Outputs
  
  output$base_data_cor <- renderDT({
    data_corte()
  }, options = list(scrollX = TRUE))
  
  output$infoBox_prog <- renderUI({
    prop <- prop.table(table(data_corte()$PROGRAMADO))
    infoBox(
      "¿Corte Programado?",
      paste0("Sí: ", round(prop["Sí"] * 100,2), "%"),
      paste0("No: ", round(prop["No"] * 100,2), "%"),
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$infoBox_tiempo <- renderUI({
    infoBox(
      "Tiempo total de corte",
      paste0(round((sum(data_corte()$Minutos,na.rm=T)/60),2), " horas"),
      paste0(round((sum(data_corte()$Minutos,na.rm=T)/60)/24,2), " días"),
      icon = icon("clock", class = "fa-fw"),
      color = "green"
    )
  })
  
  output$infoBox_hora_prog <- renderUI({
    output_data <- dif_prog()
    infoBox(
      "Tiempo de corte vs programación",
      paste0("Sí: ", round(output_data$horas[output_data$PROGRAMADO == "Sí"],2), " horas"),
      paste0("No: ", round(output_data$horas[output_data$PROGRAMADO == "No"],2), " horas"),
      icon = icon("clock", class = "fa-fw"),
      color = "green"
    )
  })
  
  output$grafico_dif_mes <- renderPlotly({
    plot_ly(dif_mes(), x = ~Mes, y = ~round(horas,2), type = "bar", color = I("#007bff")) %>%
      layout(title = "Cantidad de horas sin energía por mes",
             xaxis = list(title = "Mes"),
             yaxis = list(title = "Horas"))
  })
  
  output$grafico_bloque <- renderPlotly({
    data_filtrada <- corte_hora_bloque() %>%
      mutate(Bloque = reorder(Bloque, Horas))
    plt <- ggplot(data_filtrada) +
      geom_col(aes(Horas, Bloque), fill = "#007bff", width = 0.7) +
      xlab("Tiempo total sin energía en horas") + ggtitle("Horas de corte por bloque")
    ggplotly(plt) %>%
      layout(height = 1000)
  })
  
  ##### ----------------------------------------------------------------------
  # Extraer la ruta de cada arcviho excel:
  output$filedf2 <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath 
  })
  
  # Mostrar el widget de selección de entrada con la lista de archivos cargados por el usuario
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Selecciona las bases de datos que desea visualizar y analizar:"),
         selectizeInput("Select"," ",choices=input$file$name, multiple = TRUE,
                        options = list(plugins = list('remove_button')))
    )
  })
  
  # Lectura y guardado de cada archivo de datos:
  datos <- eventReactive(input$Select, {
    
    # Para almacenar cada dataset:
    datos_previos <- reactiveValues(datos = NULL)
    
    # Obtener los datos ya cargados:
    if (!is.null(datos_previos$datos)) {
      datos_combinados <- datos_previos$datos
    } else {
      datos_combinados <- NULL
    }
    
    # Obtener los archivos nuevos seleccionados:
    archivos_nuevos <- setdiff(input$Select, colnames(datos_combinados))
    
    # Cargar los archivos nuevos
    if (length(archivos_nuevos) > 0) {
      # Obtener la cantidad total de archivos a cargar
      total_archivos <- length(archivos_nuevos)
      # Iniciar el mensaje de progreso de carga de datos:
      withProgress(message = "Cargando datos...", value = 0, {
        
        # Cargar cada archivo nuevo y agregarlo a los datos combinados
        for (i in seq_along(archivos_nuevos)) {
          
          archivo_nombre <- archivos_nuevos[i]
          
          datos <- read_excel(input$file$datapath[input$file$name == archivo_nombre])
          # Renombrar las columnas:
          colnames(datos) <- c("Fecha", "1", "2", "3", "4", "5", "6", "7", "8")
          # Extraer mediante expresiones regulares del nombre del archivo, el nombre del bloque:
          archivo_id <- str_extract(archivo_nombre, "(?<=-)[^_-]+")
          
          # Agregar columna de bloque:
          datos_nuevos <- datos %>% 
            mutate(Bloque = archivo_id)
          
          # Combinar todas las bases de datos en una sola:
          if (is.null(datos_combinados)) {
            datos_combinados <- datos_nuevos
          } else {
            datos_combinados <- bind_rows(datos_combinados, datos_nuevos)
          }
          
          # Actualizar el valor del mensaje de progreso
          incProgress(1/total_archivos, detail = paste("Cargando", archivo_nombre))
        }
        # Actualizar los datos previos
        datos_previos$datos <- datos_combinados
      })
    }
    # Retornar la base de datos que une todos las demás bases:
    return(datos_combinados)
  })
  
  # Esta salida reactiva contiene el conjunto de datos y muestra el conjunto de datos en formato de tabla
  output$table <-renderDataTable({ 
    if(is.null(input$file)){return()}
    datos()
  }, options = list(scrollX = TRUE))
  
  # Mostar pestaña de base de datos original: unida e identificada por nombre del bloque:
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Base original", dataTableOutput("table"))
      )
  })
  
  # Evento para el botón de depuración:
  # Aquí se llama a la función debugging_BD() del archivo Funciones_aux.R:
  observeEvent(input$buttonDebug,{
    output$table2 <- renderDataTable({ 
      data_debug <- debugging_BD(datos())[[1]]
      data.frame(data_debug)
    }, options = list(scrollX = TRUE))
    
    # Mostrar la base de datos depurada:
    output$tb <- renderUI({
      tabsetPanel(
        tabPanel("Base depurada", dataTableOutput("table2"))
      )
    })
  })
  
  # Función para cargar el dataset, convertir variable a tipo fecha, renombrar las variables y filtrar por fecha:
  datasetor <- reactive({
    data_debug <- debugging_BD(datos())[[1]]
    # Variable de fecha auxiliar para filtrar:
    data_debug$Fecha_filtro  <- as.Date(data_debug$Fecha, format="%d/%m/%Y")
    # Fecha inicial y fecha final para el filtrado de datos:
    fecha_inicial <- as.Date(input$fecha1_id, format = "%d/%m/%Y")
    fecha_final <- as.Date(input$fecha2_id, format = "%d/%m/%Y")
    
    # Filtrar datos apartir de las fechas proporcionadas por el usuario:
    datos_filtrados <- data_debug %>%
      filter(Fecha_filtro >= fecha_inicial & Fecha_filtro <= fecha_final)
    
    # Eliminar variable auxiliar para filtro de fecha
    datos_filtrados <- datos_filtrados[,-10]
    
    # Dataset a retornar:
    EnM1p2 <- data.frame(datos_filtrados)
    return(EnM1p2)
  })
  
  # Mostrar la base de datos filtrada por fecha:
  output$base_data <- renderDT({
    datasetor()
  })
  
  # Crear dataset resumen con el consumo total de energía activa por día:
  dataset1 <- reactive({ 
    EnM1p2 = datasetor()
    # Extraer columna de energía activa:
    EActivaseg = (EnM1p2[,6])
    DataEActseg = data.frame(Fecha = EnM1p2$Fecha, EnerActiva = EActivaseg) 
    DataEActseg$dia = day(DataEActseg$Fecha)
    DataEActseg$mes = months(DataEActseg$Fecha)
    names(DataEActseg) = c("Fecha", "EnerActiva", "dia", "mes")
    
    # Se agrupan los datos de energía activa en Kwh por día y mes.
    k = DataEActseg %>% group_by(dia,mes) %>% summarise(totalEact = sum(EnerActiva))
    
    # Si se quiere subir la aplicación al servidor ShinyApps se debe realizar una traducción de los nombres que se obtienen con algunas 
    # operaciones de fechas (nombres de los meses y días de la semana). 
    
    seleccion_mes <- as.character(req(input$selector_mes)) # Obtengo el valor actual del selector.
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idact = which(spanish_months == seleccion_mes)
    
    if(tolower(k$mes[1]) == "april"){
      seleccion_mes = english_months[idact]
      kSelact = k[tolower(k$mes) == seleccion_mes,]
    } else{
      seleccion_mes = spanish_months[idact]
      kSelact = k[tolower(k$mes) == seleccion_mes,]
    }
    kSelact
  })
  
  # Dataset comsumo total de energía activa por bloque:
  dataset1_bloque <- reactive({ 
    EnM1p2 = datasetor()
    EActivaseg = (EnM1p2[,c(6,9)])
    DataEActseg = data.frame(Fecha = EnM1p2$Fecha, EnerActiva = EActivaseg) 
    DataEActseg$dia = day(DataEActseg$Fecha)
    DataEActseg$mes = months(DataEActseg$Fecha)
    names(DataEActseg) = c("Fecha", "EnerActiva", "Bloque", "dia", "mes")
    
    
    # Se agrupan los datos de energía activa en Kwh por día, mes y bloque
    k = DataEActseg %>% group_by(dia,mes,Bloque) %>% summarise(totalEact = sum(EnerActiva))
    
    # Si se quiere subir la aplicación al servidor ShinyApps se debe realizar una traducción de los nombres que se obtienen con algunas 
    # operaciones de fechas (nombres de los meses y días de la semana). 
    
    seleccion_mes <- as.character(req(input$selector_mes)) # Obtengo el valor actual del selector.
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idact = which(spanish_months == seleccion_mes)
    
    if(tolower(k$mes[1]) == "april"){
      seleccion_mes = english_months[idact]
      kSelact = k[tolower(k$mes) == seleccion_mes,]
    } else{
      seleccion_mes = spanish_months[idact]
      kSelact = k[tolower(k$mes) == seleccion_mes,]
    }
    kSelact
  })
  
  # Dataset para el consumo de energía activa total consumida por bloque durante toda la ventana de tiempo:
  dataset1_bloquet <- reactive({ 
    EnM1p2 = datasetor()
    EActivaseg = (EnM1p2[,c(6,9)])
    DataEActseg = data.frame(Fecha = EnM1p2$Fecha, EnerActiva = EActivaseg) 
    DataEActseg$dia = day(DataEActseg$Fecha)
    DataEActseg$mes = months(DataEActseg$Fecha)
    names(DataEActseg) = c("Fecha", "EnerActiva", "Bloque", "dia", "mes")
    
    # Se agrupan los datos de energía activa en Kwh por día y mes.
    k = DataEActseg %>% group_by(Bloque) %>% summarise(totalEact = sum(EnerActiva))
    k
  })
  
  # Dataset para el consumo de energía total reactiva consumida por bloque:
  dataset1react_bl_t <- reactive({ 
    EnM1p2 = datasetor()
    Reactivaseg = (EnM1p2[,c(7,9)])
    DataEReactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactiva = Reactivaseg) 
    DataEReactseg$dia = day(DataEReactseg$Fecha)
    DataEReactseg$mes = months(DataEReactseg$Fecha)
    names(DataEReactseg) = c("Fecha", "EnerReactiva", "Bloque", "dia", "mes")
    
    # Se agrupan los datos de energía reactiva bloque:
    kreact = DataEReactseg %>% group_by(Bloque) %>% summarise(totalEreact = sum(EnerReactiva))
  })
  
  # Dataset para el consumo de energía total aparente consumida por bloque:
  dataset1apa_bl_t <- reactive({
    EnM1p2 = datasetor()
    Aparenteseg = (EnM1p2[,c(8,9)])
    DataEApaseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparente = Aparenteseg) 
    DataEApaseg$dia = day(DataEApaseg$Fecha)
    DataEApaseg$mes = months(DataEApaseg$Fecha)
    names(DataEApaseg) = c("Fecha", "EnerAparente", "Bloque", "dia", "mes")
    DataEApaseg[is.na(DataEApaseg)] <- median(na.omit(DataEApaseg$EnerAparente))
    
    # Se agrupan los datos de energía reactiva por día y mes
    kapa = DataEApaseg %>% group_by(Bloque) %>% summarise(totalEapa = sum(EnerAparente))
  })
  # -------------------------------------------
  # Dataset para promedio:
  datasetprom <- reactive({ 
    EnM1p2 = datasetor()
    EActivaseg = (EnM1p2[,6])
    DataEActseg = data.frame(Fecha = EnM1p2$Fecha, EnerActiva = EActivaseg) 
    DataEActseg$dia = day(DataEActseg$Fecha)
    DataEActseg$mes = months(DataEActseg$Fecha)
    names(DataEActseg) = c("Fecha", "EnerActiva", "dia", "mes")
    
    # Se agrupan los datos de energía activa en Kwh por día y mes.
    k = DataEActseg %>% group_by(dia,mes) %>% summarise(totalEact = sum(EnerActiva))
    k
  })
  # -------------------------------------------------
  
  
  
  # Dataset para promedio:
  datasetpromreact <- reactive({ 
    EnM1p2 = datasetor()
    EReactseg = (EnM1p2[,7])
    DataEReactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactiva = EReactseg) 
    DataEReactseg$dia = day(DataEReactseg$Fecha)
    DataEReactseg$mes = months(DataEReactseg$Fecha)
    names(DataEReactseg) = c("Fecha", "EnerReactiva", "dia", "mes")
    
    # Se agrupan los datos de energía activa en Kwh por día y mes.
    k = DataEReactseg %>% group_by(dia,mes) %>% summarise(totalreact = sum(EnerReactiva))
    k
  })
  
  
  
  
  
  # Dataset para promedio:
  datasetpromapar <- reactive({ 
    EnM1p2 = datasetor()
    EAparseg = (EnM1p2[,8])
    DataEAparseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparente = EAparseg) 
    DataEAparseg$dia = day(DataEAparseg$Fecha)
    DataEAparseg$mes = months(DataEAparseg$Fecha)
    names(DataEAparseg) = c("Fecha", "EnerAparente", "dia", "mes")
    
    # Se agrupan los datos de energía activa en Kwh por día y mes.
    k = DataEAparseg %>% group_by(dia,mes) %>% summarise(totalaparente = sum(EnerAparente))
    k
  })
  
  
  
  
  # Para mostar edificaciones en las que se hará el análisis:
  output$edif <- renderValueBox({
    valueBox(
      "Edificación:",
      HTML(paste("Se analizarán los bloques:<br>", paste(unique(datasetor()[,9]),collapse = ", "))),
      icon = icon("building"),
      color = "light-blue" 
    )
  })
  
  # Para mostar fecha de inicio de registros de la base de datos
  output$inicio_reg <- renderValueBox({
    valueBox(
      "Inicio:",
      HTML(paste0("La base de datos contiene registros desde:<br>", aviso(debugging_BD(datos())[[2]]))),
      icon = icon("calendar-minus"),
      color = "light-blue" 
    )
  })
  
  # Para mostar fecha de fin de registros de la base de datos
  output$fin_reg <- renderValueBox({
    valueBox(
      "Fin:",
      HTML(paste0("La base de datos contiene registros hasta:<br>",aviso(debugging_BD(datos())[[3]]))),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })
  
  # Avisos para bloques (edificaciones) y fechas  en energía activa.
  output$bloque_box <- renderValueBox({
    valueBox(
      "Edificación:",
      paste0(unique(datasetor()[,9]),collapse = ", "),
      icon = icon("building"),
      color = "light-blue" 
    )
  })
  
  output$fecha1_box <- renderValueBox({
    valueBox(
      "Fecha inicial:",
      paste0("El análisis se realiza desde ", format(as.Date(aviso(as.character(input$fecha1_id))),"%d/%m/%Y")),
      icon = icon("calendar-minus"),
      color = "light-blue"
    )
  })
  
  output$fecha2_box <- renderValueBox({
    valueBox(
      "Fecha final:",
      paste0("El análisis se realiza hasta ", format(as.Date(aviso(as.character(input$fecha2_id))),"%d/%m/%Y")),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })
  
  
  # Crear dataset con energía reactiva y su consumo diario
  dataset1react <- reactive({ 
    EnM1p2 = datasetor()
    Reactivaseg = as.numeric(EnM1p2[,7])
    DataEReactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactiva = Reactivaseg) 
    DataEReactseg$dia = day(DataEReactseg$Fecha)
    DataEReactseg$mes = months(DataEReactseg$Fecha)
    names(DataEReactseg) = c("Fecha", "EnerReactiva", "dia", "mes")
    DataEReactseg[is.na(DataEReactseg)] <- median(na.omit(DataEReactseg$EnerReactiva))
    
    # Se agrupan los datos de energía reactiva por día y mes
    kreact = DataEReactseg %>% group_by(dia,mes) %>% summarise(totalEreact = sum(EnerReactiva))
    
    seleccion_mes_react <- req(input$selector_mes_react) # Obtengo el valor actual del selector(Español).
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idreact = which(spanish_months == seleccion_mes_react)
    
    if(tolower(kreact$mes[1]) == "april"){
      seleccion_mes_react = english_months[idreact]
      kSelReact = kreact[tolower(kreact$mes) == seleccion_mes_react,]
    } else{
      seleccion_mes_react = spanish_months[idreact]
      kSelReact = kreact[tolower(kreact$mes) == seleccion_mes_react,]
    }
    kSelReact
  })
  
  # Crear dataset de consumo de energía reactiva por bloque:
  dataset1react_bl <- reactive({ 
    EnM1p2 = datasetor()
    Reactivaseg = (EnM1p2[,c(7,9)])
    DataEReactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactiva = Reactivaseg) 
    DataEReactseg$dia = day(DataEReactseg$Fecha)
    DataEReactseg$mes = months(DataEReactseg$Fecha)
    names(DataEReactseg) = c("Fecha", "EnerReactiva", "Bloque", "dia", "mes")
    
    # Se agrupan los datos de energía reactiva por día, mes y bloque:
    kreact = DataEReactseg %>% group_by(dia,mes, Bloque) %>% summarise(totalEreact = sum(EnerReactiva))
    
    seleccion_mes_react <- req(input$selector_mes_react) # Obtengo el valor actual del selector(Español).
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idreact = which(spanish_months == seleccion_mes_react)
    
    if(tolower(kreact$mes[1]) == "april"){
      seleccion_mes_react = english_months[idreact]
      kSelReact = kreact[tolower(kreact$mes) == seleccion_mes_react,]
    } else{
      seleccion_mes_react = spanish_months[idreact]
      kSelReact = kreact[tolower(kreact$mes) == seleccion_mes_react,]
    }
    kSelReact
  })
  
  # Avisos para bloques y fechas en energía reactiva. 
  output$bloque_box2 <- renderValueBox({
    valueBox(
      "Edificación:",
      paste0(unique(datasetor()[,9]),collapse = ", "),
      icon = icon("building"),
      color = "light-blue" 
    )
  })
  
  output$fecha1_box2 <- renderValueBox({
    valueBox(
      "Fecha inicial:",
      paste0("El análisis se realiza desde ", format(as.Date(aviso(as.character(input$fecha1_id))),"%d/%m/%Y")),
      icon = icon("calendar-minus"),
      color = "light-blue"
    )
  })
  
  output$fecha2_box2 <- renderValueBox({
    valueBox(
      "Fecha final:",
      paste0("El análisis se realiza hasta ", format(as.Date(aviso(as.character(input$fecha2_id))),"%d/%m/%Y")),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })
  
  # Crear dataset para el consumo total de energía aparente diario
  dataset1apa <- reactive({ 
    EnM1p2 = datasetor()
    Aparenteseg = as.numeric(EnM1p2[,8])
    DataEApaseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparente = Aparenteseg) 
    DataEApaseg$dia = day(DataEApaseg$Fecha)
    DataEApaseg$mes = months(DataEApaseg$Fecha)
    names(DataEApaseg) = c("Fecha", "EnerAparente", "dia", "mes")
    
    # Se agrupan los datos de energía reactiva por día y mes
    kapa = DataEApaseg %>% group_by(dia,mes) %>% summarise(totalEapa = sum(EnerAparente))
    
    seleccion_mes_apa <- req(input$selector_mes_apa) # Obtengo el valor actual del selector(Español).
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idapa = which(spanish_months == seleccion_mes_apa)
    
    if(tolower(kapa$mes[1]) == "april"){
      seleccion_mes_apa = english_months[idapa]
      kSelApa = kapa[tolower(kapa$mes) == seleccion_mes_apa,]
    } else{
      seleccion_mes_apa = spanish_months[idapa]
      kSelApa = kapa[tolower(kapa$mes) == seleccion_mes_apa,]
    }
    kSelApa
  })
  
  # Crear dataset de energía aparente para consumo total diario por bloque:
  dataset1apa_bl <- reactive({
    EnM1p2 = datasetor()
    Aparenteseg = (EnM1p2[,c(8,9)])
    DataEApaseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparente = Aparenteseg) 
    DataEApaseg$dia = day(DataEApaseg$Fecha)
    DataEApaseg$mes = months(DataEApaseg$Fecha)
    names(DataEApaseg) = c("Fecha", "EnerAparente", "Bloque", "dia", "mes")
    DataEApaseg[is.na(DataEApaseg)] <- median(na.omit(DataEApaseg$EnerAparente))
    
    # Se agrupan los datos de energía reactiva por día y mes
    kapa = DataEApaseg %>% group_by(dia,mes, Bloque) %>% summarise(totalEapa = sum(EnerAparente))
    
    seleccion_mes_apa <- req(input$selector_mes_apa) # Obtengo el valor actual del selector(Español).
    
    english_months <- c("january", "february", "march", "april", "may", "june", 
                        "july", "august", "september", "october", "november", "december")
    spanish_months <- c("enero", "febrero", "marzo","abril","mayo","junio","julio","agosto",
                        "septiembre", "octubre", "noviembre", "diciembre")
    
    idapa = which(spanish_months == seleccion_mes_apa)
    
    if(tolower(kapa$mes[1]) == "april"){
      seleccion_mes_apa = english_months[idapa]
      kSelApa = kapa[tolower(kapa$mes) == seleccion_mes_apa,]
    } else{
      seleccion_mes_apa = spanish_months[idapa]
      kSelApa = kapa[tolower(kapa$mes) == seleccion_mes_apa,]
    }
    kSelApa
  })
  
  # Avisos para bloques y fechas  en energía aparente. 
  output$bloque_box4 <- renderValueBox({
    valueBox(
      "Edificación:",
      paste0(unique(datasetor()[,9]),collapse = ", "),
      icon = icon("building"),
      color = "light-blue" 
    )
  })
  
  output$fecha1_box4 <- renderValueBox({
    valueBox(
      "Fecha inicial:",
      paste0("El análisis se realiza desde ", format(as.Date(aviso(as.character(input$fecha1_id))),"%d/%m/%Y")),
      icon = icon("calendar-minus"),
      color = "light-blue"
    )
  })
  
  output$fecha2_box4 <- renderValueBox({
    valueBox(
      "Fecha final:",
      paste0("El análisis se realiza hasta ", format(as.Date(aviso(as.character(input$fecha2_id))),"%d/%m/%Y")),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })
  
  # Creación de gráfica de serie para consumo total diario de energía aparente.
  output$comp_mes_apa <- renderPlotly({
    CompApaData = dataset1apa()
    
    tryCatch({
      # Gráfica de serie temporal.
      g1 = ggplot(data = CompApaData, aes(x = dia, y = totalEapa))+
        geom_line(colour = "orange", size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white", colour = "orange")+
        xlab("Días del mes")+
        ylab("kvA-h")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1))) +
        scale_x_discrete(limits = factor(c(1:31)))
      ggplotly(g1)
    }, error = function(e) {
      # Captura de error para ver si el mes seleccionado no presenta registros:
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = paste0("¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes"),
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
      
    })
    
    
  })
  
  # InfoBox para serie de energía aparente mensual:
  output$prom3 <- renderInfoBox({
    dat2 = dataset1apa()
    infoBox(
      "Energía aparente (KvA-h) promedio:",
      ifelse(is.na(mean(dat2$totalEapa)) | is.infinite(mean(dat2$totalEapa)), "Sin info", format(round(mean(dat2$totalEapa),2),big.mark = ".", decimal.mark = ",")),
      icon = icon("down-left-and-up-right-to-center")
    )
  })
  
  
  output$max3 <- renderInfoBox({
    dat2 = dataset1apa()
    infoBox("Energía aparente (KvA-h) máxima:",
            icon = icon("arrows-up-to-line"),
            ifelse(is.na(max(dat2$totalEapa)) | is.infinite(max(dat2$totalEapa)), "Sin info", format(round(max(dat2$totalEapa),2),big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$min3 <- renderInfoBox({
    dat2 = dataset1apa()
    infoBox("Energía aparente (Kvar-h) mínima:",
            icon = icon("arrows-down-to-line"),
            ifelse(is.na(min(dat2$totalEapa)) | is.infinite(min(dat2$totalEapa)), "Sin info", format(round(min(dat2$totalEapa),2),big.mark = ".", decimal.mark = ","))
    )
    
  })
  
  # Creación de gráfica de serie de consumo diario de energía aparente por bloque:
  output$comp_mes_apa_bl <- renderPlotly({
    CompApaData = dataset1apa_bl()
    
    tryCatch({
      # Gráfica de serie temporal.
      g2 = ggplot(data = CompApaData, aes(x = dia, y = totalEapa, colour = Bloque))+
        geom_line(size = 1) +
        geom_point(size = 2, shape = 21, fill = "white")+
        xlab("Días del mes") +
        ylab("kvA-h") +
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1))) +
        scale_x_discrete(limits = factor(c(1:31)))
      g2 <- g2 + scale_colour_discrete(name = "Bloque")
      ggplotly(g2)
    }, error = function(e) {
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = paste0("¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes"),
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
    })
  })
  
  # Consumo mensual energía aparente.
  dataset2apa <- reactive({
    EnM1p2 = datasetor()
    Aparenteseg = as.numeric(EnM1p2[,8])
    
    mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
            "Septiembre","Octubre","Noviembre","Diciembre")
    
    # Consumo mensual.
    dataapaseg = data.frame(Fecha = EnM1p2$Fecha, EnerApaSeg =  Aparenteseg)
    names(dataapaseg) = c("Fecha", "EnerApaSeg")
    
    AparenteMesSum = dataapaseg  %>% aggregate(EnerApaSeg ~ month(Fecha), FUN = sum) 
    AparenteMesSum$porcentaje = round(((AparenteMesSum$EnerApaSeg)/sum(AparenteMesSum$EnerApaSeg))*100,3)
    AparenteMesSum$meses = mes[AparenteMesSum$`month(Fecha)`]
    AparenteMesSum
  })
  
  # InfoBox para el consumo por bloque de energía reactiva
  output$valueBoxes_apa <- renderUI({
    df = dataset1apa_bl_t()
    lapply(unique(df$Bloque), function(nivel) {
      suma = sum(df$totalEapa[df$Bloque == nivel])
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste0(format(round(suma,2), big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
      
      
    })
  })
  
  # Barplot para consumo mensual de energía aparente.
  output$por_consumo_mes_apa <- renderPlot({
    data = dataset2apa()
    
    # Crear porcentaje de consumo de energía reactiva.
    df2 <- data %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p3 = ggplot(data, aes(x = meses, y = EnerApaSeg, fill = fct_inorder(as.factor(meses)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits= data$meses) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía aparente (KvA-h) total por meses.") + xlab("Meses") + ylab("KvA-h") + 
      geom_text(aes(label =round(EnerApaSeg,3)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)), 
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p4 =  ggplot(data, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(meses)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Meses")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0"))+
      labs(title = "Porcentaje del consumo de energía aparente (KvA-h) \n por meses.") 
    
    p3+p4
  })
  
  # InfoBox para el total de consumo de energía aparente en los meses. 
  output$totalapa <- renderInfoBox({
    dat2 = dataset2apa()
    infoBox(
      "Energía aparente total:",
      format(round(sum(dat2$EnerApaSeg),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  
  # Consumo en los días de la semana para energía aparente.
  dataset3apa <- reactive({
    
    EnM1p2 = datasetor()
    
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    
    Apaseg = EnM1p2[,8]
    
    dataapaseg = data.frame(Fecha = EnM1p2$Fecha, EnerApaSeg = Apaseg)
    names(dataapaseg) = c("Fecha", "EnerApaSeg")
    
    
    ApadiaSum = dataapaseg %>% aggregate(EnerApaSeg ~ weekdays(Fecha), FUN = sum)
    
    # Función para traducir a español los dias en shiny.io
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ApadiaSum$`weekdays(Fecha)` = str_replace_all(tolower(ApadiaSum$`weekdays(Fecha)`), 
                                                  to_spanish_dict)
    
    ApadiaSum$`weekdays(Fecha)`= factor(ApadiaSum$`weekdays(Fecha)`,
                                        spanish_days, ordered = T)
    
    ApadiaSum = ApadiaSum[order(ApadiaSum$`weekdays(Fecha)`),]
    ApadiaSum$porcentaje = round(((ApadiaSum$EnerApaSeg)/sum(ApadiaSum$EnerApaSeg))*100,2)
    ApadiaSum
    
  })
  
  # Consumo por día de la semana para energía aparente.
  dataset3apa_dia_prom <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    Apaseg = EnM1p2[,8]
    
    dataapaseg = data.frame(Fecha = EnM1p2$Fecha, EnerApaSeg = Apaseg)
    names(dataapaseg) = c("Fecha", "EnerApaSeg")
    
    
    ApadiaProm = dataapaseg %>% aggregate(EnerApaSeg ~ weekdays(Fecha), FUN = sum)
    ApadiaProm$EnerApaSeg <- ApadiaProm$EnerApaSeg/count_weekdays(input$fecha1_id,input$fecha2_id)
    
    # Función para traducir a español los dias en shiny.io
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ApadiaProm$`weekdays(Fecha)` = str_replace_all(tolower(ApadiaProm$`weekdays(Fecha)`), 
                                                   to_spanish_dict)
    
    ApadiaProm$`weekdays(Fecha)`= factor(ApadiaProm$`weekdays(Fecha)`,
                                         spanish_days, ordered = T)
    
    ApadiaProm = ApadiaProm[order(ApadiaProm$`weekdays(Fecha)`),]
    ApadiaProm$porcentaje = round(((ApadiaProm$EnerApaSeg)/sum(ApadiaProm$EnerApaSeg))*100,2)
    ApadiaProm
    
  })
  
  
  # Barplot consumo energía aparente por días de la semana.
  
  output$por_consumo_dia_apa <- renderPlot({
    
    data2 = dataset3apa()
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerApaSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía aparente (KvA-h) total por días de la semana.") + xlab("Días") + ylab("KvA-h") + 
      geom_text(aes(label =round(EnerApaSeg,3)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0"))+
      labs(title = "Porcentaje del consumo de energía aparente (KvA-h) \n por días de la semana.") 
    p1+p2
  })
  
  # Consumo promedio por día de la semana (barplot) energía aparente:
  output$por_prom_consumo_dia_apa <- renderPlot({
    data2 = dataset3apa_dia_prom()
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerApaSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía aparente (KvA-h) promedio por días de la semana.") + xlab("Días") + ylab("KvA-h promedio") + 
      geom_text(aes(label = round(EnerApaSeg,2)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0")) +
      labs(title = "Porcentaje del consumo promedio de energía reactiva (Kvar-h) \n por días de la semana") 
    
    p1+p2
  })
  
  # Creación de la gráfica de energía activa por bloque:
  output$por_consumo_mes_bloque <- renderPlotly({
    CompActData = dataset1_bloque()
    
    tryCatch({
      # Gráfica de serie temporal.
      g2 <- ggplot(data = CompActData, aes(x = dia, y = totalEact, colour = Bloque)) +
        geom_line(size = 1) +
        geom_point(size = 2, shape = 21, fill = "white") +
        xlab("Días del mes") +
        ylab("kw-h") +
        scale_x_discrete(limits = factor(c(1:31))) +
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1)))
      
      g2 <- g2 + scale_colour_discrete(name = "Bloque")
      ggplotly(g2)
    }, error = function(e) {
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = paste0("¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes"),
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
      
    })
  })
  
  # InfoBox para consumo por bloque durante la ventana de tiempo:
  
  # Creación de gráfica de serie para energía activa.
  output$comp_mes <- renderPlotly({
    CompActData = dataset1()
    
    tryCatch({
      # Gráfica de serie temporal.
      g1 = ggplot(data = CompActData, aes(x = dia, y = totalEact))+
        geom_line(colour = "orange", size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white", colour = "orange")+
        xlab("Días del mes")+
        ylab("kw-h")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1))) +
        scale_x_discrete(limits = factor(c(1:31)))
      ggplotly(g1)
    }, error = function(e) {
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = paste0("¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes"),
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
      
    })
    
    
  })
  
  # InfoBox para serie de consumo de energía activa diaria: (para mes seleccionado)
  output$prom1 <- renderInfoBox({
    dat = dataset1()
    infoBox(
      "Energía activa promedio:",
      ifelse(is.na(mean(dat$totalEact)) | is.infinite(mean(dat$totalEact)), "Sin info", format(round(mean(dat$totalEact),3), big.mark = ".", decimal.mark = ",")),
      icon = icon("down-left-and-up-right-to-center")
    )
  })
  
  
  output$max1 <- renderInfoBox({
    dat = dataset1()
    infoBox("Energía activa máxima:",
            icon = icon("arrows-up-to-line"),
            ifelse(is.na(max(dat$totalEact)) | is.infinite(max(dat$totalEact)), "Sin info", format(max(dat$totalEact),big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$min1 <- renderInfoBox({
    dat = dataset1()
    infoBox("Energía activa mínima:",
            icon = icon("arrows-down-to-line"),
            ifelse(is.na(min(dat$totalEact)) | is.infinite(min(dat$totalEact)), "Sin info", format(min(dat$totalEact),big.mark = ".", decimal.mark = ","))
    )
    
  })
  
  output$prom_cost <- renderInfoBox({
    dat = dataset1()
    infoBox(
      "Energía activa promedio:",
      ifelse(is.na(mean(dat$totalEact)) | is.infinite(mean(dat$totalEact)), "Sin info", paste0("$",format(round(mean(dat$totalEact)*input$precio,3), big.mark = ".", decimal.mark = ","))),
      ifelse(is.na(mean(dat$totalEact)) | is.infinite(mean(dat$totalEact)), "", paste0("USD",format(round((mean(dat$totalEact)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","))),
      icon = icon("file-invoice-dollar")
    )
  })
  
  
  # InfoBox para el consumo por bloque de energía activa
  output$valueBoxes <- renderUI({
    df = dataset1_bloquet()
    lapply(unique(df$Bloque), function(nivel) {
      suma = sum(df$totalEact[df$Bloque == nivel])
      costo = suma*input$precio
      costo_USD = (suma*input$precio)/cop_rate
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste0("Consumo: ", format(round(suma,2), big.mark = ".", decimal.mark = ",")),
        paste0("Costo: ", 
               "$", format(round(costo,2), big.mark = ".", decimal.mark = ","), " - ",
               "USD ", format(round(costo_USD,2), big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
      
      
    })
  })
  
  # InfoBox para el consumo por bloque de energía reactiva
  output$valueBoxes_react <- renderUI({
    df = dataset1react_bl_t()
    lapply(unique(df$Bloque), function(nivel) {
      suma = sum(df$totalEreact[df$Bloque == nivel])
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste0(format(round(suma,2), big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
      
      
    })
  })
  
  # Creación de gráfica de serie para energía reactiva.
  output$comp_mes_react <- renderPlotly({
    CompReactData = dataset1react()
    tryCatch({
      # Gráfica de serie temporal.
      g1react = ggplot(data = CompReactData, aes(x = dia, y = totalEreact))+
        geom_line(colour = "orange", size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white", colour = "orange")+
        xlab("Días del mes")+
        ylab("kvar-h")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1))) +
        scale_x_discrete(limits = factor(c(1:31)))
      ggplotly(g1react)
    },error = function(e) {
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = "¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes",
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
      
    })
  })
  
  
  # InfoBox para serie de energía reactiva mensual.
  output$prom2 <- renderInfoBox({
    dat2 = dataset1react()
    infoBox(
      "Energía reactiva (Kvar-h) promedio:",
      ifelse(is.na(mean(dat2$totalEreact)) | is.infinite(mean(dat2$totalEreact)), "Sin info", format(round(mean(dat2$totalEreact),2),big.mark = ".", decimal.mark = ",")),
      icon = icon("down-left-and-up-right-to-center")
    )
  })
  
  
  output$max2 <- renderInfoBox({
    dat2 = dataset1react()
    infoBox("Energía reactiva (Kvar-h) máxima:",
            icon = icon("arrows-up-to-line"),
            ifelse(is.na(max(dat2$totalEreact)) | is.infinite(max(dat2$totalEreact)), "Sin info", format(round(max(dat2$totalEreact),2),big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$min2 <- renderInfoBox({
    dat2 = dataset1react()
    infoBox("Energía reactiva (Kvar-h) mínima:",
            icon = icon("arrows-down-to-line"),
            ifelse(is.na(min(dat2$totalEreact)) | is.infinite(min(dat2$totalEreact)), "Sin info", format(round(min(dat2$totalEreact),2),big.mark = ".", decimal.mark = ","))
    )
    
  })
  
  # Consumo mensual energía activa.
  dataset2 <- reactive({
    EnM1p2 = datasetor()
    EActivaseg = EnM1p2[,6]
    mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
            "Septiembre","Octubre","Noviembre","Diciembre")
    
    # Consumo mensual.
    datactseg = data.frame(Fecha = EnM1p2$Fecha, EnerActSeg =  EActivaseg)
    names(datactseg) = c("Fecha", "EnerActSeg")
    
    ActivaMesSum = datactseg %>% aggregate(EnerActSeg ~ month(Fecha), FUN = sum)
    ActivaMesSum$porcentaje = round(((ActivaMesSum$EnerActSeg)/sum(ActivaMesSum$EnerActSeg))*100,3)
    ActivaMesSum$meses = mes[ActivaMesSum$`month(Fecha)`]
    prom <- datactseg %>% aggregate(EnerActSeg ~ month(Fecha), FUN = mean)
    ActivaMesSum$prom <- prom[,2]
    
    names(ActivaMesSum) <- c("month", "EnerActSeg", "porcentaje", "meses", "promedio")
    
    # Crear un vector con los meses que están en el dataframe original
    meses_originales <- unique(ActivaMesSum$month)
    
    # Crear un vector con los meses que faltan
    meses_faltantes <- setdiff(1:12, meses_originales)
    
    # Crear un dataframe con los meses faltantes y las variables en cero
    df_faltantes <- data.frame(month = meses_faltantes, 
                               EnerActSeg = rep(0, length(meses_faltantes)),
                               porcentaje = rep(0, length(meses_faltantes)),
                               meses = month.name[meses_faltantes],
                               promedio = rep(0, length(meses_faltantes)))
    
    # Vector con los nombres de los meses en español
    meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    
    # Reemplazar los nombres de los meses en inglés por los nombres en español
    df_faltantes$meses <- meses_espanol[match(df_faltantes$month, 1:12)]
    
    # Combinar los dos dataframes utilizando rbind()
    df_completo <- rbind(ActivaMesSum, df_faltantes)
    
    # Ordenar el dataframe por el mes
    df_completo <- df_completo[order(df_completo$month),]
    
    ActivaMesSum = df_completo
  })
  
  ## Para el gráfico:
  dataset2_g <- reactive({
    EnM1p2 = datasetor()
    EActivaseg = EnM1p2[,6]
    
    mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
            "Septiembre","Octubre","Noviembre","Diciembre")
    
    # Consumo mensual.
    datactseg = data.frame(Fecha = EnM1p2$Fecha, EnerActSeg =  EActivaseg)
    names(datactseg) = c("Fecha", "EnerActSeg")
    
    ActivaMesSum = datactseg %>% aggregate(EnerActSeg ~ month(Fecha), FUN = sum)
    ActivaMesSum$porcentaje = round(((ActivaMesSum$EnerActSeg)/sum(ActivaMesSum$EnerActSeg))*100,3)
    ActivaMesSum$meses = mes[ActivaMesSum$`month(Fecha)`]
    ActivaMesSum
  })
  
  
  # Barplot para consumo mensual de energía activa.
  output$por_consumo_mes <- renderPlot({
    data = dataset2_g()
    
    # Crear porcentaje de consumo de energía activa.
    df2 <- data %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p3 = ggplot(data, aes(x = meses, y = EnerActSeg, fill =fct_inorder(as.factor(meses)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = data$meses) +
      ggtitle("Energía activa (Kw-h) total por meses.") + xlab("Meses") + ylab("Kw-h") + 
      geom_text(aes(label =round(EnerActSeg,3)), vjust = -0.5)+
      scale_fill_brewer(palette = "Set3")+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p4 = ggplot(data, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(meses)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Meses")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0"))+
      labs(title = "Porcentaje del consumo de energía activa (Kw-h) \n por meses.")
    p3+p4
  })
  
  
  # InfoBox para consumo promedio y maximo de energía activa en los meses. 
  output$totalact <- renderInfoBox({
    dat = dataset2()
    infoBox(
      "Energía activa promedio:",
      format(round(sum(dat$EnerActSeg),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  
  # Energía activa promedio durante toda la ventana de tiempo:
  output$promact <- renderInfoBox({
    dat = datasetprom()
    infoBox(
      "Energía activa promedio:",
      format(round(mean(dat$totalEact),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  
  
  
  # Energía reactiva promedio durante toda la ventana de tiempo:
  output$promreact <- renderInfoBox({
    dat = datasetpromreact()
    infoBox(
      "Energía reactiva promedio:",
      format(round(mean(dat$totalreact),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  
  
  
  
  # Energía aparente promedio durante toda la ventana de tiempo:
  output$promapar <- renderInfoBox({
    dat = datasetpromapar()
    infoBox(
      "Energía reactiva promedio:",
      format(round(mean(dat$totalaparente),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  
  
  
  
  # Para extraer precio del dólar:
  # Precio actual dolar:
  cop_quote <- getSymbols("COP=X", src = "yahoo")
  cop_rate <- Cl( tail(get("COP=X"), n=1) )
  #cop_rate <- cop_quote[1, "Last"]
  
  output$cop_price <- renderText({
    cop_price <- formatC(cop_rate, digits = 6)
    paste( "USD 1 =", "COP", cop_price)
  })
  
  # Costo total
  output$preciototalact <- renderInfoBox({
    dat = dataset2()
    infoBox("Energía activa máxima:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(sum(dat$EnerActSeg)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((sum(dat$EnerActSeg)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  
  
  output$preciototalreact <- renderInfoBox({
    dat = dataset2react()
    infoBox("Energía reactiva máxima:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(sum(dat$EnerReactSeg)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((sum(dat$EnerReactSeg)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  
  
  output$preciototalapar <- renderInfoBox({
    dat = dataset2apar()
    infoBox("Energía aparente máxima:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(sum(dat$EnerAparSeg)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((sum(dat$EnerAparSeg)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  
  
  output$preciopromreact <- renderInfoBox({
    dat = datasetpromreact()
    infoBox("Costo energía reactiva promedio ventana:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(mean(dat$totalreact)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((mean(dat$totalreact)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  
  
  output$preciopromapar <- renderInfoBox({
    dat = datasetpromapar()
    infoBox("Costo energía aparente promedio ventana:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(mean(dat$totalaparente)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((mean(dat$totalaparente)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  
  
  # Costo promedio energía activa durante la ventana de tiempo:
  output$preciopromact <- renderInfoBox({
    dat = datasetprom()
    infoBox("Costo energía activa promedio ventana:",
            icon = icon("file-invoice-dollar"),
            paste("$", format(round(mean(dat$totalEact)*input$precio,3), big.mark = ".", decimal.mark = ","), sep = ""),
            paste("USD", format(round((mean(dat$totalEact)*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = ""),
            color = "light-blue" 
    )
  })
  
  # Función creada para generar aviso de que no hay información sobre un mes determinado (costo).
  
  ValSin = function(dato){
    texto = paste("$", format(round(dato*input$precio,3), big.mark = ".", decimal.mark = ","), sep = "")
    if(texto == "$NA" || texto == "NaN"){
      return("No hay información de este mes.")
    }else{
      return(texto)
    }
  }
  # Para retornar el costo en dólares:
  ValDol = function(dato){
    texto = paste("USD ", format(round((dato*input$precio)/cop_rate,2), big.mark = ".", decimal.mark = ","), sep = "")
    if(texto == "$NA" || texto == "NaN"){
      return("No hay información de este mes.")
    }else{
      return(texto)
    }
  }
  
  # InfoBox para el total de consumo de energía activa por cada mes. 
  output$valueBoxes_mes_act <- renderUI({
    df = dataset2()
    lapply(unique(df$meses), function(nivel) {
      valor = df$EnerActSeg[df$meses == nivel]
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        ValSin(valor),
        ValDol(valor),
        color = "light-blue"
      )
    })
  })
  
  
  
  
  # InfoBox para el total de consumo de energía reactiva por cada mes. 
  output$valueBoxes_mes_react <- renderUI({
    df = dataset2react()
    lapply(unique(df$meses), function(nivel) {
      valor = df$EnerReactSeg[df$meses == nivel]
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        ValSin(valor),
        ValDol(valor),
        color = "light-blue"
      )
    })
  })
  
  
  
  
  
  
  # InfoBox para el total de consumo de energía aparente por cada mes. 
  output$valueBoxes_mes_apar <- renderUI({
    df = dataset2apar()
    lapply(unique(df$meses), function(nivel) {
      valor = df$EnerAparSeg[df$meses == nivel]
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        ValSin(valor),
        ValDol(valor),
        color = "light-blue"
      )
    })
  })
  
  
  
  # Consumo mensual energía reactiva.
  dataset2react <- reactive({
    EnM1p2 = datasetor()
    Reactivaseg = as.numeric(EnM1p2[,7])
    
    mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
            "Septiembre","Octubre","Noviembre","Diciembre")
    
    # Consumo mensual.
    datareactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactSeg =  Reactivaseg)
    names(datareactseg) = c("Fecha", "EnerReactSeg")
    
    ReactivaMesSum = datareactseg  %>% aggregate(EnerReactSeg ~ month(Fecha), FUN = sum) 
    ReactivaMesSum$porcentaje = round(((ReactivaMesSum$EnerReactSeg)/sum(ReactivaMesSum$EnerReactSeg))*100,3)
    ReactivaMesSum$meses = mes[ReactivaMesSum$`month(Fecha)`]
    ReactivaMesSum
    
  })
  
  
  
  
  # Consumo mensual energía aparente.
  dataset2apar <- reactive({
    EnM1p2 = datasetor()
    Aparenteseg = as.numeric(EnM1p2[,8])
    
    mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
            "Septiembre","Octubre","Noviembre","Diciembre")
    
    # Consumo mensual.
    dataaparseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparSeg =  Aparenteseg)
    names(dataaparseg) = c("Fecha", "EnerAparSeg")
    
    AparenteMesSum = dataaparseg  %>% aggregate(EnerAparSeg ~ month(Fecha), FUN = sum) 
    AparenteMesSum$porcentaje = round(((AparenteMesSum$EnerAparSeg)/sum(AparenteMesSum$EnerAparSeg))*100,3)
    AparenteMesSum$meses = mes[AparenteMesSum$`month(Fecha)`]
    AparenteMesSum
    
  })
  
  
  
  # Creación de la gráfica de consumo de energía reactiva por bloque
  output$por_consumo_mes_bloque_reac <- renderPlotly({
    CompActData = dataset1react_bl()
    
    tryCatch({
      # Gráfica de serie temporal.
      g2 <- ggplot(data = CompActData, aes(x = dia, y = totalEreact, colour = Bloque)) +
        geom_line(size = 1) +
        geom_point(size = 2, shape = 21, fill = "white") +
        xlab("Días del mes") +
        ylab("kvar-h") +
        scale_x_discrete(limits = factor(c(1:31))) +
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1)))
      
      g2 <- g2 + scale_colour_discrete(name = "Bloque")
      ggplotly(g2)
    }, error = function(e) {
      ggplotly(ggplot()) %>% 
        layout(
          annotations = list(
            list(
              text = paste0("¡Atención! \n Por favor seleccione otro mes, \n no se encontraron registros para este mes"),
              x = 0.5, y = 0.5, 
              showarrow = FALSE,
              font = list(size = 15,color = "#ff0000"),
              col = "red"
            )
          )
        )
      
    })
  })
  
  
  
  # Barplot para consumo total mensual de energía reactiva.
  output$por_consumo_mes_react <- renderPlot({
    data = dataset2react()
    
    # Crear porcentaje de consumo de energía reactiva.
    df2 <- data %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p3 = ggplot(data, aes(x = meses, y = EnerReactSeg, fill = fct_inorder(as.factor(meses)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits= data$meses) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía reactiva (Kvar-h) total por meses.") + xlab("Meses") + ylab("Kvar-h") + 
      geom_text(aes(label =round(EnerReactSeg,3)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)), 
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p4 =  ggplot(data, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(meses)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Meses")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0"))+
      labs(title = "Porcentaje del consumo de energía reactiva (Kvar-h) \n por meses.") 
    
    p3+p4
  })
  
  
  # InfoBox para el total de consumo de energía reactiva en los meses. 
  output$totalreact <- renderInfoBox({
    dat2 = dataset2react()
    infoBox(
      "Energía reactiva total:",
      format(round(sum(dat2$EnerReactSeg),3),big.mark = ".", decimal.mark = ","),
      icon = icon("plug-circle-bolt"),
      color = "light-blue" 
    )
  })
  ####
  
  # Consumo en los días de la semana para energía activa.
  dataset3 <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    EActivaseg = EnM1p2[,6]
    datactseg = data.frame(Fecha = EnM1p2$Fecha, EnerActSeg =  EActivaseg)
    names(datactseg) = c("Fecha", "EnerActSeg")
    
    
    ActdiaSum = datactseg %>% aggregate(EnerActSeg ~ weekdays(Fecha), FUN = sum)
    
    # Función para traducir a español los dias en shiny.io.
    
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ActdiaSum$`weekdays(Fecha)` = str_replace_all(tolower(ActdiaSum$`weekdays(Fecha)`), to_spanish_dict)
    
    ActdiaSum$`weekdays(Fecha)` = factor(ActdiaSum$`weekdays(Fecha)`,
                                         spanish_days, ordered = T)
    
    ActdiaSum = ActdiaSum[order(ActdiaSum$`weekdays(Fecha)`),]
    ActdiaSum$porcentaje = round(((ActdiaSum$EnerActSeg)/sum(ActdiaSum$EnerActSeg))*100,3)
    ActdiaSum
  })
  
  
  
  # Consumo en los días de la semana para energía aparente.
  dataset3apar <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    EAparseg = EnM1p2[,8]
    dataparseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparSeg =  EAparseg)
    names(dataparseg) = c("Fecha", "EnerAparSeg")
    
    
    ApardiaSum = dataparseg %>% aggregate(EnerAparSeg ~ weekdays(Fecha), FUN = sum)
    
    # Función para traducir a español los dias en shiny.io.
    
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ApardiaSum$`weekdays(Fecha)` = str_replace_all(tolower(ApardiaSum$`weekdays(Fecha)`), to_spanish_dict)
    
    ApardiaSum$`weekdays(Fecha)` = factor(ApardiaSum$`weekdays(Fecha)`,
                                          spanish_days, ordered = T)
    
    ApardiaSum = ApardiaSum[order(ApardiaSum$`weekdays(Fecha)`),]
    ApardiaSum$porcentaje = round(((ApardiaSum$EnerAparSeg)/sum(ApardiaSum$EnerAparSeg))*100,3)
    ApardiaSum
  })
  
  
  
  # Consumo promedio de energía activa por día de la semana
  dataset4 <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    EActivaseg = EnM1p2[,6]
    datactseg = data.frame(Fecha = EnM1p2$Fecha, EnerActSeg =  EActivaseg)
    names(datactseg) = c("Fecha", "EnerActSeg")
    
    ActdiaProm = datactseg %>% aggregate(EnerActSeg ~ weekdays(Fecha), FUN = sum)
    
    ActdiaProm$EnerActSeg <- ActdiaProm$EnerActSeg/count_weekdays(input$fecha1_id,input$fecha2_id)
    
    # Función para traducir a español los dias en shiny.io.
    
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ActdiaProm$`weekdays(Fecha)` = str_replace_all(tolower(ActdiaProm$`weekdays(Fecha)`), to_spanish_dict)
    
    ActdiaProm$`weekdays(Fecha)` = factor(ActdiaProm$`weekdays(Fecha)`,
                                          spanish_days, ordered = T)
    
    ActdiaProm = ActdiaProm[order(ActdiaProm$`weekdays(Fecha)`),]
    ActdiaProm$porcentaje = round(((ActdiaProm$EnerActSeg)/sum(ActdiaProm$EnerActSeg))*100,3)
    #colnames(ActdiaProm) <- c("Día", "Consumo promedio", "Porcentaje")
    ActdiaProm
  })
  
  
  # Consumo promedio de energía reactiva por día de la semana
  dataset4react <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    EReactseg = EnM1p2[,7]
    datreactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactSeg =  EReactseg)
    names(datreactseg) = c("Fecha", "EnerReactSeg")
    
    ReactdiaProm = datreactseg %>% aggregate(EnerReactSeg ~ weekdays(Fecha), FUN = sum)
    
    ReactdiaProm$EnerReactSeg <- ReactdiaProm$EnerReactSeg/count_weekdays(input$fecha1_id,input$fecha2_id)
    
    # Función para traducir a español los dias en shiny.io.
    
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ReactdiaProm$`weekdays(Fecha)` = str_replace_all(tolower(ReactdiaProm$`weekdays(Fecha)`), to_spanish_dict)
    
    ReactdiaProm$`weekdays(Fecha)` = factor(ReactdiaProm$`weekdays(Fecha)`,
                                            spanish_days, ordered = T)
    
    ReactdiaProm = ReactdiaProm[order(ReactdiaProm$`weekdays(Fecha)`),]
    ReactdiaProm$porcentaje = round(((ReactdiaProm$EnerReactSeg)/sum(ReactdiaProm$EnerReactSeg))*100,3)
    #colnames(ReactdiaProm) <- c("Día", "Consumo promedio", "Porcentaje")
    ReactdiaProm
  })
  
  
  # Consumo promedio de energía aparente por día de la semana
  dataset4apar <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    EAparseg = EnM1p2[,8]
    datreaparseg = data.frame(Fecha = EnM1p2$Fecha, EnerAparSeg =  EAparseg)
    names(datreaparseg) = c("Fecha", "EnerAparSeg")
    
    ApardiaProm = datreaparseg %>% aggregate(EnerAparSeg ~ weekdays(Fecha), FUN = sum)
    
    ApardiaProm$EnerAparSeg <- ApardiaProm$EnerAparSeg/count_weekdays(input$fecha1_id,input$fecha2_id)
    
    # Función para traducir a español los dias en shiny.io.
    
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ApardiaProm$`weekdays(Fecha)` = str_replace_all(tolower(ApardiaProm$`weekdays(Fecha)`), to_spanish_dict)
    
    ApardiaProm$`weekdays(Fecha)` = factor(ApardiaProm$`weekdays(Fecha)`,
                                           spanish_days, ordered = T)
    
    ApardiaProm = ApardiaProm[order(ApardiaProm$`weekdays(Fecha)`),]
    ApardiaProm$porcentaje = round(((ApardiaProm$EnerAparSeg)/sum(ApardiaProm$EnerAparSeg))*100,3)
    #colnames(ApardiaProm) <- c("Día", "Consumo promedio", "Porcentaje")
    ApardiaProm
  })
  
  
  
  # Barplot consumo total energía activa por días de la semana.
  output$por_consumo_dia <- renderPlot({
    data2 = dataset3()
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerActSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía activa (Kw-h) total por días de la semana.") + xlab("Días") + ylab("Kw-h") + 
      geom_text(aes(label = round(EnerActSeg,3)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0")) +
      labs(title = "Porcentaje del consumo de energía activa (Kw-h) \n por días de la semana.") 
    
    p1+p2
  })
  
  
  # InfoBox dias activa. Consumo total
  
  output$valueBoxes_dia_act <- renderUI({
    df = dataset3()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerActSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  
  output$valueBoxes_dia_apar <- renderUI({
    df = dataset3apar()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerAparSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  
  output$valueBoxes_dia_react_prom <- renderUI({
    df = dataset4react()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerReactSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  output$valueBoxes_dia_react <- renderUI({
    df = dataset3react()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerReactSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  
  # Consumo promedio por día de la semana (barplot) energía activa:
  output$por_prom_consumo_dia <- renderPlot({
    data2 = dataset4()
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerActSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía activa (Kw-h) promedio por días de la semana.") + xlab("Días") + ylab("Kw-h promedio") + 
      geom_text(aes(label = round(EnerActSeg,2)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0")) +
      labs(title = "Porcentaje del consumo promedio de energía activa (Kw-h) \n por días de la semana") 
    
    p1+p2
  })
  
  # Costo consumo promedio por día de la semana energía activa:
  # InfoBox dias activa. Consumo total
  ####
  
  output$valueBoxes_dia_act_prom <- renderUI({
    df = dataset4()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerActSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  
  
  output$valueBoxes_dia_apar_prom <- renderUI({
    df = dataset4apar()
    lapply(unique(df$`weekdays(Fecha)`), function(nivel) {
      valor = df$EnerAparSeg[df$`weekdays(Fecha)` == nivel]
      
      infoBox(
        nivel,
        icon = icon("file-invoice-dollar"),
        paste("$", format(round(valor*input$precio,1),big.mark = ".", decimal.mark = ","), sep = ""),
        paste("USD", format(round((valor*input$precio)/cop_rate,1),big.mark = ".", decimal.mark = ",")),
        color = "light-blue"
      )
    })
  })
  
  
  
  # Consumo en los días de la semana para energía reactiva.
  dataset3react <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    
    Reactivaseg = EnM1p2[,7]
    
    datareactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactSeg =  Reactivaseg)
    names(datareactseg) = c("Fecha", "EnerReactSeg")
    
    ReactdiaSum = datareactseg %>% aggregate(EnerReactSeg ~ weekdays(Fecha), FUN = sum)
    
    # Función para traducir a español los dias en shiny.io
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ReactdiaSum$`weekdays(Fecha)` = str_replace_all(tolower(ReactdiaSum$`weekdays(Fecha)`), 
                                                    to_spanish_dict)
    
    ReactdiaSum$`weekdays(Fecha)`= factor(ReactdiaSum$`weekdays(Fecha)`,
                                          spanish_days, ordered = T)
    
    ReactdiaSum = ReactdiaSum[order(ReactdiaSum$`weekdays(Fecha)`),]
    ReactdiaSum$porcentaje = round(((ReactdiaSum$EnerReactSeg)/sum(ReactdiaSum$EnerReactSeg))*100,2)
    ReactdiaSum
    
  })
  
  # Consumo promedio en los días de la semana para energía reactiva.
  dataset3react_prom <- reactive({
    EnM1p2 = datasetor()
    EnM1p2$Fecha = as.POSIXct(EnM1p2$Fecha, format='%d/%m/%Y %H:%M:%S')
    Reactivaseg = EnM1p2[,7]
    datareactseg = data.frame(Fecha = EnM1p2$Fecha, EnerReactSeg =  Reactivaseg)
    names(datareactseg) = c("Fecha", "EnerReactSeg")
    
    
    ReactdiaProm = datareactseg %>% aggregate(EnerReactSeg ~ weekdays(Fecha), FUN = sum)
    ReactdiaProm$EnerReactSeg <- ReactdiaProm$EnerReactSeg/count_weekdays(input$fecha1_id,input$fecha2_id)
    
    # Función para traducir a español los dias en shiny.io
    english_days <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
    spanish_days <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    to_spanish_dict <- spanish_days
    names(to_spanish_dict) <- english_days
    
    ReactdiaProm$`weekdays(Fecha)` = str_replace_all(tolower(ReactdiaProm$`weekdays(Fecha)`), 
                                                     to_spanish_dict)
    
    ReactdiaProm$`weekdays(Fecha)`= factor(ReactdiaProm$`weekdays(Fecha)`,
                                           spanish_days, ordered = T)
    
    ReactdiaProm = ReactdiaProm[order(ReactdiaProm$`weekdays(Fecha)`),]
    ReactdiaProm$porcentaje = round(((ReactdiaProm$EnerReactSeg)/sum(ReactdiaProm$EnerReactSeg))*100,2)
    ReactdiaProm
    
  })
  
  # Barplot consumo energía reactiva por días de la semana.
  
  output$por_consumo_dia_react <- renderPlot({
    
    data2 = dataset3react()
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerReactSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía reactiva (Kvar-h) total por días de la semana.") + xlab("Días") + ylab("Kvar-h") + 
      geom_text(aes(label =round(EnerReactSeg,3)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0"))+
      labs(title = "Porcentaje del consumo de energía reactiva (Kvar-h) \n por días de la semana.") 
    p1+p2
  })
  
  # Consumo promedio por día de la semana (barplot) energía reactiva:
  output$por_prom_consumo_dia_react <- renderPlot({
    data2 = dataset3react_prom()
    
    dias = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado","Domingo")
    
    
    df2 <- data2 %>% 
      mutate(csum = rev(cumsum(rev(porcentaje))), 
             pos = porcentaje/2 + lead(csum, 1),
             pos = if_else(is.na(pos), porcentaje/2, pos))
    
    
    p1 = ggplot(data2, aes(x = dias, y = EnerReactSeg, fill = fct_inorder(as.factor(dias)))) +
      geom_bar(stat  ="identity") + 
      scale_x_discrete(limits = dias) +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Energía reactiva (Kvar-h) promedio por días de la semana.") + xlab("Días") + ylab("Kvar-h promedio") + 
      geom_text(aes(label = round(EnerReactSeg,2)), vjust = -0.5)+
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            legend.position = "none")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    p2 = ggplot(data2, aes(x = "" , y = porcentaje, fill = fct_inorder(as.factor(dias)))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(porcentaje, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Días")) +
      theme(legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5)),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#EEEEE0"),
            plot.background = element_rect(fill = "#EEEEE0"),
            legend.background = element_rect(fill = "#EEEEE0")) +
      labs(title = "Porcentaje del consumo promedio de energía reactiva (Kvar-h) \n por días de la semana") 
    
    p1+p2
  })
  
  
  
  # Avisos para bloques y fechas en las corrientes. 
  ####
  output$bloque_box3 <- renderValueBox({
    valueBox(
      "Edificación:",
      paste0(unique(datasetor()[,9]),collapse = ", "),
      icon = icon("building"),
      color = "light-blue" 
    )
  })
  
  output$fecha1_box3 <- renderValueBox({
    valueBox(
      "Fecha inicial:",
      paste0("El análisis se realiza desde ", format(as.Date(aviso(as.character(input$fecha1_id))),"%B %d de %Y")),
      icon = icon("calendar-minus"),
      color = "light-blue"
    )
  })
  
  output$fecha2_box3 <- renderValueBox({
    valueBox(
      "Fecha final:",
      paste0("El análisis se realiza hasta ", format(as.Date(aviso(as.character(input$fecha2_id))),"%B %d de %Y")),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })
  
  ####
  
  # Variable para corriente en el sistema trifásico.
  datasetcorriente = reactive({
    EnM1p2 = datasetor()
    
    s = stack(EnM1p2[,c(2,3,4,5)])
    s$ind = factor(s$ind, labels = c("Fase 1", "Fase 2", "Fase 3", "Neutro"))
    s
  })
  
  # Boxplot de comparación de corrientes.
  output$corrientes = renderPlotly({
    s = datasetcorriente()
    
    corri = ggplot(s, aes(y = values, x = ind, fill = ind))+
      geom_boxplot() +
      stat_summary(aes(y = values, x = ind), 
                   fun = mean, geom = "point", shape = 16,
                   size = 2, color = "red", position = position_dodge(0.75), fill = "red") +
      scale_fill_manual(values=c("orange", 
                                 "orange", 
                                 "orange",
                                 "orange")) +
      ylab("Amperios")+
      xlab("Fases")+
      theme(legend.position = "none") +
      theme(plot.title = element_text(size = rel(1)),
            axis.title = element_text(size = rel(1)),
            axis.text.x = element_text(size = rel(1)),
            axis.text.y = element_text(size = rel(1)))
    ggplotly(corri)
  })
  
  # Dataset de corriente quitando atípicos:
  corrdatasetatip = reactive({
    EnM1 = datasetor()
    
    fase1 = EnM1[,2][!EnM1[,2] %in% 
                       boxplot.stats(EnM1[,2], coef = 0.5)$out]
    fase2 = EnM1[,3][!EnM1[,3] %in% 
                       boxplot.stats(EnM1[,3], coef = 0.5)$out]
    fase3 = EnM1[,4][!EnM1[,4] %in% 
                       boxplot.stats(EnM1[,4], coef = 0.5)$out]
    s = c(fase1, fase2, fase3)
    s
  })
  
  # Caja de comentarios:
  output$comment_output <- renderPrint({
    input$comments
  })
  verbatimTextOutput("comment_output")
  
  
  
  # Calidad de Energía----
  
  ## Corriente----
  
  ### Depuracion ----
  
  output$filedf2_1 <- renderTable({
    if(is.null(input$files)){return ()}
    input$files$datapath
  })
  
  # Mostrar el widget de selección de entrada con la lista de archivos cargados por el usuario
  output$selectfile1 <- renderUI({
    if(is.null(input$files)) {return()}
    list(hr(), 
         helpText("Selecciona las bases de datos que desea visualizar y analizar:"),
         selectizeInput("Select1"," ",choices=input$files$name, multiple = TRUE,
                        options = list(plugins = list('remove_button')))
    )
  })
  
  # Lectura y guardado de cada archivo de datos:
  cale <- eventReactive(input$Select1, {
    
    # Para almacenar cada dataset:
    datos_prev <- reactiveValues(cale = NULL)
    
    # Obtener los datos ya cargados:
    if (!is.null(datos_prev$cale)) {
      datos_comb <- datos_prev$cale
    } else {
      datos_comb <- NULL
    }
    
    # Obtener los archivos nuevos seleccionados:
    archivos_new <- setdiff(input$Select1, colnames(datos_comb))
    
    # Cargar los archivos nuevos
    if (length(archivos_new) > 0) {
      # Obtener la cantidad total de archivos a cargar
      total_arch <- length(archivos_new)
      # Iniciar el mensaje de progreso de carga de datos:
      withProgress(message = "Cargando datos...", value = 0, {
        
        # Cargar cada archivo nuevo y agregarlo a los datos combinados
        for (i in seq_along(archivos_new)) {
          
          arch_nombre <- archivos_new[i]
          
          cale <- read_excel(input$files$datapath[input$files$name == arch_nombre])
          # Renombrar las columnas:
          colnames(cale) <- c("Fecha", "1", "2", "3", "4", "5", "6","7","8","9", "10", "11")
          # Extraer mediante expresiones regulares del nombre del archivo, el nombre del bloque:
          arch_id <- str_extract(arch_nombre, "(?<=-)[^_-]+")
          
          # Agregar columna de bloque:
          datos_new <- cale %>% 
            mutate(Bloque = arch_id)
          
          # Combinar todas las bases de datos en una sola:
          if (is.null(datos_comb)) {
            datos_comb <- datos_new
          } else {
            datos_comb <- bind_rows(datos_comb, datos_new)
          }
          
          
          # Actualizar el valor del mensaje de progreso
          incProgress(1/total_arch, detail = paste("Cargando", arch_nombre))
        }
        # Actualizar los datos previos
        datos_prev$cale<- datos_comb
      })
    }
    # Retornar la base de datos que une todos las demás bases:
    return(datos_comb)
  })
  
  # Esta salida reactiva contiene el conjunto de datos y muestra el conjunto de datos en formato de tabla
  output$original <-renderDataTable({ 
    if(is.null(input$files)){return()}
    cale()
  }, options = list(scrollX = TRUE))
  
  # Mostar pestaña de base de datos original: unida e identificada por nombre del bloque:
  output$tb1 <- renderUI({
    if(is.null(input$files)) {return()}
    else
      tabsetPanel(
        tabPanel("Base original", dataTableOutput("original"))
      )
  })
  
  # Evento para el botón de depuración:
  # Aquí se llama a la función debug_BD() del archivo Funciones_aux.R:
  observeEvent(input$buttonDebug1,{
    output$depurada <- renderDataTable({
      data_debug1 <- debug_BD(cale())[[1]]
      data.frame(data_debug1)
    }, options = list(scrollX = TRUE))
    
    # Mostrar la base de datos depurada:
    output$tb1 <- renderUI({
      tabsetPanel(
        tabPanel("Base depurada", dataTableOutput("depurada"))
      )
    })
  })
  
  # Función para cargar el dataset, convertir variable a tipo fecha, renombrar las variables y filtrar por fecha:
  datasetor1 <- reactive({
    data_debug1 <- debug_BD(cale())[[1]]
    # Variable de fecha auxiliar para filtrar:
    data_debug1$Fecha_filt  <- as.Date(data_debug1$Fecha, format="%d/%m/%Y")
    # Fecha inicial y fecha final para el filtrado de datos:
    fecha_inicial1 <- as.Date(input$date1_id, format = "%d/%m/%Y")
    fecha_final1 <- as.Date(input$date2_id, format = "%d/%m/%Y")
    
    # Filtrar datos apartir de las fechas proporcionadas por el usuario:
    datos_filtrados1 <- data_debug1 %>%
      filter(Fecha_filt >= fecha_inicial1 & Fecha_filt <= fecha_final1)
    
    # Eliminar variable auxiliar para filtro de fecha
    datos_filtrados1 <- datos_filtrados1[,-12]
    
    # Dataset a retornar:
    EnM1p2_1 <- data.frame(datos_filtrados1)
    return(EnM1p2_1)
  })
  
  # Mostrar la base de datos filtrada por fecha:
  output$base_datos <- renderDT({
    datasetor1()
  })
  
  ### Distorsion armonica ----
  
  #### Distorsión armónica de corriente 3° orden----
  
  # Gráfica DA de 3°orden (única)
  output$arm3_comp <- renderPlotly({
    ComArm3 <- cargar_datos_arm3()
    
    tryCatch({
      p <- ggplot(data = ComArm3)+
        geom_line(aes(x = Fecha, y = Arm3.Fase1, colour = "1"))+
        geom_line(aes(x = Fecha, y = Arm3.Fase2, colour = "2"))+
        geom_line(aes(x = Fecha, y = Arm3.Fase3, colour = "3"))+
        geom_hline(yintercept = 7)+
        scale_color_discrete(name = "Fase")+
        xlab("Fecha") +
        ylab("TDH %") +
        ggtitle("Distorsión Armónica de Corriente Orden 3")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1)))
      ggplotly(p) })
  })
  
  #### Distorsión armónica de corriente 5° orden----
  # Gráfica DA de 5°orden (única)
  output$arm5_comp <- renderPlotly({
    ComArm5 <- cargar_datos_arm5()
    
    tryCatch({
      r <- ggplot(data = ComArm5)+
        geom_line(aes(x = Fecha, y = Arm5.Fase1, colour = "1"))+
        geom_line(aes(x = Fecha, y = Arm5.Fase2, colour = "2"))+
        geom_line(aes(x = Fecha, y = Arm5.Fase3, colour = "3"))+
        geom_hline(yintercept = 7)+
        scale_color_discrete(name = "Fase")+
        xlab("Fecha") +
        ylab("TDH %") +
        ggtitle("Distorsión Armónica de Corriente Orden 5")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1)))
      ggplotly(r) })
  })
  
  #### Distorsión armónica de corriente 7° orden----
  # Gráfica DA de 7°orden (única)
  output$arm7_comp <- renderPlotly({
    ComArm7 <- cargar_datos_arm7()
    
    tryCatch({
      r <- ggplot(data = ComArm7)+
        geom_line(aes(x = Fecha, y = Arm7.Fase1, colour = "1"))+
        geom_line(aes(x = Fecha, y = Arm7.Fase2, colour = "2"))+
        geom_line(aes(x = Fecha, y = Arm7.Fase3, colour = "3"))+
        geom_hline(yintercept = 7)+
        scale_color_discrete(name = "Fase")+
        xlab("Fecha") +
        ylab("TDH %") +
        ggtitle("Distorsión Armónica de Corriente Orden 7")+
        theme(axis.title = element_text(size = rel(1)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1)))
      ggplotly(r) })
  })
  
  arm_BL<- reactive({
    datasetor1()  %>% select("Fecha","Arm3.Fase1", "Arm3.Fase2","Arm3.Fase3", "Arm5.Fase1","Arm5.Fase2","Arm5.Fase3","Arm7.Fase1","Arm7.Fase2", "Arm7.Fase3", "Bloque")
  })
  
  # Función para cargar datos de distorsión armónica de 3° orden
  cargar_datos_arm3 <- function() {
    # Implementar la lógica para cargar y procesar la base de datos dinámica para Orden 3
    arm_BL() # Este es un ejemplo, ajusta según tu necesidad
  }
  
  # Función para cargar datos de distorsión armónica de 5° orden
  cargar_datos_arm5 <- function() {
    # Implementar la lógica para cargar y procesar la base de datos dinámica para Orden 5
    arm_BL() # Este es un ejemplo, ajusta según tu necesidad
  }
  
  # Función para cargar datos de distorsión armónica de 7° orden
  cargar_datos_arm7 <- function() {
    # Implementar la lógica para cargar y procesar la base de datos dinámica para Orden 7
    arm_BL() # Este es un ejemplo, ajusta según tu necesidad
  }
  
  # avisos para bloques y fechas de distorsión de Orden
  output$bl_box <- renderValueBox({
    valueBox(
      "Edificación:",
      paste0(unique(datasetor1()[,11]),collapse = ", "),
      icon = icon("building"),
      color = "light-blue"
    )
  })
  
  output$date1_box <- renderValueBox({
    valueBox(
      "Fecha inicial:",
      paste0("El análisis se realiza desde ", format(as.Date(aviso(as.character(input$date1_id))),"%d/%m/%Y")),
      icon = icon("calendar-minus"),
      color = "light-blue"
    )
  })
  
  output$date2_box <- renderValueBox({
    valueBox(
      "Fecha final:",
      paste0("El análisis se realiza hasta ", format(as.Date(aviso(as.character(input$date2_id))),"%d/%m/%Y")),
      icon = icon("calendar-plus"),
      color = "light-blue" 
    )
  })



  # -------------- Leer y clasificar múltiples bases ------------------
  
  basesProcesadas <- reactive({
    req(input$voltage_debug_files)
    files <- input$voltage_debug_files
    
    lista_bases <- lapply(1:nrow(files), function(i) {
      inFile <- files[i, ]
      base <- readxl::read_excel(inFile$datapath, skip = 1) %>% janitor::clean_names()
      
      if ("fecha" %in% names(base)) {
        base$fecha <- as.POSIXct(base$fecha, format = "%d/%m/%Y %H:%M:%S")
      }
      
      armonicos_vars <- grep("_0(3|5|7)_.*vl[123]$", names(base), value = TRUE, ignore.case = TRUE)
      thd_total_vars <- grep("thd_vl[123]$", names(base), value = TRUE, ignore.case = TRUE)
      
      tipo <- "ninguno"
      
      if (length(armonicos_vars) > 0) {
        nuevos_nombres <- sapply(armonicos_vars, function(nombre) {
          arm <- str_extract(nombre, "_0(3|5|7)_")
          arm_num <- str_remove_all(arm, "_")
          fase <- case_when(
            str_detect(nombre, "vl1") ~ "Fase1",
            str_detect(nombre, "vl2") ~ "Fase2",
            str_detect(nombre, "vl3") ~ "Fase3",
            TRUE ~ "Otro"
          )
          paste0("Arm", substr(arm_num, 2, 2), ".", fase)
        }, USE.NAMES = FALSE)
        names(base)[match(armonicos_vars, names(base))] <- nuevos_nombres
        tipo <- "armonico"
        
      } else if (length(thd_total_vars) > 0) {
        nuevos_nombres <- sapply(thd_total_vars, function(nombre) {
          fase <- case_when(
            str_detect(nombre, "vl1") ~ "Fase1",
            str_detect(nombre, "vl2") ~ "Fase2",
            str_detect(nombre, "vl3") ~ "Fase3",
            TRUE ~ "Otro"
          )
          paste0("TotalARM.", fase)
        }, USE.NAMES = FALSE)
        names(base)[match(thd_total_vars, names(base))] <- nuevos_nombres
        tipo <- "total"
      }
      
      columnas_utiles <- c("fecha", grep("^Arm|^TotalARM", names(base), value = TRUE))
      base <- base[, intersect(columnas_utiles, names(base))]
      
      bloque <- str_to_upper(str_extract(inFile$name, "(?<=CALE-)[^-]+"))
      base$Bloque <- bloque
      
      list(base = base, tipo = tipo, bloque = bloque)
    })
    
    lista_bases
  })
  
  # ----------------- Mostrar tablas -----------------
  
  output$voltage_debug_original <- renderDataTable({
    req(basesProcesadas())
    combined <- do.call(bind_rows, lapply(basesProcesadas(), function(x) x$base))
    combined
  })
  
  output$voltage_debug_depurada <- renderDataTable({
    req(basesProcesadas())
    combined <- do.call(bind_rows, lapply(basesProcesadas(), function(x) x$base))
    combined
  })
  
  
  
  
  
  output$voltage_debug_BL <- renderValueBox({
    req(baseFiltrada())
    bloques <- sapply(baseFiltrada(), function(obj) obj$bloque)
    bloques_unicos <- unique(bloques)
    
    valueBox(
      "Edificaciones:",
      paste(bloques_unicos, collapse = ", "),
      icon = icon("building"),
      color = "light-blue"
    )
  })
  
  output$voltage_debug_inicio <- renderValueBox({
    req(baseFiltrada())
    
    fechas <- lapply(baseFiltrada(), function(obj) {
      if ("fecha" %in% names(obj$base)) min(obj$base$fecha, na.rm = TRUE) else NA
    })
    fecha_minima <- min(do.call(c, fechas), na.rm = TRUE)
    
    valueBox(
      "Fecha de inicio:",
      format(fecha_minima, "%Y-%m-%d %H:%M:%S"),
      icon = icon("calendar-alt"),
      color = "teal"
    )
  })
  
  output$voltage_debug_fin <- renderValueBox({
    req(baseFiltrada())
    
    fechas <- lapply(baseFiltrada(), function(obj) {
      if ("fecha" %in% names(obj$base)) max(obj$base$fecha, na.rm = TRUE) else NA
    })
    fecha_maxima <- max(do.call(c, fechas), na.rm = TRUE)
    
    valueBox(
      "Fecha de fin:",
      format(fecha_maxima, "%Y-%m-%d %H:%M:%S"),
      icon = icon("calendar-check"),
      color = "olive"
    )
  })
  
  
  
  # ------------------ Filtrado global por fechas -------------------
  
  baseFiltrada <- reactive({
    req(basesProcesadas())
    fecha_inicio <- as.POSIXct(input$voltage_date_start)
    fecha_fin <- as.POSIXct(input$voltage_date_end + 1)
    
    lista_filtrada <- lapply(basesProcesadas(), function(obj) {
      if ("fecha" %in% names(obj$base)) {
        base <- obj$base %>% filter(fecha >= fecha_inicio & fecha <= fecha_fin)
        list(base = base, tipo = obj$tipo, bloque = obj$bloque)
      } else {
        NULL
      }
    })
    
    # Eliminar nulos
    lista_filtrada <- Filter(Negate(is.null), lista_filtrada)
    lista_filtrada
  })
  
  output$voltage_debug_filtrado <- renderDT({
    req(baseFiltrada())
    combined <- do.call(bind_rows, lapply(baseFiltrada(), function(x) x$base))
    datatable(combined, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ----------------- Límite de normativa -------------------
  
  limitesSeleccionados <- reactive({
    norma <- input$normativa_select
    
    if (norma == "IEEE 519") {
      list(armonico = 5, thd = 8)
    } else if (norma == "NTC 5001") {
      list(armonico = 3, thd = 5)
    } else {
      list(armonico = input$limite_armonico, thd = input$limite_thd)
    }
  })
  
  # ------------------ UI dinámica para todos los gráficos ------------------
  
  output$graficos_ui <- renderUI({
    req(baseFiltrada())
    bloques_ui <- lapply(baseFiltrada(), function(obj) {
      base <- obj$base
      tipo <- obj$tipo
      bloque <- obj$bloque
      
      if (tipo == "armonico") {
        armonicos <- grep("^Arm", names(base), value = TRUE)
        armonicos <- unique(str_extract(armonicos, "Arm\\d+"))
        
        plots <- lapply(armonicos, function(arm) {
          plotname <- paste0("plot_", bloque, "_", arm)
          box(title = paste("Armónico", arm, "-", bloque),
              plotlyOutput(plotname, height = "400px"),
              width = 12, status = "primary", solidHeader = TRUE)
        })
        
        tagList(plots)
        
      } else if (tipo == "total") {
        plotname <- paste0("plot_thd_", bloque)
        box(title = paste("THD Consolidado -", bloque),
            plotlyOutput(plotname, height = "400px"),
            width = 12, status = "primary", solidHeader = TRUE)
      }
    })
    
    do.call(tagList, bloques_ui)
  })
  
  # ------------------ Render dinámico de gráficos ------------------
  
  observe({
    req(baseFiltrada())
    
    lapply(baseFiltrada(), function(obj) {
      base <- obj$base
      tipo <- obj$tipo
      bloque <- obj$bloque
      
      if (tipo == "armonico") {
        armonicos_vars <- grep("^Arm", names(base), value = TRUE)
        
        armonicos <- unique(str_extract(armonicos_vars, "Arm\\d+"))
        for (arm in armonicos) {
          local({
            arm_local <- arm
            plot_id <- paste0("plot_", bloque, "_", arm_local)
            output[[plot_id]] <- renderPlotly({
              columnas <- grep(paste0("^", arm_local), names(base), value = TRUE)
              datos <- base %>%
                pivot_longer(cols = all_of(columnas), names_to = "fase", values_to = "valor") %>%
                mutate(fase = str_extract(fase, "Fase\\d"))
              
              p <- ggplot(datos, aes(x = fecha, y = valor, color = fase)) +
                geom_line(linewidth = 1) +
                geom_hline(yintercept = limitesSeleccionados()$armonico, linetype = "dashed", color = "red") +
                labs(title = paste("Armónico", arm_local, "-", bloque),
                     x = "Fecha", y = "Distorsión (%)") +
                theme_minimal()
              
              ggplotly(p)
            })
          })
        }
        
      } else if (tipo == "total") {
        local({
          plot_id <- paste0("plot_thd_", bloque)
          output[[plot_id]] <- renderPlotly({
            datos <- base %>%
              pivot_longer(cols = starts_with("TotalARM"), names_to = "fase", values_to = "valor") %>%
              mutate(fase = str_extract(fase, "Fase\\d"))
            
            p <- ggplot(datos, aes(x = fecha, y = valor, color = fase)) +
              geom_line(linewidth = 1) +
              geom_hline(yintercept = limitesSeleccionados()$thd, linetype = "dashed", color = "red") +
              labs(title = paste("THD Consolidado -", bloque),
                   x = "Fecha", y = "THD (%)") +
              theme_minimal()
            
            ggplotly(p)
          })
        })
      }
    })
  })
  
}



# Correr la aplicación 
shinyApp(ui = ui, server = server)