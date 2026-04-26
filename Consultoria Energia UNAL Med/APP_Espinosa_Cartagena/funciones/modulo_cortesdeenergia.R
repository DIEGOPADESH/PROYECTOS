modulo_server_cortes <- function(input, output, session) {
  
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(stringr)
  library(shiny)
  
  # Dataset reactivo
  data_corte <- reactive({
    validate(need(input$file_id != "", "Ingrese la base de datos a analizar."))
    inFile <- input$file_id
    cortes <- read_excel(inFile$datapath, skip = 1)
    cortes <- cortes[,c(1:3,6:9)]
    cortes$FECHA <- as.character(cortes$FECHA)
    cortes$`FECHA RESTABLECIMIENTO` <- as.character(cortes$`FECHA RESTABLECIMIENTO`)
    cortes$`FECHA RESTABLECIMIENTO` <- ifelse(is.na(cortes$`FECHA RESTABLECIMIENTO`), cortes$FECHA, as.character(cortes$`FECHA RESTABLECIMIENTO`))
    cortes$PROGRAMADO <- ifelse(is.na(cortes$PROGRAMADO), "No", "Sí")
    cortes$`HORA DE CORTE` <- substr(cortes$`HORA DE CORTE`, 12 ,19)
    cortes$`HORA DE RESTABLECIMIENTO` <- substr(cortes$`HORA DE RESTABLECIMIENTO`, 12 ,19)
    cortes$FH_corte <- paste0(cortes$FECHA," ",cortes$`HORA DE CORTE`)
    cortes$FH_rest <- paste0(cortes$`FECHA RESTABLECIMIENTO`," ",cortes$`HORA DE RESTABLECIMIENTO`)
    cortes$FH_corte <- as.POSIXct(cortes$FH_corte)
    cortes$FH_rest <- as.POSIXct(cortes$FH_rest)
    cortes$Minutos <- as.numeric(difftime(cortes$FH_rest,cortes$FH_corte, units = "mins"))
    cortes$PROGRAMADO <- as.factor(cortes$PROGRAMADO)
    meses_espanol <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                       "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    num_meses <- month(cortes$FECHA, label = TRUE)
    meses <- meses_espanol[num_meses]
    cortes$Mes <- as.character(meses)
    cortes <- cortes[,c(1,11,2:7,10)]
    return(cortes)
  })
  
  dif_prog <- reactive({
    data_corte() %>%
      group_by(PROGRAMADO) %>%
      summarise(
        horas = (sum(Minutos)/60),
        dias = (sum(Minutos)/60)/24
      )
  })
  
  meses_ordenados <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                       "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  dif_mes <- reactive({
    data_corte() %>%
      mutate(Mes = factor(Mes, levels = meses_ordenados)) %>%
      group_by(Mes) %>%
      summarise(horas = (sum(Minutos)/60))
  })
  
  corte_hora_bloque <- reactive({
    bloques <- strsplit(data_corte()$BLOQUE, ",|(?i) y ")
    bloques <- lapply(bloques, trimws)
    bloques_u <- unlist(bloques)
    duracion_u <- rep(data_corte()$Minutos/60, lengths(bloques))
    corte_bloque <- data.frame(Bloque = bloques_u, Duracion_min = duracion_u)
    corte_bloque %>%
      filter(Bloque != "TODO EL CAMPUS") %>%
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
      paste0(round((sum(data_corte()$Minutos)/60),2), " horas"),
      paste0(round((sum(data_corte()$Minutos)/60)/24,2), " días"),
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
      layout(height = 600)
  })
}
