##  Funciones auxiliares para app:

## Función para depurar la base de datos de Consumo de energía:

debugging_BD <- function(datos){
  # Redefinir nombre de columnas:
  colnames(datos) <- datos[1,]
  datos <- datos[-1, ] 
  
  # Obtener fechas de inicio y fin de la base de datos:
  fecha_ini <- datos$Fecha[1]
  fecha_fin <- datos$Fecha[dim(datos)[1]]
  
  # Quitar las columnas innecesarias de la base de datos:
  datos <- datos[,c(1:5,9,8,10)] # Datos para Consumo de energía
  # Renombrar las columnas:
  colnames(datos) <- c("Fecha","Fase_1", "Fase_2", "Fase_3", "Neutro", "Delta_Activa", "Delta_Reactiva", "Bloque")
  datos$Fecha <- as.POSIXct(datos$Fecha, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  
  datos <- datos %>%
    mutate_at(vars(c(2:7)), as.numeric)
  
  datos <- datos %>% 
    mutate_at(c(2:7), ~replace_na(.,0))
  
  # Creación de variable de energía aparente:
  datos$Aparente <- datos$Delta_Activa + datos$Delta_Reactiva
  
  # Eliminación de NA´s:
  datos <- na.omit(datos)
  
  # Reordenar las columnas de la base de datos:
  datos <- datos[,c(1:7,9,8)]
  
  list_return <- list(datos,fecha_ini, fecha_fin)
  return(list_return)
}

## Contar número de veces que aparece cada día de la semana entre dos fechas:
count_weekdays <- function(start_date, end_date) {
  # Generar secuencia de fechas entre start_date y end_date
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Obtener días de la semana correspondientes a cada fecha
  weekdays <- weekdays(dates)
  
  # Contar cantidad de veces que aparece cada día de la semana
  counts <- table(weekdays)
  
  return(counts)
}

# Función creada para generar un aviso de ingreso de información correspondiente en la sección 
# información general.      
aviso = function(av){
  if(av == ""){
    return("Ingrese el valor correspondiente.")
  }else{
    return(av)
  }
}

formatear_fecha <- function(fecha_str){
  fecha <- as.Date(fecha_str)
  fecha_formateada <- as.Date(fecha, format = "%d/%m/%Y")
  return((fecha_formateada))
}

# Generar infoboxes por bloque:
generateInfoBox <- function(df, variable, level) {
  infoBox(
    paste0("Nivel ", level), 
    nrow(filter(df, {{variable}} == level)), 
    icon = icon("car"), 
    color = "green"
  )
}

## Función para depurar la base de datos de calidad de Energía  
debug_BD <- function(cale){
  # Redefinir nombre de columnas:
  colnames(cale) <- cale[1,]
  cale <- cale[-1, ]
  
  # Obtener fechas de inicio y fin de la base de datos:
  date_ini <- cale$Fecha[1]
  date_fin <- cale$Fecha[dim(cale)[1]]
  
  # Quitar las columnas innecesarias de la base de datos:
  cale <- cale[,c(1,2:10,11)] # Datos para calidad de energía
  # Renombrar las columnas:
  colnames(cale) <- c("Fecha","Arm3 Fase1", "Arm3 Fase2","Arm3 Fase3","Arm5 Fase1","Arm5 Fase2", "Arm5 Fase3","Arm7 Fase1","Arm7 Fase2", "Arm7 Fase3", "Bloque")
  cale$Fecha <- as.POSIXct(cale$Fecha, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  
  cale <- cale %>%
    mutate_at(vars(c(2:10)), as.numeric)
  
  cale <- cale %>% 
    mutate_at(c(2:10), ~replace_na(.,0))
  
  # Eliminación de NA´s:
  cale <- na.omit(cale)
  
  list1_return <- list(cale,date_ini, date_fin)
  return(list1_return)
}

format_fecha <- function(date_str){
  fecha <- as.Date(date_str)
  fecha_formateada1 <- as.Date(fecha, format = "%d/%m/%Y")
  return((fecha_formateada1))
  
}

