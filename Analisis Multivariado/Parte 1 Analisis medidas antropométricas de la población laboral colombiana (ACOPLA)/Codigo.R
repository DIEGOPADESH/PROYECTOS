library(splitstackshape)  
library(FactoClass)
#installed.packages("GGally")
library(GGally)
#install.packages("radiant")
library(radiant)
#install.packages("crosstable")
library(crosstable)


uno <- read.table("data.txt", header=T, sep=",")

genera <- function(cedula){
  set.seed(cedula)
  aux <- stratified(uno, "CAT_IMC", 200/2100, bothSets=T)
  mue <- aux$SAMP1
  mue
}

# Cedula Juan Esteban: 1001774078
cedula <- 1001774078

datos <- genera(cedula)


# Pregunta 4 --------------------------------------------------------------

# Opción 1

plotpairs(na.omit(datos[,-c("Sexo", "CAT_IMC")]), maxg = 7)
correlation(na.omit(datos[,-c("Sexo", "CAT_IMC")]))

# Opción 2

ggpairs(na.omit(datos[,-c("CAT_IMC")]),
        columns = 2:8)+
  scale_color_manual(values = c("seagreen"))


# Discriminando por Sexo

ggpairs(na.omit(datos[,-c("CAT_IMC")]),
        columns = 2:8,
        aes(color = Sexo, alpha = .5))+
  scale_color_manual(values = c("dodgerblue", "tomato"))+
  scale_fill_manual(values = c("dodgerblue", "tomato"))


# Pregunta 5 --------------------------------------------------------------

# Opción 1

result <- cross_tabs(datos, var1 = "Sexo", var2 = "CAT_IMC")
summary(result, check = c("observed", "col_perc"))

# Opción 2

crosstable(datos, c(Sexo, CAT_IMC), total="both",
           percent_pattern = '{n} ({p_row})', percent_digits = 0) %>%
  as_flextable()


# Grafico

datos %>% count(Sexo, CAT_IMC) %>%
  mutate(percentaje = round(n/sum(n)*100,2)) %>%
  ggplot(aes(x = CAT_IMC, y = percentaje))+
  geom_col(aes(fill = Sexo), position = "dodge")+
  scale_fill_manual(values = c("dodgerblue", "tomato"))+
  xlab("IMC")+ylab("Porcentaje")+
  theme_minimal()


# Pregunta 6 --------------------------------------------------------------

# Datos de los sujetos desconocidos
datos_desconocidos <- data.frame(
  P1 = c(66.1, 55.8, 62.8, 63.9, 50.7),
  P7 = c(53.9, 50.1, 54.3, 50.6, 46.3),
  P16 = c(73.8, 76.9, 80.4, 75.6, 72.7),
  P22 = c(34.7, 39.5, 37.5, 31.5, 30.4),
  P27 = c(27.6, 24.7, 23.5, 24.9, 23.5),
  P29 = c(20.9, 17.3, 16.5, 18.6, 16.7),
  P38 = c(181.6, 154.5, 156.6, 173.1, 159.5)
)

# Calcular las medias de cada variable para cada categoría de IMC en la base de datos original
medias_imc <- aggregate(. ~ CAT_IMC, data = na.omit(datos[,-c("Sexo")]), FUN = mean)

# Calcular la distancia euclidiana entre los sujetos desconocidos y
# las medias de cada categoría de IMC

list_cat <- list()
df <- data.frame()
for (j in 1:3) {
  for (i in 1:7) {
    df[1:5,i] <- sqrt((datos_desconocidos[,i]-medias_imc[j,i+1])^2)
  }  
  colnames(df) <- names(datos_desconocidos)
  rownames(df) <- paste0("sujeto_",1:5)
  list_cat[[j]] <- df
}

names(list_cat) <- medias_imc[,1]

list_cat <- lapply(list_cat, function(x) t(x))

# Crear un vector para almacenar las categorías asignadas a cada sujeto
categorias_asignadas <- character(length = nrow(datos_desconocidos))

# Recorrer cada sujeto y asignar la categoría con la menor diferencia
for (i in 1:nrow(datos_desconocidos)) {
  diferencias <- sapply(list_cat, function(matriz) sum(matriz[,i]))
  categoria_asignada <- names(list_cat)[which.min(diferencias)]
  categorias_asignadas[i] <- categoria_asignada
}

# Agregar las categorías asignadas al dataframe de sujetos desconocidos
datos_desconocidos$Categoria_Predicha <- categorias_asignadas

# Mostrar el dataframe con las categorías asignadas
print(datos_desconocidos)
