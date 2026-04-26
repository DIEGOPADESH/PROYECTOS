# Definicion de variables

TPP <- rep(c(1, 2, 3), each = 3)

Chitosan <- rep(c(2, 3, 4), 3)

######################

PS <- c(106, 161, 182,
        145, 172, 246,
        158, 201, 321)

EE <- c(71.3, 58.2, 49.4,
        83.2, 64.5, 53.8,
        92.7, 87.1, 75.6)

Release <- c(77.2, 68.3, 57.9,
             85.8, 73.1, 62.6,
             97.1, 92.2, 81.1)
######################

library(rsm)

# PS

Modelo_PS <- rsm(PS ~ FO(TPP, Chitosan) + TWI(TPP, Chitosan))
a <- summary(Modelo_PS)
knitr::kable(a$lof[1:3,])


# EE

Modelo_EE <- rsm(EE ~ FO(TPP, Chitosan) + TWI(TPP, Chitosan))
a <- summary(Modelo_EE)
knitr::kable(a$lof[1:3,])


# Release

Modelo_Release <- rsm(Release ~ FO(TPP, Chitosan) + TWI(TPP, Chitosan))
a <- summary(Modelo_Release)
knitr::kable(a$lof[1:3,])

################# PARETO ###############

# PS

anova((lm(PS ~ TPP + Chitosan + TPP*Chitosan)))

# EE

anova((lm(EE ~ TPP + Chitosan + TPP*Chitosan)))

# Release

anova((lm(Release ~ TPP + Chitosan + TPP*Chitosan)))


########### Superficie ##############

library(rsm)

library(plot3D)



# OPTIMOS

optimos <- function(modelo, x, y){
  print(modelo$coefficients[[1]] + modelo$coefficients[[2]]*x + modelo$coefficients[[3]]*y + modelo$coefficients[[4]]*x*y)
}


optimos(Modelo_PS, -0.605364, 1.229885)

optimos(Modelo_EE, 11.500, -7.625)


optimos(Modelo_Release, 0.4125, -0.4125)





############################
############################
############################

# Superficie de respuesta

## PS

par(mfrow = c(1,2))
persp(Modelo_PS, Chitosan ~ TPP, 
      zlab = "PS", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(Modelo_PS$canonical$xs)),
      theta = 15, # coordenadas graficas
      phi = 20,
      col = c("#0000FF", "#006CFF", "#00FF00", "#8FFF00", "#FFFF00", "#FFCC00", "#FF9C00", "#FF5A04", "#FF0000"))

contour(Modelo_PS, ~ TPP + Chitosan)


## EE

par(mfrow = c(1,2))
persp(Modelo_EE, Chitosan ~ TPP, 
      zlab = "EE", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(Modelo_PS$canonical$xs)),
      theta = 250, # coordenadas graficas
      phi = 20, 
      col = c("#0000FF", "#006CFF", "#00FF00", "#8FFF00", "#FFFF00", "#FFCC00", "#FF9C00", "#FF5A04", "#FF0000"))

contour(Modelo_EE, ~ TPP + Chitosan)


## Release

par(mfrow = c(1,2))
persp(Modelo_Release, Chitosan ~ TPP, 
      zlab = "Release", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(Modelo_PS$canonical$xs)),
      theta = 250, # coordenadas graficas
      phi = 20, 
      col = c("#0000FF", "#006CFF", "#00FF00", "#8FFF00", "#FFFF00", "#FFCC00", "#FF9C00", "#FF5A04", "#FF0000"))

contour(Modelo_Release, ~ TPP + Chitosan)


# col = c("#0000FF", "#006CFF", "#00FF00", "#8FFF00", "#FFFF00", "#FFCC00", "#FF9C00", "#FF5A04", "#FF0000")




y <- matrix(c(136, 178, 250, 82, 70, 60, 87, 78, 67), nrow = 3, ncol = 3)

names.arg <- rep(c(2:4),3)


# Graficar el gráfico de barras
barplot(y, xlab="Categoría", ylab="Valor", beside = TRUE, names.arg=names.arg,
        space = c(1, 4), legend.text = c("PS(nm)", "EE(%)", "%release"), col=rep(c("blue", "red", "green"), each = 3))




