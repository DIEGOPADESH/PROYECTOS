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
summary(Modelo_PS)

# EE

Modelo_EE <- rsm(EE ~ FO(TPP, Chitosan) + TWI(TPP, Chitosan))
summary(Modelo_EE)

# Release

Modelo_Release <- rsm(Release ~ FO(TPP, Chitosan) + TWI(TPP, Chitosan))
summary(Modelo_Release)


################# PARETO ###############

# PS

anova((lm(PS ~ TPP + Chitosan + TPP*Chitosan)))

# EE

anova((lm(EE ~ TPP + Chitosan + TPP*Chitosan)))

# Release

anova((lm(Release ~ TPP + Chitosan + TPP*Chitosan)))

#################

library(pid)
paretoPlot(Modelo_Release)

########### Superficie ##############

library(rsm)
library(plot3D)

# Release

contour(Modelo_Release, ~ TPP +  Chitosan + TPP*Chitosan)
M <- mesh(seq(0,5, length.out = 50),
          seq(0,5, length.out = 50))
V <- with (M, 89.1222 + 8.6917*x - -11.4000 *y + 0.8250*x*y)
surf3D(M$x, M$y, V, colvar = V, colkey = TRUE,
       box = TRUE, bty = "b", phi = 20, theta = 20,
       xlab = "TPP", ylab = "Chitosan")



