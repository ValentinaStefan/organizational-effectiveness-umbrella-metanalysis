


# AUTOR: Valentina Oana Stefan
# FECHA: 22 - 05 - 2022


# Instalamos los paquetes que vamos a usar si no se tienen ya
# install.packages("psych")
# install.packages("metaSEM")
# install.packages("metafor")


# IMPORTANTE: El archivo Datos.csv y el script de R deben estar en la misma carpeta. Sino, 
# es necesario especificar la carpeta en la que están los datos. 
# setwd("Ubicacion del archivo de datos")


# Leemos el archivo y lo pasamos a formato data frame
datos <- read.csv("Datos.csv", header = TRUE, sep = ";")

# Estructura de los datos
str(datos)


# Creamos variables numericas para codificar las categorias de la VD y la VI


  # VARIABLE DEPENDIENTE

#Creamos la nueva columna y la rellenamos
datos$VD_cat <- vector(length = nrow(datos))

datos$VD_cat[datos$VD_tipo == "Overall performance"] <- 1
datos$VD_cat[datos$VD_tipo == "Operational performance"] <- 2
datos$VD_cat[datos$VD_tipo == "Financial performance"] <- 3


  # VARIABLE INDEPENDIENTE

datos$VI_cat <- vector(length = nrow(datos))

datos$VI_cat[datos$VI_tipo == "Organizational culture"] <- 1
datos$VI_cat[datos$VI_tipo == "HR practices"] <- 2
datos$VI_cat[datos$VI_tipo == "Leadership and management"] <- 3
datos$VI_cat[datos$VI_tipo == "Human capital"] <- 4
datos$VI_cat[datos$VI_tipo == "Employee outcomes"] <- 5



# Vamos a unificar las medidas de los tamanos del efecto para que todas sean correlaciones
# de Pearson

datos$t.efecto <- vector(mode = "numeric", length = nrow(datos))

library(psych)

i <- 0

for (i in 1:nrow(datos)) {
  
  if (datos$TE_tipo[i] == "rho" | datos$TE_tipo[i] == "r"){
    datos$t.efecto[i] <-  datos$TE[i]
  }
    else if (datos$TE_tipo[i] == "d"){
      datos$t.efecto[i] <-  d2r(datos$TE[i])
    }
  
    else if (datos$TE_tipo[i] == "z"){
      datos$t.efecto[i] <-  fisherz2r(datos$TE[i])
    }
  
    else if (datos$TE_tipo[i] == "Spearman rho"){
      datos$t.efecto[i] <-  (2 * sin( pi / 6 * datos$TE[i]))
    }
}
  

# Como no en todos los estudios se informaba de la varianza error del tamano del efecto, 
# la calcularemos en cada caso a partir de los datos disponibles

i <- 0

for (i in 1:nrow(datos)) {
  
  if (is.na(datos$SE.2[i] == TRUE)) {
    
    if (is.na(datos$SE[i]) == FALSE) {
      datos$SE.2[i] <- datos$SE[i] ^ 2
    }
      else if (is.na(datos$IC95_inf[i]) == FALSE) {
        datos$SE.2[i] <- ( (datos$TE[i] - datos$IC95_inf[i]) / 1.96 ) ^ 2
      }
    
      else if (is.na(datos$IC90_inf[i]) == FALSE) {
        datos$SE.2[i] <- ( (datos$TE[i] - datos$IC90_inf[i]) / 1.65 ) ^ 2
      }
  }
  
}


#############################################################################################

#   ANALISIS DE TRES NIVELES

library("metafor")
library("metaSEM")


# Segmentamos los datos según el tipo de efectividad para poder realizar los análisis

datos_VD.1 <- datos[which(datos$VD_cat == 1),]   # Efectividad global
datos_VD.2 <- datos[which(datos$VD_cat == 2),]   # Efectividad operacional
datos_VD.3 <- datos[which(datos$VD_cat == 3),]   # Efectividad financiera

# Para la interpretación de los próximos análisis debe tenerse en cuenta que el argumento
# X = x=model.matrix(~ -1+VI_tipo)) reordena las VIs de modo que los tamaños del efecto
# se muestran en el siguente orden: 

# Slope 1 - Resultados de los Empleados
# Slope 2 - Practicas de RRHH
# Slope 3 - Capital Humano
# Slope 4 - Liderazgo y Gestion
# Slope 5 - Cultura organizacional



# VD 1 - EFECTIVIDAD GLOBAL

fit.1 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.1,
                                  intercept.constraints=0,
                                  x=model.matrix(~ -1+VI_tipo))
summary(fit.1)


# Forest plot

datos_fit1 <- summary(fit.1)

forest.default(x = datos_fit1$coefficients[1:5,1], 
               sei = datos_fit1$coefficients[1:5,2],
               header = c("Efectividad Global", "r [CI 95%]"),
               slab = c("Resultados Empleados",
                        "Praticas RRHH",
                        "Capital Humano",
                        "Liderazgo y Gestion",
                        "Cultura Organizacional"),
               xlab = NULL,
               xlim = c(-0.5,0.9),
               alim = c(-0.1,0.6),
               at = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)) 


#### Medidas de heterogeneidad 

## Model 0: Intercepto - Tau2

Model0.1 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.1, 
                model.name="3 level model")

summary(Model0.1) 

# Likelihood-based confidence interval - I2 e IIC

summary (meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.1, 
               I2=c("I2q", "ICC"), intervals.type="LB") ) 





# VD 2 - EFECTIVIDAD OPERACIONAL


fit.2 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.2,
               intercept.constraints=0,
               x=model.matrix(~ -1+VI_tipo))
summary(fit.2)


# Forest plot

datos_fit2 <- summary(fit.2)

forest.default(x = datos_fit2$coefficients[1:5,1], 
               sei = datos_fit2$coefficients[1:5,2],
               header = c("Efectividad Operacional", "r [CI 95%]"),
               slab = c("Resultados Empleados",
                        "Praticas RRHH",
                        "Capital Humano",
                        "Liderazgo y Gestion",
                        "Cultura Organizacional"),
               xlab = NULL,
               xlim = c(-0.5,0.9),
               alim = c(-0.1,0.6),
               at = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)) 


#### Medidas de heterogeneidad 

## Model 0: Intercepto - Tau2

Model0.2 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.2, 
                  model.name="3 level model")
summary(Model0.2)

# Likelihood-based confidence interval - I2 e IIC

summary (meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.2, 
               I2=c("I2q", "ICC"), intervals.type="LB") ) 




# VD 3 - EFICACIA FINANCIAL


fit.3 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.3,
               intercept.constraints=0,
               x=model.matrix(~ -1+VI_tipo))
summary(fit.3)


# Forest plot

datos_fit3 <- summary(fit.3)    

forest.default(x = datos_fit3$coefficients[1:5,1], 
               sei = datos_fit3$coefficients[1:5,2],
               header = c("Efectividad Financiera", "r [CI 95%]"),
               slab = c("Resultados Empleados",
                        "Praticas RRHH",
                        "Capital Humano",
                        "Liderazgo y Gestion",
                        "Cultura Organizacional"),
               xlab = NULL,
               alim = c(-0.1,0.6),
               xlim = c(-0.5,0.9),
               at = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6))


#### Medidas de heterogeneidad 

## Model 0: Intercepto - Tau2

Model0.3 <- meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.3, 
                  model.name="3 level model")

summary(Model0.3)

# Likelihood-based confidence interval - I2 e IIC

summary (meta3(y = t.efecto, v = SE.2, cluster = estudio, data = datos_VD.3, 
               I2=c("I2q", "ICC"), intervals.type="LB") ) 



#  SESGO DE PUBLICACION - TEST DE IOANNIDIS Y TRIKALINOS (metafor)

# Efectividad global
tes(x = datos_VD.1$t.efecto, vi = datos_VD.1$SE.2)     # p = 0.4986

# Efectividad operacional
tes(x = datos_VD.2$t.efecto, vi = datos_VD.2$SE.2)     # p = 0.0406 (p < 0.05)

# Efectividad financiera
tes(x = datos_VD.3$t.efecto, vi = datos_VD.3$SE.2)     # p = 0.2041


