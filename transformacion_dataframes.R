
# Ejercicio de transformación de datos
# Uso de funciones de tidyr para modificación de datos verticales a horizontales, partición de columnas
# Ejemplo de funciones, for loop, números aleatorios, cbind, rbind
# Ejemplo de agrupación de datos
# Uso del operador %>% de magrittr

# @5h4n6

library(tidyr)
library(magrittr)
library(dplyr)
library(ggplot2)

# Generación de valores (en data.frame) con una función, un for loop dentro
a <- 5
set.seed(986364)
generaVentas <-function(a) {
  valores <- c()
  for(i in 1:a) {
    valores <- c(valores,rnorm(52,(100000*i*round(runif(1)*10,1)),(42*i*round(runif(1)*10,1))))
  }
  ventas <- as.data.frame(matrix(valores,ncol = 52,byrow=T))
  return(ventas)
}

# Pegamos valores con cbind y apilamos con rbind
ventas.Bogota.Sur <- cbind(año=seq(2012,2016),ciudad="BOG-SUR",generaVentas(a))
ventas.Bogota.Norte <- cbind(año=seq(2012,2016),ciudad="BOG-NTE",generaVentas(a))
ventas.Medellin.1 <- cbind(año=seq(2012,2016),ciudad="MED-1",generaVentas(a))
ventas.Medellin.2 <- cbind(año=seq(2012,2016),ciudad="MED-2",generaVentas(a))
ventas.Medellin.3 <- cbind(año=seq(2012,2016),ciudad="MED-3",generaVentas(a))
ventas.totales <- rbind(ventas.Bogota.Sur,ventas.Bogota.Norte,ventas.Medellin.1,ventas.Medellin.2,ventas.Medellin.3)
ventas.totales[,1:5]
head(ventas.totales)

# Separamos columnas
ventas.region <- separate(ventas.totales,ciudad,into=c('region','sucursal'),sep='-')
ventas.region[1:6,1:3]
# Alargamos el data.frame (ancho -> largo)
ventas.vertical <- gather(ventas.region,semana.char,usd,-año,-region,-sucursal)
head(ventas.vertical)

# Todo lo anterior en una sola pasada, gracias al operador %>% (pipe)
ventas.final <- ventas.totales %>%
  separate(ciudad,into=c('region','sucursal'),sep='-') %>%
  gather(semana.char,usd,-año,-region,-sucursal)
head(ventas.final)

# Sustituimos caracteres y tipo de dato
ventas.final$semana <- as.numeric(gsub("V","",ventas.final$semana))
head(ventas.final)

# NOTA: Hay otras librerías que tienen funciones análogas. Ver: reshape2 con melt y cast

#Grafiquemos un poco

ggplot(ventas.final) + geom_line(aes(x=semana,y=usd,group=año,color=año))
# Se ve mal porque hay varios valores para cada año

# Con el paquete dplyr, usamos summarise
ventas.año <- ventas.final %>%
  select(año,semana,usd) %>%
  group_by(año,semana) %>%
  summarise(usd=mean(usd))

ggplot(ventas.año) + geom_line(aes(x=semana,y=usd,group=año,color=año))
#Aunque... Los datos no varían mucho :D




