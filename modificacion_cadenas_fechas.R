
# Ejercicio para modificar Strings
# Datos generados de http://www.generatedata.com/

library(stringr)

datos <- read.table('~/repos/limpieza-datos/datosprueba.csv',header = T,stringsAsFactors = F,sep='@',nrows = 3)
datos
datos$telefono <- c('52-55-9874-5111','52-55-1231-9394','52-55-73422111')
datos
patron.telefono <- '[0-9]{2}-[0-9]{2}-[0-9]{4}.*'
str_detect(datos$telefono,patron.telefono)
patron.telefono.mejor <- '[0-9]{2}-[0-9]{2}-[0-9]{4}-[0-9]{4}'

patron.telefono.esconder <- '[0-9]{2}-[0-9]{2}-[0-9]{4}'
datos$nuevo.telefono <- str_replace(datos$telefono,patron.telefono.esconder,'XX-XX-XXXX')
datos
datos$lon.tel <- str_length(datos$nuevo.telefono)
datos

datos <- datos %>% separate(coordenadas,into=c('lat','lon'),sep=',')
datos


datos$sinsentido <- '   asd qwe poe era  '
str_to_title(datos$sinsentido)
str_to_upper(datos$sinsentido)
str_trim(datos$sinsentido)
str_split(datos$sinsentido,' ')



library(lubridate)
ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")
ts1 <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
ts1
ts2 <- ymd_hms("2011-08-10 14:00:00", tz = "PDT")
ts2
with_tz(ts2,'America/Chicago')
