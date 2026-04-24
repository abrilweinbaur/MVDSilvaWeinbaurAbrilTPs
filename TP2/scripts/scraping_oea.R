######################### 
# TP individual n°2
# Abril Silva Weinbaur
# Script para scrapear comunicados de la OEA
######################### 

## En el segundo commit está la versión de prueba con el mes abril

#Instalar librerias
#install.packages("here")

#Invocar librerías 
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(xml2) 

#Chequear si la carpeta existe y sino crearla
if (!dir.exists(here("TP2", "data"))) {
  dir.create(here("TP2", "data"), recursive = TRUE)
  message("Carpeta /data creada")
} else {
  message("Carpeta /data ya existe")
}

# Definir meses a scrapear

meses <- tibble(
  numero_mes = c(1, 2, 3, 4),
  nombre_mes = c("enero", "febrero", "marzo", "abril")
)

# Función para extraer el cuerpo de un comunicado

extraer_cuerpo <- function(link) {
  
  message("Leyendo comunicado: ", link)
  
  Sys.sleep(3)
  
  pagina <- read_html(link)
  
  cuerpo <- pagina |>
    html_elements("p:nth-child(5), p:nth-child(6)") |> 
    #Seleccionar (5) y (6), ya que cuando no muestra imagen es (5) pero cuando muestra una imagen al principio de la página es (6)
    html_text2()
  
  # Siempre unir en un único string
  if (length(cuerpo) == 0) {
    return(NA_character_)
  } else {
    return(paste(cuerpo, collapse = " "))
  }
}

# Función para scrapear un mes completo

scrapear_mes <- function(numero_mes, nombre_mes) {
  
  # Definir URL del mes
  url_mes <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    numero_mes,
    "&nAnio=2026"
  )
  
  message("Leyendo página de comunicados de ", nombre_mes, " de 2026...")
  
  # Leer el HTML de la página
  pagina_mes <- read_html(url_mes)
  message("Página de ", nombre_mes, " leída correctamente")
  
  # Guardar una copia del HTML descargado
  fecha_descarga <- Sys.Date() #Guarda la fecha del día en que corres el script
  
  write_html(
    pagina_mes,
    file = file.path(
      here("TP2", "data"),
      paste0("oea_comunicados_", nombre_mes, "_2026_", fecha_descarga, ".html")
    )
  )
  
  message("HTML de ", nombre_mes, " guardado en la carpeta /data")
  
  # Crear una tabla inicial con títulos y links del mes
  
  nodos_mes <- pagina_mes |>
    html_elements("#rightmaincol td:nth-child(2) a")
  
  titulos_mes <- nodos_mes |>
    html_text2()
  
  links_mes <- nodos_mes |>
    html_attr("href")
  
  links_mes_completos <- links_mes |>
    url_absolute(url_mes)
  
  tabla_mes <- tibble(
    id = paste0(nombre_mes, "_", seq_along(titulos_mes)),
    titulo = titulos_mes,
    link = links_mes_completos
  )
  
  head(tabla_mes)
  
  # Extraer cuerpo de cada comunicado
  
  tabla_mes$cuerpo <- map_chr(tabla_mes$link, extraer_cuerpo)
  
  sum(is.na(tabla_mes$cuerpo)) #Chequea si hay NAs
  
  head(tabla_mes)
  
  # Dejar solamente las variables solicitadas por la consigna
  
  tabla_mes_final <- tabla_mes |>
    select(id, titulo, cuerpo)
  
  return(tabla_mes_final)
}

# Scrapear enero, febrero, marzo y abril de 2026

tabla_final <- map2_dfr(
  meses$numero_mes,
  meses$nombre_mes,
  scrapear_mes
)

sum(is.na(tabla_final$cuerpo)) #Chequea si hay NAs

head(tabla_final)

# Guardar tabla final de enero, febrero, marzo y abril

saveRDS(
  tabla_final,
  file = here("TP2", "data", "comunicados_oea_enero_abril_2026.rds")
)

message("Tabla final de enero, febrero, marzo y abril guardada correctamente en /data")