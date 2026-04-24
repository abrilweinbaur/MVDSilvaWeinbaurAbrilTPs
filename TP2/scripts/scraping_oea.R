######################### 
# TP individual n°2
# Abril Silva Weinbaur
# Script para scrapear comunicados de la OEA
######################### 

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

# Definir URL de prueba: comunicados de abril de 2026
url_abril <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026"
message("Leyendo página de comunicados de abril de 2026...")

# Leer el HTML de la página
pagina_abril <- read_html(url_abril)
message("Página de abril leída correctamente")

# Guardar una copia del HTML descargado
fecha_descarga <- Sys.Date() #Guarda la fecha del día en que corres el script

write_html(
  pagina_abril,
  file = file.path(
    here("TP2", "data"),
    paste0("oea_comunicados_abril_2026_", fecha_descarga, ".html")
  )
)
message("HTML de abril guardado en la carpeta /data")

## PRUEBA
#Extraer títulos 
#message("Extrayendo títulos de comunicados con SelectorGadget...")

#titulos_abril <- pagina_abril |>
#  html_elements("#rightmaincol td:nth-child(2)") |>
#html_text2()

#head(titulos_abril)

#Extraer links
#links_abril <- pagina_abril |>
#  html_elements("a[href*='comunicado_prensa']") |>
#  html_attr("href")

#head(links_abril)

#links_abril_completos <- links_abril |>
#  url_absolute(url_abril)

#head(links_abril_completos)

# Crear una tabla inicial con títulos y links de abril

nodos_abril <- pagina_abril |>
  html_elements("#rightmaincol td:nth-child(2) a")

titulos_abril <- nodos_abril |>
  html_text2()

links_abril <- nodos_abril |>
  html_attr("href")

links_abril_completos <- links_abril |>
  url_absolute(url_abril)

tabla_abril <- tibble(
  id = seq_along(titulos_abril),
  titulo = titulos_abril,
  link = links_abril_completos
)

head(tabla_abril)

## PRUEBA
# Probar extracción de un comunicado individual
#url_prueba <- tabla_abril$link[1]
#message("Leyendo comunicado de prueba: ", url_prueba)

#pagina_comunicado <- read_html(url_prueba)
#message("Comunicado de prueba leído correctamente")

#pagina_comunicado

# Extraer cuerpo del comunicado de prueba con SelectorGadget
#cuerpo_prueba <- pagina_comunicado |>
#  html_elements("p:nth-child(5)") |>
#  html_text2()

#cuerpo_prueba

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
  
extraer_cuerpo(tabla_abril$link[1])

tabla_abril$cuerpo <- map_chr(tabla_abril$link, extraer_cuerpo)

sum(is.na(tabla_abril$cuerpo)) #Chequea si hay NAs

head(tabla_abril)

# Dejar solamente las variables solicitadas por la consigna

tabla_abril_final <- tabla_abril |>
  select(id, titulo, cuerpo)

head(tabla_abril_final)

# Guardar tabla final de abril

saveRDS(
  tabla_abril_final,
  file = here("TP2", "data", "comunicados_oea_abril_2026.rds")
)

message("Tabla de abril guardada correctamente en /data")

