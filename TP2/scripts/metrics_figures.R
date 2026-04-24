######################### 
# TP individual n°2
# Abril Silva Weinbaur
# Script para métricas y figuras de comunicados de la OEA
######################### 

#Invocar librerías 
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

#Chequear si la carpeta existe y sino crearla
if (!dir.exists(here("TP2", "output"))) {
  dir.create(here("TP2", "output"), recursive = TRUE)
  message("Carpeta /output creada")
} else {
  message("Carpeta /output ya existe")
}

# Cargar el texto procesado generado en processing.R

message("Leyendo archivo processed_text.rds...")

processed_text <- readRDS(
  here("TP2", "output", "processed_text.rds")
)

message("Archivo processed_text.rds cargado correctamente")

head(processed_text)

# Computar frecuencia de términos del corpus lematizado

message("Computando frecuencia de términos...")

dtm_resumida <- processed_text |>
  count(id, lemma, name = "frecuencia")

head(dtm_resumida)

# Seleccionar 5 términos relevantes para el contexto institucional de la OEA

terminos_relevantes <- c(
  "democracia", 
  "institucionalidad", 
  "elección", 
  "derecho", 
  "seguridad"
)

# Filtrar y condensar la frecuencia total de los términos seleccionados

frecuencia_terminos <- dtm_resumida |>
  filter(lemma %in% terminos_relevantes) |>
  group_by(lemma) |>
  summarise(
    frecuencia_total = sum(frecuencia),
    .groups = "drop"
  ) |>
  arrange(desc(frecuencia_total))

frecuencia_terminos

# Generar gráfico de barras con ggplot2

message("Generando gráfico de barras...")

grafico_frecuencia <- ggplot(
  frecuencia_terminos,
  aes(x = reorder(lemma, frecuencia_total), y = frecuencia_total)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frecuencia total de términos seleccionados en comunicados de la OEA",
    x = "Término",
    y = "Frecuencia total"
  ) +
  theme_minimal()

grafico_frecuencia

# Guardar figura en la carpeta /output

ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = grafico_frecuencia,
  width = 8,
  height = 5
)

message("Figura frecuencia_terminos.png guardada correctamente en /output")