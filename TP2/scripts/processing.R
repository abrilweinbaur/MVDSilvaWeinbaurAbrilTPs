######################### 
# TP individual n°2
# Abril Silva Weinbaur
# Script para procesar texto de comunicados de la OEA
######################### 

#Instalar librerias
#install.packages("udpipe")
#install.packages("stopwords")

#Invocar librerías 
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(udpipe)
library(stopwords)

#Chequear si la carpeta existe y sino crearla
if (!dir.exists(here("TP2", "output"))) {
  dir.create(here("TP2", "output"), recursive = TRUE)
  message("Carpeta /output creada")
} else {
  message("Carpeta /output ya existe")
}

# Cargar la tabla generada por el script de scraping

message("Leyendo archivo de comunicados scrapeados...")

comunicados <- readRDS(
  here("TP2", "data", "comunicados_oea_enero_abril_2026.rds")
)

message("Archivo de comunicados cargado correctamente")

head(comunicados)

# Limpiar el texto del cuerpo de cada comunicado

message("Limpiando texto de los cuerpos de los comunicados...")

comunicados_limpios <- comunicados |>
  mutate(
    cuerpo_limpio = cuerpo,
    cuerpo_limpio = str_to_lower(cuerpo_limpio),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:punct:]]", " "),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:digit:]]", " "),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[^[:alpha:]áéíóúüñÁÉÍÓÚÜÑ\\s]", " "),
    cuerpo_limpio = str_squish(cuerpo_limpio)
  )

message("Texto limpiado correctamente")

head(comunicados_limpios)

# Cargar modelo de lematización en español

message("Cargando modelo de lematización en español...")

m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

message("Modelo de lematización cargado correctamente")

# Lematizar el cuerpo limpio de cada comunicado

message("Lematizando cuerpos de comunicados...")

comunicados_lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpios$cuerpo_limpio,
  doc_id = comunicados_limpios$id
) |>
  as.data.frame() |>
  as_tibble() |>
  select(id = doc_id, lemma, upos)

message("Lematización realizada correctamente")

head(comunicados_lemas)

# Cargar stopwords

message("Cargando stopwords en español...")

stop_es <- stopwords::stopwords("es")

stop_words <- tibble(
  lemma = c(stop_es)
)

# Filtrar sustantivos, verbos y adjetivos; pasar a minúscula; remover stopwords

message("Filtrando lemmas relevantes y removiendo stopwords...")

processed_text <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(
    lemma = str_to_lower(lemma),
    lemma = str_replace_all(lemma, "[[:punct:]]", ""),
    lemma = str_replace_all(lemma, "[[:digit:]]", ""),
    lemma = str_squish(lemma)
  ) |>
  anti_join(stop_words, by = "lemma") |>
  filter(
    lemma != "",
    !is.na(lemma),
    str_length(lemma) > 2
  ) |>
  left_join(
    comunicados |> select(id, titulo),
    by = "id"
  ) |>
  select(id, titulo, lemma, upos)

message("Texto procesado correctamente")

head(processed_text)

# Guardar texto procesado

saveRDS(
  processed_text,
  file = here("TP2", "output", "processed_text.rds")
)

message("Archivo processed_text.rds guardado correctamente en /output")