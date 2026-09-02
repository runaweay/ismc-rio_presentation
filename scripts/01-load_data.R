library(pdftools)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

PDF <- "data/raw/27278.pdf"

# Filas numéricas a extraer (patrón de inicio de línea -> nombre de variable)
ANALITOS <- c(
  pH      = "^pH",
  C       = "^Carbono",
  arena   = "^Arena",
  arcilla = "^Arcilla",
  limo    = "^Limo"
)

parse_pagina <- function(pg) {

  lineas <- str_split(pg, "\n")[[1]]
  if (!any(str_detect(lineas, "de Laboratorio"))) return(NULL)

  # N° de laboratorio (6 dígitos)
  lab <- lineas[str_detect(lineas, "de Laboratorio")][1] %>%
    str_extract_all("\\b\\d{6}\\b") %>%
    unlist()

  # Cuartel: todo lo que sigue a la palabra CUARTEL, separado por 2+ espacios
  cuartel <- lineas[str_detect(lineas, "CUARTEL")][1] %>%
    str_remove("^.*CUARTEL") %>%
    str_trim() %>%
    str_split("\\s{2,}") %>%
    unlist() %>%
    keep(nzchar)

  # Valores: se descarta la etiqueta y la columna de unidad ("-" o "%")
  vals <- map(ANALITOS, function(pat) {
    ln <- lineas[str_detect(lineas, pat)][1]
    sub("^.*?\\s{2,}(-|%)\\s+", "", ln, perl = TRUE) %>%
      str_extract_all("\\d+,\\d+") %>%
      unlist() %>%
      str_replace(",", ".") %>%
      as.numeric()
  })

  # Control: todas las columnas deben tener el mismo largo
  stopifnot(
    length(lab) == length(cuartel),
    all(lengths(vals) == length(lab))
  )

  as_tibble(c(list(lab = lab, cuartel = cuartel), vals))
}

# --- Formato ancho -------------------------------------------------------
suelos <- map_dfr(pdf_text(PDF), parse_pagina) %>%
  mutate(
    prof = str_extract(cuartel, "^\\d+-\\d+"),
    id   = str_trim(str_remove(cuartel, "^\\d+-\\d+")),
    .after = lab
  ) %>%
  select(-cuartel) %>%
  mutate(
    id   = factor(id, levels = c("PF", "Sta Lucia", "PAV JMB",
                                 "PAV MOSQ", "PAV McIver")),
    prof = factor(prof, levels = c("0-10", "10-20", "20-30"))
  )

# --- Formato largo -------------------------------------------------------
suelos_long <- suelos %>%
  pivot_longer(
    cols      = c(pH, C, arena, arcilla, limo),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  mutate(variable = factor(variable,
                           levels = c("pH", "C", "arena", "arcilla", "limo")))

# --- Chequeo: las fracciones texturales deben sumar 100 ------------------
suelos %>%
  mutate(suma = arena + arcilla + limo) %>%
  filter(suma != 100)

suelos_long
