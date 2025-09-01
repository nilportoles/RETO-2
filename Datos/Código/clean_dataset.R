# ================================================================
# clean_dataset.R
# Proyecto: Reto 2 — CRONOS-2 Wave 4
# Ubicación del script: Datos/Código/clean_dataset.R
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(stringr)
})

#Rutas
raw_csv_path <- "~/Desktop/M8/RETO-2/Datos/Base de datos original/CRON2W4e01.csv"
out_dir <- "."

#Lectura del archivo y selección de variables
message("Leyendo CSV desde: ", normalizePath(raw_csv_path))
raw <- readr::read_csv(raw_csv_path, guess_max = 100000, show_col_types = FALSE)

#Variables sociodemográficas
vars_demo  <- c("idno","cntry","gndr","agea","c2weight")
#Ítems de salud mental y bienestar
vars_items <- paste0("w4q", 47:60)
vars_need  <- intersect(c(vars_demo, vars_items), names(raw))

datos <- raw |>
  dplyr::select(dplyr::all_of(vars_need)) |>
  janitor::clean_names()

#Tipos y missing values
to_num <- function(x) suppressWarnings(as.numeric(x))
item_names <- tolower(vars_items)

datos <- datos |>
  dplyr::mutate(
    gndr     = to_num(gndr),
    agea     = to_num(agea),
    c2weight = to_num(c2weight),
    dplyr::across(dplyr::all_of(tolower(vars_items)), to_num)
  ) |>
  #Missings declarados:
  dplyr::mutate(
    dplyr::across(dplyr::all_of(item_names), ~ dplyr::na_if(.x, 7)),
    dplyr::across(dplyr::all_of(item_names), ~ dplyr::na_if(.x, 8)),
    dplyr::across(dplyr::all_of(item_names), ~ dplyr::na_if(.x, 9)),
    gndr = dplyr::na_if(gndr, 9),
    agea = dplyr::na_if(agea, 999)
  )

#Grupos
datos <- datos |>
  dplyr::mutate(
    cntry = as.factor(cntry),
    gndr_lab = dplyr::case_when(
      gndr == 1 ~ "Male",
      gndr == 2 ~ "Female",
      TRUE      ~ NA_character_
    ),
    gndr_lab = factor(gndr_lab, levels = c("Male","Female")),
    age_band = cut(
      agea,
      breaks = c(17, 29, 44, 64, Inf),
      labels = c("18–29","30–44","45–64","65+"),
      right  = TRUE
    ),
    cntry_iso2 = dplyr::case_when(
      as.character(cntry) %in% c("UK","GB") ~ "GB",
      as.character(cntry) == "EL"           ~ "GR",
      TRUE                                  ~ as.character(cntry)
    )
  )

#Construcción de constructos:
pos_items <- tolower(c("w4q50","w4q52"))
dis_items <- tolower(c("w4q47","w4q48","w4q49","w4q51","w4q53","w4q54",
                       "w4q55","w4q56","w4q57","w4q58","w4q59","w4q60"))

datos <- datos |>
  dplyr::rowwise() |>
  dplyr::mutate(
    positive_affect_mean = mean(c_across(dplyr::all_of(pos_items)), na.rm = TRUE),
    distress_mean        = mean(c_across(dplyr::all_of(dis_items)), na.rm = TRUE),
    n_pos = sum(!is.na(c_across(dplyr::all_of(pos_items)))),
    n_dis = sum(!is.na(c_across(dplyr::all_of(dis_items))))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    positive_affect_mean = dplyr::if_else(n_pos == 0, NA_real_, positive_affect_mean),
    distress_mean        = dplyr::if_else(n_dis == 0, NA_real_, distress_mean),
    positive_affect_z    = as.numeric(scale(positive_affect_mean)),
    distress_z           = as.numeric(scale(distress_mean))
  ) |>
  dplyr::select(-n_pos, -n_dis)

#Salidas
out_csv <- file.path("~/Desktop/M8/RETO-2/Datos/Base de datos depurada/", "cron2_w4_clean.csv")
out_rds <- file.path("~/Desktop/M8/RETO-2/Datos/Base de datos depurada/", "cron2_w4_clean.rds")

readr::write_csv(datos, out_csv, na = "")
saveRDS(datos, out_rds)

