rm(list = ls())

#### Procesamiento SEPA-VID ####

# Cargar librerias ----
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       readxl,
       writexl,
       janitor)


# Académicos -----

academicos <- read_excel("input/data/original/acad.xlsx") |> clean_names()
retirados <- read_excel("input/data/original/retirados.xlsx", sheet="Retirados") |> clean_names()

retirados <- retirados |> mutate(rut = rut_norm)

academicos_historico <- bind_rows(academicos, retirados)

academicos_historico <- academicos_historico |> select(reparticion,
                                                       jerarquia,
                                                       sexo,
                                                       paterno,
                                                       materno,
                                                       nombres,
                                                       edad,
                                                       horas_reales,
                                                       fech_ratif_jerarquia,
                                                       fech_term_real,
                                                       rut) |>
  filter(reparticion != "-")

academicos_historico <- academicos_historico %>% 
  mutate(
    reparticion = case_when(
      str_detect(str_to_lower(reparticion), "psicología") ~ "Psicología",
      str_detect(str_to_lower(reparticion), "sociología") ~ "Sociología",
      str_detect(str_to_lower(reparticion), "antropología") ~ "Antropología",
      str_detect(str_to_lower(reparticion), "trabajo social") ~ "Trabajo social",
      str_detect(str_to_lower(reparticion), "educación") ~ "Educación",
      str_detect(str_to_lower(reparticion), "postgrado") ~ "Postgrado",
      TRUE ~ NA),
    categoria = recode(jerarquia,
                       "Investigador(a) Postdoctoral" = "Investigador(a) Postdoctoral",
                       "Pendiente" = "Pendiente",
                       "Prof. Asistente - Categ. Academica Doc." = "Docente",
                       "Prof. Asociado - Categ. Academica Doc." = "Docente",
                       "Prof.Titular - Categ. Academica Doc." = "Docente",
                       "Prof. Asistente - Categ. Academica Ord." = "Ordinaria",
                       "Prof. Asociado - Categ. Academica Ord." = "Ordinaria",
                       "Prof.Titular - Categ. Academica Ord." = "Ordinaria",
                       "Prof. Adjunto" = "Prof. Adjunto"),
    jerarquia = case_when(
      str_detect(str_to_lower(jerarquia), "titular") ~ "Titular",
      str_detect(str_to_lower(jerarquia), "asociado") ~ "Asociado",
      str_detect(str_to_lower(jerarquia), "asistente") ~ "Asistente",
      str_detect(str_to_lower(jerarquia), "postdoctoral") ~ "Postdoc",
      str_detect(str_to_lower(jerarquia), "adjunto") ~ "Adjunto",
      str_detect(str_to_lower(jerarquia), "instructor") ~ "Instructor",
      str_detect(str_to_lower(jerarquia), "ayudante") ~ "Ayudante",
      str_detect(str_to_lower(jerarquia), "evaluado") ~ "No evaluado",
    ),
    # fech_ing_u = year(fech_ing_u),
    # ingreso_reciente = case_when(
    #   fech_ing_u <= 2014 ~ "Ingreso previo a 2015",
    #   fech_ing_u > 2014 & fech_ing_u <= 2020 ~ "Ingreso 2015-2020",
    #   fech_ing_u > 2020  ~ "Ingreso 2020-2025"
    # ),
    edad_tramos = case_when(
      edad < 40 ~ "Menores de 40",
      edad >= 40 & edad < 50 ~ "40-49 años",
      edad >= 50 & edad < 60 ~ "50-59 años",
      edad >= 60 & edad < 70 ~ "60-69 años",
      edad >= 70 & edad < 80 ~ "70-79 años",
      edad >= 80 ~ "Mayores de 80",
    ),
    jerarquizacion= year(fech_ratif_jerarquia),
    retiro = year(fech_term_real),
    retiro= ifelse(retiro==2099, 2024, retiro),
    nombre_completo = paste(nombres, paterno, materno),
    nombre_completo = gsub("\\.", " ", nombre_completo), # Reemplazar puntos por espacio
    nombre_completo = gsub("\\_", " ", nombre_completo), # Reemplazar guion bajo por espacio
    nombre_completo = gsub("\\-", " ", nombre_completo), # Reemplazar guion por espacio
  ) %>% 
  select(rut_investigador=rut, nombre_completo, sexo, 
         reparticion, horas_reales, jerarquia, categoria, edad, edad_tramos, jerarquizacion, retiro)

academicos_historico <- academicos_historico %>%
  group_by(rut_investigador) %>%
  slice_max(horas_reales, with_ties = FALSE) %>%
  ungroup()

# 
# ids_duplicados <- academicos_historico %>%
#   count(rut_investigador) %>%
#   filter(n > 1) %>%
#   pull(rut_investigador)
# 
# test <- academicos_historico %>%
#   filter(rut_investigador %in% ids_duplicados) %>%
#   arrange(rut_investigador)

# Merge

load("input/data/procesadas/proyectos.rdata")

proyectos_merge <- merge(proyectos, academicos_historico, by.x = "rut_investigador")

proyectos_merge <- proyectos_merge |>
  mutate(proyecto_facso= ifelse(retiro>=anio_concurso, 1, 0)) |>
  filter(proyecto_facso==1 | is.na(proyecto_facso)) |>
  select(-c("investigador", "proyecto_facso")) |>
  mutate(jerarquia_proyecto= ifelse(anio_concurso>jerarquizacion, jerarquia, NA))

save(proyectos_merge, file="input/data/procesadas/proyectos-merge.rdata")