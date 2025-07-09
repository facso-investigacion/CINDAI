rm(list = ls())

#### Procesamiento SEPA-VID ####

# Cargar librerias ----
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       readxl,
       writexl,
       janitor,
       labelled)

load("input/data/procesadas/proyectos-merge.rdata")

## Jerarquizaciones

jeraq <- read_excel("input/data/original/acad_jerarq.xlsx") |> clean_names() |>
  select(jerarquia,
         rut,
         fech_ratif_jerarquia)

academicos <- read_excel("input/data/original/acad.xlsx") |> clean_names() |>
  select(jerarquia,
         rut,
         fech_ratif_jerarquia)

academicos_slice <- academicos %>%
  arrange(desc(fech_ratif_jerarquia)) %>%   # Ordena de más reciente a más antiguo
  slice_head(n = 49)         # Selecciona los primeros 49 casos

jeraq <- rbind(jeraq, academicos_slice)

jeraq <- jeraq %>% 
  mutate(
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
    jerarquizacion= year(fech_ratif_jerarquia)
  )   |> 
  select(jerarquia_proyecto=jerarquia, rut_investigador=rut, jerarquizacion) |>
  distinct(rut_investigador, jerarquia_proyecto, .keep_all = TRUE)


jerarq_wide <- jeraq %>%
  pivot_wider(
    id_cols = rut_investigador,               
    names_from = jerarquia_proyecto,           
    values_from = jerarquizacion              
  ) |>
  select(rut_investigador,
         instructor= Instructor,
         asistente= Asistente,
         asociado= Asociado,
         titular= Titular) |>
  mutate(
    rut_investigador= set_variable_labels(rut_investigador, "RUT Investigador"),
    instructor= set_variable_labels(instructor, "Jeraquización: Instructor"),
    asistente= set_variable_labels(asistente, "Jerarquización: Asistente"),
    asociado= set_variable_labels(asociado, "Jerarquización: Asociado"),
    titular= set_variable_labels(titular, "Jerarquización: Titular"),
    )




proyectos_merge <- proyectos_merge |>
  left_join(jerarq_wide, by="rut_investigador")
  

proyectos_merge <- proyectos_merge |>
  mutate(jerarquia_proyecto= case_when(
    anio_concurso>titular ~ "Titular",
    anio_concurso>asociado & anio_concurso<=titular ~ "Asociado",
    anio_concurso>asistente & anio_concurso<=asociado ~ "Asistente",
    anio_concurso>instructor & anio_concurso<=asistente ~ "Instructor"),
    jerarquia_proyecto= set_variable_labels(jerarquia_proyecto, "Jerarquía en el Proyecto")) |>
  select(rut_investigador,
         nombre_completo,
         sexo,
         edad,
         reparticion, 
         horas_reales,
         jerarquia_actual=jerarquia,
         categoria,
         codigo_proyecto,
         titulo,
         institucion,
         concurso,
         instrumento,
         asociativo,
         inv_aplicada,
         tipo_investigador,
         estado_proyecto,
         adjudicado,
         en_ejecucion,
         duracion,
         anio_concurso,
         fecha_inicio,
         fecha_termino,
         disciplina_principal,
         area_disciplina_principal,
         disciplina_exacta_principal,
         palabras_claves,
         monto_adjudicado,
         moneda,
         instructor,
         asistente,
         asociado,
         titular,
         jerarquia_proyecto,
         retiro)


# Exportar

data <- 
  proyectos_merge |>
  filter(is.na(retiro)) |>
  select(-retiro)

save(data, file="output/data-general.rdata")

retirados <-
  proyectos_merge |>
  filter(!is.na(retiro))

save(retirados, file="output/retirados.rdata")
