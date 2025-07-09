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

# Cargar datos ----
proyectos <- read_excel("input/data/original/proyectos.xlsx") %>% clean_names()


# Procesar proyectos ----
proyectos <- proyectos %>% 
  # simplificar categorias tipo de proyecto
  mutate(
    institucion = labelled(institucion,
                           labels=c(
                         "Agencia Nacional De Investigación Y Desarrollo (Anid)" = "ANID",
                         "Ministerio de Educación" = "MinEduc",
                         "Ministerio De Ciencia, Tecnología, Conocimiento e Innovación" = "MinCiencia",
                         "Chile" = "GORE Metropolitano de Santiago"),
                         label= "Institución"
    ),
    concurso = labelled(concurso,
                        labels = c(
                      "Fondecyt_Anid" = "FONDECYT",
                      "Iniciativa científica Milenio" = "Iniciativa Científica Milenio"),
                      label="Concurso"
    ),
    instrumento = labelled(instrumento,
                           labels=c(
                         "FONIS-Proyectos I&D" = "FONIS",
                         "Concurso Anillos en Ciencias Sociales" = "Anillos en Ciencias Sociales",
                         "Concurso Apoyo a Centros de Excelencia FONDAP" = "Apoyo a Centros de Excelencia FONDAP",
                         "Nucleo Investigación Cs. Sociales" = "Núcleos Milenio en Ciencias Sociales"),
                         label="Instrumento"
    ),
    asociativo = if_else(instrumento %in% c("Anillos en Ciencias Sociales", 
                                            "Apoyo a Centros de Excelencia FONDAP",
                                            "Núcleos Milenio en Ciencias Sociales",
                                            "Iniciativa Científica Milenio"), 1, 0),
    asociativo = labelled(asociativo,
                          labels = c("No"= 0, "Sí"= 1),
                          label= "Proyecto Asociativo"),
    inv_aplicada = if_else(instrumento %in% c("FONIDE", 
                                              "FONIS",
                                              "IDEA",
                                              "IDEA I+D",
                                              "Proyectos-Investigación"), 1, 0),
    inv_aplicada = labelled(inv_aplicada,
                          labels = c("No"= 0, "Sí"= 1),
                          label= "Investigación Aplicada"),
    adjudicado= if_else(estado_proyecto %in% c("Finalizado",
                                               "En ejecución"), 1, 0),
    adjudicado = labelled(adjudicado,
                          labels = c("No"= 0, "Sí"= 1),
                          label= "Proyecto Adjudicado"),
    en_ejecucion= ifelse(estado_proyecto=="En ejecución", 1, 0),
    en_ejecucion = labelled(en_ejecucion,
                          labels = c("No"= 0, "Sí"= 1),
                          label= "Proyecto en ejecución"),
  ) %>% 
  # pasar de formato wide a long (un investigador y proyecto por c/fila)
  pivot_longer(
    cols = matches("(investigador_\\d+|tipo_investigador\\d+|rut_investigador\\d+)"),
    names_to = c(".value", "n_investigador"),
    names_pattern = "(.*?)(\\d+)"
  ) %>%
  # eliminar filas sin autor
  filter(investigador_ != "-") %>% 
  # seleccionar columnas utiles
  select(investigador=investigador_, rut_investigador=rut_investigador_, tipo_investigador=tipo_investigador_,
         codigo_proyecto=codigo, titulo, institucion, concurso, instrumento, asociativo, inv_aplicada, 
         anio_concurso=ano_concurso, estado_proyecto, adjudicado, en_ejecucion, duracion, fecha_inicio, fecha_termino, 
         disciplina_principal, area_disciplina_principal, disciplina_exacta_principal) |>
  mutate(
    investigador = set_variable_labels(investigador, "Investigador"),
    tipo_investigador= set_variable_labels(tipo_investigador, "Rol Investigador"),
    codigo_proyecto= set_variable_labels(codigo_proyecto, "Codigo Proyecto"),
    titulo= set_variable_labels(titulo, "Título del Proyecto"),
    anio_concurso= set_variable_labels(anio_concurso, "Año del Concurso"),
    estado_proyecto= set_variable_labels(estado_proyecto, "Estado del Proyecto"),
    duracion= set_variable_labels(duracion, "Duración del proyecto"),
    fecha_inicio= set_variable_labels(fecha_inicio, "Inicio del proyecto"),
    fecha_termino= set_variable_labels(fecha_termino, "Término del proyecto"),
    disciplina_principal= set_variable_labels(disciplina_principal, "Disciplina Principal del Proyecto"),
    area_disciplina_principal= set_variable_labels(area_disciplina_principal, "Área de la disciplina principal del Proyecto"),
    disciplina_exacta_principal= set_variable_labels(disciplina_exacta_principal, "Disciplina exacta principal del Proyecto")
    )

# Bases ANID ----

anid <- read_excel("input/data/original/BDH_HISTORICA.xlsx") %>% clean_names()
milenio <- read_excel("input/data/original/BDH_PROYECTOS_MILENIO.xlsx") %>% clean_names()
# palabras_claves <- read_excel("input/data/BD_Palabras_claves_Proyectos.xlsx") |> clean_names()


# Procesar bases ----

anid_subset <- anid |> select(codigo_proyecto, monto_adjudicado, moneda, palabras_claves) |> 
  mutate(codigo_proyecto= as.character(codigo_proyecto))

milenio_subset <- milenio |> select(codigo_proyecto, monto_adjudicado=monto_adjudicado_m) |> 
  mutate(codigo_proyecto= as.character(codigo_proyecto))

# palabras_claves_subset <- palabras_claves |> select(codigo_proyecto, palabras_claves_b=palabras_claves) |> 
#   mutate(codigo_proyecto= as.character(codigo_proyecto))

data_anid <- bind_rows(anid_subset, milenio_subset)

data_anid <- data_anid |>
  mutate(
  codigo_proyecto= set_variable_labels(codigo_proyecto, "Codigo Proyecto"),
  monto_adjudicado= set_variable_labels(monto_adjudicado, "Monto adjudicado al proyecto"),
  moneda= set_variable_labels(moneda, "Moneda del presupuesto adjudicado"),
  palabras_claves= set_variable_labels(palabras_claves, "Palabras Claves")
)

# test <- anid |> filter(palabras_claves!="SIN INFORMACION")
# unique(test$agno_concurso)

# data_anid <- merge(data_anid, palabras_claves_subset, by="codigo_proyecto", all.x=T)


proyectos <- proyectos |>
  left_join(data_anid, by="codigo_proyecto")

# Estandarizar ruts

proyectos <- proyectos |>
  mutate(rut_investigador = ifelse(nchar(rut_investigador) == 9, paste0("0", rut_investigador), rut_investigador),
         rut_investigador = ifelse(nchar(rut_investigador) == 8, paste0("00", rut_investigador), rut_investigador),
         rut_investigador= set_variable_labels(rut_investigador, "RUT Investigador"))

save(proyectos, file="input/data/procesadas/proyectos.rdata")



