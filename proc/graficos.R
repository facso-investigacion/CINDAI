load("output/proyectos_proc.rdata")

data <- proyectos_merge |> filter(tipo_investigador=="Investigador responsable" &
                                    en_ejecucion==1 & institucion=="ANID" & edad_tramos!="Postdoc" & is.na(retiro)) |>
  distinct(codigo_proyecto, .keep_all = T)


# 40 académicos de la Facultad son actualmente Investigadores Responsable o Directores de un proyecto de investigación externo


data |> group_by(reparticion) |> summarise(proyectos=n()) |> 
  arrange(desc(proyectos)) |> 
  mutate(reparticion = factor(reparticion, levels = reparticion)) |>
  ggplot(aes(x = reparticion, y = proyectos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = proyectos), vjust = -0.3, size = 4) +
  labs(,
       x = "Jerarquía", y = "Cantidad de proyectos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data |> group_by(jerarquia) |> summarise(proyectos=n()) |> 
  arrange(desc(proyectos)) |> 
  mutate(jerarquia = factor(jerarquia, levels = jerarquia)) |>
  ggplot(aes(x = jerarquia, y = proyectos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = proyectos), vjust = -0.3, size = 4) +
  labs(,
       x = "Departamentos", y = "Cantidad de proyectos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data |> group_by(edad_tramos) |> summarise(proyectos=n()) |> 
  arrange(desc(proyectos)) |> 
  mutate(edad_tramos = factor(edad_tramos, levels = edad_tramos)) |>
  ggplot(aes(x = edad_tramos, y = proyectos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = proyectos), vjust = -0.3, size = 4) +
  labs(,
       x = "Edad", y = "Cantidad de proyectos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
