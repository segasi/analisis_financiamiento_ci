### Correr scripts para poder realizar análisis ----
source("02_codigo/paquetes_setup_tema.R")
source("02_codigo/cargar_limpiar_datos.R")

### Análisis de frecuencias ----

# Candidaturas por tipo usando bd_saldos_inicial
bd_saldos_inicial %>% 
  filter(ambito == "Federal") %>% 
  count(cargo, sort = T) %>% 
  mutate(total = sum(n),
         porcentaje = round((n/total)*100, 1))  

# Candidaturas por cargo, bd_saldos
bd_saldos %>% 
  count(cargo, sort = T) %>% 
  mutate(total = sum(n),
         porcentaje = round((n/total)*100, 2)) %>%   
  print(n = Inf) %>% 
  write_csv("04_datos_generados/frecuencia_candidaturas_por_cargo.csv")

# Candidaturas por cargo y tipo de candidatura, bd bd_saldos
bd_saldos %>% 
  group_by(dummy_ci) %>% 
  count(cargo, sort = T) %>% 
  ungroup() %>% 
  arrange(cargo, dummy_ci) %>% 
  group_by(cargo) %>% 
  mutate(total = sum(n),
         porcentaje = round((n/total)*100, 2)) %>%  
  ungroup() %>% 
  print(n = Inf) %>% 
  write_csv("04_datos_generados/frecuencia_candidaturas_por_tipo_y_cargo.csv")

### Gráfica y tabla de ingresos privados respecto a total de ingresos de cada CI ----
bd_ingresos %>% 
  filter(str_detect(tipo_asociacion, "Inde")) %>% 
  mutate(cargo_de_eleccion = fct_relevel(cargo_de_eleccion, "Presidente", "Senadores MR", "Diputado Federal MR"),
         # Reemplazar NAs por 0s para poder hacer el resto de los cálculos
         financiamiento_publico = ifelse(is.na(financiamiento_publico), 0, financiamiento_publico),
         ingresos_privados = total_ingresos - financiamiento_publico, 
         por_ingresos_privados = (ingresos_privados/total_ingresos)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = por_ingresos_privados,
                  y = fct_rev(cargo_de_eleccion)),
              size = 2,
              height = 0.1, 
              color = "salmon",
              alpha = 0.7) +
  scale_x_continuous(limits = c(-1, 103),
                     breaks = c(seq(0, 100, 10)), 
                     expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Gráfica 1. Porcentaje de ingresos de origen privado de cada candidatura independiente respecto a sus ingresos totales"), width = 52),
       x = "\n% de ingresos privados   \nrespecto a su total de ingresos   ",
       y = "",
       caption = "", 
       color = NULL) +
  tema +
  theme(plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        panel.grid.major.y = element_blank()) +
  ggsave("03_graficas/g1_porcentaje_ingresos_privados_vs_total_ingresos.png", width = 12, height = 8, dpi = 200)

# Tabla
bd_ingresos %>% 
  filter(str_detect(tipo_asociacion, "Inde")) %>% 
  mutate(financiamiento_publico = ifelse(is.na(financiamiento_publico), 0, financiamiento_publico),
         ingresos_privados = total_ingresos - financiamiento_publico, 
         porcentaje_ingresos_privados = (ingresos_privados/total_ingresos)*100) %>% 
  arrange(-porcentaje_ingresos_privados, -total_ingresos) %>% 
  select(nombre_completo, cargo_de_eleccion, total_ingresos, financiamiento_publico, porcentaje_ingresos_privados) %>% 
  print(n = Inf) %>% 
  write_csv("04_datos_generados/porcentaje_ingresos_privados_respecto_total_ingresos.csv")


### Gráfica y tabla de ingresos privados de CI respecto a tope de gastos ----
bd_ingresos %>% 
  filter(str_detect(tipo_asociacion, "Inde")) %>% 
  left_join(bd_saldos %>% filter(str_detect(tipo_asociacion, "Inde")) %>%
              select(tope_de_gastos, nombre_completo), by = "nombre_completo") %>% 
  mutate(cargo_de_eleccion = fct_relevel(cargo_de_eleccion, "Presidente", "Senadores MR", "Diputado Federal MR"),
         # Reemplazar NAs por 0s para poder hacer el resto de los cálculos
         financiamiento_publico = ifelse(is.na(financiamiento_publico), 0, financiamiento_publico),
         ingresos_privados = total_ingresos - financiamiento_publico, 
         por_ingresos_privados_tope = (ingresos_privados/tope_de_gastos)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = por_ingresos_privados_tope,
                  y = fct_rev(cargo_de_eleccion)),
              size = 2,
              height = 0.1, 
              color = "salmon",
              alpha = 0.7) +
  scale_x_continuous(limits = c(-1, 103),
                     breaks = c(seq(0, 100, 10)), 
                     expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Gráfica 2. Porcentaje de ingresos de origen privado de cada candidatura independiente respecto a sus tope de gastos"), width = 52),
       x = "\n% de ingresos privados   \nrespecto a su respectivo tope   ",
       y = "",
       caption = "", 
       color = NULL) +
  tema +
  theme(plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        panel.grid.major.y = element_blank()) +
  ggsave("03_graficas/g2_porcentaje_ingresos_privados_vs_tope.png", width = 12, height = 8, dpi = 200)

# Tabla 
bd_ingresos %>% 
  filter(str_detect(tipo_asociacion, "Inde")) %>% 
  left_join(bd_saldos %>% filter(str_detect(tipo_asociacion, "Inde")) %>%
              select(tope_de_gastos, nombre_completo), by = "nombre_completo") %>% 
  mutate(cargo_de_eleccion = fct_relevel(cargo_de_eleccion, "Presidente", "Senadores MR", "Diputado Federal MR"),
         # Reemplazar NAs por 0s para poder hacer el resto de los cálculos
         financiamiento_publico = ifelse(is.na(financiamiento_publico), 0, financiamiento_publico),
         ingresos_privados = total_ingresos - financiamiento_publico, 
         porcentaje_ingresos_privados_tope = (ingresos_privados/tope_de_gastos)*100) %>% 
  arrange(-porcentaje_ingresos_privados_tope) %>% 
  select(nombre_completo, cargo_de_eleccion, ingresos_privados, tope_de_gastos, porcentaje_ingresos_privados_tope) %>% 
  print(n = Inf) %>% 
  write_csv("04_datos_generados/porcentaje_ingresos_privados_respecto_tope_gastos.csv")


### Gráfica y tabla del porcentaje de gastos totales de cada candidato respecto a su tope de gastos ----
lista_cargos <- 
  bd_saldos %>% 
  distinct(as.character(cargo)) %>% 
  pull() 

lista_cargos <- lista_cargos[c(1, 2, 3)]

# Gráfica
bd_saldos <- 
  bd_saldos %>% 
  mutate(cargo = fct_rev(fct_relevel(cargo, "Presidente", "Senadores MR", "Diputado Federal MR")),
         cargo_numerico = as.numeric(cargo)) 

bd_saldos %>% 
  arrange(desc(dummy_ci)) %>% 
  ggplot() +
  geom_jitter(aes(x = por_tope,
                  y = cargo_numerico,
                  color = dummy_ci,
                  alpha = dummy_ci),
              size = 2,
              height = 0.1) +
  scale_x_continuous(limits = c(-1, 155),
                     breaks = c(seq(0, 100, 10), 125, 150), 
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:3, labels = rev(lista_cargos)) +
  scale_color_manual(values = c("salmon", "grey70")) +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  guides(colour = guide_legend(override.aes = list(size = 5)),
         alpha = FALSE) +
  # facet_zoom(x = por_tope < 115) +
  labs(title = str_wrap(str_to_upper("Gráfica 3. Porcentaje de gastos de cada candidatura respecto a su tope, por tipo de candidatura"), width = 45),
       x = "\n% de gastos respecto al tope   ",
       y = "",
       caption = "", 
       color = NULL) +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_line(linetype = 3, color = "grey80", size = 0.5), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.867, 0.9),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS Bold"),
        legend.direction = "vertical",
        legend.text = element_text(size = 13, family = "Didact Gothic Regular"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family = "Didact Gothic Regular"),
        axis.ticks = element_blank()) +
  ggsave("03_graficas/g3_porcentaje_gastos_vs_tope_todos_con_zoom.png", width = 12, height = 8, dpi = 200)


# Tabla todos los candidatos
bd_saldos %>% 
  arrange(-por_tope) %>% 
  select(nombre_completo, cargo, dummy_ci, gastos_totales, tope_de_gastos, por_tope) %>% 
  print(n = Inf) %>% 
  write_csv("04_datos_generados/porcentaje_gastos_totales_respecto_tope_de_gastos.csv")

# Tabla candidatos independientes
bd_saldos %>% 
  filter(str_detect(dummy_ci, "Ind")) %>% 
  arrange(-por_tope) %>% 
  select(nombre_completo, cargo, dummy_ci, gastos_totales, tope_de_gastos, por_tope) %>% 
  print(n = Inf)

# Frecuencia de candidaturas que cumplen diversas condiciones
bd_saldos %>% 
  filter(str_detect(cargo, "Sen")) %>% 
  group_by(dummy_ci) %>% 
  summarise(porcentaje = (sum(por_tope < 30)/n())*100) %>% 
  ungroup()

bd_saldos %>% 
  filter(str_detect(cargo, "Dip")) %>% 
  group_by(dummy_ci) %>% 
  summarise(porcentaje = (sum(por_tope > 40)/n())*100) %>% 
  ungroup()
