### Correr scripts para poder realizar análisis ----
source("02_codigo/paquetes_setup_tema.R")
source("02_codigo/cargar_limpiar_datos.R")

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

