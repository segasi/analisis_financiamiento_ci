### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----

# Base de datos de saldos de fiscalización
bd_saldos_inicial <- 
  read_excel("01_datos/Saldos Finales - FEDERAL 180828.xlsx") %>% 
  clean_names()

# Base de datos del informe de ingresos por candidato
bd_ingresos <- 
  read_excel("01_datos/CF2-Anexo-Ingreso-por-Rubro-Campaña-FEDERAL_2018_07_17.xlsx") %>% 
  clean_names()



### Transformar y renombrar diversas variables ----

# BD saldos
bd_saldos_inicial <- 
  bd_saldos_inicial %>% 
  # Cambiar texto a altas y bajas
  mutate(tipo_de_proceso = str_to_sentence(tipo_de_proceso),
         ambito = str_to_sentence(ambito),
         entidad_federativa = str_to_title(entidad_federativa),
         cargo = str_to_title(cargo),
         cargo = str_replace(cargo, "Mr", "MR"),
         cargo = str_replace(cargo, "Rp", "RP"),
         sujeto_obligado = str_to_title(sujeto_obligado),
         nombre_completo = str_to_title(nombre_completo)) %>% 
  # Cambiar texto a altas y bajas
  mutate(ingresos_percent_determinado_respecto_del_reportado = round(ingresos_percent_determinado_respecto_del_reportado*100, 1),
         gastos_percent_determinado_respecto_del_reportado = round(gastos_percent_determinado_respecto_del_reportado*100, 1),
         percent_del_tope_gastado = round(percent_del_tope_gastado*100, 1)) %>% 
  # Renombrar
  rename(entidad = entidad_federativa,
         por_tope = percent_del_tope_gastado)


# BD ingresos
bd_ingresos <- 
  bd_ingresos %>% 
  # Cambiar texto a altas y bajas
  mutate(tipo_de_proceso = str_to_sentence(tipo_de_proceso),
         ambito = str_to_sentence(ambito),
         entidad_federativa = str_to_title(entidad_federativa),
         cargo_de_eleccion = str_to_title(cargo_de_eleccion),
         cargo_de_eleccion = str_replace(cargo_de_eleccion, "Mr", "MR"),
         cargo_de_eleccion = str_replace(cargo_de_eleccion, "Rp", "RP"),
         sujeto_obligado = str_to_title(sujeto_obligado),
         nombre_completo = str_to_title(nombre_completo))

### Crear diversas variables ----
bd_saldos_inicial <- 
  bd_saldos_inicial %>% 
  ### Dummy categoríca para identificar si la candidatura es indepenidente o de otro tipo
  mutate(dummy_ci = ifelse(tipo_asociacion == "Candidatura Independiente", "C. Independiente", "Otros"), 
         cargo = case_when(cargo == "Jefe De Gobierno" ~ "Jefe de Gobierno",
                           TRUE ~ cargo))

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
