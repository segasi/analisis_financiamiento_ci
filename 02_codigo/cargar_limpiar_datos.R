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

