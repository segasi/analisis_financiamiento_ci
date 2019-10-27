### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd_fed <- 
  read_excel("01_datos/Saldos Finales - FEDERAL 180828.xlsx") %>% 
  clean_names()

bd_edo <- 
  read_excel("01_datos/Saldos Finales - LOCAL 180828.xlsx") %>% 
  clean_names()

### Unir bases de datos ----
bd_inicial <- bind_rows(bd_fed, bd_edo)


### Transformar y renombrar diversas variables ----
bd_inicial <- 
  bd_inicial %>% 
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


### Crear divers variables ----
bd_inicial <- 
  bd_inicial %>% 
  ### Dummy categoríca para identificar si la candidatura es indepenidente o de otro tipo
  mutate(dummy_ci = ifelse(tipo_asociacion == "Candidatura Independiente", "C. Independiente", "Otros"))
