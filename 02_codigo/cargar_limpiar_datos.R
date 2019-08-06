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
bd <- bind_rows(bd_fed, bd_edo)
