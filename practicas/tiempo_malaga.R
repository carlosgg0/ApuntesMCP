library(tidyverse)
library(here)

# Leemos los datos de Málaga y eliminamos las filas de resumen anual 
# (mes acaba en "-13")
dataset_malaga <- read_csv(
  here('practicas/data/malaga_2026.csv'), 
  show_col_types = FALSE) |> 
  filter(!str_detect(fecha, "-13")) |> 
  mutate(
    # Creamos una columna fecha tipo Date para representar el mes
    fecha_date = ymd(paste0(fecha, "-01"))
  )

# Visualizamos la evolución histórica con ggplot2
ggplot(dataset_malaga, 
       aes(x = fecha_date, y = tm_min)) +
  geom_line(color = "steelblue", alpha = 0.8) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Evolución histórica — Temperatura mínima en Málaga (1951-)",
    subtitle = "El dataset AEMET que usaremos a lo largo de todo el capítulo",
    x = "Año", y = "Temperatura Mínima (°C)"
  ) +
  theme(plot.title = element_text(face = "bold"))

# Visualizamos la evolución histórica de las precipitaciones
ggplot(dataset_malaga, 
       aes(x = fecha_date, y = p_mes)) +
  geom_line(color = "steelblue", alpha = 0.8) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Evolución histórica — Precipitaciones en Málaga (1951-)",
    subtitle = "El dataset AEMET que usaremos a lo largo de todo el capítulo",
    x = "Año", y = "Temperatura Mínima (°C)"
  ) +
  theme(plot.title = element_text(face = "bold"))

library(forecast)
library(tseries)
library(TTR)

# Existen algunos registros mensuales vacíos (NA), los interpolamos -- imputamos
temp_minima_limpia <- na.interp(dataset_malaga$tm_min)

# Aquí creamos la serie temporal con R
ts_malaga <- ts(
  temp_minima_limpia,
  start     = c(1951, 1),
  frequency = 12
)

cat("Clase del objeto:  ", class(ts_malaga), "\n")

cat("Frecuencia:        ", frequency(ts_malaga), "(mensual)\n")

cat("Inicio:            ", start(ts_malaga), "\n")

cat("Fin:               ", end(ts_malaga), "\n")

# Descomposición de las temperaturas mensuales
decomp_stl <- stl(ts_malaga, s.window = "periodic")

autoplot(decomp_stl) +
  labs(title = "Descomposición STL — Temperaturas Málaga") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# Descomposición para las precipitaciones mensuales
precipitaciones_limpias <- na.interp(dataset_malaga$p_mes)

ts_precipitaciones <- ts(
  precipitaciones_limpias,
  start     = c(1951, 1),
  frequency = 12
)

decomp_stl <- stl(ts_precipitaciones, s.window = 'periodic')
autoplot(decomp_stl) +
  labs(title = "Descomposición STL — Precipitaciones Málaga") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
