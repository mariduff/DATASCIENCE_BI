# Cargar librería por si acaso
library(readxl)

# Leer Excel desde Descargas
df <- read_excel("~/Downloads/ETL_analisis_puesto_filtrado_top40.xlsx")


# limpiar posibles conflictos
rm(list = ls())

# volver a cargar paquetes
library(readxl)
library(dplyr)

# leer el archivo
df <- read_excel("~/Downloads/ETL_analisis_puesto_filtrado_top40.xlsx")

# aplicar mutate correctamente (usando el prefijo dplyr:: para evitar conflictos)
df <- dplyr::mutate(
  df,
  FECHA = as.Date(FECHA),
  `EC/ANL relativo` = as.numeric(`EC/ANL relativo`)
)

str(df)


install.packages("zoo")  # solo la primera vez
library(zoo)

sum(is.na(df$`EC/ANL relativo`))

df$`EC/ANL relativo` <- zoo::na.approx(df$`EC/ANL relativo`, na.rm = FALSE)

df$`EC/ANL relativo` <- zoo::na.locf(df$`EC/ANL relativo`, na.rm = FALSE, fromLast = FALSE)
df$`EC/ANL relativo` <- zoo::na.locf(df$`EC/ANL relativo`, na.rm = FALSE, fromLast = TRUE)

library(dplyr)
library(forecast)
library(ggplot2)

# filtrar datos del puesto
puesto <- "ENFERMERO/A"

df_puesto <- df |>
  filter(PUESTO == puesto) |>
  arrange(FECHA)

head(df_puesto)

library(lubridate)

# crear serie temporal mensual
ts_puesto <- ts(df_puesto$`EC/ANL relativo`,
                start = c(year(min(df_puesto$FECHA)),
                          month(min(df_puesto$FECHA))),
                frequency = 12)

modelo_arima <- auto.arima(ts_puesto)
summary(modelo_arima)

library(dplyr)
library(lubridate)

df_puesto_mensual <- df_puesto |>
  mutate(mes = floor_date(FECHA, "month")) |>
  group_by(mes) |>
  summarise(`EC/ANL relativo` = mean(`EC/ANL relativo`, na.rm = TRUE)) |>
  arrange(mes)

library(forecast)

ts_puesto <- ts(df_puesto_mensual$`EC/ANL relativo`,
                start = c(year(min(df_puesto_mensual$mes)),
                          month(min(df_puesto_mensual$mes))),
                frequency = 12)

modelo_arima <- auto.arima(ts_puesto)
summary(modelo_arima)

pred <- forecast(modelo_arima, h = 6)

library(ggplot2)

# datos históricos (2023–junio 2025)
historico <- data.frame(
  Fecha = df_puesto_mensual$mes,
  Valor = df_puesto_mensual$`EC/ANL relativo`
)

# predicciones (julio–diciembre 2025)
futuro <- data.frame(
  Fecha = seq(max(historico$Fecha) %m+% months(1),
              by = "month", length.out = 6),
  Valor = as.numeric(pred$mean)
)

# gráfico combinado
ggplot() +
  geom_line(data = historico, aes(x = Fecha, y = Valor), color = "steelblue") +
  geom_line(data = futuro, aes(x = Fecha, y = Valor),
            color = "steelblue", linetype = "dashed") +
  labs(title = "Predicción histórica y futura (ARIMA) - ENFERMERO/A",
       x = "Fecha", y = "EC/ANL relativo") +
  theme_minimal()

# agregar bandas de confianza (80% y 95%) al mismo gráfico

# construir data.frame de predicciones con límites
futuro_conf <- data.frame(
  Fecha = futuro$Fecha,
  Pred = as.numeric(pred$mean),
  Lower80 = as.numeric(pred$lower[,1]),
  Upper80 = as.numeric(pred$upper[,1]),
  Lower95 = as.numeric(pred$lower[,2]),
  Upper95 = as.numeric(pred$upper[,2])
)

# gráfico con bandas
ggplot() +
  geom_line(data = historico, aes(x = Fecha, y = Valor), color = "steelblue") +
  geom_ribbon(data = futuro_conf,
              aes(x = Fecha, ymin = Lower95, ymax = Upper95),
              fill = "lightblue", alpha = 0.2) +
  geom_ribbon(data = futuro_conf,
              aes(x = Fecha, ymin = Lower80, ymax = Upper80),
              fill = "skyblue", alpha = 0.3) +
  geom_line(data = futuro_conf, aes(x = Fecha, y = Pred),
            color = "steelblue", linetype = "dashed") +
  labs(title = "Predicción histórica y futura (ARIMA) con intervalos - ENFERMERO/A",
       x = "Fecha", y = "EC/ANL relativo") +
  theme_minimal()


library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# seleccionar los 5 puestos más frecuentes
top_puestos <- df |> 
  count(PUESTO, sort = TRUE) |> 
  slice_head(n = 5) |> 
  pull(PUESTO)

# dataframe para almacenar todos los resultados
resultados <- data.frame()

for (p in top_puestos) {
  df_puesto <- df |> 
    filter(PUESTO == p) |> 
    mutate(mes = floor_date(FECHA, "month")) |> 
    group_by(mes) |> 
    summarise(`EC/ANL relativo` = mean(`EC/ANL relativo`, na.rm = TRUE)) |> 
    arrange(mes)
  
  # crear serie mensual
  ts_puesto <- ts(df_puesto$`EC/ANL relativo`,
                  start = c(year(min(df_puesto$mes)), month(min(df_puesto$mes))),
                  frequency = 12)
  
  modelo <- auto.arima(ts_puesto)
  pred <- forecast(modelo, h = 6)
  
  # combinar histórico y predicción
  historico <- data.frame(
    Fecha = df_puesto$mes,
    Valor = df_puesto$`EC/ANL relativo`,
    Tipo = "Histórico",
    PUESTO = p
  )
  
  futuro <- data.frame(
    Fecha = seq(max(historico$Fecha) %m+% months(1), by = "month", length.out = 6),
    Valor = as.numeric(pred$mean),
    Tipo = "Predicción",
    PUESTO = p
  )
  
  resultados <- bind_rows(resultados, historico, futuro)
}

# visualización facetada
ggplot(resultados, aes(x = Fecha, y = Valor, color = PUESTO, linetype = Tipo)) +
  geom_line(size = 0.8) +
  facet_wrap(~ PUESTO, scales = "free_y", ncol = 2) +
  scale_linetype_manual(values = c("Histórico" = "solid", "Predicción" = "dashed")) +
  labs(title = "Predicción histórica y futura (ARIMA) - 5 puestos principales",
       x = "Fecha", y = "EC/ANL relativo") +
  theme_minimal() +
  theme(legend.position = "none")

library(openxlsx)

write.xlsx(resultados,
           "~/Downloads/predicciones_ARIMA_top5.xlsx",
           overwrite = TRUE)


library(dplyr)
library(lubridate)
library(forecast)
library(openxlsx)

# lista completa de puestos
puestos <- unique(df$PUESTO)

# dataframe donde guardaremos los resultados
resultados_all <- data.frame()

for (p in puestos) {
  df_puesto <- df |>
    filter(PUESTO == p) |>
    mutate(mes = floor_date(FECHA, "month")) |>
    group_by(mes) |>
    summarise(`EC/ANL relativo` = mean(`EC/ANL relativo`, na.rm = TRUE)) |>
    arrange(mes)
  
  # saltar si el puesto tiene pocos datos válidos
  if (nrow(df_puesto) < 6) next
  
  # crear la serie temporal
  ts_puesto <- ts(df_puesto$`EC/ANL relativo`,
                  start = c(year(min(df_puesto$mes)), month(min(df_puesto$mes))),
                  frequency = 12)
  
  # intentar ajustar ARIMA
  modelo <- try(auto.arima(ts_puesto), silent = TRUE)
  if (inherits(modelo, "try-error")) next
  
  # predicción 6 meses hacia el futuro
  pred <- forecast(modelo, h = 6)
  
  # histórico
  historico <- data.frame(
    Fecha = df_puesto$mes,
    Valor = df_puesto$`EC/ANL relativo`,
    Tipo = "Histórico",
    PUESTO = p
  )
  
  # predicciones con intervalos
  futuro <- data.frame(
    Fecha = seq(max(historico$Fecha) %m+% months(1), by = "month", length.out = 6),
    Valor = as.numeric(pred$mean),
    Lower80 = as.numeric(pred$lower[,1]),
    Upper80 = as.numeric(pred$upper[,1]),
    Lower95 = as.numeric(pred$lower[,2]),
    Upper95 = as.numeric(pred$upper[,2]),
    Tipo = "Predicción",
    PUESTO = p
  )
  
  # combinar y acumular
  resultados_all <- bind_rows(resultados_all, historico, futuro)
}

# exportar todo
write.xlsx(resultados_all,
           "~/Downloads/predicciones_ARIMA_todos_puestos.xlsx",
           overwrite = TRUE)

cat("\n✅ Archivo creado: predicciones_ARIMA_todos_puestos.xlsx\n")









