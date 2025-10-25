# Cargar librería por si acaso
library(readxl)

# Leer Excel desde Descargas
df <- read_excel("~/Downloads/ETL_analisis_puesto_filtrado.xlsx")

# Ver primeras filas
head(df)

library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

df$fecha <- as.Date(df$fecha)

#Para ver ejemplos de puestos
puestos_muestra <- df %>%
  count(puesto, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(puesto)

# Creamos dataframes vacíos
df_predicciones <- data.frame()
df_historico <- data.frame()

# Bucle para cada puesto
for (p in puestos_muestra) {
  df_p <- df %>%
    filter(puesto == p) %>%
    group_by(fecha) %>%
    summarise(valor = mean(ec_anl_relativo, na.rm = TRUE)) %>%
    arrange(fecha)
  
  # Guardamos histórico
  df_historico <- bind_rows(df_historico, data.frame(
    Fecha = df_p$fecha,
    Puesto = p,
    Valor = df_p$valor,
    Tipo = "Histórico"
  ))
  
  # Serie temporal
  ts_p <- ts(df_p$valor, frequency = 12, 
             start = c(year(min(df_p$fecha)), month(min(df_p$fecha))))
  modelo <- auto.arima(ts_p)
  pred <- forecast(modelo, h = 6)
  
  fechas_pred <- seq(from = max(df_p$fecha) %m+% months(1), by = "month", length.out = 6)
  
  df_predicciones <- bind_rows(df_predicciones, data.frame(
    Fecha = fechas_pred,
    Puesto = p,
    Valor = as.numeric(pred$mean),
    Tipo = "Predicción"
  ))
}

# Unimos los dos dataframes
df_completo <- bind_rows(df_historico, df_predicciones)

# Visualización con ggplot2
library(ggplot2)

ggplot(df_completo, aes(x = Fecha, y = Valor, color = Puesto, linetype = Tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución del EC_ANL_relativo – Histórico y Predicción (por Puesto)",
    x = "Fecha",
    y = "EC_ANL_relativo",
    color = "Puesto",
    linetype = "Tipo"
  ) +
  scale_linetype_manual(values = c("Histórico" = "solid", "Predicción" = "dashed")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Para guardar el archivo:

install.packages("openxlsx")  

# Cargar la librería
library(openxlsx)

# Creamos un nuevo workbook
wb <- createWorkbook()

# Agregamos hojas
addWorksheet(wb, "Historico")
addWorksheet(wb, "Prediccion")
addWorksheet(wb, "Completo")

# Escribimos los datos
writeData(wb, "Historico", df_historico)
writeData(wb, "Prediccion", df_predicciones)
writeData(wb, "Completo", bind_rows(df_historico, df_predicciones))

# Guardamos el archivo
saveWorkbook(wb, file = "Prediccion_Puestos_R.xlsx", overwrite = TRUE)

df_completo <- bind_rows(df_historico, df_predicciones)

# Guardar todo en un solo archivo
write.xlsx(df_completo, file = "Prediccion_Puestos_R.xlsx", overwrite = TRUE)

write.xlsx(df_completo, file = "~/Desktop/Prediccion_Puestos_R.xlsx", overwrite = TRUE)

#Viendo que funciona para los 5 puestos de muestra, pasamos a hacerlo para todos los puestos:

puestos <- unique(df$puesto)

df_predicciones <- data.frame()
df_historico <- data.frame()

for (p in puestos) {
  df_p <- df %>%
    filter(puesto == p) %>%
    group_by(fecha) %>%
    summarise(valor = mean(ec_anl_relativo, na.rm = TRUE)) %>%
    arrange(fecha)
  
  if (nrow(df_p) < 12) next  # Evita puestos con pocos datos
  
  # Guardamos histórico
  df_historico <- bind_rows(df_historico, data.frame(
    Fecha = df_p$fecha,
    Puesto = p,
    Valor = df_p$valor,
    Tipo = "Histórico"
  ))
  
  # Modelo ARIMA
  ts_p <- ts(df_p$valor, frequency = 12,
             start = c(year(min(df_p$fecha)), month(min(df_p$fecha))))
  
  modelo <- auto.arima(ts_p)
  pred <- forecast(modelo, h = 6)
  
  fechas_pred <- seq(from = max(df_p$fecha) %m+% months(1),
                     by = "month", length.out = 6)
  
  df_predicciones <- bind_rows(df_predicciones, data.frame(
    Fecha = fechas_pred,
    Puesto = p,
    Valor = as.numeric(pred$mean),
    Tipo = "Predicción"
  ))
}

df_completo <- bind_rows(df_historico, df_predicciones)

# Guardar en Excel
library(openxlsx)
write.xlsx(df_completo, file = "Prediccion_Todos_Puestos.xlsx", overwrite = TRUE)

write.xlsx(df_completo, file = "~/Desktop/Prediccion_Todos_Puestos_R.xlsx", overwrite = TRUE)

