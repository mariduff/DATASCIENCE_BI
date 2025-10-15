# Cargar librería por si acaso
library(readxl)

# Leer Excel desde Descargas
df <- read_excel("~/Downloads/ETL_analisis_centro_limpio.xlsx")

# Ver primeras filas
head(df)


library(dplyr)
library(lubridate)

# Nos quedamos solo con lo necesario
df_arima <- df %>%
  select(MesAno, CD_CENTRO, EC_ANL_relativo) %>%
  arrange(CD_CENTRO, MesAno)

# Verificamos un ejemplo
head(df_arima)

# Ver nombres de centros únicos
unique(df_arima$CD_CENTRO)

# Elegimos el que sea
centro_ejemplo <- "CENTRO70"

df_centro <- df_arima %>%
  filter(CD_CENTRO == centro_ejemplo)

# Verificamos el centro
print(df_centro)

library(forecast)

# Crear serie temporal desde enero 2023 - inicio de los datos
serie <- ts(df_centro$EC_ANL_relativo, frequency = 12, start = c(2023, 1))

# Ajustar modelo ARIMA automáticamente
modelo <- auto.arima(serie)

# Predecir 6 meses
pred <- forecast(modelo, h = 6)

# Visualizar
plot(pred, main = paste("Predicción ARIMA –", centro_ejemplo))

# OTRA VISUALIZACION MEJOR:
library(ggplot2)

# Fechas futuras
fechas_futuras <- seq(max(df_centro$MesAno) + months(1), by = "month", length.out = 6)

# DataFrame de predicción
df_pred <- data.frame(
  MesAno = fechas_futuras,
  EC_ANL_relativo = as.numeric(pred$mean),
  CD_CENTRO = centro_ejemplo
)

# Combinar reales + predicción
df_total <- bind_rows(df_centro, df_pred)

# Visualización
ggplot(df_total, aes(x = MesAno, y = EC_ANL_relativo)) +
  geom_line(color = "steelblue") +
  geom_point(data = df_pred, aes(x = MesAno, y = EC_ANL_relativo), color = "red", size = 2) +
  ggtitle(paste("Predicción ARIMA para", centro_ejemplo)) +
  theme_minimal()

# Vemos que funciona, hacemos la misma función para todos los centros y lo exportamos para tener los resultados.

library(readxl)
library(dplyr)
library(forecast)
library(openxlsx)

# Cargar tus datos
df <- read_excel("~/Downloads/ETL_analisis_centro_limpio.xlsx")

# Asegurarse de que las fechas estén bien ordenadas
df <- df %>% arrange(CD_CENTRO, MesAno)

# Lista para guardar resultados
predicciones_totales <- data.frame()

# Obtener los centros únicos
centros <- unique(df$CD_CENTRO)

# Iterar por centro
for (centro in centros) {
  
  # Filtrar los datos del centro
  df_centro <- df %>% filter(CD_CENTRO == centro)
  
  # Crear serie temporal
  ts_centro <- ts(df_centro$EC_ANL_relativo, start = c(2023, 1), frequency = 12)
  
  # Ajustar ARIMA
  modelo <- auto.arima(ts_centro)
  
  # Predecir 6 meses
  forecast_centro <- forecast(modelo, h = 6)
  
  # Crear dataframe de predicción
  meses_futuros <- seq(as.Date("2025-07-01"), by = "1 month", length.out = 6)
  pred_df <- data.frame(
    CD_CENTRO = centro,
    Fecha = meses_futuros,
    Prediccion_ARIMA = as.numeric(forecast_centro$mean),
    IC_inferior = as.numeric(forecast_centro$lower[,2]),
    IC_superior = as.numeric(forecast_centro$upper[,2])
  )
  
  # Añadir al total
  predicciones_totales <- bind_rows(predicciones_totales, pred_df)
}

# Guardar en Excel
write.xlsx(predicciones_totales, "Predicciones_ARIMA_Centros.xlsx")

# Mensaje de éxito
cat("Predicciones ARIMA generadas y guardadas como 'Predicciones_ARIMA_Centros.xlsx'\n")

# Visualizamos 5 centros aleatorios para seguir viendo comportamientos:
# Seleccionamos 5 centros aleatorios
set.seed(123)
centros_muestra <- sample(unique(df$CD_CENTRO), 5)

# Visualizar cada uno
for (centro in centros_muestra) {
  ts_centro <- ts(df[df$CD_CENTRO == centro, "EC_ANL_relativo"], frequency = 12, start = c(2023, 1))
  modelo <- auto.arima(ts_centro)
  forecast_centro <- forecast::forecast(modelo, h = 6)
  
  plot(
    forecast_centro, 
    main = paste(" Predicción ARIMA – Centro", centro),
    ylab = "EC_ANL_relativo", xlab = "Año"
  )
}

# Gráfico con solo las líneas de predicción, para comparar
library(ggplot2)

# Generamos un data frame vacío
pred_df <- data.frame()

for (centro in centros_muestra) {
  ts_centro <- ts(df[df$CD_CENTRO == centro, "EC_ANL_relativo"], frequency = 12, start = c(2023, 1))
  modelo <- auto.arima(ts_centro)
  pred <- forecast(modelo, h = 6)
  
  fechas <- seq(as.Date("2025-07-01"), by = "month", length.out = 6)
  
  pred_df <- rbind(pred_df, data.frame(
    Fecha = fechas,
    Centro = centro,
    Prediccion = as.numeric(pred$mean)
  ))
}

# Plot
ggplot(pred_df, aes(x = Fecha, y = Prediccion, color = factor(Centro))) +
  geom_line() +
  labs(title = " Predicciones ARIMA – 5 Centros", y = "EC_ANL_relativo", color = "Centro") +
  theme_minimal()







