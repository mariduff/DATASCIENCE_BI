# DATASCIENCE_BI
Proyecto de TFM del máster de Ciencia de Datos

Este repositorio en Github contiene el código, datasets y documentación del Trabajo de Fin de Máster sobre "Análisis predictivo del absentismo laboral mediante Ciencia de Datos y Power BI" en una empresa sociosanitaria (residencias de mayores), cuyo objetivo es analizar y predecir el absentismo laboral utilizando técnicas de ciencia de datos (Python y R) y herramientas de Business Intelligence (Power BI) como herramienta de trabajo futura para planificar y organizar a los equipos. 


# 1º FASE - EXPLORACIÓN
El archivo "perfil_columnas.csv" (resultado del perfilado de datos) se encuentra en el directorio  
/TFM/data/processed/ de Google Drive.  
Por razones de confidencialidad, no se incluye en este repositorio.

# 2º FASE - LIMPIEZA POR CENTRO
Notebook "Limpieza.ipynb"
Archivo creado: TTT_totales_por_centro.xlsx 
Es un subconjunto del dataset original, con las filas resumen por centro.
/data/processed/TTT_totales_por_centro.xlsx
Este dataset se utiliza como base para los análisis y modelos predictivos a nivel de centro de trabajo.

# 3º FASE - ETL Y ANÁLISIS
Notebook "ETL_analisis"
Archivo creado: ETL_analisis_centro_limpio.xlsx
Se realiza procesos de transformación y nuevos cálculos, se hace análisis temporal de variables.

# 4º FASE - Entrenamiento y predicción
Notebook "Entrenamiento_predicción.ypinb
Archivo creado con 6 meses de predicción futura para el cuadro de mando en Power BI: predicciones_6meses_por_centro.xlsx y .csv
Se realiza el proceso de entrenamiento y predicción con varios modelos y se comparan (mejor XGBoost) y después se hace una predicción futuro para 6 meses.
