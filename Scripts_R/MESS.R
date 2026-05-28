## 1. Cargar librerías necesarias
library(terra)
library(dismo) # Para la función mess

# PREPARACIÓN DE VARIABLES EN LA "M" (CALIBRACIÓN) 
path_M <- "variables M/"
archivos_M <- list.files(path_M, pattern = "\\.asc$", full.names = TRUE)
env_M <- rast(archivos_M)

# Cargar tus puntos de presencia (long, lat)
puntos_presencia <- read.csv("OROV_presencias_limpias_ENMeval.csv")
# Extraer valores ambientales de los puntos en Sudamérica

valores_entrenamiento <- terra::extract(env_M, puntos_presencia[, c("long", "lat")])
valores_entrenamiento <- valores_entrenamiento[, -1] # Quitar columna ID
valores_entrenamiento <- na.omit(valores_entrenamiento) # Limpiar NAs

# PREPARACIÓN DE VARIABLES EN MÉXICO (PROYECCIÓN)
path_Mex <- "variables_mex/"
archivos_Mex <- list.files(path_Mex, pattern = "\\.asc$", full.names = TRUE)
env_Mex <- rast(archivos_Mex)

env_Mex_stack <- stack(env_Mex)

#  EJECUCIÓN DEL ANÁLISIS MESS
# v = valores extraídos de la M (donde el virus ya está presente)
# x = las condiciones ambientales en México (donde queremos evaluar la similitud)
mess_res <- mess(x = env_Mex_stack, v = valores_entrenamiento, full = TRUE)

# Convertir el resultado de vuelta a formato terra para mayor eficiencia en el manejo
mess_final <- rast(mess_res)

#  EXPORTACIÓN Y MAPEO 
# Capa 1: El valor MESS (Similitud ambiental)
# Capa 2: MoD (La variable más disímil en cada píxel)
mess_valor <- mess_final[[1]]
mod_variable <- mess_final[[2]]

# Guardar resultados
writeRaster(mess_valor, "Analisis_MESS_OROV_Mexico.tif", overwrite = TRUE)


# Verifica primero que el objeto tenga valores
print(mess_valor) 

# Usa terra::plot 



terra::plot(mess_valor, 
            main = "MESS",
            col = terrain.colors(100),
            mar = c(3, 3, 3, 6), # Aumenté el último número a 6 para dar más espacio a la derecha
            plg = list(title = "MESS\nscore", cex = 0.9))
png("Mapa_MESS_Prueba.png", width = 800, height = 600)


dev.off() # Esto cierra el archivo y lo guarda
