#######Cargar y "Staquear" las capas

library(terra)

# 1. Listar todos los archivos .asc en tu carpeta
archivos_asc <- list.files("variables M/", pattern = "\\.asc$", full.names = TRUE)

# 2. Leerlos como un SpatRaster (stack)
capas_ambientales <- rast(archivos_asc)

# 3. MUY IMPORTANTE: Asignar el sistema de coordenadas (usualmente WGS84)
crs(capas_ambientales) <- "EPSG:4326"


# Generar 10,000 puntos aleatorios dentro de la máscara de tus capas
set.seed(123) # Para que el resultado sea reproducible
puntos_background <- spatSample(capas_ambientales, size = 10000, method = "random", xy = TRUE, na.rm = TRUE)

# Limpiar para que solo queden las columnas de coordenadas
puntos_background <- puntos_background[, c("x", "y")]
colnames(puntos_background) <- c("long", "lat")

#####data frame de presencias 

presencias <- read.csv("OROV_presencias_limpias_ENMeval.csv")
puntos_presencia <- as.data.frame(presencias)


# 1. Forzar nombres de columnas idénticos (longitud y latitud)
colnames(puntos_presencia) <- c("long", "lat")
colnames(puntos_background) <- c("long", "lat")

# 2. Asegurarse de que sean data.frames puros (a veces terra devuelve tibbles o matrices)
puntos_presencia <- as.data.frame(puntos_presencia)
puntos_background <- as.data.frame(puntos_background)

# Cambia "ruta/a/tus/variables" por la carpeta real en tu PC
  ruta_variables <- "variables M/" 

# Listamos los archivos (ajusta la extensión si es .asc o .grd)
archivos <- list.files(ruta_variables, pattern = ".asc$", full.names = TRUE)

# Cargamos el SpatRaster correctamente
capas_ambientales <- terra::rast(archivos)

# VERIFICACIÓN CRÍTICA
print(hasValues(capas_ambientales)) 
# Ahora debe salir [1] TRUE



# Cargar librería
library(ENMeval)
gc()

# Extraer valores ambientales para presencia
vals_pres <- terra::extract(capas_ambientales, puntos_presencia[, c("long", "lat")])
occs_df <- cbind(puntos_presencia[, c("long", "lat")], vals_pres[, -1])

# Extraer valores para el fondo (background)
vals_bg <- terra::extract(capas_ambientales, puntos_background[, c("long", "lat")])
bg_df <- cbind(puntos_background[, c("long", "lat")], vals_bg[, -1])

# Limpiar NAs (Si un punto cae en el mar o fuera del raster, se elimina aquí)
occs_df <- na.omit(occs_df)
bg_df <- na.omit(bg_df)

# Ejecutar ENMevaluate sin pasar el objeto 'envs' (para ahorrar RAM)
res_enmeval <- ENMevaluate(
  occs = occs_df,           # Tabla con coordenadas y valores ambientales
  bg = bg_df,               # Tabla con coordenadas y valores ambientales
  envs = NULL,               # <--- Mantenlo en NULL para no saturar la RAM
  algorithm = "maxnet",
  partitions = "block",
  tune.args = list(
    fc = c("L", "LQ", "H", "LQH", "LQHPT"),
    rm = seq(0.5, 4, 0.5)
  ),
  parallel = TRUE,
  numCores = 4,              # 4 núcleos está bien si envs = NULL
  doClamp = TRUE             # <--- Nombre correcto en v2.0
)


# Extraer la tabla de resultados
resultados <- eval.results(res_enmeval)

# Ordenar por el criterio de información de Akaike (AICc) 
# El modelo con delta.AICc = 0 es el mejor balance entre ajuste y simplicidad
resultados_ordenados <- resultados[order(resultados$delta.AICc), ]

# Ver los 5 mejores
head(resultados_ordenados)

# 1. Obtener los parámetros del mejor modelo
mejor_ajuste <- resultados_ordenados[1, ]
cat("El mejor modelo es:", mejor_ajuste$tune.args, "\n")

# 2. Extraer el objeto del modelo de la lista
mod_optimo <- eval.models(res_enmeval)[[mejor_ajuste$tune.args]]

# 3. Generar la predicción espacial (Mapa)
# Usamos las capas ambientales que recargamos antes
mapa_final <- terra::predict(capas_ambientales, mod_optimo, 
                             type = "cloglog", 
                             na.rm = TRUE)

# 4. Graficar el resultado
plot(mapa_final, main = paste("Predicción Óptima:", mejor_ajuste$tune.args))
points(occs_df[,1:2], pch = 20, cex = 0.5, col = "red") # Añadir tus puntos de presencia

# Guardar el objeto completo para tu tesis
saveRDS(res_enmeval, "resultado_ENMeval_Oropouche.rds")

# Guardar el mapa en formato .tif para usarlo en QGIS o ArcGIS
writeRaster(mapa_final, "mapa_prediccion_final.tif", overwrite = TRUE)




##################################################################################

# 1. Carga tus capas ambientales de México (deben ser las mismas 11 variables)
capas_mexico <- terra::rast(list.files("variables_mex/", pattern = ".asc$", full.names = TRUE))

# 2. Asegúrate de que los nombres coincidan EXACTAMENTE con los del modelo
names(capas_mexico) <- names(capas_ambientales)

# 3. Verificación de seguridad
print(capas_mexico)

# A. Convertir el raster de México a una tabla (solo pixeles con datos)
df_mexico <- as.data.frame(capas_mexico, xy = TRUE, na.rm = TRUE)

# B. Proyectar usando el modelo óptimo
# IMPORTANTE: el data.frame no debe llevar las columnas 'x' e 'y' al modelo
pred_mexico <- predict(mod_optimo, df_mexico[, -c(1,2)], type = "cloglog")

# C. Reconstruir el raster para México
mapa_mexico <- rast(capas_mexico, nlyrs = 1)
# Asignamos los valores predichos a las celdas correspondientes
mapa_mexico[cellFromXY(mapa_mexico, df_mexico[, 1:2])] <- pred_mexico

# D. Visualizar el mapa de riesgo en México
plot(mapa_mexico, main = "Proyección de Idoneidad: Oropouche en México")

writeRaster(mapa_mexico, "Proyeccion_Oropouche_Mexico.tif", overwrite = TRUE)


