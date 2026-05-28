## 1. Instalar y cargar librerías necesarias
if(!require(usdm)) install.packages("usdm")
if(!require(terra)) install.packages("terra")

library(usdm)
library(terra)

# Cargar tus variables ambientales (ajusta la ruta a tus archivos)

files <- list.files("variables_M_biomas/", pattern='\\.asc$', full.names=TRUE)
files
env_stack <- rast(files)


# Análisis VIF simple
vif_results <- vif(env_stack)
print(vif_results)

# Selección automática (Elimina variables con VIF > 10 de forma iterativa)
# El parámetro 'th' es el umbral (threshold) --- Se decide dejar en 6 
vif_filt <- vifstep(env_stack, th = 6)

print(vif_filt)

# 6. Guardar el set de variables que pasaron el filtro
env_final <- exclude(env_stack, vif_filt)
plot(env_final) # Visualización rápida
