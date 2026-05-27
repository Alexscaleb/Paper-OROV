## Análisis de colinealidad y PCA para variables ambientales de Oropouche

# Limpieza del entorno
rm(list = ls())

# Cargar paquetes requeridos
library(terra)
library(raster)
library(usdm)
library(corrplot)
library(car)
library(spThin)

# Establecer directorio de trabajo con los ASCII (ajusta si es necesario)
setwd("variables_M_biomas/")


# Listar archivos .asc que empiezan con bio, elev, hurs o swb
asc_files <- list.files(
  pattern = "^(bio|elev|hurs|swb).*\\.asc$",
  full.names = TRUE
)

cat("Archivos encontrados:", length(asc_files), "\n")
print(asc_files)

#Crear un stack con las variables ambientales
capas_presente <- stack(asc_files)
print(capas_presente)

## Cargar los puntos de presencia de Oropouche

# Asegúrate que el archivo tenga columnas: especie, lon, lat
datos_orov <- read.csv("../OROV_presencias_limpias.csv", stringsAsFactors = FALSE)
head(datos_orov)

# Verificar coordenadas
coords <- data.frame(x = as.numeric(datos_orov$lon),
                     y = as.numeric(datos_orov$lat))
stopifnot(!any(is.na(coords))) # detiene ejecución si hay NAs

# Crear objeto con presencia y coordenadas
presencias <- data.frame(
  especie = datos_orov$especie,
  lon = coords$x,
  lat = coords$y
)



## Extraer valores de las variables ambientales para los puntos de presencia

valores_presencia <- data.frame(extract(capas_presente, presencias[, c("lon", "lat")]))
presencias_variables <- data.frame(presencias, valores_presencia)
presencias_variables <- na.omit(presencias_variables)


# Guardar archivo limpio
write.csv(presencias_variables, "OROV_1km.csv", row.names = FALSE)
cat("✅ Archivo guardado: OROV_1km_csv\n")

# Calcular matriz de correlación entre variables ambientales

# Ajusta el rango de columnas según tus datos (4:32 si son 29 variables)
vars_num <- presencias_variables[, 4:23 ]
cormatriz <- cor(vars_num, use = "complete.obs")

# Graficar correlaciones
png("vp_corr_presente.png", width = 2700, height = 2000, res = 300)
corrplot(cormatriz, type = "upper", outline = TRUE, tl.col = "black",
         mar = c(2, 0, 1, 1.5), title = "Correlación entre variables (OROV)")
dev.off()
cat("✅ Mapa de correlaciones guardado: vp_corr_presente.png\n")

## Evaluar colinealidad mediante el Factor de Inflación de Varianza (VIF)
# -----------------------------------------------------------------------------
# Valores de referencia:
# VIF > 10  → colinealidad alta
# VIF > 4–5 → colinealidad moderada; diversos autores recomiendan eliminar
#              variables dentro de este rango para reducir redundancia ambiental

vif_resultados <- vif(vars_num)
print(vif_resultados[order(-vif_resultados$VIF), ])

# Reducción automática de variables colineales (umbral ajustable)
vif_seleccion <- vifstep(vars_num, th = 6)
cat("✅ Variables retenidas tras VIF-step:\n")
print(vif_seleccion@results)

## Análisis de Componentes Principales (PCA) con visualización tipo biplot
#Instalar si no lo tienes
install.packages("factoextra")

library(factoextra)

# Escalar y centrar las variables seleccionadas por VIF
vars_final <- vars_num[, vif_seleccion@results$Variables, drop = FALSE]
head(vars_final)
# Calcular PCA
pca_orov <- prcomp(vars_final, center = TRUE, scale. = TRUE)

# Resumen de varianza explicada
summary(pca_orov)

# Visualización tipo biplot
fviz_pca_biplot(
  pca_orov,
  geom.ind = "point",       # Representa las observaciones como puntos
  col.ind = "gray30",       # Color de los puntos
  alpha.ind = 0.8,          # Transparencia
  geom.var = c("arrow", "text"), # Variables como flechas y texto
  col.var = "steelblue4",   # Color de las flechas y nombres de variables
  repel = TRUE,             # Evita que se sobrepongan las etiquetas
  labelsize = 4,            # Tamaño del texto de variables
  arrowsize = 0.6,          # Tamaño de las flechas
  title = "PCA - Biplot OROV",
  axes = c(1, 2)            # Ejes principales
)

# Exportar la gráfica en alta resolución
png("PCA_Biplot_OROV.png", width = 2500, height = 1800, res = 300)
fviz_pca_biplot(
  pca_orov,
  geom.ind = "point",
  col.ind = "gray30",
  alpha.ind = 0.8,
  geom.var = c("arrow", "text"),
  col.var = "steelblue4",
  repel = TRUE,
  labelsize = 4,
  arrowsize = 0.6,
  title = "PCA - Biplot OROV",
  axes = c(1, 2)
)
dev.off()
cat("✅ Biplot guardado: PCA_Biplot_OROV.png\n")
