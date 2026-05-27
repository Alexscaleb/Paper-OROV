library(terra)

# Carpeta donde están tus capas hurs en formato .asc 
dir_hurs <- "../variables_M_biomas/"   # <--- cámbialo

# Detectar todos los archivos que comienzan con "hurs" y terminan en .asc 
files_hurs <- list.files(
  dir_hurs,
  pattern = "^hurs.*\\.asc$",
  full.names = TRUE
)

print("Archivos detectados:")
print(files_hurs)

# Cargar los 12 meses como un stack 
hurs_stack <- rast(files_hurs)

#  Definir estaciones 
rainy_months <- 5:10                # Mayo-Octubre
dry_months   <- c(11,12,1,2,3,4)    # Nov-Abril

# Calcular promedios estacionales 
hurs_rainy <- app(hurs_stack[[rainy_months]], mean, na.rm = TRUE)
hurs_dry   <- app(hurs_stack[[dry_months]],   mean, na.rm = TRUE)

# Guardar resultados en TIFF o ASC
writeRaster(hurs_rainy, file.path(dir_hurs, "hurs_rainy.asc"), overwrite = TRUE)
writeRaster(hurs_dry,   file.path(dir_hurs, "hurs_dry.asc"),   overwrite = TRUE)

print("Listo: Se generaron hurs_rainy_mean.tif y hurs_dry_mean.tif")

