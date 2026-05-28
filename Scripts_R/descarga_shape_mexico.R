## instalar paquetes si no los tienes
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))

library(sf)
library(rnaturalearth)

# Descargar México desde Natural Earth 
mexico <- ne_countries(country = "Mexico",
                       scale = "medium",
                       returnclass = "sf")

# Asegurar CRS correcto
mexico <- st_transform(mexico, 4326)

# Guardar shapefile
st_write(mexico, "mexico_mask.shp", delete_layer = TRUE)

cat("Shapefile guardado como: mexico_mask.shp\n")
