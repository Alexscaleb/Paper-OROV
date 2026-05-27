
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Descargar todos los países del mundo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Seleccionar solo América Latina y el Caribe
latam_countries <- world %>%
  filter(
    region_un %in% c("Americas"),  # Todo el continente americano
    subregion %in% c(
      "Central America", "South America", 
      "Caribbean", "Northern America"
    ),
    admin %in% c(
      # Países desde México hasta Argentina, incluyendo islas relevantes
      "Mexico", "Belize", "Guatemala", "Honduras", "El Salvador",
      "Nicaragua", "Costa Rica", "Panama",
      "Cuba", "Jamaica", "Haiti", "Dominican Republic", "Puerto Rico",
      "Trinidad and Tobago", "The Bahamas",
      "Colombia", "Venezuela", "Ecuador", "Peru", "Bolivia",
      "Chile", "Argentina", "Paraguay", "Uruguay", "Brazil", "Suriname",
      "Guyana", "French Guiana"
    )
  )

# Guardar en GeoPackage (compatible con QGIS)
st_write(latam_countries, "Latinoamerica_Caribe.gpkg", delete_dsn = TRUE)



