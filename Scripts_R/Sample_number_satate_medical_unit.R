library(readxl)
library(dplyr)
library(leaflet)
library(sf)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

file_data <- "G:/Mi unidad/Postdoc/LCE/OROV/DATA_BASE.xlsx"

data <- read_excel(file_data, sheet = "Hoja1", col_names = TRUE)
coords <- read_excel(file_data, sheet = "Coordenadas", skip = 2)
names(data)[c(4,5,7,12,14,16)] <- c("state","medical_unit","clasificacion","edad","sexo","intervalo")

data <- data %>%
  mutate(state_unit = stringr::str_trim(paste0(state, "-", medical_unit)))

cases_for_unit <- data %>%
  group_by(state, medical_unit, state_unit) %>%
  summarise(
    casos = n(),
    edad_media = round(mean(as.numeric(edad), na.rm=TRUE), 1),
    pct_femenino = round(mean(sexo == "FEMENINO", na.rm=TRUE)*100, 1),
    .groups = "drop"
  )


names(coords)[1:3] <- c("state_unit", "lat", "lon")

coords_unicas <- coords %>%
  mutate(state_unit = stringr::str_trim(state_unit)) %>%
  distinct(state_unit, .keep_all = TRUE) 

mapa_data <- coords_unicas %>%
  left_join(cases_for_unit, by = "state_unit") %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  mutate(casos = coalesce(casos, 0))

mapa_data <- mapa_data %>%
  mutate(
    state = coalesce(state, sub("-.*", "", state_unit)),
    medical_unit = coalesce(medical_unit, sub("^.*?-", "", state_unit))
  )


mexico <- ne_states(country = "Mexico", returnclass = "sf")

limpiar_texto <- function(texto) {
  if(is.null(texto)) return(texto)
  texto <- toupper(texto)
  texto <- iconv(texto, to = "ASCII//TRANSLIT")
  texto <- str_trim(texto)
  return(texto)
}

mexico <- mexico %>%
  mutate(
      name_clean = limpiar_texto(name),
      name_clean = case_when(
      name_clean == "COAHUILA DE ZARAGOZA" ~ "COAHUILA",
      name_clean == "MICHOACAN DE OCAMPO" ~ "MICHOACAN",
      name_clean == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "VERACRUZ",
      name_clean == "ESTADO DE MEXICO" ~ "MEXICO",
      name_clean == "CIUDAD DE MEXICO" ~ "DISTRITO FEDERAL", # Cambiar según tu Excel (CDMX / DF)
      TRUE ~ name_clean
    )
  )

cases_per_state <- mapa_data %>%
  mutate(state_clean = limpiar_texto(state)) %>% 
  group_by(state_clean) %>%
  summarise(total_casos_estado = sum(casos, na.rm = TRUE), .groups = "drop")

mexico_unido <- mexico %>%
  left_join(cases_per_state, by = c("name_clean" = "state_clean")) %>%
  mutate(total_casos_estado = coalesce(total_casos_estado, 0))


map_sampling <- ggplot() +
  geom_sf(data = mexico_unido, aes(fill = total_casos_estado), color = "#444444", linewidth = 0.4) +
  
geom_point(
    data = mapa_data,
    aes(x = lon, y = lat),
    shape = 21,          
    fill = "blue",        
    color = "black",    
    stroke = 0.5,        
    size = 2,            
    alpha = 1
  ) +
  
 
 scale_fill_distiller(palette = "Reds", direction = 1, name = "Sample number") +
  
 coord_sf(xlim = c(-118, -86), ylim = c(14.5, 33)) +
  
  labs(
    title = "Distribution of arbovirus cases",
    subtitle = paste0("IMSS Medical Units"),
    x = "Longitude", y = "Latitude",
    caption = "Source: RT-qPCR Triplex (DENV/ZIKV/CHIKV/OROV)"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.major = element_line(color = "#DDDDDD")
  )

print(map_sampling)

