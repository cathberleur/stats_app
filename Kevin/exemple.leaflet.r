# Packages nécessaires

library(tidyverse)
library(sf)
library(leaflet)

# Chargement des contours communaux et création d'une indicatrice arbitraire (plus simple en facteur, notamment pour la gestion de la légende)

contours  = st_read("M:/Commun/BESTRP/Territoires/Zonage.StatApp/donnees/contoursGeographiques/communes-20220101-simpl04.shp") %>%
  filter(substr(insee,1,2) == "50") 

contours = contours %>%
  mutate(
    u = runif(nrow(contours),0,1),
    classe = case_when(u < .25 ~ "1", u >= .25 & u < .50 ~ "2", u >= .50 & u < .75 ~ "3", u >= .75 ~ "4") %>% factor(levels = as.character(1:4))
    ) %>%
  select(insee, classe) 

# Création d'une palette de couleurs

pal = colorFactor(palette = "RdYlBu", as.character(1:4), reverse = TRUE)

# Création de la carte

leaflet(data = contours) %>%
  
  # Couches OSM
  
  addTiles(group = "Plan",options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Plan") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Vue satellite") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Vue noir et blanc") %>%
  
  addLayersControl(position = "topleft",
                   baseGroups = c("Plan", "Vue noir et blanc","Vue satellite","Gris"),
                   options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Couches des polygones ou des informations que l'on souhaite ajouter

  addPolygons(
    data = contours,
    group = "exemple",
    fillColor = ~pal(contours$classe),
    fillOpacity = 0.5,opacity = 0,weight = 2, stroke = TRUE,
    color = NULL,
    label = contours$insee # information à afficher dans les popup
  ) %>%
  
  
  # Ajouter la légende
  
  addLegend("topright", pal = pal, values = ~contours$classe,
            title = paste0("Exemple de Leaflet"),
            opacity = 0.3
  )
