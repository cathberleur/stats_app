#Chargement contours
contours <- st_read("contoursGeographiques")

#Chargement zonages
urlfile<-'https://raw.githubusercontent.com/marie678/test/main/table-appartenance-geo-communes-22.csv'
zonages <- read.csv(urlfile, header=TRUE,sep=';',fileEncoding='cp1252') #création d'un df pour les zonages
liste_labels <- c('Code géographique','Libellé géographique','Département	Région','Arrondissement','Canton ville','Intercommunalité - Métropole',"Nature d'EPCI","Zone d'emploi 2020",'Unité urbaine 2020',"Tranche d'unité urbaine 2020 calculée sur la population 2017","Tranche détaillée d'unité urbaine 2020 calculée sur la population 2017","Aire d'attraction des villes 2020","Tranche d'aire d'attraction des villes 2020 calculée sur la population 2017","Tranche détaillée d'aire d'attraction des villes 2020 calculée sur la population 2017","Catégorie commune dans aire d'attraction des villes 2020","Bassin de vie 2012")