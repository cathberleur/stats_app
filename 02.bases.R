## Chargement contours
contours <- st_read("contoursGeographiques")
contours <- st_read("M:/Commun/BESTRP/Territoires/Zonage.StatApp/test.code/stats_app-main/stats_app-main/contoursGeographiques") #ssmsi

## Chargement zonages
urlfile<-'https://raw.githubusercontent.com/marie678/test/main/table-appartenance-geo-communes-22.csv'
zonages <- read.csv(urlfile, header=TRUE,sep=';',fileEncoding='cp1252') #création d'un df pour les zonages
liste_labels <- c('Code géographique','Libellé géographique','Département	Région','Arrondissement','Canton ville','Intercommunalité - Métropole',"Nature d'EPCI","Zone d'emploi 2020",'Unité urbaine 2020',"Tranche d'unité urbaine 2020 calculée sur la population 2017","Tranche détaillée d'unité urbaine 2020 calculée sur la population 2017","Aire d'attraction des villes 2020","Tranche d'aire d'attraction des villes 2020 calculée sur la population 2017","Tranche détaillée d'aire d'attraction des villes 2020 calculée sur la population 2017","Catégorie commune dans aire d'attraction des villes 2020","Bassin de vie 2012")

zonages <- read.csv("M:/Commun/BESTRP/Territoires/Zonage.StatApp/test.code/stats_app-main/stats_app-main/table-appartenance-geo-communes-22.csv",sep=';',fileEncoding='cp1252')#ssmsi?

## chargement base
#chez nous, données secrétisées:
load("C:\\Users\\marie\\OneDrive\\Documents\\cours\\ensae\\stat app\\donnees.secretisees.delinquance.RData") # marie
load("~/Documents/ENSAE/Stats_app/Data/donnees.secretisees.delinquance.RData") #cathu
load("C:/Users/Utilisateur/Desktop/donnees.secretisees.delinquance.RData") #clem
# load("/Users/sklenard/Downloads/donnees.secretisees.delinquance.RData")# Gaby 

#ssmsi
#load("M:/Commun/BESTRP/Territoires/Zonage.StatApp/test.code/donnees.secretisees.delinquance.RData")#chemin pour le SSMSI.
load("M:/Commun/BESTRP/Territoires/Zonage.StatApp/test.code/t.init.RData")#chemin pour le SSMSI (vraie base!)

#Découpage base
df_1619 <- subset(t.del, t.del$annee %in% c(2016, 2017, 2018, 2019)) #on sélectionne les années


