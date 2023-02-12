
# Chargement des packages pour la manipulation des données:

library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)

# Chargement des données:

# 1) Fichier géolocalisé (secrétisé) du SSMSI des communes de commission des actes de
# délinquance, des communes des victimes et des communes des mis an cause:
load("/Users/sklenard/Downloads/donnees.secretisees.delinquance.RData") # Fichier t.del (tibble)

# 2) Fichier des différents zonages de l'Insee:
# Chargement depuis notre repo sous github :
urlfile<-'https://raw.githubusercontent.com/marie678/test/main/table-appartenance-geo-communes-22.csv'
zonages <- read.csv(urlfile, header=TRUE,sep=';',fileEncoding='cp1252') #création d'un df pour les zonages
liste_labels <- c('Code géographique','Libellé géographique','Département	Région','Arrondissement','Canton ville','Intercommunalité - Métropole',"Nature d'EPCI","Zone d'emploi 2020",'Unité urbaine 2020',"Tranche d'unité urbaine 2020 calculée sur la population 2017","Tranche détaillée d'unité urbaine 2020 calculée sur la population 2017","Aire d'attraction des villes 2020","Tranche d'aire d'attraction des villes 2020 calculée sur la population 2017","Tranche détaillée d'aire d'attraction des villes 2020 calculée sur la population 2017","Catégorie commune dans aire d'attraction des villes 2020","Bassin de vie 2012")
# on le convertit en un fichier tibble:
t.zonages <- as_tibble(zonages)

# Subsetting: 
# On se restreint à la période 2016-2019:
t.del1619 <- t.del %>%
  filter(annee >= 2016 & annee <= 2019)

# Passage du format wide au format long:
t.del1619_long <- t.del1619 %>% 
  pivot_longer(cols = starts_with("cog_com_22_"), 
               names_to = "communes_3_lieux", 
               values_to = "code_commune")
t.del1619_long

# Jointure des tables t.del1619_long et t.zonages
t.del1619_long_zon <- t.del1619_long %>% 
  left_join(y = t.zonages, 
            by = c("code_commune" = "CODGEO"))

# Codage des indicatrices pour détecter les atteintes pour lesquelles les 3 lieux se trouvent
# dans le même zonage, ou bien seuls 2 lieux ou 1 seul:

# on repasse en format wide:
t.del1619_wide_zon <- t.del1619_long_zon %>%
  pivot_wider(id_cols =TYPEQU,  
              names_from = REG, 
              values_from = NB_EQUIP_TOT, 
              names_prefix = "nb_equip_reg")









