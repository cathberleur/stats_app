
# 1) Chargement des packages pour la manipulation des données:

library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(readxl)
library(questionr)
library(forcats)
library(data.table)
library(openxlsx)

# 2) Chargement des données:

# a) Fichier géolocalisé (secrétisé) du SSMSI des communes de commission des actes de
# délinquance, des communes des victimes et des communes des mis an cause:
load("/Users/sklenard/Downloads/donnees.secretisees.delinquance.RData") # Fichier t.del (tibble)

# b) Table de passage communes -> zonages de l'Insee:
chemin <-"/Users/sklenard/Documents/GitHub/stats_app/table-appartenance-geo-communes-22_v2022-09-27.xlsx"

t.zonages <- readxl::read_excel(path = chemin, sheet = "COM",skip=5) # création d'un tibble.
names(t.zonages)

# c) Contours géographiques des différents zonages (utiles pour les fonds de carte):
# TODO!

# 3) Création de la base d'étude (jointure entre les différentes bases)

# Checks sur les 3 variables de jointure (code commune des 3 lieux caractéristiques d'une atteinte:
# lieu de l'atteinte, lieu d'habitation de la victime, lieu d'habitation du mis en cause.
sum(is.na(t.del$cog_com_22_inf)) # 39 957 valeurs manquantes
sum(is.na(t.del$cog_com_22_vict)) # 518 354 valeurs manquantes
sum(is.na(t.del$cog_com_22_mec)) # 5 178 917 valeurs manquantes

# Renommage des noms de colonne dans le tibble t.zonages (selon les 3 lieux)
t.zonages_inf <- t.zonages
names(t.zonages_inf) <-paste0(names(t.zonages_inf),"_inf")

t.zonages_vict <- t.zonages
names(t.zonages_vict) <-paste0(names(t.zonages_vict),"_vict")

t.zonages_mec <- t.zonages
names(t.zonages_mec) <-paste0(names(t.zonages_mec),"_mec")

# Deux sous-bases séparant la fenêtre temporelle des données en deux (avant-après Covid):

table(t.del$annee) # on observe bien une baisse du nombre d'atteintes enregistrées en 2020
# et 2021 par rapport à la période pré-Covid 2016-2019.

tab <-table(t.del$classe,t.del$annee)
cprop(tab)
# Par contre en structure par type d'atteinte, on reste très stable sur toute la période
# 2016-2021 (pas de déformation de la structure par le Covid).

# On préfère néanmoins travailler sur la période 2016-2019, en particulier parce-qu'il se 
# joue probablement des choses singulières durant les années Covid, notamment en termes de
# répartition territoriale et de distance entre lieu de l'atteinte et lieux d'habitation
# de la victime et du mis en cause. On se laisse la possibilité de répliquer l'analyse sur les
# années 2020-2021, pour comparer.

# On crée donc deux sous-bases correspondant à chaque sous-période et faisant la jointure des 
# données géolocalisées sur les atteintes (SSMSI) et celles sur les différents zonages de l'Insee.
# Pour chaque atteinte, on associe à chacun des trois lieux (inf/vict/mec), les zonages correspondants.

#a) 1ère sous-base: 2016-2019

t.del1619 <- t.del %>%
  filter(annee >= 2016 & annee <= 2019) %>%
  left_join(y = t.zonages_inf, 
            by = c("cog_com_22_inf" = "CODGEO_inf")) %>%
  left_join(y = t.zonages_vict, 
            by = c("cog_com_22_vict" = "CODGEO_vict")) %>%
  left_join(y = t.zonages_mec, 
            by = c("cog_com_22_mec" = "CODGEO_mec"))

#b) 2ème sous-base: 2020-2021

t.del2021 <- t.del %>%
  filter(annee >= 2020 & annee <= 2021) %>%
  left_join(y = t.zonages_inf, 
            by = c("cog_com_22_inf" = "CODGEO_inf")) %>%
  left_join(y = t.zonages_vict, 
            by = c("cog_com_22_vict" = "CODGEO_vict")) %>%
  left_join(y = t.zonages_mec, 
            by = c("cog_com_22_mec" = "CODGEO_mec"))

# on peut supprimer la base initiale:
rm(t.del)

# Premiers tris à plat:

# Nombre d'atteintes par année:
tab1 <-freq(t.del1619$annee,total=TRUE)
tab1

# Répartition des atteintes par type:
tab2 <-freq(t.del1619$classe,total=TRUE,sort="dec")
tab2

# recodage de la variable "classe": grâce à la répartition des atteintes par type obtenue le 09/02
# au SSMSI à partir des vraies données et en faisant l'hypothèse que les modalités de la variable 
# classe sont classés dans le même ordre dans les deux fichiers lorsque l'on trie les fréquences
# associées à chaque modalité par ordre décroissant:

# sous-base 2016-2019:

t.del1619$classe2 <- fct_recode(
  t.del1619$classe,
  "Vols sans violence contre des personnes" = "W",
  "Destructions et dégradations" = "K",
  "Vols dans les véhicules" = "R",
  "Cambriolages de logement" = "B",
  "Coups et blessures volontaires en dehors de la sphère familiale" = "X",
  "Vols de véhicules" = "T",
  "Coups et blessures volontaires dans la sphère familiale" = "V",
  "Vols d'accessoires sur véhicules" = "S",
  "Vols violents sans arme" = "G",
  "Violences sexuelles" = "A",
  "Vols avec armes" = "O",
  "Homicides" = "D",
)


tab3 <-freq(t.del1619$classe2,total=TRUE,sort="dec")
tab3

# sous-base 2020-2021:

t.del2021$classe2 <- fct_recode(
  t.del2021$classe,
  "Vols sans violence contre des personnes" = "W",
  "Destructions et dégradations" = "K",
  "Vols dans les véhicules" = "R",
  "Cambriolages de logement" = "B",
  "Coups et blessures volontaires en dehors de la sphère familiale" = "X",
  "Vols de véhicules" = "T",
  "Coups et blessures volontaires dans la sphère familiale" = "V",
  "Vols d'accessoires sur véhicules" = "S",
  "Vols violents sans arme" = "G",
  "Violences sexuelles" = "A",
  "Vols avec armes" = "O",
  "Homicides" = "D",
)

tab4 <-freq(t.del2021$classe2,total=TRUE,sort="dec")
tab4

# Ventilation des atteintes selon leur type et l'année:

tab5 <- table(t.del1619$classe2,t.del1619$annee)
cprop(tab5)
# c'est très stable, ouf!!

tab6 <- table(t.del2021$classe2,t.del2021$annee)
cprop(tab6)
# pas de déformation de la structure pendant le Covid...

# Création de variables catégorielles classant les atteintes selon la répartition des 3 lieux (vict/inf/mec)
# au sein des différents zonages:

# Objectif: on souhaite étudier la répartition territoriale des différents types d'atteinte, au travers
# de la localisation de l'infraction, de celle du lieu d'habitation de la victime et de celle du lieu
# d'habitation du mis en cause. On souhaiterait pour les différents zonages, repérer les atteintes
# pour lesquelles les 3 lieux (inf/vict/mec) correspondent au même zonage, celles pour lesquelles
# seuls 2 lieux (inf/vict ou inf/mec ou vict/mec) correspondent au même zonage, et enfin celles 
# pour lesquelles les 3 lieux

# Avertissement: comme cette opération de création de variables catégorielles est très consommatrice de mémoire vive
# en utilisannt un mutate et une when_case du package dplyr, on va passer le tible t.del1619 en format
# "data.table" et utiliser la fonction fcase():

# transformation du tible t.del1619 en data.table:

del1619_dt <- as.data.table(t.del1619)

# 1) Codage de la variable catégorielle "communes_atteinte":
# Cette variable catégorielle classe chaque atteinte suivant que les 3 lieux (inf/vict/mec) s'inscrivent
# dans la même commune ou pas (en distinguant les cas où seuls 2 des 3 lieux s'inscrivent dans la même
# commune).

del1619_dt[ , communes_atteinte := data.table::fcase(
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
  (cog_com_22_inf == cog_com_22_vict) & (cog_com_22_vict == cog_com_22_mec), "a",
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) & 
  (cog_com_22_inf == cog_com_22_vict), "b",
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
  (cog_com_22_vict == cog_com_22_mec),"c",
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) & 
  (cog_com_22_inf == cog_com_22_mec),"d",
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE),"e",
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==TRUE) &
  !(cog_com_22_inf == cog_com_22_vict),"f",  
  default ="g")
]


# On a volontairement opté ici pour des libellés courts sous forme de lettre, pour faciliter l'affichage
# des statistiques descriptives. Voici la signification des libellés:
#a: les 3 lieux renseignés (inf/vict/mec) se situent dans la même commune,
#b: seuls 2 des 3 lieux renseignés (inf/vict) se situent dans la même commune,
#c: seuls 2 des 3 lieux renseignés (vict/mec) se situent dans la même commune,
#d: seuls 2 des 3 lieux renseignés (inf/mec) se situent dans la même commune,
#e: les 3 lieux renseignés (inf/vict/mec) se situent dans des communes distinctes"
#f: les 2 lieux renseignés (inf/vict) se situent dans des communes distinctes et le lieu du mec n'est pas renseigné
#g: autres cas.

# 1er tableau de fréquences selon le type d'atteinte:
tab_communes <- table(del1619_dt$classe2, del1619_dt$communes_atteinte)
ptab_communes <- lprop(tab_communes) # pourcentages en lignes (=types d'atteinte)
ptab_communes


# 2) Codage de la variable catégorielle "ZE_atteinte":
# Cette variable catégorielle classe chaque atteinte suivant que les 3 lieux (inf/vict/mec) s'inscrivent
# dans la même zone d'emploi (ZE) ou pas (en distinguant les cas où seuls 2 des 3 lieux s'inscrivent dans
# la même ZE).

del1619_dt[ , ZE_atteinte := data.table::fcase(
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
    (ZE2020_inf == ZE2020_vict) & (ZE2020_vict == ZE2020_mec), "a",
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
  (ZE2020_inf == ZE2020_vict), "b",
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
  (ZE2020_vict == ZE2020_mec),"c",
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
  (ZE2020_inf == ZE2020_mec),"d",
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE),"e",
  (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==TRUE) &
    !(ZE2020_inf == ZE2020_vict),"f",  
  default ="g")
]
# On a volontairement opté ici pour des libellés courts sous forme de lettre, pour faciliter l'affichage
# des statistiques descriptives. Voici la signification des libellés:
#a: les 3 lieux renseignés (inf/vict/mec) se situent dans la même ZE,
#b: seuls 2 des 3 lieux renseignés (inf/vict) se situent dans la même ZE,
#c: seuls 2 des 3 lieux renseignés (vict/mec) se situent dans la même ZE,
#d: seuls 2 des 3 lieux renseignés (inf/mec) se situent dans la même ZE,
#e: les 3 lieux renseignés (inf/vict/mec) se situent dans des ZE distinctes"
#f: les 2 lieux renseignés (inf/vict) se situent dans des ZE distinctes et le lieu du mec n'est pas renseigné
#g: autres cas.

# 1er tableau de fréquences selon le type d'atteinte:
tab_ZE <- table(del1619_dt$classe2, del1619_dt$ZE_atteinte)
ptab_ZE <- lprop(tab_ZE) # pourcentages en lignes (=types d'atteinte)
ptab_ZE

# 3) Codage de la variable catégorielle "UU_atteinte":
# Cette variable catégorielle classe chaque atteinte suivant que les 3 lieux (inf/vict/mec) s'inscrivent
# dans la même unité urbaine (UU) ou pas (en distinguant les cas où seuls 2 des 3 lieux s'inscrivent dans
# la même UU).

del1619_dt[ , UU_atteinte := data.table::fcase(
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
    (UU2020_inf == UU2020_vict) & (UU2020_vict == UU2020_mec), "a",
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
  (UU2020_inf == UU2020_vict), "b",
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
  (UU2020_vict == UU2020_mec),"c",
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
  (UU2020_inf == UU2020_mec),"d",
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE),"e",
  (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==TRUE) &
    !(UU2020_inf == UU2020_vict),"f",  
  default ="g")
]
# On a volontairement opté ici pour des libellés courts sous forme de lettre, pour faciliter l'affichage
# des statistiques descriptives. Voici la signification des libellés:
#a: les 3 lieux renseignés (inf/vict/mec) se situent dans la même UU,
#b: seuls 2 des 3 lieux renseignés (inf/vict) se situent dans la même UU,
#c: seuls 2 des 3 lieux renseignés (vict/mec) se situent dans la même UU,
#d: seuls 2 des 3 lieux renseignés (inf/mec) se situent dans la même UU,
#e: les 3 lieux renseignés (inf/vict/mec) se situent dans des UU distinctes"
#f: les 2 lieux renseignés (inf/vict) se situent dans des UU distinctes et le lieu du mec n'est pas renseigné
#g: autres cas.

# 1er tableau de fréquences selon le type d'atteinte:
tab_UU <- table(del1619_dt$classe2, del1619_dt$UU_atteinte)
ptab_UU <- lprop(tab_UU) # pourcentages en lignes (=types d'atteinte)
ptab_UU


# 4) Codage de la variable catégorielle "AAV_atteinte":
# Cette variable catégorielle classe chaque atteinte suivant que les 3 lieux (inf/vict/mec) s'inscrivent
# dans la même aire d'attraction des villes (AAV) ou pas (en distinguant les cas où seuls 2 des 3 lieux s'inscrivent dans
# la même AAV).

del1619_dt[ , AAV_atteinte := data.table::fcase(
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
    (AAV2020_inf == AAV2020_vict) & (AAV2020_vict == AAV2020_mec), "a",
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
  (AAV2020_inf == AAV2020_vict), "b",
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
  (AAV2020_vict == AAV2020_mec),"c",
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
  (AAV2020_inf == AAV2020_mec),"d",
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE),"e",
  (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==TRUE) &
    !(AAV2020_inf == AAV2020_vict),"f",  
  default ="g")
]
# On a volontairement opté ici pour des libellés courts sous forme de lettre, pour faciliter l'affichage
# des statistiques descriptives. Voici la signification des libellés:
#a: les 3 lieux renseignés (inf/vict/mec) se situent dans la même AAV,
#b: seuls 2 des 3 lieux renseignés (inf/vict) se situent dans la même AAV,
#c: seuls 2 des 3 lieux renseignés (vict/mec) se situent dans la même AAV,
#d: seuls 2 des 3 lieux renseignés (inf/mec) se situent dans la même AAV,
#e: les 3 lieux renseignés (inf/vict/mec) se situent dans des AAV distinctes"
#f: les 2 lieux renseignés (inf/vict) se situent dans des AAV distinctes et le lieu du mec n'est pas renseigné
#g: autres cas.

# 1er tableau de fréquences selon le type d'atteinte:
tab_AAV <- table(del1619_dt$classe2, del1619_dt$AAV_atteinte)
ptab_AAV <- lprop(tab_AAV) # pourcentages en lignes (=types d'atteinte)
ptab_AAV

# 5) Grille de densité: TODO?

# Export Excel des tableaux pour chaque type de zonage (au format .xlsx):

# a) Mise au format des tableaux pour l'export:

# Transformation des tableaux en data.frame:
ptab_communes <- as.data.frame(ptab_communes)
ptab_ZE <- as.data.frame(ptab_ZE)
ptab_UU <- as.data.frame(ptab_UU)
ptab_AAV <- as.data.frame(ptab_AAV)

# Mise en format "wide":
ptab_communes<- ptab_communes %>% pivot_wider(names_from =Var2,values_from = Freq )
ptab_ZE<- ptab_ZE %>% pivot_wider(names_from =Var2,values_from = Freq )
ptab_UU<- ptab_UU %>% pivot_wider(names_from =Var2,values_from = Freq )
ptab_AAV<- ptab_AAV %>% pivot_wider(names_from =Var2,values_from = Freq )

# On renomme certaines variables:
ptab_communes = rename(ptab_communes, "Type_atteintes" = "Var1")
ptab_ZE = rename(ptab_ZE, "Type_atteintes" = "Var1")
ptab_UU = rename(ptab_UU, "Type_atteintes" = "Var1")
ptab_AAV = rename(ptab_AAV, "Type_atteintes" = "Var1")

# Préparation d'une section "lisez-moi":
modalites_lieux_atteintes=c("a","b","c","d","e","f","g")
label_atteintes=c("les 3 lieux renseignés (inf/vict/mec) se situent dans le même zonage",
                  "seuls 2 des 3 lieux renseignés (inf/vict) se situent dans le même zonage",
                  "seuls 2 des 3 lieux renseignés (vict/mec) se situent dans le même zonage",
                  "seuls 2 des 3 lieux renseignés (inf/mec) se situent dans le même zonage",
                  "les 3 lieux renseignés (inf/vict/mec) se situent dans des zonages distincts",
                  "les 2 lieux renseignés (inf/vict) se situent dans des zonages distincts et le lieu du mec n'est pas renseigné",
                  "autres cas")

lisez_moi = data.frame(modalites_lieux_atteintes, label_atteintes)

# Nombre d'atteintes dans la sous-base 2016-2019 selon le type:
nb_atteintes_1619 <- table(t.del1619$classe2)
nb_atteintes_1619

nb_atteintes_1619 <- as.data.frame(nb_atteintes_1619)

# b) Export au format .xlsx:
dataset_names <- list('lisez_moi' = lisez_moi,'nombre_atteintes' = nb_atteintes_1619,'communes' = ptab_communes, 'ZE' = ptab_ZE,'UU' = ptab_ZE,'AAV' = ptab_ZE)
write.xlsx(dataset_names, file = '/Users/sklenard/Documents/Statapp//lieux_atteintes2016_19_zonages.xlsx')












