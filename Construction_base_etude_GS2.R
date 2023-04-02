
# Construction de la base d'étude:

# Mon Working Directory (WD): à préciser par chaque utilisateur du programme (il s'agit du dossier créé sur votre ordi
# en local où se trouvent l'ensemble des fichiers R.data, .xlsx, .csv etc à charger et ce sera aussi l'endroit où seront
# stockées les différentes sorties):

setwd("/Users/sklenard/Documents/Statapp/WD_Gabriel")

# 0) Chargement des librairies utiles:
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
library(sf)

# 1) Chargement des différentes fichiers:

# a) Fichier "flouté" des atteintes géolocalisées (source: SSMSI):

load("donnees.secretisees.delinquance.RData") # Fichier t.del (tibble)
# Etant donné la volumétrie, on le transforme en un fichier data.table:
del2016_2021_dt <- as.data.table(t.del)
# suppression du fichier "tibble" (on ne travaillera a priori qu'en data.table):
rm(t.del)
names(del2016_2021_dt)

# b) Fichier sur le zonage "zone d'emploi" (ZE 2020 au 1er janvier 2022):
t.zonage_ZE_2020 <- readxl::read_excel(path = "ZE2020_au_01-01-2022.xlsx", sheet = "Composition_communale",skip=5) # création d'un tibble.
zonage_ZE_2020_dt <- as.data.table(t.zonage_ZE_2020)
names(zonage_ZE_2020_dt)
# On renomme les variables selon les 3 lieux associés à l'atteinte:
zonage_ZE_2020_dt_inf <- zonage_ZE_2020_dt
names(zonage_ZE_2020_dt_inf) <-paste0(names(zonage_ZE_2020_dt_inf),"_inf")

zonage_ZE_2020_dt_vict <- zonage_ZE_2020_dt
names(zonage_ZE_2020_dt_vict) <-paste0(names(zonage_ZE_2020_dt_vict),"_vict")

zonage_ZE_2020_dt_mec <- zonage_ZE_2020_dt
names(zonage_ZE_2020_dt_mec) <-paste0(names(zonage_ZE_2020_dt_mec),"_mec")

# Contours géographiques des ZE 2020 (au 1er janvier 2022):
contours_ZE_2020 <- st_read("ze2020_2022")

# c) Fichier sur le zonage "bassin de vie" (BV 2022 au 1er janvier 2022):
t.zonage_BV_2022 <- readxl::read_excel(path = "BV2022_au_01-01-2022.xlsx", sheet = "Composition_communale",skip=5) # création d'un tibble.
zonage_BV_2022_dt <- as.data.table(t.zonage_BV_2022)
names(zonage_BV_2022_dt)
# On renomme les variables selon les 3 lieux associés à l'atteinte:
zonage_BV_2022_dt_inf <- zonage_BV_2022_dt
names(zonage_BV_2022_dt_inf) <-paste0(names(zonage_BV_2022_dt_inf),"_inf")

zonage_BV_2022_dt_vict <- zonage_BV_2022_dt
names(zonage_BV_2022_dt_vict) <-paste0(names(zonage_BV_2022_dt_vict),"_vict")

zonage_BV_2022_dt_mec <- zonage_BV_2022_dt
names(zonage_BV_2022_dt_mec) <-paste0(names(zonage_BV_2022_dt_mec),"_mec")

# Contours géographiques des BV 2022 (au 1er janvier 2022):
contours_BV_2022 <- st_read("bv2022_2022")
contours_com_BV_2022 <- st_read("com_bv2022_2022")

# Compléter avec les autres zonages d'étude...

# Ajout des zonages administratifs (département, région, EPCI, etc.):
# Dans cet exemple, il faut remplacer "mon_IDEP" par votre IDEP

regions <- read_delim("v_region_2023.csv", 
                      locale = locale(encoding ="UTF-8"),
                      col_types = cols(REG = col_character()))

names(regions)


# Charger les données socio-démographiques relatives aux communes (Insee):

t.infos_communes <- readxl::read_excel(path = "base_cc_comparateur.xlsx", sheet = "COM",skip=5) # création d'un tibble.
# conversion de certaines variables caractères en numérique:
t.infos_communes$NBMENFISC20 <- as.numeric(t.infos_communes$NBMENFISC20)
t.infos_communes$PIMP20 <- as.numeric(t.infos_communes$PIMP20)
t.infos_communes$MED20 <- as.numeric(t.infos_communes$MED20)
t.infos_communes$TP6020 <- as.numeric(t.infos_communes$TP6020)

infos_communes_dt <- as.data.table(t.infos_communes)
names(infos_communes_dt)

# 2) Construction de la base d'étude:

# Appariement avec les Zones d'emploi (ZE) selon les 3 codes communes associés à chaque atteinte:
del2016_2021_dt2 <- 
  merge(x = del2016_2021_dt,
        y = zonage_ZE_2020_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO_inf",
        all.x = TRUE)
rm(del2016_2021_dt)
del2016_2021_dt3 <- 
  merge(x = del2016_2021_dt2,
        y = zonage_ZE_2020_dt_vict,
        by.x = "cog_com_22_vict",
        by.y = "CODGEO_vict",
        all.x = TRUE)
rm(del2016_2021_dt2)
del2016_2021_dt4 <- 
  merge(x = del2016_2021_dt3,
        y = zonage_ZE_2020_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "CODGEO_mec",
        all.x = TRUE)
rm(del2016_2021_dt3)

# Appariement avec les Bassins de vie (BV) selon les 3 codes communes associés à chaque atteinte:
zonage_BV_2022_dt_inf <-zonage_BV_2022_dt_inf[ , .(CODGEO_inf, BV2022_inf, LIBBV2022_inf,TYPE_COM_inf)]
del2016_2021_dt5 <- 
  merge(x = del2016_2021_dt4,
        y = zonage_BV_2022_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO_inf",
        all.x = TRUE)
rm(del2016_2021_dt4)

zonage_BV_2022_dt_vict <-zonage_BV_2022_dt_vict[ , .(CODGEO_vict, BV2022_vict, LIBBV2022_vict,TYPE_COM_vict)]
del2016_2021_dt6 <- 
  merge(x = del2016_2021_dt5,
        y = zonage_BV_2022_dt_vict,
        by.x = "cog_com_22_vict",
        by.y = "CODGEO_vict",
        all.x = TRUE)
rm(del2016_2021_dt5)

zonage_BV_2022_dt_mec <-zonage_BV_2022_dt_mec[ , .(CODGEO_mec, BV2022_mec, LIBBV2022_mec,TYPE_COM_mec)]
del2016_2021_dt7 <- 
  merge(x = del2016_2021_dt6,
        y = zonage_BV_2022_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "CODGEO_mec",
        all.x = TRUE)
rm(del2016_2021_dt6)

head(del2016_2021_dt7)

del2016_2021_dt7 <- del2016_2021_dt7[order(cog_com_22_inf, annee)]

# Ajout/calcul de variables utiles pour l'étude:

# I) Recodage de la variable "classe" donnant le type d'atteinte:

del2016_2021_dt7[ , classe2 := data.table::fcase(
  classe=="W", "Vols sans violence contre des personnes",
  classe=="K", "Destructions et dégradations",
  classe=="R","Vols dans les véhicules",
  classe=="B","Cambriolages de logement",
  classe=="X","Coups et blessures volontaires en dehors de la sphère familiale",
  classe=="T","Vols de véhicules", 
  classe=="V","Coups et blessures volontaires dans la sphère familiale",
  classe=="S","Vols d'accessoires sur véhicules",
  classe=="G","Vols violents sans arme",
  classe=="A","Violences sexuelles",
  classe=="O","Vols avec armes",
  default ="Homicides")
]

# II) Création de variables catégorielles classant les atteintes selon la répartition des 3 lieux (vict/inf/mec)
# au sein des différents zonages:

# 1) Zonages administratifs:

# a) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même commune ou pas:

del2016_2021_dt7[ , a_3_memes_communes := data.table::fcase(
  (cog_com_22_inf == cog_com_22_vict) & (cog_com_22_vict == cog_com_22_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même département ou pas:

del2016_2021_dt7[ , a_3_memes_dep := data.table::fcase(
  (DEP_inf == DEP_vict) & (DEP_vict == DEP_mec), "oui",
  default ="non")
]

# c) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même région ou pas:

del2016_2021_dt7[ , a_3_memes_reg := data.table::fcase(
  (REG_inf == REG_vict) & (REG_vict == REG_mec), "oui",
  default ="non")
]

# Tester les autres zonages administratifs: EPCI, cantons, etc.

# 2) Zonages statistiques:

# a) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même zone d'emploi (ZE) ou pas:

del2016_2021_dt7[ , a_3_memes_ZE := data.table::fcase(
  (ZE2020_inf == ZE2020_vict) & (ZE2020_vict == ZE2020_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même bassin de vie (BV) ou pas:

del2016_2021_dt7[ , a_3_memes_BV := data.table::fcase(
  (BV2022_inf == BV2022_vict) & (BV2022_vict == BV2022_mec), "oui",
  default ="non")
]

# Tester les autres zonages statistiques: UU, AAV, GCD, centralités...

# Construction d'une base d'étude au niveau communal:
# Justification: le niveau communal apparaît comme le niveau de maille géographique le plus fin pour lequel on dispose le
# plus d'infos: 
# a) infos sur l'organisation spatiale de la délinquance (SSMSI);
# b) infos sur les caractéristiques physiques, morphologiques du territoire
# c) infos socio-démo-économiques
# d) infos sur le positionnement de la commune au sein des différents zonages statistiques

# idée: construire un tableau rassemblant des variables synthétisant ces différentes infos au niveau de chaque commune.
# Puis effectuer une analyse statistique de ce tableau, afin de caractériser, typologiser les différentes formes d'organisation 
# de la délinquance sur le territoire français.

# 0) On commence par restreindre le champ d'analyse aux seules atteintes pour lesquelles les 3 lieux (inf, vict, mec) sont renseignés:

sous_base_analyse1 <- del2016_2021_dt7[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)]

# 1) On calcule, au niveau communal et pour chaque type d'atteinte, la proportion d'atteintes survenues dans la commune dont le triplet de
# lieux (inf, vict, mec) s'inscrivent dans le même zonage (administratif/statistique):

# on crée un compteur:
sous_base_analyse1$compteur <-1

test1 <- sous_base_analyse1[ , .(
  Nb_atteintes = sum(compteur, na.rm = TRUE),
  Nb_atteintes_cambr = sum(compteur*(classe2 == "Cambriolages de logement"), na.rm = TRUE),
  Nb_atteintes_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_destr_degrad = sum(compteur*(classe2 == "Destructions et dégradations"), na.rm = TRUE),
  Nb_atteintes_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_atteintes_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_atteintes_vols_armes = sum(compteur*(classe2 == "Vols avec armes"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic = sum(compteur*(classe2 == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic = sum(compteur*(classe2 == "Vols de véhicules"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol = sum(compteur*(classe2 == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme = sum(compteur*(classe2 == "Vols violents sans arme"), na.rm = TRUE),
  Nb_atteintes_1ZE = sum(compteur*(a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1ZE = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1ZE = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1ZE = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1ZE = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1ZE = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1ZE = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1ZE = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1ZE = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_1BV = sum(compteur*(a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1BV = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_BV == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]
head(test1)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le triplet de lieux
# (inf,vict,mec) s'inscrit dans un même zonage z (z= ZE ou BV pour les zonages statistiques et z=COM ou DEP ou REG pour les
# zonages administratifs)

# a) Proportion d'atteintes dont le triplet de lieu s'inscrit dans une même ZE:
test1$Prop_atteintes_1ZE <-test1$Nb_atteintes_1ZE/test1$Nb_atteintes*100
test1$Prop_atteintes_cambr_1ZE <-test1$Nb_atteintes_cambr_1ZE/test1$Nb_atteintes_cambr*100
test1$Prop_atteintes_blessures_famil_1ZE <-test1$Nb_atteintes_blessures_famil_1ZE/test1$Nb_atteintes_blessures_famil*100
test1$Prop_atteintes_blessures_horsfamil_1ZE <-test1$Nb_atteintes_blessures_horsfamil_1ZE/test1$Nb_atteintes_blessures_horsfamil*100
test1$Prop_atteintes_destr_degrad_1ZE <-test1$Nb_atteintes_destr_degrad_1ZE/test1$Nb_atteintes_destr_degrad*100
test1$Prop_atteintes_homic_1ZE <-test1$Nb_atteintes_homic_1ZE/test1$Nb_atteintes_homic*100
test1$Prop_atteintes_viol_sex_1ZE <-test1$Nb_atteintes_viol_sex_1ZE/test1$Nb_atteintes_viol_sex*100
test1$Prop_atteintes_vols_armes_1ZE <-test1$Nb_atteintes_vols_armes_1ZE/test1$Nb_atteintes_vols_armes*100
test1$Prop_atteintes_vols_acces_vehic_1ZE <-test1$Nb_atteintes_vols_acces_vehic_1ZE/test1$Nb_atteintes_vols_acces_vehic*100
test1$Prop_atteintes_vols_ds_vehic_1ZE <-test1$Nb_atteintes_vols_ds_vehic_1ZE/test1$Nb_atteintes_vols_ds_vehic*100
test1$Prop_atteintes_vols_de_vehic_1ZE <-test1$Nb_atteintes_vols_de_vehic_1ZE/test1$Nb_atteintes_vols_de_vehic*100
test1$Prop_atteintes_vols_sansviol_1ZE <-test1$Nb_atteintes_vols_sansviol_1ZE/test1$Nb_atteintes_vols_sansviol*100
test1$Prop_atteintes_vols_violants_sansarme_1ZE <-test1$Nb_atteintes_vols_violants_sansarme_1ZE/test1$Nb_atteintes_vols_violants_sansarme*100

# b) Proportion d'atteintes dont le triplet de lieu s'inscrit dans un même BV:
test1$Prop_atteintes_1BV <-test1$Nb_atteintes_1BV/test1$Nb_atteintes*100
test1$Prop_atteintes_cambr_1BV <-test1$Nb_atteintes_cambr_1BV/test1$Nb_atteintes_cambr*100
test1$Prop_atteintes_blessures_famil_1BV <-test1$Nb_atteintes_blessures_famil_1BV/test1$Nb_atteintes_blessures_famil*100
test1$Prop_atteintes_blessures_horsfamil_1BV <-test1$Nb_atteintes_blessures_horsfamil_1BV/test1$Nb_atteintes_blessures_horsfamil*100
test1$Prop_atteintes_destr_degrad_1BV <-test1$Nb_atteintes_destr_degrad_1BV/test1$Nb_atteintes_destr_degrad*100
test1$Prop_atteintes_homic_1BV <-test1$Nb_atteintes_homic_1BV/test1$Nb_atteintes_homic*100
test1$Prop_atteintes_viol_sex_1BV <-test1$Nb_atteintes_viol_sex_1BV/test1$Nb_atteintes_viol_sex*100
test1$Prop_atteintes_vols_armes_1BV <-test1$Nb_atteintes_vols_armes_1BV/test1$Nb_atteintes_vols_armes*100
test1$Prop_atteintes_vols_acces_vehic_1BV <-test1$Nb_atteintes_vols_acces_vehic_1BV/test1$Nb_atteintes_vols_acces_vehic*100
test1$Prop_atteintes_vols_ds_vehic_1BV <-test1$Nb_atteintes_vols_ds_vehic_1BV/test1$Nb_atteintes_vols_ds_vehic*100
test1$Prop_atteintes_vols_de_vehic_1BV <-test1$Nb_atteintes_vols_de_vehic_1BV/test1$Nb_atteintes_vols_de_vehic*100
test1$Prop_atteintes_vols_sansviol_1BV <-test1$Nb_atteintes_vols_sansviol_1BV/test1$Nb_atteintes_vols_sansviol*100
test1$Prop_atteintes_vols_violants_sansarme_1BV <-test1$Nb_atteintes_vols_violants_sansarme_1BV/test1$Nb_atteintes_vols_violants_sansarme*100

head(test1)

# Ajout de variables socio-démo-économiques au niveau communal:

# Appariement avec le fichier contenant des infos socio-démo-économiques sur les communes:
test2 <- 
  merge(x = test1,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)
head(test2)





