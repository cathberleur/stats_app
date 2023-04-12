# Construction de la base d'étude:

# Mon Working Directory (WD): à préciser par chaque utilisateur du programme (il s'agit du dossier créé sur votre ordi
# en local où se trouvent l'ensemble des fichiers R.data, .xlsx, .csv etc à charger et ce sera aussi l'endroit où seront
# stockées les différentes sorties):

setwd("/Users/sklenard/Documents/Statapp/WD_Gabriel") # à modifier par chaque utilisateur.

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

# Enregistrez au préalable dans votre WD le fichier donnees.secretisees.delinquance.RData
# mis à disposition par le SSMSI.

load("donnees.secretisees.delinquance.RData") # Fichier t.del (tibble)
# Etant donné la volumétrie, on le transforme en un fichier data.table:
del2016_2021_dt <- as.data.table(t.del)
# suppression du fichier "tibble" (on ne travaillera a priori qu'en data.table):
rm(t.del)
names(del2016_2021_dt)

# b) Fichier "Table d'appartenance géographique des communes" (source: Insee):
# Ce fichier relie chaque commune à ses différents zonages statistiques et administratifs.
# Nous avons retenu la version du fichier la plus récente (mise en ligne en 2023).

# Vous pouvez télécharger le fichier depuis https://www.insee.fr/fr/information/2028028
# et enregistrez le fichier dézippé dans wotre WD ;)

# Pour le fichier sur les bassins de vie, vous pouvez télécharger le fichier sous
# https://www.insee.fr/fr/information/6676988 et ensuite enregistrer le fichier dézippé
# dans votre WD ;)

# - Les communes:
t.communes_zonages <- readxl::read_excel(path = "table-appartenance-geo-communes-23.xlsx", sheet = "COM",skip=5) # création d'un tibble.
# - Les arrondissements municipaux (Paris, Lyon et Marseille):
t.arm_zonages <- readxl::read_excel(path = "table-appartenance-geo-communes-23.xlsx", sheet = "ARM",skip=5) # création d'un tibble.
t.arm_zonages <- t.arm_zonages[,1:17] # on exclut la variable "COM"
# on concatène les deux tibbles:
t.communes_zonages <-rbind(t.communes_zonages,t.arm_zonages)
# on transforme le tibble en data.table:
communes_zonages_dt <- as.data.table(t.communes_zonages)
# Ajout d'une variable caractérisant les différents bassins de vie (BV):
t.type_BV2022<- readxl::read_excel(path = "BV2022_au_01-01-2022.xlsx", sheet = "BV2022",skip=5) # création d'un tibble.
type_BV2022_dt <- as.data.table(t.type_BV2022)
type_BV2022_dt <-type_BV2022_dt[ , .(BV2022, TYPE)]
communes_zonages_dt <- 
  merge(x = communes_zonages_dt,
        y = type_BV2022_dt,
        by.x = "BV2022",
        by.y = "BV2022",
        all.x = TRUE)

# On renomme les variables selon les 3 lieux associés à l'atteinte:
communes_zonages_dt_inf <- communes_zonages_dt
names(communes_zonages_dt_inf) <-paste0(names(communes_zonages_dt_inf),"_inf")

communes_zonages_dt_vict <- communes_zonages_dt
names(communes_zonages_dt_vict) <-paste0(names(communes_zonages_dt_vict),"_vict")

communes_zonages_dt_mec <- communes_zonages_dt
names(communes_zonages_dt_mec) <-paste0(names(communes_zonages_dt_mec),"_mec")


# c) Fichier sur la grille de densité en 4 modalités (mix de la grille de densité d'Eurostat et des AAV de l'Insee):
# Enregistrez au préalable dans votre WD le fichier grille.densité.Rdata mis à disposition par Kevin sur Osmose
load("grille.densité.rdata") # Fichier t.del (tibble)
tab.dens <- tab.dens %>% rename(code_commune = `code commune`, GRD = gri.den)
# Etant donné la volumétrie, on le transforme en un fichier data.table:
communes_grille_densite_dt <- as.data.table(tab.dens)
names(communes_grille_densite_dt)


# On renomme les variables selon les 3 lieux associés à l'atteinte:
communes_grille_densite_dt_inf <- communes_grille_densite_dt
names(communes_grille_densite_dt_inf) <-paste0(names(communes_grille_densite_dt_inf),"_inf")

communes_grille_densite_dt_vict <- communes_grille_densite_dt
names(communes_grille_densite_dt_vict) <-paste0(names(communes_grille_densite_dt_vict),"_vict")

communes_grille_densite_dt_mec <- communes_grille_densite_dt
names(communes_grille_densite_dt_mec) <-paste0(names(communes_grille_densite_dt_mec),"_mec")

# d) Fichier sur le zonage de l'ANCT: les centralités
# Enregistrez au préalable dans votre WD le fichier Excel 202009_data_etudescentralites_inrae_anct.xlsx
# mis à disposition sous Osmose.

t.communes_centralites <- readxl::read_excel(path = "202009_data_etudescentralites_inrae_anct.xlsx", sheet = "Table") # création d'un tibble.
communes_centralites_dt <- as.data.table(t.communes_centralites)
communes_centralites_dt <-communes_centralites_dt[ , .(DC,P_NP5CLA,Tag_Centralite,SCORE,R_SCORE,
                                                       R_tvapop0616,R_tvaemp0616,R_MED16,
                                                       R_HC_REPORT_ARCX)]
names(communes_centralites_dt)
# ce zonage a été développé par l'ANCT (Inrae) dans le cadre d'une étude intitulée:
# Centralités : comment les identifier et quels rôles dans les dynamiques territoirales ?

communes_centralites_dt_inf <- communes_centralites_dt
names(communes_centralites_dt_inf) <-paste0(names(communes_centralites_dt_inf),"_inf")

communes_centralites_dt_vict <- communes_centralites_dt
names(communes_centralites_dt_vict) <-paste0(names(communes_centralites_dt_vict),"_vict")

communes_centralites_dt_mec <- communes_centralites_dt
names(communes_centralites_dt_mec) <-paste0(names(communes_centralites_dt_mec),"_mec")

# e) Fichier "Comparateur des communes" de l'Insee: https://www.insee.fr/fr/statistiques/2521169#consulter
# Pensez à télécharger le fichier base_cc_comparateur.xlsx et à l'enregistrer dans votre WD.

# Indicateurs socio-démographiques relatifs aux communes:

t.infos_communes <- readxl::read_excel(path = "base_cc_comparateur.xlsx", sheet = "COM",skip=5) # création d'un tibble.
# conversion de certaines variables caractères en numérique:
t.infos_communes$NBMENFISC20 <- as.numeric(t.infos_communes$NBMENFISC20)
t.infos_communes$PIMP20 <- as.numeric(t.infos_communes$PIMP20)
t.infos_communes$MED20 <- as.numeric(t.infos_communes$MED20)
t.infos_communes$TP6020 <- as.numeric(t.infos_communes$TP6020)

infos_communes_dt <- as.data.table(t.infos_communes)
names(infos_communes_dt)

# f) Base du dossier complet (https://www.insee.fr/fr/statistiques/5359146):
# Près de 1900 indicateurs au niveau communal!
# Info très riche sur le profil démographique et socio-économique de chaque commune
# Mais il faudra sélectionner les indicateurs les plus pertinents pour nous!


# TODO !

# 2) Construction de la base d'étude:

# Appariement du fichier du SSMSI (atteintes géolocalisées en France entre 2016 et 2021) 
# avec le fichier des zonages administratifs et statistiques (cf. supra b)).
# L'appariement s'effectue via 3 merge du type left-join en prenant successivement comme clé de jointure
#les 3 codes communes associés à chaque atteinte (commune du lieu de l'infraction, commune de la victime,
# commune du mis en cause):

del2016_2021_dt2 <- 
  merge(x = del2016_2021_dt,
        y = communes_zonages_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO_inf",
        all.x = TRUE)
rm(del2016_2021_dt)
del2016_2021_dt3 <- 
  merge(x = del2016_2021_dt2,
        y = communes_zonages_dt_vict,
        by.x = "cog_com_22_vict",
        by.y = "CODGEO_vict",
        all.x = TRUE)
rm(del2016_2021_dt2)
del2016_2021_dt4 <- 
  merge(x = del2016_2021_dt3,
        y = communes_zonages_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "CODGEO_mec",
        all.x = TRUE)
rm(del2016_2021_dt3)

# Sur le même modèle que ci-dessus, on apparie maintenant avec le fichier contenant
# la grille de densité (cf. supra c)):
del2016_2021_dt5 <- 
  merge(x = del2016_2021_dt4,
        y = communes_grille_densite_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "code_commune_inf",
        all.x = TRUE)
rm(del2016_2021_dt4)
del2016_2021_dt6 <- 
  merge(x = del2016_2021_dt5,
        y = communes_grille_densite_dt_vict,
        by.x = "cog_com_22_vict",
        by.y = "code_commune_vict",
        all.x = TRUE)
rm(del2016_2021_dt5)
del2016_2021_dt7 <- 
  merge(x = del2016_2021_dt6,
        y = communes_grille_densite_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "code_commune_mec",
        all.x = TRUE)
rm(del2016_2021_dt6)

# Sur le même modèle que ci-dessus, on apparie maintenant avec le fichier contenant
# le zonage de l'INRAE sur les centralités (cf. supra d)):
del2016_2021_dt8 <- 
  merge(x = del2016_2021_dt7,
        y = communes_centralites_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "DC_inf",
        all.x = TRUE)
rm(del2016_2021_dt7)
del2016_2021_dt9 <- 
  merge(x = del2016_2021_dt8,
        y = communes_centralites_dt_vict,
        by.x = "cog_com_22_vict",
        by.y = "DC_vict",
        all.x = TRUE)
rm(del2016_2021_dt8)
del2016_2021_dt10 <- 
  merge(x = del2016_2021_dt9,
        y = communes_centralites_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "DC_mec",
        all.x = TRUE)
rm(del2016_2021_dt9)

# TODO: enrichir la base avec des indicateurs démographiques, morphologiques, socio-économiques sur les
# communes et les différents zonages.


# Output n°1: une base des atteintes entre 2016 et 2021 géolocalisées selon 3 dimensions:
# le lieu de l'infraction, le lieu de domiciliation de la victime, le lieu de domiciliation du mis en cause.
# Pour chacun de ces 3 lieux, la base donne les différents zonages statistiques et administratifs associés.

del2016_2021_dt10 <- del2016_2021_dt10[order(cog_com_22_inf, annee)]
head(del2016_2021_dt10)

# Ajout/calcul de variables utiles pour l'étude:

# I) Recodage de la variable "classe" donnant le type d'atteinte: 
# WARNING: ce recodage ne sera plus nécessaire lorsque le code tournera sur les "vraies" données au SSMSI !!!

del2016_2021_dt10[ , classe2 := data.table::fcase(
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

# II) Création de variables catégorielles classant les atteintes selon que les 3 lieux (vict/inf/mec)
# s'inscrivent dans un même zonage ou pas:

# 1) Zonages administratifs:

# a) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même commune ou pas:

del2016_2021_dt10[ , a_3_memes_communes := data.table::fcase(
  (cog_com_22_inf == cog_com_22_vict) & (cog_com_22_vict == cog_com_22_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même département ou pas:

del2016_2021_dt10[ , a_3_memes_dep := data.table::fcase(
  (DEP_inf == DEP_vict) & (DEP_vict == DEP_mec), "oui",
  default ="non")
]

# c) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même région ou pas:

del2016_2021_dt10[ , a_3_memes_reg := data.table::fcase(
  (REG_inf == REG_vict) & (REG_vict == REG_mec), "oui",
  default ="non")
]

# d) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même EPCI (Intercommunalité/Métropole)
# ou pas:

del2016_2021_dt10[ , a_3_memes_epci := data.table::fcase(
  (EPCI_inf == EPCI_vict) & (EPCI_vict == EPCI_mec), "oui",
  default ="non")
]

# e) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même CV (Canton/Ville)
# ou pas:

del2016_2021_dt10[ , a_3_memes_cv := data.table::fcase(
  (CV_inf == CV_vict) & (CV_vict == CV_mec), "oui",
  default ="non")
]



# 2) Zonages statistiques:

# a) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même zone d'emploi (ZE) ou pas:

del2016_2021_dt10[ , a_3_memes_ZE := data.table::fcase(
  (ZE2020_inf == ZE2020_vict) & (ZE2020_vict == ZE2020_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même bassin de vie (BV) ou pas:

del2016_2021_dt10[ , a_3_memes_BV := data.table::fcase(
  (BV2022_inf == BV2022_vict) & (BV2022_vict == BV2022_mec), "oui",
  default ="non")
]

# c) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même unité urbaine (UU) ou pas:

del2016_2021_dt10[ , a_3_memes_UU := data.table::fcase(
  !(UU2020_inf =="01000") & (UU2020_inf == UU2020_vict) & (UU2020_vict == UU2020_mec), "oui",
  default ="non")
]
# note: pour éviter de classer en "oui" des atteintes dont les 3 lieux seraient tous les classés en 01000
# (i.e hors UU), on rajoute la condition qui assure que l'infraction ait bien eu lieu dans une UU. 
# En effet, le zonage UU ne constitue pas une partition du territoire français.

# d) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même aire d'attraction des villes (AAV)
# ou pas:

del2016_2021_dt10[ , a_3_memes_AAV := data.table::fcase(
  !(AAV2020_inf =="000") & (AAV2020_inf == AAV2020_vict) & (AAV2020_vict == AAV2020_mec), "oui",
  default ="non")
]
# note: pour éviter de classer en "oui" des atteintes dont les 3 lieux seraient tous les classés en 000
# (i.e hors AAV), on rajoute la condition qui assure que l'infraction ait bien eu lieu dans une AAV. 
# En effet, le zonage AAV ne constitue pas une partition du territoire français.


# e) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même grille de densité 
# (mix de la grille communale de densité et de AAV) ou pas:

del2016_2021_dt10[ , a_3_memes_GD := data.table::fcase(
  (TYPE_inf == TYPE_vict) & (TYPE_vict == TYPE_mec), "oui",
  default ="non")
]

# f) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même niveau de centralité 
# de la commune (cf. étude de l'INRIA sur les centralités) ou pas:

del2016_2021_dt10[ , a_3_memes_CENTR := data.table::fcase(
  (P_NP5CLA_inf == P_NP5CLA_vict) & (P_NP5CLA_vict == P_NP5CLA_mec), "oui",
  default ="non")
]

# 3) Autres variables utiles pour l'analyse:

# on crée un compteur:
del2016_2021_dt10$compteur <-1


###################################################################################################

# Output n°2: agrégation de la base initiale au niveau communal:

# Justification: le niveau communal apparaît comme le niveau de maille géographique le plus fin pour lequel on dispose le
# plus d'infos: 
# a) infos sur l'organisation spatiale de la délinquance (SSMSI);
# b) infos sur les caractéristiques physiques, morphologiques du territoire
# c) infos socio-démo-économiques
# d) infos sur le positionnement de la commune au sein des différents zonages statistiques

# idée: construire un tableau rassemblant des variables synthétisant ces différentes infos au niveau de chaque commune.
# Puis effectuer une analyse statistique de ce tableau, afin de caractériser, typologiser les différentes formes d'organisation 
# de la délinquance sur le territoire français.

# 0) On commence par restreindre le champ d'analyse aux seules atteintes pour lesquelles les 3 lieux
# (inf, vict, mec) sont renseignés (car c'est l'objet central de l'étude):

del2016_2021_dt_etude <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)]
# On passe de 6,5 millions à 1,3 millions d'atteintes!

# 1) On calcule, au niveau communal et pour chaque type d'atteinte, la proportion d'atteintes survenues dans la commune dont le triplet de
# lieux (inf, vict, mec) s'inscrivent dans le même zonage (administratif/statistique):

del2016_2021_dt_com <- del2016_2021_dt_etude[ , .(
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
  Nb_atteintes_vols_violants_sansarme_1BV = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_1GD = sum(compteur*(a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1GD = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1GD = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1GD = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1GD = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1GD = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1GD = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1GD = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1GD = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_1UU = sum(compteur*(a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1UU = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1UU = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1UU = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1UU = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1UU = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1UU = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1UU = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1UU = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_1AAV = sum(compteur*(a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1AAV = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1AAV = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1AAV = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1AAV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1AAV = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1AAV = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1AAV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1AAV = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(compteur*(a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1CENTR = sum(compteur*(classe2 == "Cambriolages de logement" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1CENTR = sum(compteur*(classe2 == "Destructions et dégradations" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1CENTR = sum(compteur*(classe2 == "Vols avec armes" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1CENTR = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1CENTR = sum(compteur*(classe2 == "Vols dans les véhicules" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1CENTR = sum(compteur*(classe2 == "Vols de véhicules" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1CENTR = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & a_3_memes_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(compteur*(classe2 == "Vols violents sans arme" & a_3_memes_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]
head(del2016_2021_dt_com)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le triplet de lieux
# (inf,vict,mec) s'inscrit dans un même zonage z (z= ZE ou BV pour les zonages statistiques et z=COM ou DEP ou REG pour les
# zonages administratifs)

# a) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même zone d'emploi (ZE):
del2016_2021_dt_com$Prop_atteintes_1ZE <-del2016_2021_dt_com$Nb_atteintes_1ZE/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1ZE <-del2016_2021_dt_com$Nb_atteintes_cambr_1ZE/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1ZE <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1ZE/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1ZE <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1ZE/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1ZE <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1ZE/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1ZE <-del2016_2021_dt_com$Nb_atteintes_homic_1ZE/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1ZE <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1ZE/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1ZE <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1ZE/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

# b) Proportion d'atteintes dont le triplet de lieux s'inscrit dans un même bassin de vie (BV):
del2016_2021_dt_com$Prop_atteintes_1BV <-del2016_2021_dt_com$Nb_atteintes_1BV/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1BV <-del2016_2021_dt_com$Nb_atteintes_cambr_1BV/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1BV <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1BV/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1BV <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1BV/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1BV <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1BV/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1BV <-del2016_2021_dt_com$Nb_atteintes_homic_1BV/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1BV <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1BV/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1BV/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1BV/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1BV/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1BV/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1BV/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1BV <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1BV/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

# c) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même grille de densité (GD):
del2016_2021_dt_com$Prop_atteintes_1GD <-del2016_2021_dt_com$Nb_atteintes_1GD/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1GD <-del2016_2021_dt_com$Nb_atteintes_cambr_1GD/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1GD <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1GD/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1GD <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1GD/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1GD <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1GD/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1GD <-del2016_2021_dt_com$Nb_atteintes_homic_1GD/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1GD <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1GD/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1GD/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1GD/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1GD/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1GD/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1GD/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1GD <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1GD/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

# d) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même unité urbaine (UU):
del2016_2021_dt_com$Prop_atteintes_1UU <-del2016_2021_dt_com$Nb_atteintes_1UU/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1UU <-del2016_2021_dt_com$Nb_atteintes_cambr_1UU/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1UU <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1UU/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1UU <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1UU/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1UU <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1UU/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1UU <-del2016_2021_dt_com$Nb_atteintes_homic_1UU/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1UU <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1UU/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1UU/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1UU/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1UU/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1UU/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1UU/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1UU <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1UU/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

# e) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même aire d'attraction des villes (AAV):
del2016_2021_dt_com$Prop_atteintes_1AAV <-del2016_2021_dt_com$Nb_atteintes_1AAV/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1AAV <-del2016_2021_dt_com$Nb_atteintes_cambr_1AAV/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1AAV <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1AAV/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1AAV <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1AAV/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1AAV <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1AAV/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1AAV <-del2016_2021_dt_com$Nb_atteintes_homic_1AAV/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1AAV <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1AAV/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1AAV <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1AAV/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

# f) Proportion d'atteintes dont le triplet de lieux s'inscrit dans une même centralité (CENTR) au sens de l'INRAE:
del2016_2021_dt_com$Prop_atteintes_1CENTR <-del2016_2021_dt_com$Nb_atteintes_1CENTR/del2016_2021_dt_com$Nb_atteintes*100
del2016_2021_dt_com$Prop_atteintes_cambr_1CENTR <-del2016_2021_dt_com$Nb_atteintes_cambr_1CENTR/del2016_2021_dt_com$Nb_atteintes_cambr*100
del2016_2021_dt_com$Prop_atteintes_blessures_famil_1CENTR <-del2016_2021_dt_com$Nb_atteintes_blessures_famil_1CENTR/del2016_2021_dt_com$Nb_atteintes_blessures_famil*100
del2016_2021_dt_com$Prop_atteintes_blessures_horsfamil_1CENTR <-del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil_1CENTR/del2016_2021_dt_com$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_com$Prop_atteintes_destr_degrad_1CENTR <-del2016_2021_dt_com$Nb_atteintes_destr_degrad_1CENTR/del2016_2021_dt_com$Nb_atteintes_destr_degrad*100
del2016_2021_dt_com$Prop_atteintes_homic_1CENTR <-del2016_2021_dt_com$Nb_atteintes_homic_1CENTR/del2016_2021_dt_com$Nb_atteintes_homic*100
del2016_2021_dt_com$Prop_atteintes_viol_sex_1CENTR <-del2016_2021_dt_com$Nb_atteintes_viol_sex_1CENTR/del2016_2021_dt_com$Nb_atteintes_viol_sex*100
del2016_2021_dt_com$Prop_atteintes_vols_armes_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_armes_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_armes*100
del2016_2021_dt_com$Prop_atteintes_vols_acces_vehic_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_ds_vehic_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_de_vehic_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_de_vehic_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_com$Prop_atteintes_vols_sansviol_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_sansviol_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_com$Prop_atteintes_vols_violants_sansarme_1CENTR <-del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme_1CENTR/del2016_2021_dt_com$Nb_atteintes_vols_violants_sansarme*100

head(del2016_2021_dt_com)

# Ajout de variables socio-démo-économiques au niveau communal:

# Appariement avec le fichier contenant des infos socio-démo-économiques sur les communes:
del2016_2021_dt_com <- 
  merge(x = del2016_2021_dt_com,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# On joint au fichier communal ci-dessus les infos sur les différents zonages:
del2016_2021_dt_com <- 
  merge(x = del2016_2021_dt_com,
        y = communes_zonages_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO_inf",
        all.x = TRUE)
del2016_2021_dt_com <- 
  merge(x = del2016_2021_dt_com,
        y = communes_grille_densite_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "code_commune_inf",
        all.x = TRUE)
del2016_2021_dt_com <- 
  merge(x = del2016_2021_dt_com,
        y = communes_centralites_dt_inf,
        by.x = "cog_com_22_inf",
        by.y = "DC_inf",
        all.x = TRUE)

head(del2016_2021_dt_com)

# Variante: on pourrait suffixer toutes les variables du fichier infos_communes_dt par _inf ou _vict ou _mec
# et merger avec la base del2016_2021_dt_etude par code commune inf ,vict et mec, ce qui permettrait de mesurer pour chaque atteinte
# les écarts entre les valeurs prises par chaque indicateur socio/démo/éco dans les 3 communes inf/vict/mec..
# TODO!


