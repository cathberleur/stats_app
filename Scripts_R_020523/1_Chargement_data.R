
# Chargement des différentes sources de données utilisées dans ce mémoire:


# a) Fichier "flouté" des atteintes géolocalisées (source: SSMSI):
load("donnees.secretisees.delinquance.RData") # Fichier t.del (tibble)
# à Kevin et Aurélien: remplacer ce fichier par le fichier non secrétisé!!

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

# c) Fichier sur la grille de densité en 4 modalités (mix de la grille de densité d'Eurostat et des AAV de l'Insee):
# Enregistrez au préalable dans votre WD le fichier grille.densité.Rdata mis à disposition par le SSMSI sur Osmose
load("grille.densité.rdata") # Fichier t.del (tibble)
tab.dens <- tab.dens %>% rename(code_commune = `code commune`, GRD = gri.den)
communes_grille_densite_dt <- as.data.table(tab.dens)
names(communes_grille_densite_dt)

# d) Fichier sur le zonage de l'ANCT: les centralités
# ce zonage a été développé par l'ANCT (Inrae) dans le cadre d'une étude intitulée:
# Centralités : comment les identifier et quels rôles dans les dynamiques territoriales ?

# Enregistrez au préalable dans votre WD le fichier Excel 202009_data_etudescentralites_inrae_anct.xlsx
# mis à disposition sous Osmose.

t.communes_centralites <- readxl::read_excel(path = "202009_data_etudescentralites_inrae_anct.xlsx", sheet = "Table") # création d'un tibble.
communes_centralites_dt <- as.data.table(t.communes_centralites)
communes_centralites_dt <-communes_centralites_dt[ , .(DC,P_NP5CLA,Tag_Centralite,SCORE,R_SCORE,
                                                       R_tvapop0616,R_tvaemp0616,R_MED16,
                                                       R_HC_REPORT_ARCX)]
names(communes_centralites_dt)

# e) Fichier "Comparateur des communes" de l'Insee: https://www.insee.fr/fr/statistiques/2521169 #consulter
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

dossier_complet_var_insee <- read_delim("meta_dossier_complet.csv")
dossier_complet_insee <- read_delim("dossier_complet.csv") # création d'un tibble.
names(dossier_complet_insee)

# g) Fichier des distances à vol d'oiseau (en km) entre la commune de l'infraction, la commune du lieu de domiciliation de
# la victime et la commune du lieu de domiciliation du mis en cause.
# Ce fichier .Rdata a été généré par le SSMSI et mis à disposition sous Osmose.
# Téléchargez le fichier et sauvegardez-le dans votre WD.

load("donnees.secretisees.delinquance.distances.rdata") # (tibble).
# à Kevin et Aurélien: remplacer le fichier secrétisé par le fichier non secrétisé!!




