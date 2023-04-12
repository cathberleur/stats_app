
# Partie 2 du rapport Statapp: les fais stylisés (statistiques descriptives)

library(readr)
library(corrplot)


# WARNING: on travaille dans cette partie descriptive à partir de la base d'étude de la délinquance
# construite au niveau communal "del2016_2021_dt_com" générée par le programme "Construction_base_etude_GS3.R":

#source("Construction_base_etude_GS3.R")
names(del2016_2021_dt_com)

# I) Organisation spatiale de la délinquance par région

# On agrège la base d'étude communale par région (on se base sur la région où a été commise l'infraction):
del2016_2021_dt_com$compteur <-1

del2016_2021_dt_reg <- del2016_2021_dt_com[ , .(
  Nb_atteintes = sum(Nb_atteintes, na.rm = TRUE),
  Nb_atteintes_cambr = sum(Nb_atteintes_cambr, na.rm = TRUE),
  Nb_atteintes_blessures_famil = sum(Nb_atteintes_blessures_famil, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil = sum(Nb_atteintes_blessures_horsfamil, na.rm = TRUE),
  Nb_atteintes_destr_degrad = sum(Nb_atteintes_destr_degrad, na.rm = TRUE),
  Nb_atteintes_homic = sum(Nb_atteintes_homic, na.rm = TRUE),
  Nb_atteintes_viol_sex = sum(Nb_atteintes_viol_sex, na.rm = TRUE),
  Nb_atteintes_vols_armes = sum(Nb_atteintes_vols_armes, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic = sum(Nb_atteintes_vols_acces_vehic, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic = sum(Nb_atteintes_vols_ds_vehic, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic = sum(Nb_atteintes_vols_de_vehic, na.rm = TRUE),
  Nb_atteintes_vols_sansviol = sum(Nb_atteintes_vols_sansviol, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme = sum(Nb_atteintes_vols_violants_sansarme, na.rm = TRUE),
  Nb_atteintes_1ZE = sum(Nb_atteintes_1ZE, na.rm = TRUE),
  Nb_atteintes_cambr_1ZE = sum(Nb_atteintes_cambr_1ZE, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(Nb_atteintes_blessures_famil_1ZE, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(Nb_atteintes_blessures_horsfamil_1ZE, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1ZE = sum(Nb_atteintes_destr_degrad_1ZE, na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(Nb_atteintes_homic_1ZE, na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(Nb_atteintes_viol_sex_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_armes_1ZE = sum(Nb_atteintes_vols_armes_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1ZE = sum(Nb_atteintes_vols_acces_vehic_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1ZE = sum(Nb_atteintes_vols_ds_vehic_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1ZE = sum(Nb_atteintes_vols_de_vehic_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1ZE = sum(Nb_atteintes_vols_sansviol_1ZE, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1ZE = sum(Nb_atteintes_vols_violants_sansarme_1ZE, na.rm = TRUE),
  Nb_atteintes_1BV = sum(Nb_atteintes_1BV, na.rm = TRUE),
  Nb_atteintes_cambr_1BV = sum(Nb_atteintes_cambr_1BV, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(Nb_atteintes_blessures_famil_1BV, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(Nb_atteintes_blessures_horsfamil_1BV, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(Nb_atteintes_destr_degrad_1BV, na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(Nb_atteintes_homic_1BV, na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(Nb_atteintes_viol_sex_1BV, na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(Nb_atteintes_vols_armes_1BV, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(Nb_atteintes_vols_acces_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(Nb_atteintes_vols_ds_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(Nb_atteintes_vols_de_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(Nb_atteintes_vols_sansviol_1BV, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(Nb_atteintes_vols_violants_sansarme_1BV, na.rm = TRUE),
  Nb_atteintes_1GD = sum(Nb_atteintes_1GD, na.rm = TRUE),
  Nb_atteintes_cambr_1GD = sum(Nb_atteintes_cambr_1GD, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(Nb_atteintes_blessures_famil_1GD, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(Nb_atteintes_blessures_horsfamil_1GD, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1GD = sum(Nb_atteintes_destr_degrad_1GD, na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(Nb_atteintes_homic_1GD, na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(Nb_atteintes_viol_sex_1GD, na.rm = TRUE),
  Nb_atteintes_vols_armes_1GD = sum(Nb_atteintes_vols_armes_1GD, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1GD = sum(Nb_atteintes_vols_acces_vehic_1GD, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1GD = sum(Nb_atteintes_vols_ds_vehic_1GD, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1GD = sum(Nb_atteintes_vols_de_vehic_1GD, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1GD = sum(Nb_atteintes_vols_sansviol_1GD, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1GD = sum(Nb_atteintes_vols_violants_sansarme_1GD, na.rm = TRUE),
  Nb_atteintes_1UU = sum(Nb_atteintes_1UU, na.rm = TRUE),
  Nb_atteintes_cambr_1UU = sum(Nb_atteintes_cambr_1UU, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(Nb_atteintes_blessures_famil_1UU, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(Nb_atteintes_blessures_horsfamil_1UU, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1UU = sum(Nb_atteintes_destr_degrad_1UU, na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(Nb_atteintes_homic_1UU, na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(Nb_atteintes_viol_sex_1UU, na.rm = TRUE),
  Nb_atteintes_vols_armes_1UU = sum(Nb_atteintes_vols_armes_1UU, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1UU = sum(Nb_atteintes_vols_acces_vehic_1UU, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1UU = sum(Nb_atteintes_vols_ds_vehic_1UU, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1UU = sum(Nb_atteintes_vols_de_vehic_1UU, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1UU = sum(Nb_atteintes_vols_sansviol_1UU, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1UU = sum(Nb_atteintes_vols_violants_sansarme_1UU, na.rm = TRUE),
  Nb_atteintes_1AAV = sum(Nb_atteintes_1AAV, na.rm = TRUE),
  Nb_atteintes_cambr_1AAV = sum(Nb_atteintes_cambr_1AAV, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(Nb_atteintes_blessures_famil_1AAV, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(Nb_atteintes_blessures_horsfamil_1AAV, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1AAV = sum(Nb_atteintes_destr_degrad_1AAV, na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(Nb_atteintes_homic_1AAV, na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(Nb_atteintes_viol_sex_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_armes_1AAV = sum(Nb_atteintes_vols_armes_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1AAV = sum(Nb_atteintes_vols_acces_vehic_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1AAV = sum(Nb_atteintes_vols_ds_vehic_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1AAV = sum(Nb_atteintes_vols_de_vehic_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1AAV = sum(Nb_atteintes_vols_sansviol_1AAV, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1AAV = sum(Nb_atteintes_vols_violants_sansarme_1AAV, na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(Nb_atteintes_1CENTR, na.rm = TRUE),
  Nb_atteintes_cambr_1CENTR = sum(Nb_atteintes_cambr_1CENTR, na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(Nb_atteintes_blessures_famil_1CENTR, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(Nb_atteintes_blessures_horsfamil_1CENTR, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1CENTR = sum(Nb_atteintes_destr_degrad_1CENTR, na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(Nb_atteintes_homic_1CENTR, na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(Nb_atteintes_viol_sex_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_armes_1CENTR = sum(Nb_atteintes_vols_armes_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1CENTR = sum(Nb_atteintes_vols_acces_vehic_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1CENTR = sum(Nb_atteintes_vols_ds_vehic_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1CENTR = sum(Nb_atteintes_vols_de_vehic_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1CENTR = sum(Nb_atteintes_vols_sansviol_1CENTR, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(Nb_atteintes_vols_violants_sansarme_1CENTR, na.rm = TRUE),
  P19_POP=sum(P19_POP,na.rm = TRUE)),
  by = .(REG_inf)]
head(del2016_2021_dt_reg)


# On récupère le fichier des libellés des régions
regions <- read_delim("v_region_2023.csv",show_col_types = FALSE)
regions_dt <- as.data.table(regions)
regions_dt <- regions_dt[ , .(REG,LIBELLE)]
names(regions_dt)

# On fait la jointure avec ce fichier:
del2016_2021_dt_reg <- 
  merge(x = del2016_2021_dt_reg,
        y = regions_dt,
        by.x = "REG_inf",
        by.y = "REG",
        all.x = TRUE)
head(del2016_2021_dt_reg)

# On récupère les indicateurs de dispersion du revenu par UC de Filosofi (Insee) par région en 2020:
t.infos_disp_rev_reg<- readxl::read_excel(path = "FILO2020_DISP_REG.xlsx", sheet = "ENSEMBLE",skip=5) # création d'un tibble.
infos_disp_rev_reg_dt <- as.data.table(t.infos_disp_rev_reg)
# On fait la jointure avec ce fichier:
del2016_2021_dt_reg <- 
  merge(x = del2016_2021_dt_reg,
        y = infos_disp_rev_reg_dt,
        by.x = "REG_inf",
        by.y = "CODGEO",
        all.x = TRUE)
head(del2016_2021_dt_reg)

# On calcule pour chaque commune et chaque type d'atteinte: la proportion (en %) d'atteintes dont le triplet de lieux
# (inf,vict,mec) s'inscrit dans un même zonage z (z= ZE ou BV pour les zonages statistiques et z=COM ou DEP ou REG pour les
# zonages administratifs)

# a) Proportion d'atteintes d'un type donné pour 1000 habitants (population 2019):
del2016_2021_dt_reg$Prop_atteintes_pop <-del2016_2021_dt_reg$Nb_atteintes/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_cambr_pop <-del2016_2021_dt_reg$Nb_atteintes_cambr/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_pop <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_pop <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_pop <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_homic_pop <-del2016_2021_dt_reg$Nb_atteintes_homic/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_viol_sex_pop <-del2016_2021_dt_reg$Nb_atteintes_viol_sex/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_armes_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_armes/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol/del2016_2021_dt_reg$P19_POP*1000
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_pop <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme/del2016_2021_dt_reg$P19_POP*1000

# b) Part des atteintes d'un type donné dans l'ensemble des atteintes (en %):
del2016_2021_dt_reg$Part_atteintes_cambr <-del2016_2021_dt_reg$Nb_atteintes_cambr/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_blessures_famil <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_blessures_horsfamil <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_destr_degrad <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_homic <-del2016_2021_dt_reg$Nb_atteintes_homic/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_viol_sex <-del2016_2021_dt_reg$Nb_atteintes_viol_sex/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_armes <-del2016_2021_dt_reg$Nb_atteintes_vols_armes/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_acces_vehic <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_ds_vehic <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_de_vehic <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_sansviol <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Part_atteintes_vols_violants_sansarme <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme/del2016_2021_dt_reg$Nb_atteintes*100

# c) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans une même ZE (en %):
del2016_2021_dt_reg$Prop_atteintes_1ZE <-del2016_2021_dt_reg$Nb_atteintes_1ZE/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1ZE <-del2016_2021_dt_reg$Nb_atteintes_cambr_1ZE/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1ZE <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1ZE/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1ZE <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1ZE/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1ZE <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1ZE/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1ZE <-del2016_2021_dt_reg$Nb_atteintes_homic_1ZE/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1ZE <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1ZE/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1ZE <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1ZE/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

# d) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans un même BV (en %):
del2016_2021_dt_reg$Prop_atteintes_1BV <-del2016_2021_dt_reg$Nb_atteintes_1BV/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1BV <-del2016_2021_dt_reg$Nb_atteintes_cambr_1BV/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1BV <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1BV/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1BV <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1BV/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1BV <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1BV/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1BV <-del2016_2021_dt_reg$Nb_atteintes_homic_1BV/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1BV <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1BV/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1BV <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1BV/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

# e) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans une même GD (en %):
del2016_2021_dt_reg$Prop_atteintes_1GD <-del2016_2021_dt_reg$Nb_atteintes_1GD/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1GD <-del2016_2021_dt_reg$Nb_atteintes_cambr_1GD/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1GD <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1GD/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1GD <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1GD/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1GD <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1GD/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1GD <-del2016_2021_dt_reg$Nb_atteintes_homic_1GD/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1GD <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1GD/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1GD <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1GD/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

# f) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans une même UU (en %):
del2016_2021_dt_reg$Prop_atteintes_1UU <-del2016_2021_dt_reg$Nb_atteintes_1UU/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1UU <-del2016_2021_dt_reg$Nb_atteintes_cambr_1UU/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1UU <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1UU/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1UU <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1UU/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1UU <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1UU/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1UU <-del2016_2021_dt_reg$Nb_atteintes_homic_1UU/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1UU <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1UU/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1UU <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1UU/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

# g) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans une même AAV (en %):
del2016_2021_dt_reg$Prop_atteintes_1AAV <-del2016_2021_dt_reg$Nb_atteintes_1AAV/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1AAV <-del2016_2021_dt_reg$Nb_atteintes_cambr_1AAV/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1AAV <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1AAV/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1AAV <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1AAV/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1AAV <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1AAV/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1AAV <-del2016_2021_dt_reg$Nb_atteintes_homic_1AAV/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1AAV <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1AAV/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1AAV <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1AAV/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

# h) Proportion d'atteintes d'un type donné dont le triplet de lieux s'inscrit dans une même CENTR (en %):
del2016_2021_dt_reg$Prop_atteintes_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_1CENTR/del2016_2021_dt_reg$Nb_atteintes*100
del2016_2021_dt_reg$Prop_atteintes_cambr_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_cambr_1CENTR/del2016_2021_dt_reg$Nb_atteintes_cambr*100
del2016_2021_dt_reg$Prop_atteintes_blessures_famil_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_blessures_famil_1CENTR/del2016_2021_dt_reg$Nb_atteintes_blessures_famil*100
del2016_2021_dt_reg$Prop_atteintes_blessures_horsfamil_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil_1CENTR/del2016_2021_dt_reg$Nb_atteintes_blessures_horsfamil*100
del2016_2021_dt_reg$Prop_atteintes_destr_degrad_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_destr_degrad_1CENTR/del2016_2021_dt_reg$Nb_atteintes_destr_degrad*100
del2016_2021_dt_reg$Prop_atteintes_homic_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_homic_1CENTR/del2016_2021_dt_reg$Nb_atteintes_homic*100
del2016_2021_dt_reg$Prop_atteintes_viol_sex_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_viol_sex_1CENTR/del2016_2021_dt_reg$Nb_atteintes_viol_sex*100
del2016_2021_dt_reg$Prop_atteintes_vols_armes_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_armes_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_armes*100
del2016_2021_dt_reg$Prop_atteintes_vols_acces_vehic_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_acces_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_ds_vehic_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_ds_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_de_vehic_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_de_vehic*100
del2016_2021_dt_reg$Prop_atteintes_vols_sansviol_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_sansviol_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_sansviol*100
del2016_2021_dt_reg$Prop_atteintes_vols_violants_sansarme_1CENTR <-del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme_1CENTR/del2016_2021_dt_reg$Nb_atteintes_vols_violants_sansarme*100

head(del2016_2021_dt_reg)

# Transformation en tibble (pour travailler avec tidyverse lors de l'analyse des données):

del2016_2021_tb_reg <- as_tibble(del2016_2021_dt_reg)

# Premier filtrage: 
# on exclut les régions hors France Métropole ainsi les régions non renseignées:

del2016_2021_tb_reg <- del2016_2021_tb_reg %>% filter(!is.na(REG_inf) & !(REG_inf %in% c("01","02","03","04","06"))) %>% relocate(LIBELLE,P19_POP, .after = REG_inf)

# A) Tableaux de statistiques pour l'analyse de la délinquance par région:

# 1) Proportion d'atteintes pour 1000 habitants, par région et type de délinquance:

tab_del_1000hbt_reg <- del2016_2021_tb_reg %>% select(LIBELLE,P19_POP,ends_with("pop"))

# 2) Structure de la délinquance par type pour chaque région:

tab_structure_del_reg <- del2016_2021_tb_reg %>% select(LIBELLE,P19_POP,starts_with("Part"))

# 3) Proportion d'atteintes (pour un type donné) dont le triplet de lieux (inf,vict,mec) 
# s'inscrit dans une même zone d'emploi (ZE):

tab_prop_del_1ZE_reg <- del2016_2021_tb_reg %>% select(LIBELLE,P19_POP,ends_with("1ZE") & starts_with("Prop"))

# 4) Proportion d'atteintes (pour un type donné) dont le triplet de lieux (inf,vict,mec) 
# s'inscrit dans un même bassin de vie (BV):

tab_prop_del_1BV_reg <- del2016_2021_tb_reg %>% select(LIBELLE,P19_POP,ends_with("1BV") & starts_with("Prop"))

# 5) Poursuivre avec les autres types de zonage (UU,AAV,GD,CENTR) -> TODO!

#B) Description spatiale: représentation cartographique des tableaux de A)

library(rgdal)
library(sf)
library(terra)
contours_reg_fr<- readOGR(dsn = "contours_regions", layer = "regions_2015_metropole_region", verbose =FALSE)
# ça ne marche pas!

#C) Matrices de corrélation:

# 1) Proportion d'atteintes (pour un type donné) dont le triplet de lieux (inf,vict,mec) 
# s'inscrit dans une même zone d'emploi (ZE):

tab1 <- del2016_2021_tb_reg %>% select(ends_with("1ZE") & starts_with("Prop"))
mcor1 <- cor(tab1)
corrplot(mcor1, type="upper", order="hclust", tl.col="black", tl.srt=45)

# 2) Proportion d'atteintes (pour un type donné) dont le triplet de lieux (inf,vict,mec) 
# s'inscrit dans un même bassin de vie (BV):

tab2 <- del2016_2021_tb_reg %>% select(ends_with("1BV") & starts_with("Prop"))
mcor2 <- cor(tab2)
corrplot(mcor2, type="upper", order="hclust", tl.col="black", tl.srt=45)

# TODO: tester les autres zonages...

#D) Indices de corrélation spatiale:

# TODO!

# II) Organisation spatiale de la délinquance par département

# on réplique les mêmes types d'analyses faites au niveau régional TODO

# III) Organisation spatiale de la délinquance par commune

# on réplique les mêmes types d'analyses faites au niveau régional TODO



