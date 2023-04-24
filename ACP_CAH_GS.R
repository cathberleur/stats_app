

# Partie 3 du Rapport Statapp: analyse factorielle et classification des atteintes dans la dimension spatiale

# ACP-CAH

# Analyse factorielle:

library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)
library(corrplot)

# I) ACP au niveau communal

# A) ACP sur l'ensemble des types d'atteintes (les atteintes doivent bien ici être associées à un couple de communes
# (I,V) renseigné):

# 1) Etape préliminaire: préparation du tableau de données en vue de l'ACP 

# a) Agrégation des atteintes par communes (lieu de commission de l'infraction):

# Note: On part du data.table des atteintes géolocalisées "del2016_2021_dt10" généré par 
# le script "Construction_base_etude.R":

# Warning: on se restreint ici aux seules atteintes associées à un couple de communes (I,V) renseigné!

del2016_2021_IV_meme_zonage <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
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
  Nb_atteintes_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1ZE = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1ZE = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1ZE = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1ZE = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1ZE = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1ZE = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1ZE = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1ZE = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1BV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1GD = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1GD = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1GD = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1GD = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1GD = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1GD = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1GD = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1GD = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1UU = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1UU = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1UU = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1UU = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1UU = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1UU = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1UU = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1UU = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1AAV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1AAV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1AAV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1AAV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1AAV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1AAV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1AAV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1AAV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1CENTR = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1CENTR = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1CENTR = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1CENTR = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1CENTR = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1CENTR = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1CENTR = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# on apparie ce fichier communal d'atteintes avec le fichier contenant des infos socio-éco et démo sur les communes
# (comparateur des communes de l'Insee):

del2016_2021_IV_meme_zonage <- 
  merge(x = del2016_2021_IV_meme_zonage,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# on apparie ce fichier avec le fichier Insee contenant les infos sur les différents zonages administratifs et d'études
# associés à chaque commune

del2016_2021_IV_meme_zonage <- 
  merge(x = del2016_2021_IV_meme_zonage,
        y = communes_zonages_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# on transforme le tableau en tibble:
del2016_2021_IV_meme_zonage <- as_tibble(del2016_2021_IV_meme_zonage)

names(del2016_2021_IV_meme_zonage)

# b) Retraitement sur les variables: on procède à des regroupements de types d'atteintes "proches" pour simplifier un 
# peu l'analyse

# Pour des raisons de lisibilité, nous travaillons ici non pas sur les 12 types atteintes du fichier du SSMSI, mais sur
# 7 types d'atteintes, après réagrégation de certains types d'atteintes "proches". Dans le détail, nous avons 
# procédé aux regroupements d'atteintes suivants:

# 1er regroupement: "vols d'accessoires dans les véhicules", "vols de véhicules" et "vols dans les véhicules"
# -> vols en rapport avec les véhicules
# 2eme regroupement:
# "coups et blessures volontaires dans la sphère familiale","coups et blessures volontaires en dehors de la sphère familiae" et "homocides".
# -> atteintes physiques corporelles graves et directes
# 3eme regroupement: "vols violents sans arme" et les "vols violents avec arme"
# -> vols violents

del2016_2021_IV_meme_zonage <- del2016_2021_IV_meme_zonage %>%
  mutate(Nb_atteintes_vols_vehic = Nb_atteintes_vols_acces_vehic + Nb_atteintes_vols_ds_vehic + 
           Nb_atteintes_vols_de_vehic,
         Nb_atteintes_bless_famil_homic = Nb_atteintes_blessures_famil + Nb_atteintes_blessures_horsfamil +
           Nb_atteintes_homic,
         Nb_atteintes_vols_violents = Nb_atteintes_vols_armes + Nb_atteintes_vols_violants_sansarme,
         Nb_atteintes_vols_vehic_1ZE = Nb_atteintes_vols_acces_vehic_1ZE + Nb_atteintes_vols_ds_vehic_1ZE + 
           Nb_atteintes_vols_de_vehic_1ZE,
         Nb_atteintes_bless_famil_homic_1ZE = Nb_atteintes_blessures_famil_1ZE + Nb_atteintes_blessures_horsfamil_1ZE +
           Nb_atteintes_homic_1ZE,
         Nb_atteintes_vols_violents_1ZE = Nb_atteintes_vols_armes_1ZE + Nb_atteintes_vols_violants_sansarme_1ZE,
         Nb_atteintes_vols_vehic_1BV = Nb_atteintes_vols_acces_vehic_1BV + Nb_atteintes_vols_ds_vehic_1BV + 
           Nb_atteintes_vols_de_vehic_1BV,
         Nb_atteintes_bless_famil_homic_1BV = Nb_atteintes_blessures_famil_1BV + Nb_atteintes_blessures_horsfamil_1BV +
           Nb_atteintes_homic_1BV,
         Nb_atteintes_vols_violents_1BV = Nb_atteintes_vols_armes_1BV + Nb_atteintes_vols_violants_sansarme_1BV,
         Nb_atteintes_vols_vehic_1GD = Nb_atteintes_vols_acces_vehic_1GD + Nb_atteintes_vols_ds_vehic_1GD + 
           Nb_atteintes_vols_de_vehic_1GD,
         Nb_atteintes_bless_famil_homic_1GD = Nb_atteintes_blessures_famil_1GD + Nb_atteintes_blessures_horsfamil_1GD +
           Nb_atteintes_homic_1GD,
         Nb_atteintes_vols_violents_1GD = Nb_atteintes_vols_armes_1GD + Nb_atteintes_vols_violants_sansarme_1GD,
         Nb_atteintes_vols_vehic_1UU = Nb_atteintes_vols_acces_vehic_1UU + Nb_atteintes_vols_ds_vehic_1UU + 
           Nb_atteintes_vols_de_vehic_1UU,
         Nb_atteintes_bless_famil_homic_1UU = Nb_atteintes_blessures_famil_1UU + Nb_atteintes_blessures_horsfamil_1UU +
           Nb_atteintes_homic_1UU,
         Nb_atteintes_vols_violents_1UU = Nb_atteintes_vols_armes_1UU + Nb_atteintes_vols_violants_sansarme_1UU,
         Nb_atteintes_vols_vehic_1AAV = Nb_atteintes_vols_acces_vehic_1AAV + Nb_atteintes_vols_ds_vehic_1AAV + 
           Nb_atteintes_vols_de_vehic_1AAV,
         Nb_atteintes_bless_famil_homic_1AAV = Nb_atteintes_blessures_famil_1AAV + Nb_atteintes_blessures_horsfamil_1AAV +
           Nb_atteintes_homic_1AAV,
         Nb_atteintes_vols_violents_1AAV = Nb_atteintes_vols_armes_1AAV + Nb_atteintes_vols_violants_sansarme_1AAV,
         Nb_atteintes_vols_vehic_1CENTR = Nb_atteintes_vols_acces_vehic_1CENTR + Nb_atteintes_vols_ds_vehic_1CENTR + 
           Nb_atteintes_vols_de_vehic_1CENTR,
         Nb_atteintes_bless_famil_homic_1CENTR = Nb_atteintes_blessures_famil_1CENTR + Nb_atteintes_blessures_horsfamil_1CENTR +
           Nb_atteintes_homic_1CENTR,
         Nb_atteintes_vols_violents_1CENTR = Nb_atteintes_vols_armes_1CENTR + Nb_atteintes_vols_violants_sansarme_1CENTR)
  
# c) Calcul des ratios communaux sur le volume de la délinquance (pour 1000 habitants) et sa structure (en %) par type d'atteinte:

del2016_2021_IV_meme_zonage <- del2016_2021_IV_meme_zonage %>%
  mutate(
    # ratios sur la structure de la délinquance par type (en %):
    P_vols_vehic = Nb_atteintes_vols_vehic/Nb_atteintes*100,
    P_bless_famil_homic = Nb_atteintes_bless_famil_homic/Nb_atteintes*100,
    P_vols_violents = Nb_atteintes_vols_violents/Nb_atteintes*100,
    P_cambr = Nb_atteintes_cambr/Nb_atteintes*100,
    P_destr_degrad = Nb_atteintes_destr_degrad/Nb_atteintes*100,
    P_viol_sex = Nb_atteintes_viol_sex/Nb_atteintes*100,
    P_vols_sansviol = Nb_atteintes_vols_sansviol/Nb_atteintes*100,
    # volume de la délinquance par type (pour 1000 habitants):
    vols_vehic = Nb_atteintes_vols_vehic/P19_POP*1000,
    bless_famil_homic = Nb_atteintes_bless_famil_homic/P19_POP*1000,
    vols_violents = Nb_atteintes_vols_violents/P19_POP*1000,
    cambr = Nb_atteintes_cambr/P19_POP*1000,
    destr_degrad = Nb_atteintes_destr_degrad/P19_POP*1000,
    viol_sex = Nb_atteintes_viol_sex/P19_POP*1000,
    vols_sansviol = Nb_atteintes_vols_sansviol/P19_POP*1000,
    # part des atteintes associées à un couple de communes (I,V) présent dans un même zonage d'étude (en % de chaque type d'atteinte)
    # Zone d'emploi (ZE):
    P_atteintes_1ZE = Nb_atteintes_1ZE/Nb_atteintes*100,
    P_vols_vehic_1ZE = Nb_atteintes_vols_vehic_1ZE/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1ZE = Nb_atteintes_bless_famil_homic_1ZE/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1ZE = Nb_atteintes_vols_violents_1ZE/Nb_atteintes_vols_violents*100,
    P_cambr_1ZE = Nb_atteintes_cambr_1ZE/Nb_atteintes_cambr*100,
    P_destr_degrad_1ZE = Nb_atteintes_destr_degrad_1ZE/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1ZE = Nb_atteintes_viol_sex_1ZE/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1ZE = Nb_atteintes_vols_sansviol_1ZE/Nb_atteintes_vols_sansviol*100,
    # Bassin de vie (BV):
    P_atteintes_1BV = Nb_atteintes_1BV/Nb_atteintes*100,
    P_vols_vehic_1BV = Nb_atteintes_vols_vehic_1BV/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1BV = Nb_atteintes_bless_famil_homic_1BV/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1BV = Nb_atteintes_vols_violents_1BV/Nb_atteintes_vols_violents*100,
    P_cambr_1BV = Nb_atteintes_cambr_1BV/Nb_atteintes_cambr*100,
    P_destr_degrad_1BV = Nb_atteintes_destr_degrad_1BV/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1BV = Nb_atteintes_viol_sex_1BV/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1BV = Nb_atteintes_vols_sansviol_1BV/Nb_atteintes_vols_sansviol*100,
    # Grille de densité (GD):
    P_atteintes_1GD = Nb_atteintes_1GD/Nb_atteintes*100,
    P_vols_vehic_1GD = Nb_atteintes_vols_vehic_1GD/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1GD = Nb_atteintes_bless_famil_homic_1GD/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1GD = Nb_atteintes_vols_violents_1GD/Nb_atteintes_vols_violents*100,
    P_cambr_1GD = Nb_atteintes_cambr_1GD/Nb_atteintes_cambr*100,
    P_destr_degrad_1GD = Nb_atteintes_destr_degrad_1GD/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1GD = Nb_atteintes_viol_sex_1GD/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1GD = Nb_atteintes_vols_sansviol_1GD/Nb_atteintes_vols_sansviol*100,
    # Unité urbaine (UU):
    P_atteintes_1UU = Nb_atteintes_1UU/Nb_atteintes*100,
    P_vols_vehic_1UU = Nb_atteintes_vols_vehic_1UU/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1UU = Nb_atteintes_bless_famil_homic_1UU/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1UU = Nb_atteintes_vols_violents_1UU/Nb_atteintes_vols_violents*100,
    P_cambr_1UU = Nb_atteintes_cambr_1UU/Nb_atteintes_cambr*100,
    P_destr_degrad_1UU = Nb_atteintes_destr_degrad_1UU/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1UU = Nb_atteintes_viol_sex_1UU/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1UU = Nb_atteintes_vols_sansviol_1UU/Nb_atteintes_vols_sansviol*100,
    # Aire d'attraction des villes (AAV):
    P_atteintes_1AAV = Nb_atteintes_1AAV/Nb_atteintes*100,
    P_vols_vehic_1AAV = Nb_atteintes_vols_vehic_1AAV/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1AAV = Nb_atteintes_bless_famil_homic_1AAV/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1AAV = Nb_atteintes_vols_violents_1AAV/Nb_atteintes_vols_violents*100,
    P_cambr_1AAV = Nb_atteintes_cambr_1AAV/Nb_atteintes_cambr*100,
    P_destr_degrad_1AAV = Nb_atteintes_destr_degrad_1AAV/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1AAV = Nb_atteintes_viol_sex_1AAV/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1AAV = Nb_atteintes_vols_sansviol_1AAV/Nb_atteintes_vols_sansviol*100,
    # Centralités (CENTR):
    P_atteintes_1CENTR = Nb_atteintes_1CENTR/Nb_atteintes*100,
    P_vols_vehic_1CENTR = Nb_atteintes_vols_vehic_1CENTR/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1CENTR = Nb_atteintes_bless_famil_homic_1CENTR/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1CENTR = Nb_atteintes_vols_violents_1CENTR/Nb_atteintes_vols_violents*100,
    P_cambr_1CENTR = Nb_atteintes_cambr_1CENTR/Nb_atteintes_cambr*100,
    P_destr_degrad_1CENTR = Nb_atteintes_destr_degrad_1CENTR/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1CENTR = Nb_atteintes_viol_sex_1CENTR/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1CENTR = Nb_atteintes_vols_sansviol_1CENTR/Nb_atteintes_vols_sansviol*100
  )
    
names(del2016_2021_IV_meme_zonage)

# ACP n° 1:

# Individus: 8 804 communes
# Champ retenu:
# 1- communes de la France métropolitaine.
# 2- communes de plus de 1000 habitants.
# 3- communes ayant au moins une atteinte associée à un couple de communes (I,V) renseigné.
# 4- communes ayant au moins une atteinte dans chacun des 7 types d'atteintes

# Variables actives: 7 + 7
# 1- ratios sur la structure de la délinquance par type (en %): 7 variables
# 2- volume de la délinquance par type (pour 1000 habitants): 7 variables

# Variables supplémentaires: 6
# a) variables quantitatives:
# -part des atteintes associées à un couple de communes (I,V) présent dans un même zonage d'étude
# remarque: on ne distingue pas ici les différents types d'atteinte.
# - MED20 (revenu médian de la commune en 2020).
# on pourrait en rajouter d'autres, notamment sur la dimension socio-éco-démo des communes (TODO!)
# ce serait pas mal de projeter une variable géographique du type (latitude, longitude) des communes
# -> ça permettrait de relier le profil des communes en termes de délinquance et leur position géographique
# sur le territoire français.
# b) variables qualitatives:
# TODO!
# variables géographiques (du type REGION ?)...


# L'ACP est normée (toutes les variables ont été standardisées, étant donné qu'elles ne sont pas exprimées
# dans la même unité).

data_acp1 <- del2016_2021_IV_meme_zonage %>% filter(!(DEP.x %in% c("971","972","973","974","976")) &
             Nb_atteintes_cambr>0 &
             Nb_atteintes_bless_famil_homic>0 &
             Nb_atteintes_destr_degrad>0 &
             Nb_atteintes_viol_sex>0 &
             Nb_atteintes_vols_vehic>0 &
             Nb_atteintes_vols_violents>0 &
             Nb_atteintes_vols_sansviol>0 & 
             P19_POP>1000) %>%
         select(cog_com_22_inf,LIBGEO.x,DEP.x,REG.x,MED20,
         P_atteintes_1ZE,P_atteintes_1BV,P_atteintes_1GD,P_atteintes_1UU,P_atteintes_1AAV,P_atteintes_1CENTR,
         P_vols_vehic,P_bless_famil_homic,P_vols_violents,P_cambr,P_destr_degrad,
         P_viol_sex,P_vols_sansviol,
         vols_vehic,bless_famil_homic,vols_violents,
         cambr,destr_degrad,
         viol_sex,vols_sansviol
         ) %>%
         column_to_rownames(var="cog_com_22_inf")

# Statistiques descriptives univariées sur les variables en entrée de l'ACP:
summary(data_acp1)

# Distribution des ratios de structure par type d'atteinte
boxplot(data_acp1[11:17])
# ça semble assez gaussien ;)

# Distribution des volumes d'atteintes pour 1000 hbts, par type d'atteinte
boxplot(data_acp1[18:24])
# Beaucoup d'outliers pour les volumes d'atteintes pour 1000 habitants.
# Distribution très asymétrique à droite ! Quelques communes affichent des gros chiffres par 
# rapport au reste des communes !!

# Matrice de corrélation entre toutes les variables quantitatives:
mcor <- cor(data_acp1[4:24])
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# Principales remarques:
# - les variables mesurant le volume de chaque type d'atteinte (pour 1000 hbts) sont très corrélées postivement entre elles.
# -> effet taille: des communes sont fortement impactées par la délinquance (et ce pour tous les types d'atteintes), et
# à l'inverse, les communes peu impactées par la délinquance globalement le sont souvent pour tous les types d'atteintes.

# - en revanche, les ratios de structure de la délinquance par type semblent peu corrélés entre eux et pas corrélé aux
# variables mesurant le volume de la délinquance pour 1000 hbts.

# - enfin, on constate que les variables mesurant la part d'atteintes avec (I,V) ds un même zonage sont peu corrélées avec 
# variables actives de l'ACP, ce qui n'est pas très bon signe ici, étant donné que l'on aimerait justement montrer
# que certains profils de communes en termes de délinquance (volume/structure) sont davantage "adaptés" à certains 
# zonages d'études plutôt que d'autres.

# On réalise l'ACP:

# Dans un premier temps, on paramètre l'ACP à partir du tableau de bord "Shiny":
data_acp1_shiny <- data_acp1[,4:24]
resACP1 = PCAshiny(data_acp1_shiny)

# Code R à retenir pour réaliser les différentes étapes de cette ACP (en vue du rapport):

# à insérer ici...

# Interprétation des résultats de l'ACP n°1:

# Nombre d'axes factoriels à retenir pour l'interprétation: 
# 3 axes factoriels peuvent être retenus ici, ils expliques à eux trois XX% de l'inertie totale.

# 1er axe factoriel: volume de la délinquance à l'échelle communale pour 1000 habitants

# 2eme axe factoriel: structure de la délinquance à l'échelle communale (opposition entre les communes
# avec une prééminence des atteintes corporelles et celles avec une prééminence des vols de véhicules/cambriolages)

# 3eme axe factoriel: encore la structure de la délinquance (opposition entre violences sexuelles et destructions/dégradations)

# Variantes de l'ACP n°1: on peut envisager des variations autour des variables actives retenues dans l'ACP
# Variante 1: on ne retient que les ratios de structure par type d'atteinte comme variables actives
data_acp1bis_shiny <- data_acp1[,11:17]
resACP1bis = PCAshiny(data_acp1bis_shiny)
# les résultats sont très mauvais! 

# Variante 2: on ne retient que les volumes d'atteintes pour 1000hbts par type comme variables actives
data_acp1ter_shiny <- data_acp1[,18:24]
resACP1ter = PCAshiny(data_acp1ter_shiny)
# les résultats sont très mauvais!

# Variante 3: on ne retient que les part d'atteintes associées à un couple de communes (I,V) se situant dans un même zonage d'étude
data_acp1quater_shiny <- data_acp1[,5:10]
resACP1quater = PCAshiny(data_acp1quater_shiny)
# les premiers résultats méritent que l'on approfondisse un peu cette piste (car l'avantage ici est que les axes factoriels
# sont construite à partir du lien entre délinquance et zonage d'étude...)

# Variante 4: on ne retient que des variables socio-démo-éco sur les communes comme variables actives
# et ensuite on projète en supplémentaire les variables  sur les atteintes...
# TODO!

# B) ACP sur les seules atteintes corporelles (les atteintes doivent bien ici être associées à un triplet de communes
# (I,V,M) renseigné):

# 1) Etape préliminaire: préparation du tableau de données en vue de l'ACP 

# a) Agrégation des atteintes par communes (lieu de commission de l'infraction):

# Note: On part du data.table des atteintes géolocalisées "del2016_2021_dt10" généré par 
# le script "Construction_base_etude.R":

# Warning: on se restreint ici aux seules atteintes corporelles* associées à un triplet de communes (I,V,M) renseigné!
# * il s'agit des atteintes suivantes (pour lesquelles on a le plus de (I,V,M) renseignés:
# Coups et blessures volontaires dans la sphère familiale
# Coups et blessures volontaires en dehors de la sphère familiale
# Violences sexuelles
# Homicides

del2016_2021_IVM_meme_zonage <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE)
                                                                                    & (is.na(cog_com_22_mec)==FALSE), .(
  Nb_atteintes_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_atteintes_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
  Nb_atteintes_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# on apparie ce fichier communal d'atteintes avec le fichier contenant des infos socio-éco et démo sur les communes
# (comparateur des communes de l'Insee):

del2016_2021_IVM_meme_zonage <- 
  merge(x = del2016_2021_IVM_meme_zonage,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# on apparie ce fichier avec le fichier Insee contenant les infos sur les différents zonages administratifs et d'études
# associés à chaque commune

del2016_2021_IVM_meme_zonage <- 
  merge(x = del2016_2021_IVM_meme_zonage,
        y = communes_zonages_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# on transforme le tableau en tibble:
del2016_2021_IVM_meme_zonage <- as_tibble(del2016_2021_IVM_meme_zonage)

names(del2016_2021_IVM_meme_zonage)

# b) Retraitement sur les variables: on calcule un nombre d'atteintes corporelles total


del2016_2021_IVM_meme_zonage <- del2016_2021_IVM_meme_zonage %>%
  mutate(Nb_atteintes_corpo = Nb_atteintes_blessures_famil + Nb_atteintes_blessures_horsfamil + 
           Nb_atteintes_homic + Nb_atteintes_viol_sex,
         Nb_atteintes_corpo_1ZE = Nb_atteintes_blessures_famil_1ZE + Nb_atteintes_blessures_horsfamil_1ZE + 
           Nb_atteintes_homic_1ZE + Nb_atteintes_viol_sex_1ZE,
         Nb_atteintes_corpo_1BV = Nb_atteintes_blessures_famil_1BV + Nb_atteintes_blessures_horsfamil_1BV + 
           Nb_atteintes_homic_1BV + Nb_atteintes_viol_sex_1BV,
         Nb_atteintes_corpo_1GD = Nb_atteintes_blessures_famil_1GD + Nb_atteintes_blessures_horsfamil_1GD + 
           Nb_atteintes_homic_1GD + Nb_atteintes_viol_sex_1GD,
         Nb_atteintes_corpo_1UU = Nb_atteintes_blessures_famil_1UU + Nb_atteintes_blessures_horsfamil_1UU + 
           Nb_atteintes_homic_1UU + Nb_atteintes_viol_sex_1UU,
         Nb_atteintes_corpo_1AAV = Nb_atteintes_blessures_famil_1AAV + Nb_atteintes_blessures_horsfamil_1AAV + 
           Nb_atteintes_homic_1AAV + Nb_atteintes_viol_sex_1AAV,
         Nb_atteintes_corpo_1CENTR = Nb_atteintes_blessures_famil_1CENTR + Nb_atteintes_blessures_horsfamil_1CENTR + 
           Nb_atteintes_homic_1CENTR + Nb_atteintes_viol_sex_1CENTR
  )

# c) Calcul des ratios communaux sur le volume des atteintes corporelles (pour 1000 habitants) et sa structure (en %) par type d'atteinte:

del2016_2021_IVM_meme_zonage <- del2016_2021_IVM_meme_zonage %>%
  mutate(
    # ratios sur la structure des atteintes corporelles par type (en %):
    P_bless_famil = Nb_atteintes_blessures_famil/Nb_atteintes_corpo*100,
    P_bless_h_famil = Nb_atteintes_blessures_horsfamil/Nb_atteintes_corpo*100,
    P_homic = Nb_atteintes_homic/Nb_atteintes_corpo*100,
    P_viol_sex = Nb_atteintes_viol_sex/Nb_atteintes_corpo*100,
    # volume des atteintes corporelles par type (pour 1000 habitants):
    bless_famil = Nb_atteintes_blessures_famil/P19_POP*1000,
    bless_h_famil = Nb_atteintes_blessures_horsfamil/P19_POP*1000,
    homic = Nb_atteintes_homic/P19_POP*1000,
    viol_sex = Nb_atteintes_viol_sex/P19_POP*1000,
    # part des atteintes corporelles associées à un triplet de communes (I,V,M) présent dans un même zonage d'étude (en % de chaque type d'atteinte)
    # Zone d'emploi (ZE):
    P_atteintes_corpo_1ZE=Nb_atteintes_corpo_1ZE/Nb_atteintes_corpo*100,
    P_bless_famil_1ZE = Nb_atteintes_blessures_famil_1ZE/Nb_atteintes_corpo*100,
    P_bless_h_famil_1ZE = Nb_atteintes_blessures_horsfamil_1ZE/Nb_atteintes_corpo*100,
    P_homic_1ZE = Nb_atteintes_homic_1ZE/Nb_atteintes_corpo*100,
    P_viol_sex_1ZE = Nb_atteintes_viol_sex_1ZE/Nb_atteintes_corpo*100,
    # Bassin de vie (BV):
    P_atteintes_corpo_1BV=Nb_atteintes_corpo_1BV/Nb_atteintes_corpo*100,
    P_bless_famil_1BV = Nb_atteintes_blessures_famil_1BV/Nb_atteintes_corpo*100,
    P_bless_h_famil_1BV = Nb_atteintes_blessures_horsfamil_1BV/Nb_atteintes_corpo*100,
    P_homic_1BV = Nb_atteintes_homic_1BV/Nb_atteintes_corpo*100,
    P_viol_sex_1BV = Nb_atteintes_viol_sex_1BV/Nb_atteintes_corpo*100,
    # Grille de densité (GD):
    P_atteintes_corpo_1GD=Nb_atteintes_corpo_1GD/Nb_atteintes_corpo*100,
    P_bless_famil_1GD = Nb_atteintes_blessures_famil_1GD/Nb_atteintes_corpo*100,
    P_bless_h_famil_1GD = Nb_atteintes_blessures_horsfamil_1GD/Nb_atteintes_corpo*100,
    P_homic_1GD = Nb_atteintes_homic_1GD/Nb_atteintes_corpo*100,
    P_viol_sex_1GD = Nb_atteintes_viol_sex_1GD/Nb_atteintes_corpo*100,
    # Unité urbaine (UU):
    P_atteintes_corpo_1UU=Nb_atteintes_corpo_1UU/Nb_atteintes_corpo*100,
    P_bless_famil_1UU = Nb_atteintes_blessures_famil_1UU/Nb_atteintes_corpo*100,
    P_bless_h_famil_1UU = Nb_atteintes_blessures_horsfamil_1UU/Nb_atteintes_corpo*100,
    P_homic_1UU = Nb_atteintes_homic_1UU/Nb_atteintes_corpo*100,
    P_viol_sex_1UU = Nb_atteintes_viol_sex_1UU/Nb_atteintes_corpo*100,
    # Aire d'attraction des villes (AAV):
    P_atteintes_corpo_1AAV=Nb_atteintes_corpo_1AAV/Nb_atteintes_corpo*100,
    P_bless_famil_1AAV = Nb_atteintes_blessures_famil_1AAV/Nb_atteintes_corpo*100,
    P_bless_h_famil_1AAV = Nb_atteintes_blessures_horsfamil_1AAV/Nb_atteintes_corpo*100,
    P_homic_1AAV = Nb_atteintes_homic_1AAV/Nb_atteintes_corpo*100,
    P_viol_sex_1AAV = Nb_atteintes_viol_sex_1AAV/Nb_atteintes_corpo*100,
    # Centralités (CENTR):
    P_atteintes_corpo_1CENTR=Nb_atteintes_corpo_1CENTR/Nb_atteintes_corpo*100,
    P_bless_famil_1CENTR = Nb_atteintes_blessures_famil_1CENTR/Nb_atteintes_corpo*100,
    P_bless_h_famil_1CENTR = Nb_atteintes_blessures_horsfamil_1CENTR/Nb_atteintes_corpo*100,
    P_homic_1CENTR = Nb_atteintes_homic_1CENTR/Nb_atteintes_corpo*100,
    P_viol_sex_1CENTR = Nb_atteintes_viol_sex_1CENTR/Nb_atteintes_corpo*100,
  )

names(del2016_2021_IVM_meme_zonage)

# ACP n° 2:

# Individus: 1 418 communes
# Champ retenu:
# 1- communes de la France métropolitaine.
# 2- communes de plus de 1000 habitants.
# 3- communes ayant au moins une atteinte associée à un triplet de communes (I,V,M) renseigné.
# 4- communes ayant au moins une atteinte dans chacun des 4 types d'atteintes corporelles

# Variables actives: 4 + 4
# 1- ratios sur la structure de la délinquance par type (en %): 4 variables
# 2- volume de la délinquance par type (pour 1000 habitants): 4 variables

# Variables supplémentaires: 6
# a) variables quantitatives:
# -part des atteintes associées à un triplet de communes (I,VnM) présent dans un même zonage d'étude
# remarque: on ne distingue pas ici les différents types d'atteinte.
# - MED20 (revenu médian de la commune en 2020).
# on pourrait en rajouter d'autres, notamment sur la dimension socio-éco-démo des communes (TODO!)
# ce serait pas mal de projeter une variable géographique du type (latitude, longitude) des communes
# -> ça permettrait de relier le profil des communes en termes de délinquance et leur position géographique
# sur le territoire français.
# b) variables qualitatives:
# TODO!
# variables géographiques (du type REGION ?)...


# L'ACP est normée (toutes les variables ont été standardisées, étant donné qu'elles ne sont pas exprimées
# dans la même unité).

data_acp2 <- del2016_2021_IVM_meme_zonage %>% filter(!(DEP.x %in% c("971","972","973","974","976")) &
                                                       Nb_atteintes_blessures_famil>0 &
                                                       Nb_atteintes_blessures_horsfamil>0 &
                                                       Nb_atteintes_homic>0 &
                                                       Nb_atteintes_viol_sex>0 &
                                                       P19_POP>1000) %>%
  select(cog_com_22_inf,LIBGEO.x,DEP.x,REG.x,MED20,
         P_atteintes_corpo_1ZE,P_atteintes_corpo_1BV,P_atteintes_corpo_1GD,P_atteintes_corpo_1UU,
         P_atteintes_corpo_1AAV,P_atteintes_corpo_1CENTR,
         P_bless_famil,P_bless_h_famil,P_homic,P_viol_sex,
         bless_famil,bless_h_famil,homic,viol_sex) %>%
  column_to_rownames(var="cog_com_22_inf")


# Statistiques descriptives univariées sur les variables en entrée de l'ACP:
summary(data_acp2)

boxplot(data_acp1[11:17])

boxplot(data_acp1[18:24])
# Beaucoup d'outliers pour les volumes d'atteintes pour 1000 habitants.
# Distribution très asymétrique à droite !

# Matrice de corrélation entre toutes les variables quantitatives:
mcor <- cor(data_acp2[,4:18])
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# Principales remarques:
# - les variables mesurant le volume de chaque type d'atteinte (pour 1000 hbts) sont très corrélées postivement entre elles.
# -> effet taille: des communes sont fortement impactées par la délinquance (et ce pour tous les types d'atteintes), et
# à l'inverse, les communes peu impactées par la délinquance globalement le sont souvent pour tous les types d'atteintes.

# - en revanche, les ratios de structure de la délinquance par type semblent peu corrélés entre eux et pas corrélé aux
# variables mesurant le volume de la délinquance pour 1000 hbts.

# - enfin, on constate que les variables mesurant la part d'atteintes avec (I,V) ds un même zonage sont peu corrélées avec 
# variables actives de l'ACP, ce qui n'est pas très bon signe ici, étant donné que l'on aimerait justement montrer
# que certains profils de communes en termes de délinquance (volume/structure) sont davantage "adaptés" à certains 
# zonages d'études plutôt que d'autres.

# On réalise l'ACP:

# Dans un premier temps, on paramètre l'ACP à partir du tableau de bord "Shiny":
data_acp2_shiny <- data_acp2[,4:18]
resACP2 = PCAshiny(data_acp2_shiny)

# Code R à retenir pour réaliser les différentes étapes de cette ACP (en vue du rapport):

# à insérer ici...

# Interprétation des résultats de l'ACP n°2:

# Nombre d'axes factoriels à retenir pour l'interprétation: 
# 3 axes factoriels peuvent être retenus ici, ils expliques à eux trois 83% de l'inertie totale.

# 1er axe factoriel: volume des atteintes corporelles à l'échelle communale pour 1000 habitants

# 2eme axe factoriel: structure des atteintes corporelles à l'échelle communale (opposition entre les communes
# avec une prééminence des violences sexuelles et des homicides et celles avec une prééminence des
# blessures volontaires extra-familiales)

# 3eme axe factoriel: encore la structure des atteintes corporelles (opposition entre blessures intra-familiales
# et blessures extra-familiales)

# Variantes possibles de l'ACP n°2:


# II) ACP au niveau départemental

# A) ACP sur l'ensemble des types d'atteintes (les atteintes doivent bien ici être associées à un couple de communes
# (I,V) renseigné):

# 1) Etape préliminaire: préparation du tableau de données en vue de l'ACP 

# a) Agrégation des atteintes par département (lieu de commission de l'infraction):

# Note: On part du data.table des atteintes géolocalisées "del2016_2021_dt10" généré par 
# le script "Construction_base_etude.R":

# Warning: on se restreint ici aux seules atteintes associées à un couple de communes (I,V) renseigné!

del2016_2021_IV_meme_zonage2 <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
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
  Nb_atteintes_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1ZE = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1ZE = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1ZE = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1ZE = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1ZE = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1ZE = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1ZE = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1ZE = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_atteintes_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1BV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_atteintes_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1GD = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1GD = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1GD = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1GD = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1GD = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1GD = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1GD = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1GD = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_atteintes_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1UU = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1UU = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1UU = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1UU = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1UU = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1UU = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1UU = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1UU = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_atteintes_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1AAV = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1AAV = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1AAV = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1AAV = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1AAV = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1AAV = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1AAV = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1AAV = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_atteintes_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_cambr_1CENTR = sum(compteur*(classe2 == "Cambriolages de logement" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_destr_degrad_1CENTR = sum(compteur*(classe2 == "Destructions et dégradations" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_armes_1CENTR = sum(compteur*(classe2 == "Vols avec armes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1CENTR = sum(compteur*(classe2 == "Vols d'accessoires sur véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1CENTR = sum(compteur*(classe2 == "Vols dans les véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1CENTR = sum(compteur*(classe2 == "Vols de véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1CENTR = sum(compteur*(classe2 == "Vols sans violence contre des personnes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1CENTR = sum(compteur*(classe2 == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(DEP_inf)]

# on apparie ce fichier départemental d'atteintes avec le fichier contenant des infos socio-éco et démo sur les communes
# (comparateur des communes de l'Insee):

# on commence par réagréger les infos communales au niveau départemental:
infos_dep_dt <- infos_communes_dt[ ,.(
  P19_POP = sum(P19_POP, na.rm=TRUE)),
  by= .(DEP)]

del2016_2021_IV_meme_zonage2 <- 
  merge(x = del2016_2021_IV_meme_zonage2,
        y = infos_dep_dt,
        by.x = "DEP_inf",
        by.y = "DEP",
        all.x = TRUE)

# on apparie ce fichier avec le fichier Insee contenant les infos sur les différents zonages administratifs et d'études
# associés à chaque commune

# del2016_2021_IV_meme_zonage <- 
#   merge(x = del2016_2021_IV_meme_zonage,
#         y = communes_zonages_dt,
#         by.x = "cog_com_22_inf",
#         by.y = "CODGEO",
#         all.x = TRUE)

# on transforme le tableau en tibble:
del2016_2021_IV_meme_zonage2 <- as_tibble(del2016_2021_IV_meme_zonage2)

names(del2016_2021_IV_meme_zonage2)

# b) Retraitement sur les variables: on procède à des regroupements de types d'atteintes "proches" pour simplifier un 
# peu l'analyse

# Pour des raisons de lisibilité, nous travaillons ici non pas sur les 12 types atteintes du fichier du SSMSI, mais sur
# 7 types d'atteintes, après réagrégation de certains types d'atteintes "proches". Dans le détail, nous avons 
# procédé aux regroupements d'atteintes suivants:

# 1er regroupement: "vols d'accessoires dans les véhicules", "vols de véhicules" et "vols dans les véhicules"
# -> vols en rapport avec les véhicules
# 2eme regroupement:
# "coups et blessures volontaires dans la sphère familiale","coups et blessures volontaires en dehors de la sphère familiae" et "homocides".
# -> atteintes physiques corporelles graves et directes
# 3eme regroupement: "vols violents sans arme" et les "vols violents avec arme"
# -> vols violents

del2016_2021_IV_meme_zonage2 <- del2016_2021_IV_meme_zonage2 %>%
  mutate(Nb_atteintes_vols_vehic = Nb_atteintes_vols_acces_vehic + Nb_atteintes_vols_ds_vehic + 
           Nb_atteintes_vols_de_vehic,
         Nb_atteintes_bless_famil_homic = Nb_atteintes_blessures_famil + Nb_atteintes_blessures_horsfamil +
           Nb_atteintes_homic,
         Nb_atteintes_vols_violents = Nb_atteintes_vols_armes + Nb_atteintes_vols_violants_sansarme,
         Nb_atteintes_vols_vehic_1ZE = Nb_atteintes_vols_acces_vehic_1ZE + Nb_atteintes_vols_ds_vehic_1ZE + 
           Nb_atteintes_vols_de_vehic_1ZE,
         Nb_atteintes_bless_famil_homic_1ZE = Nb_atteintes_blessures_famil_1ZE + Nb_atteintes_blessures_horsfamil_1ZE +
           Nb_atteintes_homic_1ZE,
         Nb_atteintes_vols_violents_1ZE = Nb_atteintes_vols_armes_1ZE + Nb_atteintes_vols_violants_sansarme_1ZE,
         Nb_atteintes_vols_vehic_1BV = Nb_atteintes_vols_acces_vehic_1BV + Nb_atteintes_vols_ds_vehic_1BV + 
           Nb_atteintes_vols_de_vehic_1BV,
         Nb_atteintes_bless_famil_homic_1BV = Nb_atteintes_blessures_famil_1BV + Nb_atteintes_blessures_horsfamil_1BV +
           Nb_atteintes_homic_1BV,
         Nb_atteintes_vols_violents_1BV = Nb_atteintes_vols_armes_1BV + Nb_atteintes_vols_violants_sansarme_1BV,
         Nb_atteintes_vols_vehic_1GD = Nb_atteintes_vols_acces_vehic_1GD + Nb_atteintes_vols_ds_vehic_1GD + 
           Nb_atteintes_vols_de_vehic_1GD,
         Nb_atteintes_bless_famil_homic_1GD = Nb_atteintes_blessures_famil_1GD + Nb_atteintes_blessures_horsfamil_1GD +
           Nb_atteintes_homic_1GD,
         Nb_atteintes_vols_violents_1GD = Nb_atteintes_vols_armes_1GD + Nb_atteintes_vols_violants_sansarme_1GD,
         Nb_atteintes_vols_vehic_1UU = Nb_atteintes_vols_acces_vehic_1UU + Nb_atteintes_vols_ds_vehic_1UU + 
           Nb_atteintes_vols_de_vehic_1UU,
         Nb_atteintes_bless_famil_homic_1UU = Nb_atteintes_blessures_famil_1UU + Nb_atteintes_blessures_horsfamil_1UU +
           Nb_atteintes_homic_1UU,
         Nb_atteintes_vols_violents_1UU = Nb_atteintes_vols_armes_1UU + Nb_atteintes_vols_violants_sansarme_1UU,
         Nb_atteintes_vols_vehic_1AAV = Nb_atteintes_vols_acces_vehic_1AAV + Nb_atteintes_vols_ds_vehic_1AAV + 
           Nb_atteintes_vols_de_vehic_1AAV,
         Nb_atteintes_bless_famil_homic_1AAV = Nb_atteintes_blessures_famil_1AAV + Nb_atteintes_blessures_horsfamil_1AAV +
           Nb_atteintes_homic_1AAV,
         Nb_atteintes_vols_violents_1AAV = Nb_atteintes_vols_armes_1AAV + Nb_atteintes_vols_violants_sansarme_1AAV,
         Nb_atteintes_vols_vehic_1CENTR = Nb_atteintes_vols_acces_vehic_1CENTR + Nb_atteintes_vols_ds_vehic_1CENTR + 
           Nb_atteintes_vols_de_vehic_1CENTR,
         Nb_atteintes_bless_famil_homic_1CENTR = Nb_atteintes_blessures_famil_1CENTR + Nb_atteintes_blessures_horsfamil_1CENTR +
           Nb_atteintes_homic_1CENTR,
         Nb_atteintes_vols_violents_1CENTR = Nb_atteintes_vols_armes_1CENTR + Nb_atteintes_vols_violants_sansarme_1CENTR)

# c) Calcul des ratios communaux sur le volume de la délinquance (pour 1000 habitants) et sa structure (en %) par type d'atteinte:

del2016_2021_IV_meme_zonage2 <- del2016_2021_IV_meme_zonage2 %>%
  mutate(
    # ratios sur la structure de la délinquance par type (en %):
    P_vols_vehic = Nb_atteintes_vols_vehic/Nb_atteintes*100,
    P_bless_famil_homic = Nb_atteintes_bless_famil_homic/Nb_atteintes*100,
    P_vols_violents = Nb_atteintes_vols_violents/Nb_atteintes*100,
    P_cambr = Nb_atteintes_cambr/Nb_atteintes*100,
    P_destr_degrad = Nb_atteintes_destr_degrad/Nb_atteintes*100,
    P_viol_sex = Nb_atteintes_viol_sex/Nb_atteintes*100,
    P_vols_sansviol = Nb_atteintes_vols_sansviol/Nb_atteintes*100,
    # volume de la délinquance par type (pour 1000 habitants):
    vols_vehic = Nb_atteintes_vols_vehic/P19_POP*1000,
    bless_famil_homic = Nb_atteintes_bless_famil_homic/P19_POP*1000,
    vols_violents = Nb_atteintes_vols_violents/P19_POP*1000,
    cambr = Nb_atteintes_cambr/P19_POP*1000,
    destr_degrad = Nb_atteintes_destr_degrad/P19_POP*1000,
    viol_sex = Nb_atteintes_viol_sex/P19_POP*1000,
    vols_sansviol = Nb_atteintes_vols_sansviol/P19_POP*1000,
    # part des atteintes associées à un couple de communes (I,V) présent dans un même zonage d'étude (en % de chaque type d'atteinte)
    # Zone d'emploi (ZE):
    P_atteintes_1ZE = Nb_atteintes_1ZE/Nb_atteintes*100,
    P_vols_vehic_1ZE = Nb_atteintes_vols_vehic_1ZE/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1ZE = Nb_atteintes_bless_famil_homic_1ZE/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1ZE = Nb_atteintes_vols_violents_1ZE/Nb_atteintes_vols_violents*100,
    P_cambr_1ZE = Nb_atteintes_cambr_1ZE/Nb_atteintes_cambr*100,
    P_destr_degrad_1ZE = Nb_atteintes_destr_degrad_1ZE/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1ZE = Nb_atteintes_viol_sex_1ZE/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1ZE = Nb_atteintes_vols_sansviol_1ZE/Nb_atteintes_vols_sansviol*100,
    # Bassin de vie (BV):
    P_atteintes_1BV = Nb_atteintes_1BV/Nb_atteintes*100,
    P_vols_vehic_1BV = Nb_atteintes_vols_vehic_1BV/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1BV = Nb_atteintes_bless_famil_homic_1BV/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1BV = Nb_atteintes_vols_violents_1BV/Nb_atteintes_vols_violents*100,
    P_cambr_1BV = Nb_atteintes_cambr_1BV/Nb_atteintes_cambr*100,
    P_destr_degrad_1BV = Nb_atteintes_destr_degrad_1BV/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1BV = Nb_atteintes_viol_sex_1BV/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1BV = Nb_atteintes_vols_sansviol_1BV/Nb_atteintes_vols_sansviol*100,
    # Grille de densité (GD):
    P_atteintes_1GD = Nb_atteintes_1GD/Nb_atteintes*100,
    P_vols_vehic_1GD = Nb_atteintes_vols_vehic_1GD/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1GD = Nb_atteintes_bless_famil_homic_1GD/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1GD = Nb_atteintes_vols_violents_1GD/Nb_atteintes_vols_violents*100,
    P_cambr_1GD = Nb_atteintes_cambr_1GD/Nb_atteintes_cambr*100,
    P_destr_degrad_1GD = Nb_atteintes_destr_degrad_1GD/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1GD = Nb_atteintes_viol_sex_1GD/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1GD = Nb_atteintes_vols_sansviol_1GD/Nb_atteintes_vols_sansviol*100,
    # Unité urbaine (UU):
    P_atteintes_1UU = Nb_atteintes_1UU/Nb_atteintes*100,
    P_vols_vehic_1UU = Nb_atteintes_vols_vehic_1UU/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1UU = Nb_atteintes_bless_famil_homic_1UU/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1UU = Nb_atteintes_vols_violents_1UU/Nb_atteintes_vols_violents*100,
    P_cambr_1UU = Nb_atteintes_cambr_1UU/Nb_atteintes_cambr*100,
    P_destr_degrad_1UU = Nb_atteintes_destr_degrad_1UU/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1UU = Nb_atteintes_viol_sex_1UU/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1UU = Nb_atteintes_vols_sansviol_1UU/Nb_atteintes_vols_sansviol*100,
    # Aire d'attraction des villes (AAV):
    P_atteintes_1AAV = Nb_atteintes_1AAV/Nb_atteintes*100,
    P_vols_vehic_1AAV = Nb_atteintes_vols_vehic_1AAV/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1AAV = Nb_atteintes_bless_famil_homic_1AAV/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1AAV = Nb_atteintes_vols_violents_1AAV/Nb_atteintes_vols_violents*100,
    P_cambr_1AAV = Nb_atteintes_cambr_1AAV/Nb_atteintes_cambr*100,
    P_destr_degrad_1AAV = Nb_atteintes_destr_degrad_1AAV/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1AAV = Nb_atteintes_viol_sex_1AAV/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1AAV = Nb_atteintes_vols_sansviol_1AAV/Nb_atteintes_vols_sansviol*100,
    # Centralités (CENTR):
    P_atteintes_1CENTR = Nb_atteintes_1CENTR/Nb_atteintes*100,
    P_vols_vehic_1CENTR = Nb_atteintes_vols_vehic_1CENTR/Nb_atteintes_vols_vehic*100,
    P_bless_famil_homic_1CENTR = Nb_atteintes_bless_famil_homic_1CENTR/Nb_atteintes_bless_famil_homic*100,
    P_vols_violents_1CENTR = Nb_atteintes_vols_violents_1CENTR/Nb_atteintes_vols_violents*100,
    P_cambr_1CENTR = Nb_atteintes_cambr_1CENTR/Nb_atteintes_cambr*100,
    P_destr_degrad_1CENTR = Nb_atteintes_destr_degrad_1CENTR/Nb_atteintes_destr_degrad*100,
    P_viol_sex_1CENTR = Nb_atteintes_viol_sex_1CENTR/Nb_atteintes_viol_sex*100,
    P_vols_sansviol_1CENTR = Nb_atteintes_vols_sansviol_1CENTR/Nb_atteintes_vols_sansviol*100
  )

names(del2016_2021_IV_meme_zonage2)

# ACP n° 3:

# Individus: 96 départements
# Champ retenu:
# départements de la France métropolitaine.

# Variables actives: 7 + 7
# 1- ratios sur la structure de la délinquance par type (en %): 7 variables
# 2- volume de la délinquance par type (pour 1000 habitants): 7 variables

# Variables supplémentaires: 6
# a) variables quantitatives:
# -part des atteintes associées à un couple de communes (I,V) présent dans un même zonage d'étude
# remarque: on ne distingue pas ici les différents types d'atteinte.

# on pourrait en rajouter d'autres, notamment sur la dimension socio-éco-démo des communes (TODO!)

# b) variables qualitatives:
# TODO!

# L'ACP est normée (toutes les variables ont été standardisées, étant donné qu'elles ne sont pas exprimées
# dans la même unité).

data_acp3 <- del2016_2021_IV_meme_zonage2 %>% filter(!(DEP_inf %in% c("971","972","973","974","976")) &
                                                      (is.na(DEP_inf)==FALSE)) %>%
                                                      select(DEP_inf,
         P_atteintes_1ZE,P_atteintes_1BV,P_atteintes_1GD,P_atteintes_1UU,P_atteintes_1AAV,P_atteintes_1CENTR,
         P_vols_vehic,P_bless_famil_homic,P_vols_violents,P_cambr,P_destr_degrad,
         P_viol_sex,P_vols_sansviol,
         vols_vehic,bless_famil_homic,vols_violents,
         cambr,destr_degrad,
         viol_sex,vols_sansviol) %>%
  column_to_rownames(var="DEP_inf")

# Statistiques descriptives univariées sur les variables en entrée de l'ACP:
summary(data_acp3)

# Distribution des ratios de structure par type d'atteinte
boxplot(data_acp3[7:13])

# Distribution des volumes d'atteintes pour 1000 hbts, par type d'atteinte
boxplot(data_acp1[14:20])
# Beaucoup d'outliers pour les volumes d'atteintes pour 1000 habitants.
# Distribution très asymétrique à droite ! Quelques communes affichent des gros chiffres par 
# rapport au reste des communes !!

# Matrice de corrélation entre toutes les variables quantitatives:
mcor <- cor(data_acp3)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# Principales remarques:
# - les variables mesurant le volume de chaque type d'atteinte (pour 1000 hbts) sont très corrélées postivement entre elles.
# -> effet taille: des communes sont fortement impactées par la délinquance (et ce pour tous les types d'atteintes), et
# à l'inverse, les communes peu impactées par la délinquance globalement le sont souvent pour tous les types d'atteintes.

# - en revanche, les ratios de structure de la délinquance par type semblent peu corrélés entre eux et pas corrélé aux
# variables mesurant le volume de la délinquance pour 1000 hbts.

# - enfin, on constate que les variables mesurant la part d'atteintes avec (I,V) ds un même zonage sont peu corrélées avec 
# variables actives de l'ACP, ce qui n'est pas très bon signe ici, étant donné que l'on aimerait justement montrer
# que certains profils de communes en termes de délinquance (volume/structure) sont davantage "adaptés" à certains 
# zonages d'études plutôt que d'autres.

# On réalise l'ACP:

# Dans un premier temps, on paramètre l'ACP à partir du tableau de bord "Shiny":
data_acp3_shiny <- data_acp3
resACP3 = PCAshiny(data_acp3_shiny)

# on refait l'ACP sans aucune variable supplémentaire:
data_acp3bis_shiny <- data_acp3[,7:20]
resACP3bis = PCAshiny(data_acp3bis_shiny)


# Code R à retenir pour réaliser les différentes étapes de cette ACP (en vue du rapport):

# à insérer ici...

# Interprétation des résultats de l'ACP n°3:

# Nombre d'axes factoriels à retenir pour l'interprétation: 
# 3 axes factoriels peuvent être retenus ici, ils expliques à eux trois XX% de l'inertie totale.

# 1er axe factoriel: 
# 2eme axe factoriel: 
# 3eme axe factoriel: 


# B) ACP sur les seules atteintes corporelles (les atteintes doivent bien ici être associées à un triplet de communes
# (I,V,M) renseigné):

# 1) Etape préliminaire: préparation du tableau de données en vue de l'ACP 

# a) Agrégation des atteintes par département (lieu de commission de l'infraction):

# Note: On part du data.table des atteintes géolocalisées "del2016_2021_dt10" généré par 
# le script "Construction_base_etude.R":

# Warning: on se restreint ici aux seules atteintes corporelles* associées à un triplet de communes (I,V,M) renseigné!
# * il s'agit des atteintes suivantes (pour lesquelles on a le plus de (I,V,M) renseignés:
# Coups et blessures volontaires dans la sphère familiale
# Coups et blessures volontaires en dehors de la sphère familiale
# Violences sexuelles
# Homicides

del2016_2021_IVM_meme_zonage2 <- del2016_2021_dt10[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE)
                                                  & (is.na(cog_com_22_mec)==FALSE), .(
                                                    Nb_atteintes_blessures_famil = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
                                                    Nb_atteintes_homic = sum(compteur*(classe2 == "Homicides"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex = sum(compteur*(classe2 == "Violences sexuelles"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1ZE = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1ZE = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1ZE = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1BV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1BV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1BV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1GD = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1GD = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1GD = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1UU = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1UU = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1UU = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1AAV = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1AAV = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1AAV = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_famil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_blessures_horsfamil_1CENTR = sum(compteur*(classe2 == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_homic_1CENTR = sum(compteur*(classe2 == "Homicides" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
                                                    Nb_atteintes_viol_sex_1CENTR = sum(compteur*(classe2 == "Violences sexuelles" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                                                  by = .(DEP_inf)]

# on apparie ce fichier communal d'atteintes avec le fichier contenant des infos socio-éco et démo sur les communes
# (comparateur des communes de l'Insee):

del2016_2021_IVM_meme_zonage2 <- 
  merge(x = del2016_2021_IVM_meme_zonage2,
        y = infos_dep_dt,
        by.x = "DEP_inf",
        by.y = "DEP",
        all.x = TRUE)

# on apparie ce fichier avec le fichier Insee contenant les infos sur les différents zonages administratifs et d'études
# associés à chaque commune

# del2016_2021_IVM_meme_zonage <- 
#   merge(x = del2016_2021_IVM_meme_zonage,
#         y = communes_zonages_dt,
#         by.x = "cog_com_22_inf",
#         by.y = "CODGEO",
#         all.x = TRUE)

# on transforme le tableau en tibble:
del2016_2021_IVM_meme_zonage2 <- as_tibble(del2016_2021_IVM_meme_zonage2)

names(del2016_2021_IVM_meme_zonage2)

# b) Retraitement sur les variables: on calcule un nombre d'atteintes corporelles total


del2016_2021_IVM_meme_zonage2 <- del2016_2021_IVM_meme_zonage2 %>%
  mutate(Nb_atteintes_corpo = Nb_atteintes_blessures_famil + Nb_atteintes_blessures_horsfamil + 
           Nb_atteintes_homic + Nb_atteintes_viol_sex,
         Nb_atteintes_corpo_1ZE = Nb_atteintes_blessures_famil_1ZE + Nb_atteintes_blessures_horsfamil_1ZE + 
           Nb_atteintes_homic_1ZE + Nb_atteintes_viol_sex_1ZE,
         Nb_atteintes_corpo_1BV = Nb_atteintes_blessures_famil_1BV + Nb_atteintes_blessures_horsfamil_1BV + 
           Nb_atteintes_homic_1BV + Nb_atteintes_viol_sex_1BV,
         Nb_atteintes_corpo_1GD = Nb_atteintes_blessures_famil_1GD + Nb_atteintes_blessures_horsfamil_1GD + 
           Nb_atteintes_homic_1GD + Nb_atteintes_viol_sex_1GD,
         Nb_atteintes_corpo_1UU = Nb_atteintes_blessures_famil_1UU + Nb_atteintes_blessures_horsfamil_1UU + 
           Nb_atteintes_homic_1UU + Nb_atteintes_viol_sex_1UU,
         Nb_atteintes_corpo_1AAV = Nb_atteintes_blessures_famil_1AAV + Nb_atteintes_blessures_horsfamil_1AAV + 
           Nb_atteintes_homic_1AAV + Nb_atteintes_viol_sex_1AAV,
         Nb_atteintes_corpo_1CENTR = Nb_atteintes_blessures_famil_1CENTR + Nb_atteintes_blessures_horsfamil_1CENTR + 
           Nb_atteintes_homic_1CENTR + Nb_atteintes_viol_sex_1CENTR
  )

# c) Calcul des ratios communaux sur le volume des atteintes corporelles (pour 1000 habitants) et sa structure (en %) par type d'atteinte:

del2016_2021_IVM_meme_zonage2 <- del2016_2021_IVM_meme_zonage2 %>%
  mutate(
    # ratios sur la structure des atteintes corporelles par type (en %):
    P_bless_famil = Nb_atteintes_blessures_famil/Nb_atteintes_corpo*100,
    P_bless_h_famil = Nb_atteintes_blessures_horsfamil/Nb_atteintes_corpo*100,
    P_homic = Nb_atteintes_homic/Nb_atteintes_corpo*100,
    P_viol_sex = Nb_atteintes_viol_sex/Nb_atteintes_corpo*100,
    # volume des atteintes corporelles par type (pour 1000 habitants):
    bless_famil = Nb_atteintes_blessures_famil/P19_POP*1000,
    bless_h_famil = Nb_atteintes_blessures_horsfamil/P19_POP*1000,
    homic = Nb_atteintes_homic/P19_POP*1000,
    viol_sex = Nb_atteintes_viol_sex/P19_POP*1000,
    # part des atteintes corporelles associées à un triplet de communes (I,V,M) présent dans un même zonage d'étude (en % de chaque type d'atteinte)
    # Zone d'emploi (ZE):
    P_atteintes_corpo_1ZE=Nb_atteintes_corpo_1ZE/Nb_atteintes_corpo*100,
    P_bless_famil_1ZE = Nb_atteintes_blessures_famil_1ZE/Nb_atteintes_corpo*100,
    P_bless_h_famil_1ZE = Nb_atteintes_blessures_horsfamil_1ZE/Nb_atteintes_corpo*100,
    P_homic_1ZE = Nb_atteintes_homic_1ZE/Nb_atteintes_corpo*100,
    P_viol_sex_1ZE = Nb_atteintes_viol_sex_1ZE/Nb_atteintes_corpo*100,
    # Bassin de vie (BV):
    P_atteintes_corpo_1BV=Nb_atteintes_corpo_1BV/Nb_atteintes_corpo*100,
    P_bless_famil_1BV = Nb_atteintes_blessures_famil_1BV/Nb_atteintes_corpo*100,
    P_bless_h_famil_1BV = Nb_atteintes_blessures_horsfamil_1BV/Nb_atteintes_corpo*100,
    P_homic_1BV = Nb_atteintes_homic_1BV/Nb_atteintes_corpo*100,
    P_viol_sex_1BV = Nb_atteintes_viol_sex_1BV/Nb_atteintes_corpo*100,
    # Grille de densité (GD):
    P_atteintes_corpo_1GD=Nb_atteintes_corpo_1GD/Nb_atteintes_corpo*100,
    P_bless_famil_1GD = Nb_atteintes_blessures_famil_1GD/Nb_atteintes_corpo*100,
    P_bless_h_famil_1GD = Nb_atteintes_blessures_horsfamil_1GD/Nb_atteintes_corpo*100,
    P_homic_1GD = Nb_atteintes_homic_1GD/Nb_atteintes_corpo*100,
    P_viol_sex_1GD = Nb_atteintes_viol_sex_1GD/Nb_atteintes_corpo*100,
    # Unité urbaine (UU):
    P_atteintes_corpo_1UU=Nb_atteintes_corpo_1UU/Nb_atteintes_corpo*100,
    P_bless_famil_1UU = Nb_atteintes_blessures_famil_1UU/Nb_atteintes_corpo*100,
    P_bless_h_famil_1UU = Nb_atteintes_blessures_horsfamil_1UU/Nb_atteintes_corpo*100,
    P_homic_1UU = Nb_atteintes_homic_1UU/Nb_atteintes_corpo*100,
    P_viol_sex_1UU = Nb_atteintes_viol_sex_1UU/Nb_atteintes_corpo*100,
    # Aire d'attraction des villes (AAV):
    P_atteintes_corpo_1AAV=Nb_atteintes_corpo_1AAV/Nb_atteintes_corpo*100,
    P_bless_famil_1AAV = Nb_atteintes_blessures_famil_1AAV/Nb_atteintes_corpo*100,
    P_bless_h_famil_1AAV = Nb_atteintes_blessures_horsfamil_1AAV/Nb_atteintes_corpo*100,
    P_homic_1AAV = Nb_atteintes_homic_1AAV/Nb_atteintes_corpo*100,
    P_viol_sex_1AAV = Nb_atteintes_viol_sex_1AAV/Nb_atteintes_corpo*100,
    # Centralités (CENTR):
    P_atteintes_corpo_1CENTR=Nb_atteintes_corpo_1CENTR/Nb_atteintes_corpo*100,
    P_bless_famil_1CENTR = Nb_atteintes_blessures_famil_1CENTR/Nb_atteintes_corpo*100,
    P_bless_h_famil_1CENTR = Nb_atteintes_blessures_horsfamil_1CENTR/Nb_atteintes_corpo*100,
    P_homic_1CENTR = Nb_atteintes_homic_1CENTR/Nb_atteintes_corpo*100,
    P_viol_sex_1CENTR = Nb_atteintes_viol_sex_1CENTR/Nb_atteintes_corpo*100,
  )

names(del2016_2021_IVM_meme_zonage2)

# ACP n° 4:

# Individus: 1 96 départements
# Champ retenu:
# départements de la France métropolitaine.

# Variables actives: 4 + 4
# 1- ratios sur la structure de la délinquance par type (en %): 4 variables
# 2- volume de la délinquance par type (pour 1000 habitants): 4 variables

# Variables supplémentaires: 6
# a) variables quantitatives:
# -part des atteintes associées à un triplet de communes (I,VnM) présent dans un même zonage d'étude
# remarque: on ne distingue pas ici les différents types d'atteinte.

# on pourrait en rajouter d'autres, notamment sur la dimension socio-éco-démo des communes (TODO!)

# b) variables qualitatives:
# TODO!


# L'ACP est normée (toutes les variables ont été standardisées, étant donné qu'elles ne sont pas exprimées
# dans la même unité).

data_acp4 <- del2016_2021_IVM_meme_zonage2 %>% filter(!(DEP_inf %in% c("971","972","973","974","976")) &
                                                       (is.na(DEP_inf)==FALSE)) %>%
  select(DEP_inf,
         P_atteintes_corpo_1ZE,P_atteintes_corpo_1BV,P_atteintes_corpo_1GD,P_atteintes_corpo_1UU,
         P_atteintes_corpo_1AAV,P_atteintes_corpo_1CENTR,
         P_bless_famil,P_bless_h_famil,P_homic,P_viol_sex,
         bless_famil,bless_h_famil,homic,viol_sex) %>%
  column_to_rownames(var="DEP_inf")


# Statistiques descriptives univariées sur les variables en entrée de l'ACP:
summary(data_acp4)

boxplot(data_acp4[7:10])

boxplot(data_acp4[11:14])
# Beaucoup d'outliers pour les volumes d'atteintes pour 1000 habitants.
# Distribution très asymétrique à droite !

# Matrice de corrélation entre toutes les variables quantitatives:
mcor <- cor(data_acp4)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# Principales remarques:


# On réalise l'ACP:

# Dans un premier temps, on paramètre l'ACP à partir du tableau de bord "Shiny":
data_acp4_shiny <- data_acp4
resACP4 = PCAshiny(data_acp4_shiny)

# on refait l'ACP sans aucune variable supplémentaire:
data_acp4bis_shiny <- data_acp4[,7:14]
resACP4bis = PCAshiny(data_acp4bis_shiny)


# Code R à retenir pour réaliser les différentes étapes de cette ACP (en vue du rapport):

# à insérer ici...

# Interprétation des résultats de l'ACP n°2:

# Nombre d'axes factoriels à retenir pour l'interprétation: 
# 3 axes factoriels peuvent être retenus ici, ils expliques à eux trois 83% de l'inertie totale.

# 1er axe factoriel: volume des atteintes corporelles à l'échelle communale pour 1000 habitants

# 2eme axe factoriel: structure des atteintes corporelles à l'échelle communale (opposition entre les communes
# avec une prééminence des violences sexuelles et des homicides et celles avec une prééminence des
# blessures volontaires extra-familiales)

# 3eme axe factoriel: encore la structure des atteintes corporelles (opposition entre blessures intra-familiales
# et blessures extra-familiales)

# Variantes possibles de l'ACP n°2:




