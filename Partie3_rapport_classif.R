
# Partie 3 du Rapport Statapp: analyse factorielle et classification des atteintes dans la dimension spatiale

# ACP-CAH

# Analyse factorielle:

library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)

# on se base sur les tableaux générés dans le programme "Partie2_rapport_stat_des.R":
#source("Partie2_rapport_stat_des.R")

# A) ACP sur les régions (individus) et les proportions d'atteintes (on balaie ici tous les types d'atteintes)
# dont le triplet de lieux s'inscrit dans un même zonage (variables):

# 1) Dans une même zone d'emploi (ZE):

# a) On sélectionne les variables de la forme "Prop_type_atteinte_1ZE" comme variables actives:

tab_prop_del_1ZE_reg_acp <- del2016_2021_tb_reg %>% select(LIBELLE,ends_with("1ZE") & starts_with("Prop"))

# Comme les libellés des variables sont très longs et risquent de mal s'afficher sur le cercle de 
# corrélation, on renomme les variables actives avec des libellés plus courts:
tab_prop_del_1ZE_reg_acp <- tab_prop_del_1ZE_reg_acp %>% 
  rename(P_cambr_1ZE=Prop_atteintes_cambr_1ZE,
         P_bless_fam_1ZE=Prop_atteintes_blessures_famil_1ZE,
         P_bless_hfam_1ZE=Prop_atteintes_blessures_horsfamil_1ZE,
         P_destr_degrad_1ZE=Prop_atteintes_destr_degrad_1ZE,
         P_homic_1ZE=Prop_atteintes_homic_1ZE,
         P_viol_sex_1ZE=Prop_atteintes_viol_sex_1ZE,
         P_vol_armes_1ZE=Prop_atteintes_vols_armes_1ZE,
         P_vol_access_veh_1ZE=Prop_atteintes_vols_acces_vehic_1ZE,
         P_vol_ds_veh_1ZE=Prop_atteintes_vols_ds_vehic_1ZE,
         P_vol_de_veh_1ZE=Prop_atteintes_vols_de_vehic_1ZE,
         P_vol_sansviol_1ZE=Prop_atteintes_vols_sansviol_1ZE,
         P_vol_viol_sansarme_1ZE=Prop_atteintes_vols_violants_sansarme_1ZE)
         
tab_prop_del_1ZE_reg_acp_df <- as.data.frame(tab_prop_del_1ZE_reg_acp)

# on transforme la variable "LIBELLE" en row.names:
rownames(tab_prop_del_1ZE_reg_acp_df) <- tab_prop_del_1ZE_reg_acp_df[,1]
tab_prop_del_1ZE_reg_acp_df[,1] <- NULL

summary(tab_prop_del_1ZE_reg_acp_df)

res_acp_del_1ZE_reg <- PCA(tab_prop_del_1ZE_reg_acp_df, scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(res_acp_del_1ZE_reg)

dimdesc(res_acp_del_1ZE_reg)

plot(res_acp_del_1ZE_reg,select="cos2 0.7")

res = PCAshiny(tab_prop_del_1ZE_reg_acp_df)

# b) On sélectionne les variables de la forme "Prop_type_atteinte_1ZE" et "Prop_type_atteinte_pop"
# (proportion d'atteintes pour 1000 habitants) comme variables actives:

tab_prop_del_1ZE_reg_acp2 <- del2016_2021_tb_reg %>% select(LIBELLE,ends_with("1ZE") & starts_with("Prop"),ends_with("pop") & starts_with("Prop")) %>% select(-Prop_atteintes_1ZE,-Prop_atteintes_pop)

# Comme les libellés des variables sont très longs et risquent de mal s'afficher sur le cercle de 
# corrélation, on renomme les variables actives avec des libellés plus courts:
tab_prop_del_1ZE_reg_acp2 <- tab_prop_del_1ZE_reg_acp2 %>% 
                             rename(P_cambr_1ZE=Prop_atteintes_cambr_1ZE,
                                    P_bless_fam_1ZE=Prop_atteintes_blessures_famil_1ZE,
                                    P_bless_hfam_1ZE=Prop_atteintes_blessures_horsfamil_1ZE,
                                    P_destr_degrad_1ZE=Prop_atteintes_destr_degrad_1ZE,
                                    P_homic_1ZE=Prop_atteintes_homic_1ZE,
                                    P_viol_sex_1ZE=Prop_atteintes_viol_sex_1ZE,
                                    P_vol_armes_1ZE=Prop_atteintes_vols_armes_1ZE,
                                    P_vol_access_veh_1ZE=Prop_atteintes_vols_acces_vehic_1ZE,
                                    P_vol_ds_veh_1ZE=Prop_atteintes_vols_ds_vehic_1ZE,
                                    P_vol_de_veh_1ZE=Prop_atteintes_vols_de_vehic_1ZE,
                                    P_vol_sansviol_1ZE=Prop_atteintes_vols_sansviol_1ZE,
                                    P_vol_viol_sansarme_1ZE=Prop_atteintes_vols_violants_sansarme_1ZE,
                                    P_cambr_pop=Prop_atteintes_cambr_pop,
                                    P_bless_fam_pop=Prop_atteintes_blessures_famil_pop,
                                    P_bless_hfam_pop=Prop_atteintes_blessures_horsfamil_pop,
                                    P_destr_degrad_pop=Prop_atteintes_destr_degrad_pop,
                                    P_homic_pop=Prop_atteintes_homic_pop,
                                    P_viol_sex_pop=Prop_atteintes_viol_sex_pop,
                                    P_vol_armes_pop=Prop_atteintes_vols_armes_pop,
                                    P_vol_access_veh_pop=Prop_atteintes_vols_acces_vehic_pop,
                                    P_vol_ds_veh_pop=Prop_atteintes_vols_ds_vehic_pop,
                                    P_vol_de_veh_pop=Prop_atteintes_vols_de_vehic_pop,
                                    P_vol_sansviol_pop=Prop_atteintes_vols_sansviol_pop,
                                    P_vol_viol_sansarme_pop=Prop_atteintes_vols_violants_sansarme_pop
                                    )

tab_prop_del_1ZE_reg_acp2_df <- as.data.frame(tab_prop_del_1ZE_reg_acp2)

# on transforme la variable "LIBELLE" en row.names:
rownames(tab_prop_del_1ZE_reg_acp2_df) <- tab_prop_del_1ZE_reg_acp2_df[,1]
tab_prop_del_1ZE_reg_acp2_df[,1] <- NULL

summary(tab_prop_del_1ZE_reg_acp2_df)

res_acp_del_1ZE_reg2 <- PCA(tab_prop_del_1ZE_reg_acp2_df, scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(res_acp_del_1ZE_reg)

dimdesc(res_acp_del_1ZE_reg)

plot(res_acp_del_1ZE_reg,select="cos2 0.7")

res = PCAshiny(tab_prop_del_1ZE_reg_acp2_df)

# c) On sélectionne les variables de la forme "Prop_type_atteinte_1ZE" et "Prop_type_atteinte_pop"
# (proportion d'atteintes pour 1000 habitants) comme variables actives et on rajoute des variables
# illustratives pour mieux interpréter les axes (caractéristiques socio-démo-éco des régions):

# Ajout des variables supplémentaires suivantes:
# densité de la population (nb d'habitants au km2)
# indicateur de dispersion des revenus des ménages/niveaux de vie (écart inter-décile, indice de Gini...)
# liste à compléter (TOTO!)




# B) ACP sur les régions (individus) et les proportions d'atteintes (pour un type d'atteinte donné)
# dont le triplet de lieux s'inscrit dans un même zonage (on balaie ici les différents types de zonage):






