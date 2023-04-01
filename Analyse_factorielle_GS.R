
# Analyse factorielle:

library(FactoMineR)
library(factoextra)
library(FactoInvestigate)

# Je ne peux charger sur mon ordi le package "Factoshiny"!

test2_df <- as.data.frame(test2)

# on sélectionne les variables qui nous intéressent dans le dataframe:
test2_df_acp <-subset(test2_df,select=c('cog_com_22_inf','Prop_atteintes_cambr_1ZE','Prop_atteintes_blessures_famil_1ZE',
                                        'Prop_atteintes_blessures_horsfamil_1ZE','Prop_atteintes_destr_degrad_1ZE',
                                        'Prop_atteintes_viol_sex_1ZE','Prop_atteintes_vols_armes_1ZE',
                                        'Prop_atteintes_vols_acces_vehic_1ZE','Prop_atteintes_vols_ds_vehic_1ZE',
                                        'Prop_atteintes_vols_de_vehic_1ZE','Prop_atteintes_vols_sansviol_1ZE',
                                        'Prop_atteintes_vols_violants_sansarme_1ZE','REG','DEP'))
  
# on ne retient que les communes pour lesquelles les différentes proportions ne sont pas des Nan:
test2_df_acp <- na.omit(test2_df_acp)

# on transforme la variable "cog_com_22_inf" en row.names:
rownames(test2_df_acp) <- test2_df_acp[,1]
test2_df_acp[,1] <- NULL

resultats_acp <- PCA(test2_df_acp[,1:11], scale.unit = FALSE, ncp = 5, graph = TRUE)
resultats_acp

# On ne voit pas grand chose dans le nuage d'individus (1879 individus c'est beaucoup!!)

# On peut réduire à un département: le 92 par exemple..., au hasard!

test2_df_acp_93 <-subset(test2_df_acp,DEP == "93")

resultats_acp_93 <- PCA(test2_df_acp_93[,1:11], scale.unit = FALSE, ncp = 5, graph = TRUE)
resultats_acp_93

# Autre stratégie: on fait une ACP au niveau départemental:

# On commence par procéder à l'agrégation par département:

test3_dep <- test2[ , .(
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
  Nb_atteintes_blessures_famil_1BV = sum(Nb_atteintes_cambr_1BV, na.rm = TRUE),
  Nb_atteintes_blessures_horsfamil_1BV = sum(Nb_atteintes_blessures_horsfamil_1BV, na.rm = TRUE),
  Nb_atteintes_destr_degrad_1BV = sum(Nb_atteintes_destr_degrad_1BV, na.rm = TRUE),
  Nb_atteintes_homic_1BV = sum(Nb_atteintes_homic_1BV, na.rm = TRUE),
  Nb_atteintes_viol_sex_1BV = sum(Nb_atteintes_viol_sex_1BV, na.rm = TRUE),
  Nb_atteintes_vols_armes_1BV = sum(Nb_atteintes_vols_armes_1BV, na.rm = TRUE),
  Nb_atteintes_vols_acces_vehic_1BV = sum(Nb_atteintes_vols_acces_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_ds_vehic_1BV = sum(Nb_atteintes_vols_ds_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_de_vehic_1BV = sum(Nb_atteintes_vols_de_vehic_1BV, na.rm = TRUE),
  Nb_atteintes_vols_sansviol_1BV = sum(Nb_atteintes_vols_sansviol_1BV, na.rm = TRUE),
  Nb_atteintes_vols_violants_sansarme_1BV = sum(Nb_atteintes_vols_violants_sansarme_1BV, na.rm = TRUE)),
  by = .(DEP)]
head(test3_dep)

# On calcule pour chaque département et chaque type d'atteinte: la proportion (en %) d'atteintes dont le triplet de lieux
# (inf,vict,mec) s'inscrit dans un même zonage z (z= ZE ou BV pour les zonages statistiques et z=COM ou DEP ou REG pour les
# zonages administratifs)

# a) Proportion d'atteintes dont le triplet de lieu s'inscrit dans une même ZE:
test3_dep$Prop_atteintes_1ZE <-test3_dep$Nb_atteintes_1ZE/test3_dep$Nb_atteintes*100
test3_dep$Prop_cambr_1ZE <-test3_dep$Nb_atteintes_cambr_1ZE/test3_dep$Nb_atteintes_cambr*100
test3_dep$Prop_blessures_famil_1ZE <-test3_dep$Nb_atteintes_blessures_famil_1ZE/test3_dep$Nb_atteintes_blessures_famil*100
test3_dep$Prop_blessures_horsfamil_1ZE <-test3_dep$Nb_atteintes_blessures_horsfamil_1ZE/test3_dep$Nb_atteintes_blessures_horsfamil*100
test3_dep$Prop_destr_degrad_1ZE <-test3_dep$Nb_atteintes_destr_degrad_1ZE/test3_dep$Nb_atteintes_destr_degrad*100
test3_dep$Prop_homic_1ZE <-test3_dep$Nb_atteintes_homic_1ZE/test3_dep$Nb_atteintes_homic*100
test3_dep$Prop_viol_sex_1ZE <-test3_dep$Nb_atteintes_viol_sex_1ZE/test3_dep$Nb_atteintes_viol_sex*100
test3_dep$Prop_vols_armes_1ZE <-test3_dep$Nb_atteintes_vols_armes_1ZE/test3_dep$Nb_atteintes_vols_armes*100
test3_dep$Prop_vols_acces_vehic_1ZE <-test3_dep$Nb_atteintes_vols_acces_vehic_1ZE/test3_dep$Nb_atteintes_vols_acces_vehic*100
test3_dep$Prop_vols_ds_vehic_1ZE <-test3_dep$Nb_atteintes_vols_ds_vehic_1ZE/test3_dep$Nb_atteintes_vols_ds_vehic*100
test3_dep$Prop_vols_de_vehic_1ZE <-test3_dep$Nb_atteintes_vols_de_vehic_1ZE/test3_dep$Nb_atteintes_vols_de_vehic*100
test3_dep$Prop_vols_sansviol_1ZE <-test3_dep$Nb_atteintes_vols_sansviol_1ZE/test3_dep$Nb_atteintes_vols_sansviol*100
test3_dep$Prop_vols_violants_sansarme_1ZE <-test3_dep$Nb_atteintes_vols_violants_sansarme_1ZE/test3_dep$Nb_atteintes_vols_violants_sansarme*100

# b) Proportion d'atteintes dont le triplet de lieu s'inscrit dans un même BV:
test3_dep$Prop_atteintes_1BV <-test3_dep$Nb_atteintes_1BV/test3_dep$Nb_atteintes*100
test3_dep$Prop_cambr_1BV <-test3_dep$Nb_atteintes_cambr_1BV/test3_dep$Nb_atteintes_cambr*100
test3_dep$Prop_blessures_famil_1BV <-test3_dep$Nb_atteintes_blessures_famil_1BV/test3_dep$Nb_atteintes_blessures_famil*100
test3_dep$Prop_blessures_horsfamil_1BV <-test3_dep$Nb_atteintes_blessures_horsfamil_1BV/test3_dep$Nb_atteintes_blessures_horsfamil*100
test3_dep$Prop_destr_degrad_1BV <-test3_dep$Nb_atteintes_destr_degrad_1BV/test3_dep$Nb_atteintes_destr_degrad*100
test3_dep$Prop_homic_1BV <-test3_dep$Nb_atteintes_homic_1BV/test3_dep$Nb_atteintes_homic*100
test3_dep$Prop_viol_sex_1BV <-test3_dep$Nb_atteintes_viol_sex_1BV/test3_dep$Nb_atteintes_viol_sex*100
test3_dep$Prop_vols_armes_1BV <-test3_dep$Nb_atteintes_vols_armes_1BV/test3_dep$Nb_atteintes_vols_armes*100
test3_dep$Prop_vols_acces_vehic_1BV <-test3_dep$Nb_atteintes_vols_acces_vehic_1BV/test3_dep$Nb_atteintes_vols_acces_vehic*100
test3_dep$Prop_vols_ds_vehic_1BV <-test3_dep$Nb_atteintes_vols_ds_vehic_1BV/test3_dep$Nb_atteintes_vols_ds_vehic*100
test3_dep$Prop_vols_de_vehic_1BV <-test3_dep$Nb_atteintes_vols_de_vehic_1BV/test3_dep$Nb_atteintes_vols_de_vehic*100
test3_dep$Prop_vols_sansviol_1BV <-test3_dep$Nb_atteintes_vols_sansviol_1BV/test3_dep$Nb_atteintes_vols_sansviol*100
test3_dep$Prop_vols_violants_sansarme_1BV <-test3_dep$Nb_atteintes_vols_violants_sansarme_1BV/test3_dep$Nb_atteintes_vols_violants_sansarme*100

head(test3_dep)

test3_dep_df <- as.data.frame(test3_dep)

# on sélectionne les variables qui nous intéressent dans le dataframe:
test3_dep_df_acp <-subset(test3_dep_df,select=c('DEP','Prop_atteintes_1ZE','Prop_cambr_1ZE','Prop_blessures_famil_1ZE',
                                        'Prop_blessures_horsfamil_1ZE','Prop_destr_degrad_1ZE',
                                        'Prop_viol_sex_1ZE','Prop_vols_armes_1ZE',
                                        'Prop_vols_acces_vehic_1ZE','Prop_vols_ds_vehic_1ZE',
                                        'Prop_vols_de_vehic_1ZE','Prop_vols_sansviol_1ZE',
                                        'Prop_vols_violants_sansarme_1ZE'))

# on ne retient que les départements pour lesquels aucune proportion ne sont des NA:
test3_dep_df_acp <- na.omit(test3_dep_df_acp)

# on réagrège au niveau départemental certaines variables socio-démo-économiques données par l'Insee au niveau communal:
infos_dep_dt <- infos_communes_dt[ , .(
  P19_POP = sum(P19_POP, na.rm = TRUE),
  SUPERF = sum(SUPERF, na.rm = TRUE),
  P19_LOG = sum(P19_LOG, na.rm = TRUE),
  P19_RSECOCC = sum(P19_RSECOCC, na.rm = TRUE),
  MED20 = sum(MED20, na.rm = TRUE),
  P19_ACT1564 = sum(P19_ACT1564, na.rm = TRUE),
  P19_CHOMEUR1564 = sum(P19_CHOMEUR1564, na.rm = TRUE),
  P19_POP1564 = sum(P19_POP1564, na.rm = TRUE)
  ),
  by = .(DEP)]

head(infos_dep_dt)

test3_dep_df_acp <- 
  merge(x = test3_dep_df_acp,
        y = infos_dep_dt,
        by.x = "DEP",
        by.y = "DEP",
        all.x = TRUE)
head(test3_dep_df_acp)

# Il semble que l'on n'ait pas toutes les infos sur les DROM (on décide de les exclure ici de l'analyse):

test3_dep_df_acp <-subset(test3_dep_df_acp,DEP !="971" & DEP !="972" & DEP !="973" & DEP != "974" & DEP != "976")

# on transforme la variable "DEP" en row.names:
rownames(test3_dep_df_acp) <- test3_dep_df_acp[,1]
test3_dep_df_acp[,1] <- NULL

summary(test3_dep_df_acp)

resultats_acp_dep <- PCA(test3_dep_df_acp[,2:11], scale.unit = FALSE, ncp = 5, graph = TRUE)
resultats_acp_dep

