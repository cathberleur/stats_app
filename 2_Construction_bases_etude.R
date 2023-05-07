
# Construction des bases de données en vue de l'analyse:

# Inputs: les différentes sources de données mentionnées dans le script R "1_Chargement_data.R".

# Outputs: 3 bases permettant d'analyser la délinquance dans la dimension spatiale à 3 niveaux:
# l'atteinte; la commune; le département.

# 1- La base "atteintes" -> observations: les atteintes.

# 2- La base "delinquance_com" -> observations: les communes.

# 3- La base "delinquance_dep" -> observations: les départements.

###############################################################################################

# 1- Construction de la base "" (analyse au niveau de l'atteinte.)

# Objectif: on souhaite ici relier chaque atteinte caractérisée par le triplet de communes
# (cog_com22_inf,cog_com22_vict,cog_com22_mec) aux différents zonages d'étude correspondant à 
# ces trois communes

# a) On renomme les variables du fichier "communes_zonages_dt" selon les 3 lieux associés à l'atteinte:
communes_zonages_dt_inf <- communes_zonages_dt
names(communes_zonages_dt_inf) <-paste0(names(communes_zonages_dt_inf),"_inf")

communes_zonages_dt_vict <- communes_zonages_dt
names(communes_zonages_dt_vict) <-paste0(names(communes_zonages_dt_vict),"_vict")

communes_zonages_dt_mec <- communes_zonages_dt
names(communes_zonages_dt_mec) <-paste0(names(communes_zonages_dt_mec),"_mec")

# b) On renomme les variables du fichier "communes_grille_densite_dt" selon les 3 lieux associés à l'atteinte:
communes_grille_densite_dt_inf <- communes_grille_densite_dt
names(communes_grille_densite_dt_inf) <-paste0(names(communes_grille_densite_dt_inf),"_inf")

communes_grille_densite_dt_vict <- communes_grille_densite_dt
names(communes_grille_densite_dt_vict) <-paste0(names(communes_grille_densite_dt_vict),"_vict")

communes_grille_densite_dt_mec <- communes_grille_densite_dt
names(communes_grille_densite_dt_mec) <-paste0(names(communes_grille_densite_dt_mec),"_mec")

# c) On renomme les variables du fichier "communes_centralite_dt" selon les 3 lieux associés à l'atteinte:
communes_centralites_dt_inf <- communes_centralites_dt
names(communes_centralites_dt_inf) <-paste0(names(communes_centralites_dt_inf),"_inf")

communes_centralites_dt_vict <- communes_centralites_dt
names(communes_centralites_dt_vict) <-paste0(names(communes_centralites_dt_vict),"_vict")

communes_centralites_dt_mec <- communes_centralites_dt
names(communes_centralites_dt_mec) <-paste0(names(communes_centralites_dt_mec),"_mec")



# On apparie le fichier du SSMSI (atteintes géolocalisées en France entre 2016 et 2021) 
# avec les fichiers relatifs aux zonages
# Pour chaque fichier, l'appariement s'effectue via 3 merges du type "left-join" en prenant successivement comme clé
# de jointure les 3 codes communes associés à chaque atteinte (commune du lieu de l'infraction, commune de la victime,
# commune du mis en cause):

# Appariement avec les fichiers "communes_zonages_dt":

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
# la grille de densité:

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
# le zonage de l'INRAE sur les centralités:

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
atteintes <- 
  merge(x = del2016_2021_dt9,
        y = communes_centralites_dt_mec,
        by.x = "cog_com_22_mec",
        by.y = "DC_mec",
        all.x = TRUE)
rm(del2016_2021_dt9)

# Création de variables supplémentaires utiles pour l'analyse:

# I) Recodage de la variable "classe" donnant le type d'atteinte: 
# TRES IMPORTANT!!! ce recodage ne sera plus nécessaire lorsque le code tournera sur les "vraies" données au SSMSI !!!
# @Kevin et Aurélien: il faudra commenter ce bout de code avant de faire tourner sur les vraies bases!
# Pour rappel, ce recodage résulte juste ici d'une "imputation" des différentes modalités de la variable "classe" en 
# comparant les fréquences des atteintes calculées à partir de la base secrétisée et celles calculées à partir de la
# base non secrétisée et en faisant l'hypothèse qu'une atteinte qui a le même rang en termes de fréquence dans les 2 bases.
# Donc il se peut très bien qu'il y ait des erreurs de reclassement ci-dessous!!

atteintes[ , classe := data.table::fcase(
  classe=="W", "Vols sans violence contre des personnes",
  classe=="K", "Destructions et dégradations",
  classe=="R","Vols dans les véhicules",
  classe=="B","Cambriolages de logement",
  classe=="X","Vols de véhicules",
  classe=="T","Coups et blessures volontaires en dehors de la sphère familiale", 
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

atteintes[ , a_3_memes_communes := data.table::fcase(
  (cog_com_22_inf == cog_com_22_vict) & (cog_com_22_vict == cog_com_22_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même département ou pas:

atteintes[ , a_3_memes_dep := data.table::fcase(
  (DEP_inf == DEP_vict) & (DEP_vict == DEP_mec), "oui",
  default ="non")
]

# c) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même région ou pas:

atteintes[ , a_3_memes_reg := data.table::fcase(
  (REG_inf == REG_vict) & (REG_vict == REG_mec), "oui",
  default ="non")
]

# d) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans la même EPCI (Intercommunalité/Métropole)
# ou pas:

atteintes[ , a_3_memes_epci := data.table::fcase(
  (EPCI_inf == EPCI_vict) & (EPCI_vict == EPCI_mec), "oui",
  default ="non")
]

# e) pour chaque atteinte, on teste si les 3 lieux s'inscrivent dans le même CV (Canton/Ville)
# ou pas:

atteintes[ , a_3_memes_cv := data.table::fcase(
  (CV_inf == CV_vict) & (CV_vict == CV_mec), "oui",
  default ="non")
]


# 2) Zonages statistiques:

# a) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans la même zone d'emploi (ZE) ou pas:

atteintes[ , IV_ds_meme_ZE := data.table::fcase(
(is.na(ZE2020_inf)==FALSE) &  (ZE2020_inf == ZE2020_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_ZE := data.table::fcase(
(is.na(ZE2020_inf)==FALSE) &  (ZE2020_inf == ZE2020_vict) & (ZE2020_vict == ZE2020_mec), "oui",
  default ="non")
]

# b) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans le même bassin de vie (BV) ou pas:

atteintes[ , IV_ds_meme_BV := data.table::fcase(
(is.na(BV2022_inf)==FALSE) &  (BV2022_inf == BV2022_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_BV := data.table::fcase(
(is.na(BV2022_inf)==FALSE) & (BV2022_inf == BV2022_vict) & (BV2022_vict == BV2022_mec), "oui",
  default ="non")
]

# c) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans la même unité urbaine (UU) ou pas:

atteintes[ , IV_ds_meme_UU := data.table::fcase(
  !(substr(UU2020_inf,3,5) =="000") & (is.na(UU2020_inf)==FALSE) & (UU2020_inf == UU2020_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_UU := data.table::fcase(
  !(substr(UU2020_inf,3,5) =="000") & (is.na(UU2020_inf)==FALSE) & (UU2020_inf == UU2020_vict) & (UU2020_vict == UU2020_mec), "oui",
  default ="non")
]
# note: pour éviter de classer en "oui" des atteintes dont les 3 communes seraient tous classées en XX000
# (i.e hors UU), on rajoute la condition qui assure que l'infraction ait bien eu lieu dans une UU. 
# En effet, le zonage UU ne constitue pas une partition du territoire français (il y a donc une classe résiduelle "hors UU").

# d) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans la même aire d'attraction
#des villes (AAV) ou pas:

atteintes[ , IV_ds_meme_AAV := data.table::fcase(
  !(AAV2020_inf =="000") & (is.na(AAV2020_inf)==FALSE) & (AAV2020_inf == AAV2020_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_AAV := data.table::fcase(
  !(AAV2020_inf =="000") & (is.na(AAV2020_inf)==FALSE) & (AAV2020_inf == AAV2020_vict) & (AAV2020_vict == AAV2020_mec), "oui",
  default ="non")
]
# note: pour éviter de classer en "oui" des atteintes dont les 3 communes seraient tous classées en 000
# (i.e hors AAV), on rajoute la condition qui assure que l'infraction ait bien eu lieu dans une AAV. 
# En effet, le zonage AAV ne constitue pas une partition du territoire français (il y a donc une classe résiduelle "hors AAV").

# e) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans la même grille de densité 
# (mix de la grille communale de densité et de AAV) ou pas:

atteintes[ , IV_ds_meme_GD := data.table::fcase(
  (is.na(TYPE_inf)==FALSE) & (TYPE_inf == TYPE_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_GD := data.table::fcase(
  (is.na(TYPE_inf)==FALSE) & (TYPE_inf == TYPE_vict) & (TYPE_vict == TYPE_mec), "oui",
  default ="non")
]

# f) pour chaque atteinte, on teste si le couple (I,V) / le triplet de communes (I,V,M) s'inscrit dans le même niveau de centralité 
# de la commune (cf. étude de l'INRAE sur les centralités) ou pas:

atteintes[ , IV_ds_meme_CENTR := data.table::fcase(
  !(P_NP5CLA_inf =="DCN0") & (is.na(P_NP5CLA_inf)==FALSE) & (P_NP5CLA_inf == P_NP5CLA_vict), "oui",
  default ="non")
]

atteintes[ , IVM_ds_meme_CENTR := data.table::fcase(
  !(P_NP5CLA_inf =="DCN0") & (is.na(P_NP5CLA_inf)==FALSE) & (P_NP5CLA_inf == P_NP5CLA_vict) & (P_NP5CLA_vict == P_NP5CLA_mec), "oui",
  default ="non")
]
# note: pour éviter de classer en "oui" des atteintes dont les 3 communes seraient tous classées en 000
# (i.e hors CENTRALITE), on rajoute la condition qui assure que l'infraction ait bien eu lieu dans une CENTRALITE 
# En effet, le zonage CENTRALITE ne constitue pas une partition du territoire français (il y a donc une classe résiduelle "hors CENTR").


# Autres indicatrices utiles:

# on teste si la commune de l'infraction se situe dans une UU: 
atteintes[ , I_ds_UU := data.table::fcase(
  !(substr(UU2020_inf,3,5) =="000") & (is.na(UU2020_inf)==FALSE), "oui",
  default ="non")
]

# on teste si la commune de la victime se situe dans une UU: 
atteintes[ , V_ds_UU := data.table::fcase(
  !(substr(UU2020_vict,3,5) =="000") & (is.na(UU2020_vict)==FALSE), "oui",
  default ="non")
]

# on teste si la commune du mis en cause se situe dans une UU: 
atteintes[ , M_ds_UU := data.table::fcase(
  !(substr(UU2020_mec,3,5) =="000") & (is.na(UU2020_mec)==FALSE), "oui",
  default ="non")
]


# on teste si la commune de l'infraction se situe dans une AAV: 
atteintes[ , I_ds_AAV := data.table::fcase(
  !(AAV2020_inf =="000") & (is.na(AAV2020_inf)==FALSE), "oui",
  default ="non")
]

# on teste si la commune de la victime se situe dans une AAV: 
atteintes[ , V_ds_AAV := data.table::fcase(
  !(AAV2020_vict =="000") & (is.na(AAV2020_vict)==FALSE), "oui",
  default ="non")
]

# on teste si la commune du mis en cause se situe dans une AAV: 
atteintes[ , M_ds_AAV := data.table::fcase(
  !(AAV2020_mec =="000") & (is.na(AAV2020_mec)==FALSE), "oui",
  default ="non")
]

# on teste si la commune de l'infraction se situe dans une centralité: 
atteintes[ , I_ds_CENTR := data.table::fcase(
  !(P_NP5CLA_inf == "DCN0") & (is.na(P_NP5CLA_inf)==FALSE), "oui",
  default ="non")
]

# on teste si la commune de la victime se situe dans une centralité: 
atteintes[ , V_ds_CENTR := data.table::fcase(
  !(P_NP5CLA_vict == "DCN0") & (is.na(P_NP5CLA_vict)==FALSE), "oui",
  default ="non")
]

# on teste si la commune du mis en cause se situe dans une centralité: 
atteintes[ , M_ds_CENTR := data.table::fcase(
  !(P_NP5CLA_mec == "DCN0") & (is.na(P_NP5CLA_mec)==FALSE), "oui",
  default ="non")
]


# 3) Autres variables utiles pour l'analyse:

# on crée un compteur:
atteintes$compteur <-1
# Autre syntaxe probablement plus rapide:
# atteintes[, compteur := 1]

# on ajoute l'information relative à la distance "à vol d'oiseau" entre les communes I, V et M pour chaque atteinte:
del.dist_dt <- as.data.table(t.del.dist)

atteintes <- 
  merge(x = atteintes,
        y = del.dist_dt,
        by.x = "id",
        by.y = "id",
        all.x = TRUE)

atteintes <- atteintes[order(cog_com_22_inf, annee)]

names(atteintes)

# Output: la base "atteintes" est ainsi générée sous la forme d'un data.table de 6 455 519 lignes et 107 variables.
# Il sera possible ensuite de le transformer en tibble pour certaines analyses statistiques.

# 2- Construction de la base "delinquance_com" (analyse au niveau de la commune.)

# Objectif: on souhaite ici disposer d'une base de la délinquance au niveau communal.
# On souhaite disposer pour chaque commune, du nombre d'infractions (I), de victimes (V) et de mis en cause (M). 
# On souhaite disposer pour chaque commune du volume de la délinquance (en absolu et en relatif "pour 1000 hbts") pour 
# les différents types d'atteinte;
# On souhaite disposer pour chaque commune de la structure de la délinquance par type d'atteinte;
# On souhaite avoir pour chaque commune la proportion d'atteintes (d'un type donné) associées à un couple de communes
# (infraction, victime) relevant d'un même zonage d'étude.
# On souhaite avoir pour chaque commune la proportion d'atteintes corporelles (d'un type donné) associées à un triplet
# de communes (infraction, victime, mis en cause) relevant d'un même zonage d'étude.
# On souhaite disposer d'une série d'indicateurs démographiques et socio-économiques caractérisant chaque commune.
# On souhaite caractériser le positionnement de la commune au sein de chaque zonage.

# Agrégation des atteintes par commune de l'infraction: calcul du nombre d'infractions (I) dans chaque commune,
# selon le type de l'atteinte.
  nb_I_com <- atteintes[(is.na(cog_com_22_inf)==FALSE), .(
  Nb_I = sum(compteur, na.rm = TRUE),
  Nb_I_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
  Nb_I_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_I_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_I_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
  Nb_I_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
  Nb_I_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
  Nb_I_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
  Nb_I_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_I_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_I_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
  Nb_I_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# Agrégation des atteintes par commune de domiciliation de la victime: calcul du nombre de victimes (V) dans chaque commune,
# selon le type de l'atteinte.
  nb_V_com <- atteintes[(is.na(cog_com_22_vict)==FALSE), .(
  Nb_V = sum(compteur, na.rm = TRUE),
  Nb_V_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
  Nb_V_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_V_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_V_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
  Nb_V_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
  Nb_V_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
  Nb_V_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
  Nb_V_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_V_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_V_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
  Nb_V_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_V_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
  by = .(cog_com_22_vict)]

# Agrégation des atteintes par commune de domiciliation du mis en cause: calcul du nombre de mis en cause (M) dans
#chaque commune, selon le type de l'atteinte.
  nb_M_com <- atteintes[(is.na(cog_com_22_mec)==FALSE), .(
  Nb_M = sum(compteur, na.rm = TRUE),
  Nb_M_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
  Nb_M_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
  Nb_M_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
  Nb_M_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
  Nb_M_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
  Nb_M_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
  Nb_M_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
  Nb_M_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
  Nb_M_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
  Nb_M_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
  Nb_M_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
  Nb_M_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
  by = .(cog_com_22_mec)]

# Agrégation des atteintes par commune de l'infraction: calcul du nombre d'infractions (I) dans chaque commune,
# associées à un couple de communes (I,V) inscrites dans un même zonage d'étude.

# Warning: on se restreint ici aux seules atteintes associées à un couple de communes (I,V) renseigné!

  nb_I_IV_meme_zonage <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  # Nombre d'infractions avec un couple de communes (I,V) dans la même zone d'emploi (ZE):
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1ZE = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1ZE = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1ZE = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1ZE = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1ZE = sum(compteur*(classe == "Homicides" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1ZE = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1ZE = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1ZE = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1ZE = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1ZE = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1ZE = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1ZE = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un couple de communes (I,V) dans le même bassin de vie (BV):
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1BV = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1BV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1BV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1BV = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1BV = sum(compteur*(classe == "Homicides" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1BV = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1BV = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1BV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1BV = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1BV = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1BV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1BV = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_BV == "oui"), na.rm = TRUE),

  # Nombre d'infractions avec un couple de communes (I,V) dans la même grille de densité (GD):
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1GD = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1GD = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1GD = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1GD = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1GD = sum(compteur*(classe == "Homicides" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1GD = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1GD = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1GD = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1GD = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1GD = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1GD = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1GD = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un couple de communes (I,V) dans la même unité urbaine (UU):
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1UU = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1UU = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1UU = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1UU = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1UU = sum(compteur*(classe == "Homicides" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1UU = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1UU = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1UU = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1UU = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1UU = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1UU = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1UU = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un couple de communes (I,V) dans la même aire d'attraction des villes (AAV):
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1AAV = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1AAV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1AAV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1AAV = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1AAV = sum(compteur*(classe == "Homicides" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1AAV = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1AAV = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1AAV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1AAV = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1AAV = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1AAV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1AAV = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),

  # Nombre d'infractions avec un couple de communes (I,V) dans la même centralité (CENTR):
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_cambr_IV_1CENTR = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IV_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IV_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IV_1CENTR = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_homic_IV_1CENTR = sum(compteur*(classe == "Homicides" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IV_1CENTR = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IV_1CENTR = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IV_1CENTR = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IV_1CENTR = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IV_1CENTR = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IV_1CENTR = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IV_1CENTR = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# Agrégation des atteintes par commune de l'infraction: calcul du nombre d'infractions (I) dans chaque commune,
# associées à un triplet de communes (I,V,M) inscrites dans un même zonage d'étude.

# Warning: on se restreint ici aux seules atteintes associées à un triplet de communes (I,V,M) renseignées!
# On se limite ici aux seules atteintes corporelles, celles pour lesquelles la commmune du mis en cause
# est le mieux renseigné.

nb_I_IVM_meme_zonage <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                                            (is.na(cog_com_22_mec)==FALSE), .(
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même zone d'emploi (ZE):
  Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1ZE = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1ZE = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1ZE = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1ZE = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1ZE = sum(compteur*(classe == "Homicides" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1ZE = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1ZE = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1ZE = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1ZE = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1ZE = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1ZE = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1ZE = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans le même bassin de vie (BV):
  Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1BV = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1BV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1BV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1BV = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1BV = sum(compteur*(classe == "Homicides" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1BV = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1BV = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1BV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1BV = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1BV = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1BV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1BV = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même grille de densité (GD):
  Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1GD = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1GD = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1GD = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1GD = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1GD = sum(compteur*(classe == "Homicides" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1GD = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1GD = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1GD = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1GD = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1GD = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1GD = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1GD = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même unité urbaine (UU):
  Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1UU = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1UU = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1UU = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1UU = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1UU = sum(compteur*(classe == "Homicides" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1UU = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1UU = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1UU = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1UU = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1UU = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1UU = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1UU = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même aire d'attraction des villes (AAV):
  Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1AAV = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1AAV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1AAV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1AAV = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1AAV = sum(compteur*(classe == "Homicides" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1AAV = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1AAV = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1AAV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1AAV = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1AAV = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1AAV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1AAV = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  
  # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même centralité (CENTR):
  Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_cambr_IVM_1CENTR = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_bless_famil_IVM_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_bless_horsfamil_IVM_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_destr_degrad_IVM_1CENTR = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_homic_IVM_1CENTR = sum(compteur*(classe == "Homicides" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_viol_sex_IVM_1CENTR = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_armes_IVM_1CENTR = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_acces_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_ds_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_de_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_sansviol_IVM_1CENTR = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
  Nb_I_vols_viol_sansarme_IVM_1CENTR = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by = .(cog_com_22_inf)]

# TODO !!!
# Il restera à dénombrer les infractions selon le statut de la commune de I, celle de V (et celle de M pour les atteintes
# corpo) au sein du zonage d'étude (lorsque (I,V) ou (I,V,M) s'inscrit dans un même zonage)
# Cas à traiter:
# 1) Statut de la commune au sein d'un bassin de vie: modalités 11- Pôle partiel/12- Commune associée à un pôle partiel/20- Pôle
# 2) Statut de la commune au sein d'une aire d'attraction des villes (AAV): 11- Commune centre/12- Autre commune du pôle principal
# /13- Commune d'un pôle secondaire/20- Commune de la couronne
# 3) Statut de la commune dans la grille de densité: urbain dense/urbain moyennement dense/...

# On rassemble toutes ces bases en une unique base communale:
delinquance_com <- 
  merge(x = nb_I_com,
        y = nb_V_com,
        by.x = "cog_com_22_inf",
        by.y = "cog_com_22_vict",
        all.x = TRUE)
delinquance_com <- 
  merge(x = delinquance_com,
        y = nb_M_com,
        by.x = "cog_com_22_inf",
        by.y = "cog_com_22_mec",
        all.x = TRUE)
delinquance_com <- 
  merge(x = delinquance_com,
        y = nb_I_IV_meme_zonage,
        by.x = "cog_com_22_inf",
        by.y = "cog_com_22_inf",
        all.x = TRUE)
delinquance_com <- 
  merge(x = delinquance_com,
        y = nb_I_IVM_meme_zonage,
        by.x = "cog_com_22_inf",
        by.y = "cog_com_22_inf",
        all.x = TRUE)

# On apparie la base à celle des principaux indicateurs démographiques et socio-économiques (issus du comparateur 
# des communes de l'Insee):
delinquance_com <- 
  merge(x = delinquance_com,
        y = infos_communes_dt,
        by.x = "cog_com_22_inf",
        by.y = "CODGEO",
        all.x = TRUE)

# On peut transformer cette data.table (de "seulement" 35 000 lignes) en tibble pour travailler avec les fonctions
# de tidyverse.

delinquance_com <- as_tibble(delinquance_com)

# On apparie la base avec une sélection d'indicateurs pertinents issus du dossier complet sur les communes (source: Insee):

# Sélection des indicateurs pertinents:

# On récupère les variables utiles pour calculer les variables supplémentaires suivantes:
# Part des jeunes (15-29 ans) dans la population communale (en %): P19_POP1529/P19_POP
# Part des résidences de tourisme dans l'ensemble des logements de la commune (en %): RT23/P19_LOG
# Nombre des campings pour 1000 habitants: CPG23/P19_POP*1000
# Rapport interdécile de niveau de vie de la commune (D9/D1): RD20 = D920/D120
# Part des hôtels dans l'ensemble des logements de la commune: HT23/P19_LOG

dossier_complet_insee2 <- dossier_complet_insee %>%
                         select(CODGEO,P19_POP1529,RT23,CPG23,RD20,D920,D120,HT23)

delinquance_com <- delinquance_com %>%
  left_join(y = dossier_complet_insee2, 
            by = c("cog_com_22_inf" = "CODGEO"))


# On peut apparier le fichier obtenu avec les zonages d'étude de la commune d'infraction:

communes_zonages <- as_tibble(communes_zonages_dt)
communes_zonages <- communes_zonages %>%
                    select(CODGEO,ZE2020,UU2020,TUU2017,TDUU2017,AAV2020,
                                               TAAV2017,TDAAV2017,CATEAAV2020,BV2022,TYPE)

delinquance_com <- delinquance_com %>%
  left_join(y = communes_zonages, 
            by = c("cog_com_22_inf" = "CODGEO"))

# Pour des raisons de lisibilité, nous aurons besoin parfois de réduire le nombre de type d'atteintes à analyser:
# en passant des 12 types atteintes du fichier du SSMSI à
# 7 types d'atteintes, après réagrégation de certains types d'atteintes "proches". Dans le détail, nous avons pu 
# procéder aux regroupements d'atteintes suivants:

# 1er regroupement: "vols d'accessoires dans les véhicules", "vols de véhicules" et "vols dans les véhicules"
# -> vols en rapport avec les véhicules
# 2eme regroupement:
# "coups et blessures volontaires dans la sphère familiale","coups et blessures volontaires en dehors de la sphère familiale" et "homocides".
# -> atteintes physiques corporelles graves et directes
# 3eme regroupement: "vols violents sans arme" et les "vols violents avec arme"
# -> vols violents

delinquance_com <- delinquance_com %>%
  mutate(Nb_I_vols_vehic = Nb_I_vols_acces_vehic + Nb_I_vols_ds_vehic + 
           Nb_I_vols_de_vehic,
         Nb_I_bless_famil_homic = Nb_I_bless_famil + Nb_I_bless_horsfamil +
           Nb_I_homic,
         Nb_I_vols_violents = Nb_I_vols_armes + Nb_I_vols_viol_sansarme,
         Nb_I_vols_vehic_IV_1ZE = Nb_I_vols_acces_vehic_IV_1ZE + Nb_I_vols_ds_vehic_IV_1ZE + 
           Nb_I_vols_de_vehic_IV_1ZE,
         Nb_I_bless_famil_homic_IV_1ZE = Nb_I_bless_famil_IV_1ZE + Nb_I_bless_horsfamil_IV_1ZE +
           Nb_I_homic_IV_1ZE,
         Nb_I_vols_violents_IV_1ZE = Nb_I_vols_armes_IV_1ZE + Nb_I_vols_viol_sansarme_IV_1ZE,
         Nb_I_vols_vehic_IV_1BV = Nb_I_vols_acces_vehic_IV_1BV + Nb_I_vols_ds_vehic_IV_1BV + 
           Nb_I_vols_de_vehic_IV_1BV,
         Nb_I_bless_famil_homic_IV_1BV = Nb_I_bless_famil_IV_1BV + Nb_I_bless_horsfamil_IV_1BV +
           Nb_I_homic_IV_1BV,
         Nb_I_vols_violents_IV_1BV = Nb_I_vols_armes_IV_1BV + Nb_I_vols_viol_sansarme_IV_1BV,
         Nb_I_vols_vehic_IV_1GD = Nb_I_vols_acces_vehic_IV_1GD + Nb_I_vols_ds_vehic_IV_1GD + 
           Nb_I_vols_de_vehic_IV_1GD,
         Nb_I_bless_famil_homic_IV_1GD = Nb_I_bless_famil_IV_1GD + Nb_I_bless_horsfamil_IV_1GD +
           Nb_I_homic_IV_1GD,
         Nb_I_vols_violents_IV_1GD = Nb_I_vols_armes_IV_1GD + Nb_I_vols_viol_sansarme_IV_1GD,
         Nb_I_vols_vehic_IV_1UU = Nb_I_vols_acces_vehic_IV_1UU + Nb_I_vols_ds_vehic_IV_1UU + 
           Nb_I_vols_de_vehic_IV_1UU,
         Nb_I_bless_famil_homic_IV_1UU = Nb_I_bless_famil_IV_1UU + Nb_I_bless_horsfamil_IV_1UU +
           Nb_I_homic_IV_1UU,
         Nb_I_vols_violents_IV_1UU = Nb_I_vols_armes_IV_1UU + Nb_I_vols_viol_sansarme_IV_1UU,
         Nb_I_vols_vehic_IV_1AAV = Nb_I_vols_acces_vehic_IV_1AAV + Nb_I_vols_ds_vehic_IV_1AAV + 
           Nb_I_vols_de_vehic_IV_1AAV,
         Nb_I_bless_famil_homic_IV_1AAV = Nb_I_bless_famil_IV_1AAV + Nb_I_bless_horsfamil_IV_1AAV +
           Nb_I_homic_IV_1AAV,
         Nb_I_vols_violents_IV_1AAV = Nb_I_vols_armes_IV_1AAV + Nb_I_vols_viol_sansarme_IV_1AAV,
         Nb_I_vols_vehic_IV_1CENTR = Nb_I_vols_acces_vehic_IV_1CENTR + Nb_I_vols_ds_vehic_IV_1CENTR + 
           Nb_I_vols_de_vehic_IV_1CENTR,
         Nb_I_bless_famil_homic_IV_1CENTR = Nb_I_bless_famil_IV_1CENTR + Nb_I_bless_horsfamil_IV_1CENTR +
           Nb_I_homic_IV_1CENTR,
         Nb_I_vols_violents_IV_1CENTR = Nb_I_vols_armes_IV_1CENTR + Nb_I_vols_viol_sansarme_IV_1CENTR,
         Nb_I_vols_vehic_IVM_1ZE = Nb_I_vols_acces_vehic_IVM_1ZE + Nb_I_vols_ds_vehic_IVM_1ZE + 
           Nb_I_vols_de_vehic_IVM_1ZE,
         Nb_I_bless_famil_homic_IVM_1ZE = Nb_I_bless_famil_IVM_1ZE + Nb_I_bless_horsfamil_IVM_1ZE +
           Nb_I_homic_IVM_1ZE,
         Nb_I_vols_violents_IVM_1ZE = Nb_I_vols_armes_IVM_1ZE + Nb_I_vols_viol_sansarme_IVM_1ZE,
         Nb_I_vols_vehic_IVM_1BV = Nb_I_vols_acces_vehic_IVM_1BV + Nb_I_vols_ds_vehic_IVM_1BV + 
           Nb_I_vols_de_vehic_IVM_1BV,
         Nb_I_bless_famil_homic_IVM_1BV = Nb_I_bless_famil_IVM_1BV + Nb_I_bless_horsfamil_IVM_1BV +
           Nb_I_homic_IVM_1BV,
         Nb_I_vols_violents_IVM_1BV = Nb_I_vols_armes_IVM_1BV + Nb_I_vols_viol_sansarme_IVM_1BV,
         Nb_I_vols_vehic_IVM_1GD = Nb_I_vols_acces_vehic_IVM_1GD + Nb_I_vols_ds_vehic_IVM_1GD + 
           Nb_I_vols_de_vehic_IVM_1GD,
         Nb_I_bless_famil_homic_IVM_1GD = Nb_I_bless_famil_IVM_1GD + Nb_I_bless_horsfamil_IVM_1GD +
           Nb_I_homic_IVM_1GD,
         Nb_I_vols_violents_IVM_1GD = Nb_I_vols_armes_IVM_1GD + Nb_I_vols_viol_sansarme_IVM_1GD,
         Nb_I_vols_vehic_IVM_1UU = Nb_I_vols_acces_vehic_IVM_1UU + Nb_I_vols_ds_vehic_IVM_1UU + 
           Nb_I_vols_de_vehic_IVM_1UU,
         Nb_I_bless_famil_homic_IVM_1UU = Nb_I_bless_famil_IVM_1UU + Nb_I_bless_horsfamil_IVM_1UU +
           Nb_I_homic_IVM_1UU,
         Nb_I_vols_violents_IVM_1UU = Nb_I_vols_armes_IVM_1UU + Nb_I_vols_viol_sansarme_IVM_1UU,
         Nb_I_vols_vehic_IVM_1AAV = Nb_I_vols_acces_vehic_IVM_1AAV + Nb_I_vols_ds_vehic_IVM_1AAV + 
           Nb_I_vols_de_vehic_IVM_1AAV,
         Nb_I_bless_famil_homic_IVM_1AAV = Nb_I_bless_famil_IVM_1AAV + Nb_I_bless_horsfamil_IVM_1AAV +
           Nb_I_homic_IVM_1AAV,
         Nb_I_vols_violents_IVM_1AAV = Nb_I_vols_armes_IVM_1AAV + Nb_I_vols_viol_sansarme_IVM_1AAV,
         Nb_I_vols_vehic_IVM_1CENTR = Nb_I_vols_acces_vehic_IVM_1CENTR + Nb_I_vols_ds_vehic_IVM_1CENTR + 
           Nb_I_vols_de_vehic_IVM_1CENTR,
         Nb_I_bless_famil_homic_IVM_1CENTR = Nb_I_bless_famil_IVM_1CENTR + Nb_I_bless_horsfamil_IVM_1CENTR +
           Nb_I_homic_IVM_1CENTR,
         Nb_I_vols_violents_IVM_1CENTR = Nb_I_vols_armes_IVM_1CENTR + Nb_I_vols_viol_sansarme_IVM_1CENTR,
         Nb_V_vols_vehic = Nb_V_vols_acces_vehic + Nb_V_vols_ds_vehic + 
           Nb_V_vols_de_vehic,
         Nb_V_bless_famil_homic = Nb_V_bless_famil + Nb_V_bless_horsfamil +
           Nb_V_homic,
         Nb_V_vols_violents = Nb_V_vols_armes + Nb_V_vols_viol_sansarme,
         Nb_M_vols_vehic = Nb_M_vols_acces_vehic + Nb_M_vols_ds_vehic + 
           Nb_M_vols_de_vehic,
         Nb_M_bless_famil_homic = Nb_M_bless_famil + Nb_M_bless_horsfamil +
           Nb_M_homic,
         Nb_M_vols_violents = Nb_M_vols_armes + Nb_M_vols_viol_sansarme,
         ) %>%
         mutate(
        Nb_I_corpo = Nb_I_bless_famil_homic + Nb_I_viol_sex,
        Nb_V_corpo = Nb_V_bless_famil_homic + Nb_V_viol_sex,
        Nb_M_corpo = Nb_M_bless_famil_homic + Nb_M_viol_sex,
        Nb_I_corpo_IV_1ZE = Nb_I_bless_famil_homic_IV_1ZE + Nb_I_viol_sex_IV_1ZE,
        Nb_I_corpo_IV_1BV = Nb_I_bless_famil_homic_IV_1BV + Nb_I_viol_sex_IV_1BV,
        Nb_I_corpo_IV_1GD = Nb_I_bless_famil_homic_IV_1GD + Nb_I_viol_sex_IV_1GD,
        Nb_I_corpo_IV_1UU = Nb_I_bless_famil_homic_IV_1UU + Nb_I_viol_sex_IV_1UU,
        Nb_I_corpo_IV_1AAV = Nb_I_bless_famil_homic_IV_1AAV + Nb_I_viol_sex_IV_1AAV,
        Nb_I_corpo_IV_1CENTR = Nb_I_bless_famil_homic_IV_1CENTR + Nb_I_viol_sex_IV_1CENTR,
        Nb_I_corpo_IVM_1ZE = Nb_I_bless_famil_homic_IVM_1ZE + Nb_I_viol_sex_IVM_1ZE,
        Nb_I_corpo_IVM_1BV = Nb_I_bless_famil_homic_IVM_1BV + Nb_I_viol_sex_IVM_1BV,
        Nb_I_corpo_IVM_1GD = Nb_I_bless_famil_homic_IVM_1GD + Nb_I_viol_sex_IVM_1GD,
        Nb_I_corpo_IVM_1UU = Nb_I_bless_famil_homic_IVM_1UU + Nb_I_viol_sex_IVM_1UU,
        Nb_I_corpo_IVM_1AAV = Nb_I_bless_famil_homic_IVM_1AAV + Nb_I_viol_sex_IVM_1AAV,
        Nb_I_corpo_IVM_1CENTR = Nb_I_bless_famil_homic_IVM_1CENTR + Nb_I_viol_sex_IVM_1CENTR)


# Calcul de toutes les variables relatives au volume de délinquance (pour 1000 habitants):

delinquance_com <- delinquance_com %>%
                   mutate(
                     I = Nb_I/P19_POP*1000,
                     I_cambr = Nb_I_cambr/P19_POP*1000,
                     I_bless_famil = Nb_I_bless_famil/P19_POP*1000,
                     I_bless_horsfamil = Nb_I_bless_horsfamil/P19_POP*1000,
                     I_destr_degrad = Nb_I_destr_degrad/P19_POP*1000,
                     I_homic = Nb_I_homic/P19_POP*1000,
                     I_viol_sex = Nb_I_viol_sex/P19_POP*1000,
                     I_vols_armes = Nb_I_vols_armes/P19_POP*1000,
                     I_vols_acces_vehic = Nb_I_vols_acces_vehic/P19_POP*1000,
                     I_vols_ds_vehic = Nb_I_vols_ds_vehic/P19_POP*1000,
                     I_vols_de_vehic = Nb_I_vols_de_vehic/P19_POP*1000,
                     I_vols_sansviol = Nb_I_vols_sansviol/P19_POP*1000,
                     I_vols_viol_sansarme = Nb_I_vols_viol_sansarme/P19_POP*1000,
                     I_vols_vehic = Nb_I_vols_vehic/P19_POP*1000,
                     I_bless_famil_homic = Nb_I_bless_famil_homic/P19_POP*1000,
                     I_vols_violents = Nb_I_vols_violents/P19_POP*1000,
                     I_corpo = Nb_I_corpo/P19_POP*1000)
                
# Calcul de toutes les variables relatives au volume de victimes (pour 1000 habitants):

delinquance_com <- delinquance_com %>%
  mutate(
    V = Nb_V/P19_POP*1000,
    V_cambr = Nb_V_cambr/P19_POP*1000,
    V_bless_famil = Nb_V_bless_famil/P19_POP*1000,
    V_bless_horsfamil = Nb_V_bless_horsfamil/P19_POP*1000,
    V_destr_degrad = Nb_V_destr_degrad/P19_POP*1000,
    V_homic = Nb_V_homic/P19_POP*1000,
    V_viol_sex = Nb_V_viol_sex/P19_POP*1000,
    V_vols_armes = Nb_V_vols_armes/P19_POP*1000,
    V_vols_acces_vehic = Nb_V_vols_acces_vehic/P19_POP*1000,
    V_vols_ds_vehic = Nb_V_vols_ds_vehic/P19_POP*1000,
    V_vols_de_vehic = Nb_V_vols_de_vehic/P19_POP*1000,
    V_vols_sansviol = Nb_V_vols_sansviol/P19_POP*1000,
    V_vols_viol_sansarme = Nb_V_vols_viol_sansarme/P19_POP*1000,
    V_vols_vehic = Nb_V_vols_vehic/P19_POP*1000,
    V_bless_famil_homic = Nb_V_bless_famil_homic/P19_POP*1000,
    V_vols_violents = Nb_V_vols_violents/P19_POP*1000,
    V_corpo = Nb_V_corpo/P19_POP*1000
    )

# Calcul de toutes les variables relatives au volume de mis en cause (pour 1000 habitants):

delinquance_com <- delinquance_com %>%
  mutate(
    M = Nb_M/P19_POP*1000,
    M_cambr = Nb_M_cambr/P19_POP*1000,
    M_bless_famil = Nb_M_bless_famil/P19_POP*1000,
    M_bless_horsfamil = Nb_M_bless_horsfamil/P19_POP*1000,
    M_destr_degrad = Nb_M_destr_degrad/P19_POP*1000,
    M_homic = Nb_M_homic/P19_POP*1000,
    M_viol_sex = Nb_M_viol_sex/P19_POP*1000,
    M_vols_armes = Nb_M_vols_armes/P19_POP*1000,
    M_vols_acces_vehic = Nb_M_vols_acces_vehic/P19_POP*1000,
    M_vols_ds_vehic = Nb_M_vols_ds_vehic/P19_POP*1000,
    M_vols_de_vehic = Nb_M_vols_de_vehic/P19_POP*1000,
    M_vols_sansviol = Nb_M_vols_sansviol/P19_POP*1000,
    M_vols_viol_sansarme = Nb_M_vols_viol_sansarme/P19_POP*1000,
    M_vols_vehic = Nb_M_vols_vehic/P19_POP*1000,
    M_bless_famil_homic = Nb_M_bless_famil_homic/P19_POP*1000,
    M_vols_violents = Nb_M_vols_violents/P19_POP*1000,
    M_corpo = Nb_M_corpo/P19_POP*1000
    )

# Calcul de toutes les variables relatives à la structure de la délinquance par type d'atteinte (en %):

delinquance_com <- delinquance_com %>%
  mutate(
    P_I_cambr = Nb_I_cambr/Nb_I*100,
    P_I_bless_famil = Nb_I_bless_famil/Nb_I*100,
    P_I_bless_horsfamil = Nb_I_bless_horsfamil/Nb_I*100,
    P_I_destr_degrad = Nb_I_destr_degrad/Nb_I*100,
    P_I_homic = Nb_I_homic/Nb_I*100,
    P_I_viol_sex = Nb_I_viol_sex/Nb_I*100,
    P_I_vols_armes = Nb_I_vols_armes/Nb_I*100,
    P_I_vols_acces_vehic = Nb_I_vols_acces_vehic/Nb_I*100,
    P_I_vols_ds_vehic = Nb_I_vols_ds_vehic/Nb_I*100,
    P_I_vols_de_vehic = Nb_I_vols_de_vehic/Nb_I*100,
    P_I_vols_sansviol = Nb_I_vols_sansviol/Nb_I*100,
    P_I_vols_viol_sansarme = Nb_I_vols_viol_sansarme/Nb_I*100,
    P_I_vols_vehic = Nb_I_vols_vehic/Nb_I*100,
    P_I_bless_famil_homic = Nb_I_bless_famil_homic/Nb_I*100,
    P_I_vols_violents = Nb_I_vols_violents/Nb_I*100,
    P_I_corpo = Nb_I_corpo/Nb_I*100)


# Calcul de toutes les variables relatives à la répartition des différents types d'atteintes selon que le couple
# (I,V) associé ou le triplet (I,V,M) associé s'inscrit ou non dans un même zonage d'étude (en %):

delinquance_com <- delinquance_com %>%
  mutate(
# (I,V) dans même ZE:
P_I_IV_1ZE = Nb_I_IV_1ZE/Nb_I*100,
P_I_cambr_IV_1ZE = Nb_I_cambr_IV_1ZE/Nb_I_cambr*100,
P_I_bless_famil_IV_1ZE = Nb_I_bless_famil_IV_1ZE/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1ZE = Nb_I_bless_horsfamil_IV_1ZE/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1ZE = Nb_I_destr_degrad_IV_1ZE/Nb_I_destr_degrad*100,
P_I_homic_IV_1ZE = Nb_I_homic_IV_1ZE/Nb_I_homic*100,
P_I_viol_sex_IV_1ZE = Nb_I_viol_sex_IV_1ZE/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1ZE = Nb_I_vols_armes_IV_1ZE/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1ZE = Nb_I_vols_acces_vehic_IV_1ZE/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1ZE = Nb_I_vols_ds_vehic_IV_1ZE/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1ZE = Nb_I_vols_de_vehic_IV_1ZE/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1ZE = Nb_I_vols_sansviol_IV_1ZE/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1ZE = Nb_I_vols_viol_sansarme_IV_1ZE/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1ZE = Nb_I_vols_vehic_IV_1ZE/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1ZE = Nb_I_bless_famil_homic_IV_1ZE/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1ZE = Nb_I_vols_violents_IV_1ZE/Nb_I_vols_violents*100,
P_I_corpo_IV_1ZE = Nb_I_corpo_IV_1ZE/Nb_I_corpo*100,
# (I,V) dans même BV:
P_I_IV_1BV = Nb_I_IV_1BV/Nb_I*100,
P_I_cambr_IV_1BV = Nb_I_cambr_IV_1BV/Nb_I_cambr*100,
P_I_bless_famil_IV_1BV = Nb_I_bless_famil_IV_1BV/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1BV = Nb_I_bless_horsfamil_IV_1BV/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1BV = Nb_I_destr_degrad_IV_1BV/Nb_I_destr_degrad*100,
P_I_homic_IV_1BV = Nb_I_homic_IV_1BV/Nb_I_homic*100,
P_I_viol_sex_IV_1BV = Nb_I_viol_sex_IV_1BV/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1BV = Nb_I_vols_armes_IV_1BV/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1BV = Nb_I_vols_acces_vehic_IV_1BV/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1BV = Nb_I_vols_ds_vehic_IV_1BV/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1BV = Nb_I_vols_de_vehic_IV_1BV/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1BV = Nb_I_vols_sansviol_IV_1BV/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1BV = Nb_I_vols_viol_sansarme_IV_1BV/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1BV = Nb_I_vols_vehic_IV_1BV/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1BV = Nb_I_bless_famil_homic_IV_1BV/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1BV = Nb_I_vols_violents_IV_1BV/Nb_I_vols_violents*100,
P_I_corpo_IV_1BV = Nb_I_corpo_IV_1BV/Nb_I_corpo*100,
# (I,V) dans même GD:
P_I_IV_1GD = Nb_I_IV_1GD/Nb_I*100,
P_I_cambr_IV_1GD = Nb_I_cambr_IV_1GD/Nb_I_cambr*100,
P_I_bless_famil_IV_1GD = Nb_I_bless_famil_IV_1GD/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1GD = Nb_I_bless_horsfamil_IV_1GD/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1GD = Nb_I_destr_degrad_IV_1GD/Nb_I_destr_degrad*100,
P_I_homic_IV_1GD = Nb_I_homic_IV_1GD/Nb_I_homic*100,
P_I_viol_sex_IV_1GD = Nb_I_viol_sex_IV_1GD/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1GD = Nb_I_vols_armes_IV_1GD/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1GD = Nb_I_vols_acces_vehic_IV_1GD/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1GD = Nb_I_vols_ds_vehic_IV_1GD/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1GD = Nb_I_vols_de_vehic_IV_1GD/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1GD = Nb_I_vols_sansviol_IV_1GD/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1GD = Nb_I_vols_viol_sansarme_IV_1GD/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1GD = Nb_I_vols_vehic_IV_1GD/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1GD = Nb_I_bless_famil_homic_IV_1GD/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1GD = Nb_I_vols_violents_IV_1GD/Nb_I_vols_violents*100,
P_I_corpo_IV_1GD = Nb_I_corpo_IV_1GD/Nb_I_corpo*100,
# (I,V) dans même UU:
P_I_IV_1UU = Nb_I_IV_1UU/Nb_I*100,
P_I_cambr_IV_1UU = Nb_I_cambr_IV_1UU/Nb_I_cambr*100,
P_I_bless_famil_IV_1UU = Nb_I_bless_famil_IV_1UU/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1UU = Nb_I_bless_horsfamil_IV_1UU/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1UU = Nb_I_destr_degrad_IV_1UU/Nb_I_destr_degrad*100,
P_I_homic_IV_1UU = Nb_I_homic_IV_1UU/Nb_I_homic*100,
P_I_viol_sex_IV_1UU = Nb_I_viol_sex_IV_1UU/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1UU = Nb_I_vols_armes_IV_1UU/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1UU = Nb_I_vols_acces_vehic_IV_1UU/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1UU = Nb_I_vols_ds_vehic_IV_1UU/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1UU = Nb_I_vols_de_vehic_IV_1UU/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1UU = Nb_I_vols_sansviol_IV_1UU/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1UU = Nb_I_vols_viol_sansarme_IV_1UU/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1UU = Nb_I_vols_vehic_IV_1UU/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1UU = Nb_I_bless_famil_homic_IV_1UU/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1UU = Nb_I_vols_violents_IV_1UU/Nb_I_vols_violents*100,
P_I_corpo_IV_1UU = Nb_I_corpo_IV_1UU/Nb_I_corpo*100,
# (I,V) dans même AAV:
P_I_IV_1AAV = Nb_I_IV_1AAV/Nb_I*100,
P_I_cambr_IV_1AAV = Nb_I_cambr_IV_1AAV/Nb_I_cambr*100,
P_I_bless_famil_IV_1AAV = Nb_I_bless_famil_IV_1AAV/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1AAV = Nb_I_bless_horsfamil_IV_1AAV/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1AAV = Nb_I_destr_degrad_IV_1AAV/Nb_I_destr_degrad*100,
P_I_homic_IV_1AAV = Nb_I_homic_IV_1AAV/Nb_I_homic*100,
P_I_viol_sex_IV_1AAV = Nb_I_viol_sex_IV_1AAV/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1AAV = Nb_I_vols_armes_IV_1AAV/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1AAV = Nb_I_vols_acces_vehic_IV_1AAV/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1AAV = Nb_I_vols_ds_vehic_IV_1AAV/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1AAV = Nb_I_vols_de_vehic_IV_1AAV/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1AAV = Nb_I_vols_sansviol_IV_1AAV/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1AAV = Nb_I_vols_viol_sansarme_IV_1AAV/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1AAV = Nb_I_vols_vehic_IV_1AAV/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1AAV = Nb_I_bless_famil_homic_IV_1AAV/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1AAV = Nb_I_vols_violents_IV_1AAV/Nb_I_vols_violents*100,
P_I_corpo_IV_1AAV = Nb_I_corpo_IV_1AAV/Nb_I_corpo*100,
# (I,V) dans même CENTR:
P_I_IV_1CENTR = Nb_I_IV_1CENTR/Nb_I*100,
P_I_cambr_IV_1CENTR = Nb_I_cambr_IV_1CENTR/Nb_I_cambr*100,
P_I_bless_famil_IV_1CENTR = Nb_I_bless_famil_IV_1CENTR/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IV_1CENTR = Nb_I_bless_horsfamil_IV_1CENTR/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IV_1CENTR = Nb_I_destr_degrad_IV_1CENTR/Nb_I_destr_degrad*100,
P_I_homic_IV_1CENTR = Nb_I_homic_IV_1CENTR/Nb_I_homic*100,
P_I_viol_sex_IV_1CENTR = Nb_I_viol_sex_IV_1CENTR/Nb_I_viol_sex*100,
P_I_vols_armes_IV_1CENTR = Nb_I_vols_armes_IV_1CENTR/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IV_1CENTR = Nb_I_vols_acces_vehic_IV_1CENTR/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IV_1CENTR = Nb_I_vols_ds_vehic_IV_1CENTR/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IV_1CENTR = Nb_I_vols_de_vehic_IV_1CENTR/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IV_1CENTR = Nb_I_vols_sansviol_IV_1CENTR/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IV_1CENTR = Nb_I_vols_viol_sansarme_IV_1CENTR/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IV_1CENTR = Nb_I_vols_vehic_IV_1CENTR/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IV_1CENTR = Nb_I_bless_famil_homic_IV_1CENTR/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IV_1CENTR = Nb_I_vols_violents_IV_1CENTR/Nb_I_vols_violents*100,
P_I_corpo_IV_1CENTR = Nb_I_corpo_IV_1CENTR/Nb_I_corpo*100,
# (I,V,M) dans même ZE:
P_I_cambr_IVM_1ZE = Nb_I_cambr_IVM_1ZE/Nb_I_cambr*100,
P_I_bless_famil_IVM_1ZE = Nb_I_bless_famil_IVM_1ZE/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1ZE = Nb_I_bless_horsfamil_IVM_1ZE/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1ZE = Nb_I_destr_degrad_IVM_1ZE/Nb_I_destr_degrad*100,
P_I_homic_IVM_1ZE = Nb_I_homic_IVM_1ZE/Nb_I_homic*100,
P_I_viol_sex_IVM_1ZE = Nb_I_viol_sex_IVM_1ZE/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1ZE = Nb_I_vols_armes_IVM_1ZE/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1ZE = Nb_I_vols_acces_vehic_IVM_1ZE/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1ZE = Nb_I_vols_ds_vehic_IVM_1ZE/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1ZE = Nb_I_vols_de_vehic_IVM_1ZE/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1ZE = Nb_I_vols_sansviol_IVM_1ZE/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1ZE = Nb_I_vols_viol_sansarme_IVM_1ZE/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1ZE = Nb_I_vols_vehic_IVM_1ZE/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1ZE = Nb_I_bless_famil_homic_IVM_1ZE/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1ZE = Nb_I_vols_violents_IVM_1ZE/Nb_I_vols_violents*100,
P_I_corpo_IVM_1ZE = Nb_I_corpo_IVM_1ZE/Nb_I_corpo*100,
# (I,V,M) dans même BV:
P_I_cambr_IVM_1BV = Nb_I_cambr_IVM_1BV/Nb_I_cambr*100,
P_I_bless_famil_IVM_1BV = Nb_I_bless_famil_IVM_1BV/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1BV = Nb_I_bless_horsfamil_IVM_1BV/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1BV = Nb_I_destr_degrad_IVM_1BV/Nb_I_destr_degrad*100,
P_I_homic_IVM_1BV = Nb_I_homic_IVM_1BV/Nb_I_homic*100,
P_I_viol_sex_IVM_1BV = Nb_I_viol_sex_IVM_1BV/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1BV = Nb_I_vols_armes_IVM_1BV/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1BV = Nb_I_vols_acces_vehic_IVM_1BV/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1BV = Nb_I_vols_ds_vehic_IVM_1BV/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1BV = Nb_I_vols_de_vehic_IVM_1BV/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1BV = Nb_I_vols_sansviol_IVM_1BV/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1BV = Nb_I_vols_viol_sansarme_IVM_1BV/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1BV = Nb_I_vols_vehic_IVM_1BV/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1BV = Nb_I_bless_famil_homic_IVM_1BV/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1BV = Nb_I_vols_violents_IVM_1BV/Nb_I_vols_violents*100,
P_I_corpo_IVM_1BV = Nb_I_corpo_IVM_1BV/Nb_I_corpo*100,
# (I,V,M) dans même GD:
P_I_cambr_IVM_1GD = Nb_I_cambr_IVM_1GD/Nb_I_cambr*100,
P_I_bless_famil_IVM_1GD = Nb_I_bless_famil_IVM_1GD/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1GD = Nb_I_bless_horsfamil_IVM_1GD/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1GD = Nb_I_destr_degrad_IVM_1GD/Nb_I_destr_degrad*100,
P_I_homic_IVM_1GD = Nb_I_homic_IVM_1GD/Nb_I_homic*100,
P_I_viol_sex_IVM_1GD = Nb_I_viol_sex_IVM_1GD/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1GD = Nb_I_vols_armes_IVM_1GD/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1GD = Nb_I_vols_acces_vehic_IVM_1GD/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1GD = Nb_I_vols_ds_vehic_IVM_1GD/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1GD = Nb_I_vols_de_vehic_IVM_1GD/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1GD = Nb_I_vols_sansviol_IVM_1GD/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1GD = Nb_I_vols_viol_sansarme_IVM_1GD/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1GD = Nb_I_vols_vehic_IVM_1GD/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1GD = Nb_I_bless_famil_homic_IVM_1GD/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1GD = Nb_I_vols_violents_IVM_1GD/Nb_I_vols_violents*100,
P_I_corpo_IVM_1GD = Nb_I_corpo_IVM_1GD/Nb_I_corpo*100,
# (I,V,M) dans même UU:
P_I_cambr_IVM_1UU = Nb_I_cambr_IVM_1UU/Nb_I_cambr*100,
P_I_bless_famil_IVM_1UU = Nb_I_bless_famil_IVM_1UU/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1UU = Nb_I_bless_horsfamil_IVM_1UU/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1UU = Nb_I_destr_degrad_IVM_1UU/Nb_I_destr_degrad*100,
P_I_homic_IVM_1UU = Nb_I_homic_IVM_1UU/Nb_I_homic*100,
P_I_viol_sex_IVM_1UU = Nb_I_viol_sex_IVM_1UU/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1UU = Nb_I_vols_armes_IVM_1UU/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1UU = Nb_I_vols_acces_vehic_IVM_1UU/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1UU = Nb_I_vols_ds_vehic_IVM_1UU/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1UU = Nb_I_vols_de_vehic_IVM_1UU/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1UU = Nb_I_vols_sansviol_IVM_1UU/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1UU = Nb_I_vols_viol_sansarme_IVM_1UU/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1UU = Nb_I_vols_vehic_IVM_1UU/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1UU = Nb_I_bless_famil_homic_IVM_1UU/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1UU = Nb_I_vols_violents_IVM_1UU/Nb_I_vols_violents*100,
P_I_corpo_IVM_1UU = Nb_I_corpo_IVM_1UU/Nb_I_corpo*100,
# (I,V,M) dans même AAV:
P_I_cambr_IVM_1AAV = Nb_I_cambr_IVM_1AAV/Nb_I_cambr*100,
P_I_bless_famil_IVM_1AAV = Nb_I_bless_famil_IVM_1AAV/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1AAV = Nb_I_bless_horsfamil_IVM_1AAV/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1AAV = Nb_I_destr_degrad_IVM_1AAV/Nb_I_destr_degrad*100,
P_I_homic_IVM_1AAV = Nb_I_homic_IVM_1AAV/Nb_I_homic*100,
P_I_viol_sex_IVM_1AAV = Nb_I_viol_sex_IVM_1AAV/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1AAV = Nb_I_vols_armes_IVM_1AAV/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1AAV = Nb_I_vols_acces_vehic_IVM_1AAV/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1AAV = Nb_I_vols_ds_vehic_IVM_1AAV/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1AAV = Nb_I_vols_de_vehic_IVM_1AAV/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1AAV = Nb_I_vols_sansviol_IVM_1AAV/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1AAV = Nb_I_vols_viol_sansarme_IVM_1AAV/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1AAV = Nb_I_vols_vehic_IVM_1AAV/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1AAV = Nb_I_bless_famil_homic_IVM_1AAV/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1AAV = Nb_I_vols_violents_IVM_1AAV/Nb_I_vols_violents*100,
P_I_corpo_IVM_1AAV = Nb_I_corpo_IVM_1AAV/Nb_I_corpo*100,
# (I,V,M) dans même CENTR:
P_I_cambr_IVM_1CENTR = Nb_I_cambr_IVM_1CENTR/Nb_I_cambr*100,
P_I_bless_famil_IVM_1CENTR = Nb_I_bless_famil_IVM_1CENTR/Nb_I_bless_famil*100,
P_I_bless_horsfamil_IVM_1CENTR = Nb_I_bless_horsfamil_IVM_1CENTR/Nb_I_bless_horsfamil*100,
P_I_destr_degrad_IVM_1CENTR = Nb_I_destr_degrad_IVM_1CENTR/Nb_I_destr_degrad*100,
P_I_homic_IVM_1CENTR = Nb_I_homic_IVM_1CENTR/Nb_I_homic*100,
P_I_viol_sex_IVM_1CENTR = Nb_I_viol_sex_IVM_1CENTR/Nb_I_viol_sex*100,
P_I_vols_armes_IVM_1CENTR = Nb_I_vols_armes_IVM_1CENTR/Nb_I_vols_armes*100,
P_I_vols_acces_vehic_IVM_1CENTR = Nb_I_vols_acces_vehic_IVM_1CENTR/Nb_I_vols_acces_vehic*100,
P_I_vols_ds_vehic_IVM_1CENTR = Nb_I_vols_ds_vehic_IVM_1CENTR/Nb_I_vols_ds_vehic*100,
P_I_vols_de_vehic_IVM_1CENTR = Nb_I_vols_de_vehic_IVM_1CENTR/Nb_I_vols_de_vehic*100,
P_I_vols_sansviol_IVM_1CENTR = Nb_I_vols_sansviol_IVM_1CENTR/Nb_I_vols_sansviol*100,
P_I_vols_viol_sansarme_IVM_1CENTR = Nb_I_vols_viol_sansarme_IVM_1CENTR/Nb_I_vols_viol_sansarme*100,
P_I_vols_vehic_IVM_1CENTR = Nb_I_vols_vehic_IVM_1CENTR/Nb_I_vols_vehic*100,
P_I_bless_famil_homic_IVM_1CENTR = Nb_I_bless_famil_homic_IVM_1CENTR/Nb_I_bless_famil_homic*100,
P_I_vols_violents_IVM_1CENTR = Nb_I_vols_violents_IVM_1CENTR/Nb_I_vols_violents*100,
P_I_corpo_IVM_1CENTR = Nb_I_corpo_IVM_1CENTR/Nb_I_corpo*100)

# Calculs d'indicateurs démographiques et socio-économiques sur les communes:

# densité de population (P19_POP/SUPERF), en nombre d'hbts/km2.
# proportion de résidences secondaires (en % de logements): P19_RSECOCC/P19_LOG
# proportion de chômeurs (en % de la population des 15-64 ans): P19_CHOMEUR1564/P19_POP1564
# Part des jeunes (15-29 ans) dans la population communale (en %): P19_POP1529/P19_POP
# Part des résidences de tourisme dans l'ensemble des logements de la commune (en %): RT23/P19_LOG
# Nombre des campings pour 1000 habitants: CPG23/P19_POP*1000
# Rapport interdécile de niveau de vie de la commune (D9/D1): RD20 = D920/D120
# Part des hôtels dans l'ensemble des logements de la commune: HT23/P19_LOG
delinquance_com <- delinquance_com %>%
  mutate(densite_pop=P19_POP/SUPERF,
         part_res_secondaires=P19_RSECOCC/P19_LOG*100,
         part_chomeurs=P19_CHOMEUR1564/P19_POP1564*100,
         P_pop15_29ans = P19_POP1529/P19_POP*100,
         P_res_tourisme=RT23/P19_LOG*100,
         P_camping_1000hbt=CPG23/P19_POP*1000,
         P_hotel=HT23/P19_LOG*100) 

# Ajout de l'information sur les distances médianes entre les communes I,V et M:
# TODO !!


# 3- Construction de la base "delinquance_dep" (analyse au niveau du département.)

# # Agrégation des atteintes par commune de l'infraction: calcul du nombre d'infractions (I) dans chaque département,
# # selon le type de l'atteinte.
# nb_I_dep <- atteintes[(is.na(cog_com_22_inf)==FALSE), .(
#   Nb_I = sum(compteur, na.rm = TRUE),
#   Nb_I_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
#   Nb_I_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
#   Nb_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
#   Nb_I_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
#   Nb_I_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
#   Nb_I_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
#   Nb_I_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
#   Nb_I_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
#   Nb_I_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
#   by = .(DEP_inf)]
# 
# # Agrégation des atteintes par commune de domiciliation de la victime: calcul du nombre de victimes (V) dans chaque commune,
# # selon le type de l'atteinte.
# nb_V_dep <- atteintes[(is.na(cog_com_22_vict)==FALSE), .(
#   Nb_V = sum(compteur, na.rm = TRUE),
#   Nb_V_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
#   Nb_V_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
#   Nb_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
#   Nb_V_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
#   Nb_V_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
#   Nb_V_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
#   Nb_V_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
#   Nb_V_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
#   Nb_V_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
#   Nb_V_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
#   Nb_V_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
#   Nb_V_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
#   by = .(DEP_vict)]
# 
# # Agrégation des atteintes par commune de domiciliation du mis en cause: calcul du nombre de mis en cause (M) dans
# #chaque commune, selon le type de l'atteinte.
# nb_M_dep <- atteintes[(is.na(cog_com_22_mec)==FALSE), .(
#   Nb_M = sum(compteur, na.rm = TRUE),
#   Nb_M_cambr = sum(compteur*(classe == "Cambriolages de logement"), na.rm = TRUE),
#   Nb_M_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"), na.rm = TRUE),
#   Nb_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"), na.rm = TRUE),
#   Nb_M_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"), na.rm = TRUE),
#   Nb_M_homic = sum(compteur*(classe == "Homicides"), na.rm = TRUE),
#   Nb_M_viol_sex = sum(compteur*(classe == "Violences sexuelles"), na.rm = TRUE),
#   Nb_M_vols_armes = sum(compteur*(classe == "Vols avec armes"), na.rm = TRUE),
#   Nb_M_vols_acces_vehic = sum(compteur*(classe == "Vols d'accessoires sur véhicules"), na.rm = TRUE),
#   Nb_M_vols_ds_vehic = sum(compteur*(classe == "Vols dans les véhicules"), na.rm = TRUE),
#   Nb_M_vols_de_vehic = sum(compteur*(classe == "Vols de véhicules"), na.rm = TRUE),
#   Nb_M_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"), na.rm = TRUE),
#   Nb_M_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"), na.rm = TRUE)),
#   by = .(DEP_mec)]
# 
# # Agrégation des atteintes par commune de l'infraction: calcul du nombre d'infractions (I) dans chaque commune,
# # associées à un couple de communes (I,V) inscrites dans un même zonage d'étude.
# 
# # Warning: on se restreint ici aux seules atteintes associées à un couple de communes (I,V) renseigné!
# 
# nb_I_IV_meme_zonage_dep <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
#   # Nombre d'infractions avec un couple de communes (I,V) dans la même zone d'emploi (ZE):
#   Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1ZE = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1ZE = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1ZE = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1ZE = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1ZE = sum(compteur*(classe == "Homicides" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1ZE = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1ZE = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1ZE = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1ZE = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1ZE = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1ZE = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1ZE = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_ZE == "oui"), na.rm = TRUE),
#   
#   # Nombre d'infractions avec un couple de communes (I,V) dans le même bassin de vie (BV):
#   Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1BV = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1BV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1BV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1BV = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1BV = sum(compteur*(classe == "Homicides" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1BV = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1BV = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1BV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1BV = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1BV = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1BV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1BV = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_BV == "oui"), na.rm = TRUE),
#   
#   # Nombre d'infractions avec un couple de communes (I,V) dans la même grille de densité (GD):
#   Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1GD = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1GD = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1GD = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1GD = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1GD = sum(compteur*(classe == "Homicides" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1GD = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1GD = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1GD = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1GD = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1GD = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1GD = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1GD = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_GD == "oui"), na.rm = TRUE),
#   
#   # Nombre d'infractions avec un couple de communes (I,V) dans la même unité urbaine (UU):
#   Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1UU = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1UU = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1UU = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1UU = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1UU = sum(compteur*(classe == "Homicides" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1UU = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1UU = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1UU = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1UU = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1UU = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1UU = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1UU = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_UU == "oui"), na.rm = TRUE),
#   
#   # Nombre d'infractions avec un couple de communes (I,V) dans la même aire d'attraction des villes (AAV):
#   Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1AAV = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1AAV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1AAV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1AAV = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1AAV = sum(compteur*(classe == "Homicides" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1AAV = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1AAV = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1AAV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1AAV = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1AAV = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1AAV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1AAV = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_AAV == "oui"), na.rm = TRUE),
#   
#   # Nombre d'infractions avec un couple de communes (I,V) dans la même centralité (CENTR):
#   Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_cambr_IV_1CENTR = sum(compteur*(classe == "Cambriolages de logement" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_bless_famil_IV_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_bless_horsfamil_IV_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_destr_degrad_IV_1CENTR = sum(compteur*(classe == "Destructions et dégradations" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_homic_IV_1CENTR = sum(compteur*(classe == "Homicides" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_viol_sex_IV_1CENTR = sum(compteur*(classe == "Violences sexuelles" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_armes_IV_1CENTR = sum(compteur*(classe == "Vols avec armes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_acces_vehic_IV_1CENTR = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_ds_vehic_IV_1CENTR = sum(compteur*(classe == "Vols dans les véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_de_vehic_IV_1CENTR = sum(compteur*(classe == "Vols de véhicules" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_sansviol_IV_1CENTR = sum(compteur*(classe == "Vols sans violence contre des personnes" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE),
#   Nb_I_vols_viol_sansarme_IV_1CENTR = sum(compteur*(classe == "Vols violents sans arme" & IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
#   by = .(DEP_inf)]
# 
# # Agrégation des atteintes par département de l'infraction: calcul du nombre d'infractions (I) dans chaque commune,
# # associées à un triplet de communes (I,V,M) inscrites dans un même zonage d'étude.
# 
# # Warning: on se restreint ici aux seules atteintes associées à un triplet de communes (I,V,M) renseignées!
# # On se limite ici aux seules atteintes corporelles, celles pour lesquelles la commmune du mis en cause
# # est le mieux renseigné.
# 
# nb_I_IVM_meme_zonage_dep <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
#                                     (is.na(cog_com_22_mec)==FALSE), .(
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même zone d'emploi (ZE):
#                                       Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1ZE = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1ZE = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1ZE = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1ZE = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1ZE = sum(compteur*(classe == "Homicides" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1ZE = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1ZE = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1ZE = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1ZE = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1ZE = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1ZE = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1ZE = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
#                                       
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans le même bassin de vie (BV):
#                                       Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1BV = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1BV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1BV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1BV = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1BV = sum(compteur*(classe == "Homicides" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1BV = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1BV = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1BV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1BV = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1BV = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1BV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1BV = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_BV == "oui"), na.rm = TRUE),
#                                       
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même grille de densité (GD):
#                                       Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1GD = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1GD = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1GD = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1GD = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1GD = sum(compteur*(classe == "Homicides" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1GD = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1GD = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1GD = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1GD = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1GD = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1GD = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1GD = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_GD == "oui"), na.rm = TRUE),
#                                       
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même unité urbaine (UU):
#                                       Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1UU = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1UU = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1UU = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1UU = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1UU = sum(compteur*(classe == "Homicides" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1UU = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1UU = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1UU = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1UU = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1UU = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1UU = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1UU = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_UU == "oui"), na.rm = TRUE),
#                                       
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même aire d'attraction des villes (AAV):
#                                       Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1AAV = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1AAV = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1AAV = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1AAV = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1AAV = sum(compteur*(classe == "Homicides" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1AAV = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1AAV = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1AAV = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1AAV = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1AAV = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1AAV = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1AAV = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
#                                       
#                                       # Nombre d'infractions avec un triplet de communes (I,V,M) dans la même centralité (CENTR):
#                                       Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_cambr_IVM_1CENTR = sum(compteur*(classe == "Cambriolages de logement" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_famil_IVM_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_bless_horsfamil_IVM_1CENTR = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_destr_degrad_IVM_1CENTR = sum(compteur*(classe == "Destructions et dégradations" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_homic_IVM_1CENTR = sum(compteur*(classe == "Homicides" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_viol_sex_IVM_1CENTR = sum(compteur*(classe == "Violences sexuelles" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_armes_IVM_1CENTR = sum(compteur*(classe == "Vols avec armes" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_acces_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols d'accessoires sur véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_ds_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols dans les véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_de_vehic_IVM_1CENTR = sum(compteur*(classe == "Vols de véhicules" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_sansviol_IVM_1CENTR = sum(compteur*(classe == "Vols sans violence contre des personnes" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE),
#                                       Nb_I_vols_viol_sansarme_IVM_1CENTR = sum(compteur*(classe == "Vols violents sans arme" & IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
#                                   by = .(DEP_inf)]
# 
# # TODO !!!
# # Il restera à dénombrer les infractions selon le statut de la commune de I, celle de V (et celle de M pour les atteintes
# # corpo) au sein du zonage d'étude (lorsque (I,V) ou (I,V,M) s'inscrit dans un même zonage)
# # Cas à traiter:
# # 1) Statut de la commune au sein d'un bassin de vie: modalités 11- Pôle partiel/12- Commune associée à un pôle partiel/20- Pôle
# # 2) Statut de la commune au sein d'une aire d'attraction des villes (AAV): 11- Commune centre/12- Autre commune du pôle principal
# # /13- Commune d'un pôle secondaire/20- Commune de la couronne
# # 3) Statut de la commune dans la grille de densité: urbain dense/urbain moyennement dense/...
# 
# # On rassemble toutes ces bases en une unique base communale:
# delinquance_dep <- 
#   merge(x = nb_I_dep,
#         y = nb_V_dep,
#         by.x = "DEP_inf",
#         by.y = "DEP_vict",
#         all.x = TRUE)
# delinquance_dep <- 
#   merge(x = delinquance_dep,
#         y = nb_M_dep,
#         by.x = "DEP_inf",
#         by.y = "DEP_mec",
#         all.x = TRUE)
# delinquance_dep <- 
#   merge(x = delinquance_dep,
#         y = nb_I_IV_meme_zonage_dep,
#         by.x = "DEP_inf",
#         by.y = "DEP_inf",
#         all.x = TRUE)
# delinquance_dep <- 
#   merge(x = delinquance_dep,
#         y = nb_I_IVM_meme_zonage_dep,
#         by.x = "DEP_inf",
#         by.y = "DEP_inf",
#         all.x = TRUE)
# 
# dossier_complet_insee3 <- dossier_complet_insee[,.(DEP,P19_POP,P19_LOG,MED20,
#                                                    P19_POP1529,RT23,CPG23,RD20,
#                                                    D920,D120,HT23)]
# 
# dossier_complet_insee_dep <- dossier_complet_insee3[,.(
#   P19_POP = sum(P19_POP,na.rm=TRUE),
#   P19_LOG= sum(P19_POP,na.rm=TRUE),
#   MED20= mean(P19_POP,na.rm=TRUE),
#   P19_POP1529= sum(P19_POP,na.rm=TRUE),
#   RT23= sum(P19_POP,na.rm=TRUE),
#   CPG23= sum(P19_POP,na.rm=TRUE),
#   RD20= mean(P19_POP,na.rm=TRUE),
#   D920= mean(P19_POP,na.rm=TRUE),
#   D120= mean(P19_POP,na.rm=TRUE),
#   HT23= sum(P19_POP,na.rm=TRUE) 
# ),
# by = .(DEP)]
# 
# delinquance_dep <- 
#   merge(x = delinquance_dep,
#         y = dossier_complet_insee_dep,
#         by.x = "DEP_inf",
#         by.y = "DEP",
#         all.x = TRUE)
# 
# 
# 
# 
