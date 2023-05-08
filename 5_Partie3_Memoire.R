
# Partie 3 du mémoire: Classification des communes et des départements de France métropolitaine au regard de la délinquance


# ACP n°1: au niveau départemental sur les atteintes non corporelles

# Tableau de données de l'ACP n°1:

data.ACP_1 <- delinquance_dep %>% filter (is.na(DEP_inf)==FALSE & !(DEP_inf %in% c("971","972","973","974","976"))) %>%
                              select(DEP_inf,
                                     # Variables actives mesurant le volume de délinquance (par type et pour 1000 hbts):
                                     I_cambr,I_destr_degrad,I_vols_vehic,I_vols_sansviol,I_vols_violents,  
                                     # Variables actives mesurant la structure de la délinquance (par type et pour 1000 hbts):
                                     P_I_cambr,P_I_destr_degrad,P_I_vols_vehic,P_I_vols_sansviol,P_I_vols_violents,
                                     # Variables actives mesurant le degré de concentration spatiale de la délinquance (distance médiane entre I et V en km):
                                     dist_mediane_I_V
                                     ) %>%
                             column_to_rownames(var="DEP_inf")

# On effectue l'ACP sur ce jeu de données:
res.PCA_1<-PCA(data.ACP_1,graph=TRUE)
