
# Partie 1 du mémoire Stat'app: les données

# Création du tableau 1: Atteintes selon leur type et la qualité de l'information sur les 3 lieux associés

data_tableau1 <- atteintes[ , .(classe,cog_com_22_inf,cog_com_22_vict,cog_com_22_mec,compteur)]

# Légende: I=lieu de l'infraction; V=lieu de domiciliation de la victime; M=lieu de domiciliation du mis en cause

# a) Les 3 lieux (IVM) sont renseignés:
data_tableau1[ , compteur_IVM_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_vict) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_mec) == FALSE), 1,
  default =0)
]
# b) Seuls les deux lieux (IV) sont renseignés:
data_tableau1[ , compteur_IV_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_vict) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_mec) == TRUE), 1,
  default =0)
]
# c) Seuls les deux lieux (IM) sont renseignés:
data_tableau1[ , compteur_IM_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_vict) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_mec) == FALSE), 1,
  default =0)
]
# d) Seuls les deux lieux (VM) sont renseignés:
data_tableau1[ , compteur_VM_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_vict) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_mec) == FALSE), 1,
  default =0)
]
# e) Seul le lieu (I) est renseigné:
data_tableau1[ , compteur_I_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_vict) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_mec) == TRUE), 1,
  default =0)
]
# f) Seul le lieu (V) est renseigné:
data_tableau1[ , compteur_V_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_vict) == FALSE) & 
    (is.na(data_tableau1$cog_com_22_mec) == TRUE), 1,
  default =0)
]
# g) Seul le lieu (M) est renseigné:
data_tableau1[ , compteur_M_non_na := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_vict) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_mec) == FALSE), 1,
  default =0)
]
# h) Aucun des 3 lieux n'est renseigné:
data_tableau1[ , compteur_Neant := data.table::fcase(
  (is.na(data_tableau1$cog_com_22_inf) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_vict) == TRUE) & 
    (is.na(data_tableau1$cog_com_22_mec) == TRUE), 1,
  default =0)
]

tableau1 <- data_tableau1[ , .(
  Nb_atteintes = sum(compteur, na.rm = TRUE),
  Nb_atteintes_Neant = sum(compteur_Neant, na.rm = TRUE),
  Nb_atteintes_I = sum(compteur_I_non_na, na.rm = TRUE),
  Nb_atteintes_V = sum(compteur_V_non_na, na.rm = TRUE),
  Nb_atteintes_M = sum(compteur_M_non_na, na.rm = TRUE),
  Nb_atteintes_IV = sum(compteur_IV_non_na, na.rm = TRUE),
  Nb_atteintes_IM = sum(compteur_IM_non_na, na.rm = TRUE),
  Nb_atteintes_VM = sum(compteur_VM_non_na, na.rm = TRUE),
  Nb_atteintes_IVM = sum(compteur_IVM_non_na, na.rm = TRUE)),
  by = .(classe)]

tableau1 <- as.data.frame(tableau1)
tableau1$Neant <- round(tableau1$Nb_atteintes_Neant/tableau1$Nb_atteintes*100,2)
tableau1$I <- round(tableau1$Nb_atteintes_I/tableau1$Nb_atteintes*100,2)
tableau1$V <- round(tableau1$Nb_atteintes_V/tableau1$Nb_atteintes*100,2)
tableau1$M <- round(tableau1$Nb_atteintes_M/tableau1$Nb_atteintes*100,2)
tableau1$IV <- round(tableau1$Nb_atteintes_IV/tableau1$Nb_atteintes*100,2)
tableau1$IM <- round(tableau1$Nb_atteintes_IM/tableau1$Nb_atteintes*100,2)
tableau1$VM <- round(tableau1$Nb_atteintes_VM/tableau1$Nb_atteintes*100,2)
tableau1$IVM <- round(tableau1$Nb_atteintes_IVM/tableau1$Nb_atteintes*100,2)

tableau1 <- tableau1[order(tableau1$Nb_atteintes,decreasing = TRUE),]
tableau1 <- subset(tableau1, select=c(classe, Nb_atteintes,Neant,I,V,M,IV,IM,VM,IVM))

# Output:
tableau1

