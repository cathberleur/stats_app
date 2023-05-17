
# Partie 2 du mémoire Stat'app: 

# Création du tableau 2: Distribution du nombre d'infraction au niveau communal (pour 1000 habitants), 
# selon le type d'atteinte

#define quantiles of interest
q = c(.1, .25, .5, .75, .90, .95, .99)

tableau2 <- delinquance_com %>% 
        select(cog_com_22_inf,I,I_cambr,I_bless_famil,I_bless_horsfamil,
               I_destr_degrad,I_homic,I_viol_sex,I_vols_armes,I_vols_acces_vehic,
               I_vols_ds_vehic,I_vols_de_vehic,I_vols_sansviolence,I_vols_viol_sansarme) %>%
        pivot_longer(cols= starts_with("I"),
                     names_to = "type_atteinte",
                     values_to = "Nb_inf_pour_1000_hbts") %>%
  group_by(type_atteinte) %>%
  summarize(P10 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[1],na.rm=TRUE),1), 
            P25 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[2],na.rm=TRUE),1),
            P50 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[3],na.rm=TRUE),1),
            P75 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[4],na.rm=TRUE),1),
            P90 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[5],na.rm=TRUE),1),
            P95 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[6],na.rm=TRUE),1),
            P99 = round(quantile(Nb_inf_pour_1000_hbts, probs = q[7],na.rm=TRUE),1),
            )  %>%
  arrange(type_atteinte) %>%
  mutate(
    type = case_when(
      type_atteinte == "I" ~ "13-Ensemble",
      type_atteinte == "I_bless_famil" ~ "07-Coups et blessures volontaires dans la sphère familiale",
      type_atteinte == "I_bless_horsfamil" ~ "06-Coups et blessures volontaires en dehors de la sphère familiale",
      type_atteinte == "I_cambr" ~ "04-Cambriolages de logement",
      type_atteinte == "I_destr_degrad" ~ "02-Destructions et dégradations",
      type_atteinte == "I_homic" ~ "12-Homicides",
      type_atteinte == "I_viol_sex" ~ "10-Violences sexuelles",
      type_atteinte == "I_vols_acces_vehic" ~ "08-Vols d'accessoires sur véhicules",
      type_atteinte == "I_vols_armes" ~ "11-Vols avec armes",
      type_atteinte == "I_vols_de_vehic" ~ "05-Vols de véhicules",
      type_atteinte == "I_vols_ds_vehic" ~ "03-Vols dans les véhicules",
      type_atteinte == "I_vols_sansviolence" ~ "01-Vols sans violence contre des personnes",
      .default = "09-Vols violents sans arme"
    )) %>%
  arrange(type) %>% select(-type_atteinte) 

tableau2 <-cbind(tableau2[,8],tableau2[,1:7])

# graphique 1: lien entre nombre d'infractions dans une commune et sa taille (en termes de nombre d'habitants)
# scatter plot par type d'atteinte (avec en ordonnée le log du nombre d'infractions au niveau communal
# et en abscisse le nombre d'habitants en log) - le faire par type d'atteinte

data.graphique1 <- delinquance_com %>% 
  select(Nb_I,Nb_I_cambr,Nb_I_bless_famil,Nb_I_bless_horsfamil,
         Nb_I_destr_degrad,Nb_I_homic,Nb_I_viol_sex,Nb_I_vols_armes,Nb_I_vols_acces_vehic,
         Nb_I_vols_ds_vehic,Nb_I_vols_de_vehic,Nb_I_vols_sansviol,Nb_I_vols_viol_sansarme,P19_POP) %>% 
  log()

plot(data.graphique1$P19_POP,data.graphique1$Nb_I,main = "", xlab="Nombre d'habitants (log)", ylab="Nombre d'infractions (log)")

       
# On génère un plot par type d'atteinte: à mettre en Annexe?
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_cambr,main = "Cambriolages", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_bless_famil,main = "Blessures intra-familiales", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_bless_horsfamil,main = "Blessures extra-familiales", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_destr_degrad,main = "Destructions, dégradations", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_homic,main = "Homicides", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_viol_sex,main = "Violences sexuelles", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_armes,main = "Violences avec armes", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_acces_vehic,main = "Vols d'accessoires de véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_ds_vehic,main = "Vols dans les véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_de_vehic,main = "Vols de véhicules", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_sansviol,main = "Vol sans violence", xlab="Pop (log)", ylab="Nb I (log)")
plot(data.graphique1$P19_POP,data.graphique1$Nb_I_vols_viol_sansarme,main = "Vol violents sans armes", xlab="Pop (log)", ylab="Nb I (log)")

# Graphique 2: matrice de corrélation des nombres d'infractions au niveau communal (pour 1000 habitants) par type d'atteinte
data.graphique2 <-  delinquance_com %>% filter (P19_POP>0) %>%
  select(I_cambr,I_bless_famil,I_bless_horsfamil,
         I_destr_degrad,I_homic,I_viol_sex,I_vols_armes,I_vols_vehic,
         I_vols_sansviolence,I_vols_viol_sansarme) %>%
  drop_na()

# Matrice de corrélation:
data.graphique2.cor = cor(data.graphique2)
data.graphique2.cor

corrplot(data.graphique2.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)


# Graphs 2a et 2b à  mettre en Annexe (ou à supprimer du Mémoire-> car résultats peu intéressants):

# Création de la graphique 2a_bis: calcul des coefficients de corrélation de Pearson entre le nombre d'infractions (I) et le nombre
# de victimes (pour 1000 habitants et selon le type d'atteinte):

data.graphique2a_annexe <-  delinquance_com %>%
                        select(I,I_cambr,I_bless_famil,I_bless_horsfamil,
                        I_destr_degrad,I_homic,I_viol_sex,I_vols_armes,I_vols_acces_vehic,
                        I_vols_ds_vehic,I_vols_de_vehic,I_vols_sansviolence,I_vols_viol_sansarme,
                        V,V_cambr,V_bless_famil,V_bless_horsfamil,
                        V_destr_degrad,V_homic,V_viol_sex,V_vols_armes,V_vols_acces_vehic,
                        V_vols_ds_vehic,V_vols_de_vehic,V_vols_sansviol,V_vols_viol_sansarme) %>%
                 filter(I>0 & I_cambr>0 & I_bless_famil>0 & I_bless_horsfamil>0 & I_destr_degrad>0 &
                           I_homic>0 & I_viol_sex>0 & I_vols_armes>0 & I_vols_acces_vehic &
                           I_vols_ds_vehic>0 & I_vols_de_vehic>0 & I_vols_sansviolence>0 &
                           I_vols_viol_sansarme>0 & 
                           V>0 & V_cambr>0 & V_bless_famil>0 & V_bless_horsfamil>0 & V_destr_degrad>0 &
                           V_homic>0 & V_viol_sex>0 & V_vols_armes>0 & V_vols_acces_vehic &
                           V_vols_ds_vehic>0 & V_vols_de_vehic>0 & V_vols_sansviol>0 &
                           V_vols_viol_sansarme>0) 

# Matrice de corrélation:
data.graphique2a_annexe.cor = cor(data.graphique2a_annexe)

corrplot(data.graphique2a_annexe.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Graphique 2b: calcul des coefficients de corrélation de Pearson entre le nombre d'infractions (I),le nombre
# de victimes (V) et le nombre de mis en cause (M) (pour 1000 habitants et selon le type d'atteinte corporelle):

data.graphique2b_annexe <-  delinquance_com %>%
  select(I_bless_famil,I_bless_horsfamil,I_homic,I_viol_sex,
         V_bless_famil,V_bless_horsfamil,V_homic,V_viol_sex,
         M_bless_famil,M_bless_horsfamil,M_homic,M_viol_sex) %>%
  filter(I_bless_famil>0 & I_bless_horsfamil>0 &
           I_homic>0 & I_viol_sex>0 &
           V_bless_famil>0 & V_bless_horsfamil>0 &
           V_homic>0 & V_viol_sex>0 &
           M_bless_famil>0 & M_bless_horsfamil>0 &
           M_homic>0 & M_viol_sex>0) 

# Matrice de corrélation:
data.graphique2b_annexe.cor = cor(data.graphique2b)

corrplot(data.graphique2b_annexe.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Idée-proposition: on constate que pour chaque type d'atteintes a les variables
# I_a et V_a sont très corrélées entre elles (de même entre I_a, V_a et M_a pour les atteintes
# corporelles), mais en fait c'est très attendu, étant donné que par construction même ces variables
# sont linéairement liées..., donc 

# Ne verrait-on pas mieux les atteintes où les victimes habitent dans une commune différente du lieu
# de commission de l'infraction, si l'on calculait l'écart entre I_a et V_a et que l'on en représentait 
# le box plot par type d'atteinte?
# ci-dessous, une proposition de graphiques 3a et 3b:

# Graphique 3a: Distribution de la différence des log du nombre d'infractions et du nombre de victimes 
# mesurée dans chaque commune (pour 1000 habitants) - 
# Note: seules les atteintes non corporelles sont considérées ici!
data.graphique3a <-  delinquance_com %>% filter(P19_POP>0) %>%
  select(I_cambr,I_destr_degrad,I_vols_armes,I_vols_vehic,
         I_vols_sansviolence,I_vols_viol_sansarme,
         V_cambr,V_destr_degrad,V_vols_armes,V_vols_vehic,
        V_vols_sansviol,V_vols_viol_sansarme) %>%
  filter(I_cambr>0 & I_destr_degrad>0 &
           I_vols_armes>0 & I_vols_vehic>0 & I_vols_sansviolence>0 &
           I_vols_viol_sansarme>0 & 
           V_cambr>0 & V_destr_degrad>0 &
          V_vols_armes>0 &
           V_vols_vehic>0 & V_vols_sansviol>0 &
           V_vols_viol_sansarme>0) %>% log() %>%
  mutate(dlog_I_V_cambr = I_cambr - V_cambr,
         dlog_I_V_destr_degrad = I_destr_degrad - V_destr_degrad,
         dlog_I_V_vols_armes = I_vols_armes - V_vols_armes,
         dlog_I_V_vols_vehic = I_vols_vehic - V_vols_vehic,
         dlog_I_V_vols_sansviol = I_vols_sansviolence - V_vols_sansviol,
         dlog_I_V_vols_viol_sansarme = I_vols_viol_sansarme - V_vols_viol_sansarme
        ) %>% select(dlog_I_V_cambr,dlog_I_V_destr_degrad,
                     dlog_I_V_vols_armes,
                     dlog_I_V_vols_vehic,dlog_I_V_vols_sansviol,dlog_I_V_vols_viol_sansarme) %>%
   pivot_longer(cols = everything(),names_to = "type_atteinte",names_prefix = "dlog_I_V_",values_to = "dlog_I_V")

# Création du graphique 3a: 
  ggplot(data.graphique3a, aes(x = type_atteinte, y = dlog_I_V)) + geom_boxplot()
  
  # pour le commentaire, on sort les quartiles:

  q = c(.25, .5, .75)
  
   quartiles.graphique3a <- data.graphique3a %>% 
    group_by(type_atteinte) %>%
    summarize( 
              P25 = round(quantile(dlog_I_V, probs = q[1],na.rm=TRUE),1),
              P50 = round(quantile(dlog_I_V, probs = q[2],na.rm=TRUE),1),
              P75 = round(quantile(dlog_I_V, probs = q[3],na.rm=TRUE),1),
    ) 
   quartiles.graphique3a
   
# Graphique 3b: 
# 1- Distribution de la différence des log du nombre d'infractions et du nombre de victimes
# mesurée dans chaque commune (pour 1000 habitants) 
# 2- Distribution de la différence des log du nombre d'infractions et du nombre de mis en cause 
# mesurée dans chaque commune (pour 1000 habitants) 
# Note: seules les atteintes corporelles sont considérées ici!
   data.graphique3b_1 <-  delinquance_com %>% filter(P19_POP>0) %>%
     select(I_bless_famil,I_bless_horsfamil,I_homic,I_viol_sex,
            V_bless_famil,V_bless_horsfamil,V_homic,V_viol_sex,
            M_bless_famil,M_bless_horsfamil,M_homic,M_viol_sex) %>%
     filter(I_bless_famil>0 & I_bless_horsfamil>0 & I_homic>0 & I_viol_sex>0 &
            V_bless_famil>0 & V_bless_horsfamil>0 & V_homic>0 & V_viol_sex>0 &
            M_bless_famil>0 & M_bless_horsfamil>0 & M_homic>0 & M_viol_sex>0) %>% log() %>%
     mutate(dlog_I_V_bless_famil = I_bless_famil - V_bless_famil,
            dlog_I_V_bless_horsfamil = I_bless_horsfamil - V_bless_horsfamil,
            dlog_I_V_homic = I_homic - V_homic,
            dlog_I_V_viol_sex = I_viol_sex - V_viol_sex
     ) %>% select(starts_with("dlog")) %>%
     pivot_longer(cols = everything(),names_to = "type_atteinte",names_prefix = "dlog_I_V_",values_to = "dlog_I_V")
   
   data.graphique3b_2 <-  delinquance_com %>% filter(P19_POP>0) %>%
     select(I_bless_famil,I_bless_horsfamil,I_homic,I_viol_sex,
            V_bless_famil,V_bless_horsfamil,V_homic,V_viol_sex,
            M_bless_famil,M_bless_horsfamil,M_homic,M_viol_sex) %>%
     filter(I_bless_famil>0 & I_bless_horsfamil>0 & I_homic>0 & I_viol_sex>0 &
              V_bless_famil>0 & V_bless_horsfamil>0 & V_homic>0 & V_viol_sex>0 &
              M_bless_famil>0 & M_bless_horsfamil>0 & M_homic>0 & M_viol_sex>0) %>% log() %>%
     mutate(
            dlog_I_M_bless_famil = I_bless_famil - M_bless_famil,
            dlog_I_M_bless_horsfamil = I_bless_horsfamil - M_bless_horsfamil,
            dlog_I_M_homic = I_homic - M_homic,
            dlog_I_M_viol_sex = I_viol_sex - M_viol_sex
     ) %>% select(starts_with("dlog")) %>%
     pivot_longer(cols = everything(),names_to = "type_atteinte",names_prefix = "dlog_I_M_",values_to = "dlog_I_M")
   
# Création du graphique 3b_1:
  ggplot(data.graphique3b_1, aes(x = type_atteinte, y = dlog_I_V)) + geom_boxplot()
  
  # pour le commentaire, on sort les quartiles:
  
  q = c(.25, .5, .75)
  
  quartiles.graphique3b_1<- data.graphique3b_1 %>% 
    group_by(type_atteinte) %>%
    summarize( 
      P25 = round(quantile(dlog_I_V, probs = q[1],na.rm=TRUE),1),
      P50 = round(quantile(dlog_I_V, probs = q[2],na.rm=TRUE),1),
      P75 = round(quantile(dlog_I_V, probs = q[3],na.rm=TRUE),1),
    ) 
  quartiles.graphique3b_1
  
# Création du graphique 3b_2:
  ggplot(data.graphique3b_2, aes(x = type_atteinte, y = dlog_I_M)) + geom_boxplot()

  # pour le commentaire, on sort les quartiles:
  
  q = c(.25, .5, .75)
  
  quartiles.graphique3b_2 <- data.graphique3b_2 %>% 
    group_by(type_atteinte) %>%
    summarize( 
      P25 = round(quantile(dlog_I_M, probs = q[1],na.rm=TRUE),1),
      P50 = round(quantile(dlog_I_M, probs = q[2],na.rm=TRUE),1),
      P75 = round(quantile(dlog_I_M, probs = q[3],na.rm=TRUE),1),
    ) 
  
  quartiles.graphique3b_2
  

# Tableau 3a: Proportion d'infractions (I) associées à un couple de communes (I,V) présentes
# dans un même zonage d'étude - selon le type d'atteinte 

tableau3a <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) &
                         (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) &
                         (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) &
                         (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE), .(
  Nb_I_IV = sum(compteur, na.rm = TRUE),
  Nb_I_IV_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui")),na.rm = TRUE),
  Nb_I_IV_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui")),na.rm = TRUE),
  Nb_I_IV_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui")),na.rm = TRUE),
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV_UU*100,1),
                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV_AAV*100,1),
                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV_CENTR*100,1))
                  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
                    ][order(classe)]

tableau3a

# Tableau 3b: Proportion d'infractions corporelles (I) associées à un triplet de communes (I,V,M) présentes
# dans un même zonage d'étude - selon le type d'atteinte


tableau3b <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
                         (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
                         (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
                         (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) & (is.na(TYPE_mec)==FALSE) &
                         (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE) & (is.na(P_NP5CLA_mec)==FALSE) &
                         (classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles")), .(
  Nb_I_IVM = sum(compteur, na.rm = TRUE),
  Nb_I_IVM_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui") & (M_ds_UU == "oui")),na.rm = TRUE),
  Nb_I_IVM_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui") & (M_ds_AAV == "oui")),na.rm = TRUE),
  Nb_I_IVM_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui") & (M_ds_CENTR == "oui")),na.rm = TRUE),
  Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM_UU*100,1),
                         Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                         Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                         Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                         Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM_AAV*100,1),
                         Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM_CENTR*100,1))
  ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
  ][order(classe)]

tableau3b

# Afin de mieux comparer la pertinence des différents zonages d'étude pour appréhender la géographie des différents types
# d'atteinte, on va tenir compte des différences de taille des différents zonages (par ex: les ZE contiennent beaucoup plus
# de communes qu'une UU)
# Pour ce faire, on va diviser les proportions mesurées dans le tableau 3a par la probabilité que deux communes prises au
# hasard (évenements équiprobables) se situent dans un même zonage z = 1/(nb de zonages)^2
# Et de même dans le tableau 3b, on va diviser les proportions trouvées par la proba que 3 communes se situent dans un
# même zonage z =1(nb de zonages)^3.

# Calcul des probas pour chaque zonage:
# on commence par déterminer le nombre de zonages de chaque type:
nb_ZE <-atteintes[(is.na(ZE2020_inf)==FALSE),
                  .(nb_ZE2020 = uniqueN(ZE2020_inf))]
print(nb_ZE) # 306 ZE
nb_BV <-atteintes[(is.na(BV2022_inf)==FALSE),
                  .(nb_BV2022 = uniqueN(BV2022_inf))]
print(nb_BV) # 1 707 BV
nb_UU <-atteintes[(is.na(UU2020_inf)==FALSE),
                  .(nb_UU2020 = uniqueN(UU2020_inf))]
print(nb_UU) # 2 565 UU (warning: ne forment pas une partition du territoire français)
nb_AAV <-atteintes[(is.na(AAV2020_inf)==FALSE),
                  .(nb_AAV2020 = uniqueN(AAV2020_inf))]
print(nb_AAV) # 700 AAV (warning: ne forment pas une partition du territoire français)
nb_GD <-atteintes[(is.na(GRD_inf)==FALSE),
                   .(nb_GD = uniqueN(GRD_inf))]
print(nb_GD) # 4 GD (mais attention, ce n'est pas comparable avec les 4 premiers zonages, car ça ne correspond pas à 
# des zones géographiques précises ici mais plutôt à une catégorisation des communes entre urbain dense,urbain densité
# intermédiaire,rural périurbain,rural non périurbain);
nb_CENTR <-atteintes[(is.na(P_NP5CLA_inf)==FALSE),
                  .(nb_CENTR = uniqueN(P_NP5CLA_inf))]
print(nb_CENTR) # 5 CENTR (mais attention, ce n'est pas comparable avec les 4 premiers zonages, car ça ne correspond pas à 
# des zones géographiques précises ici mais plutôt à une catégorisation des communes selon son niveau de centralité:
# commune non centre/centre local d'équipements et de services/centre intermédiaire d'équipements et de services/
# centre structurant d'équipements et de services/centre majeur d'équipements et de services);


# Pour les zonages qui constituent une partition du territoire, le calcul est direct:
# ZE: proba = 1/306^2 = 1.067965e-05
# BV: proba = 1/1707^2 = 3.431887e-07
# GD: proba = 1/4^2 = 0.0625
# CENTR: proba = 1/5^2 = 0.04

# Pour les zonages UU et AAV, le calcul est légèrement plus compliqué:
# Contrairement aux autre zonages, UU et AAV ne forment pas une partition du territoire français...
# -> il semble donc difficile de les mettre sur les mêmes plans que les autres en calculant une proba, non?
# A minima, pour ces 2 zonages, il faut conditionner la proba que deux communes se situent dans le même zonage par l'évenement
# que deux communes soient tirés dans une UU resp. dans une AAV.
# Du coup, on aurait: proba (2 communes se situent dans la même UU sachant que les 2 communes appartiennent à une UU)
# = proba(la 1ère commune appartient à la UU x ET la 2ème commune appartient à la UU x)/proba (2 communes appartiennent à une UU)
# = 1/(nb de UU)^2/(nb de commune qui appartiennent à une UU/nb de communes total)^2

# UU: proba = (1/2565^2)/(7721/34960)^2 = 3.116169e-06
nb_com <-atteintes[(is.na(cog_com_22_inf)==FALSE),
                  .(nb_com = uniqueN(cog_com_22_inf,na.rm = TRUE))]
print(nb_com) # 34 960 communes.
nb_com_UU <-atteintes[(is.na(cog_com_22_inf)==FALSE) & (I_ds_UU == "oui"),
                   .(nb_com_UU = uniqueN(cog_com_22_inf,na.rm = TRUE))]
print(nb_com_UU) # 7 721 communes.

# AAV: proba = (1/700^2)/(26107/34960)^2 = 3.659593e-06
nb_com_AAV <-atteintes[(is.na(cog_com_22_inf)==FALSE) & (I_ds_AAV == "oui"),
                      .(nb_com_AAV = uniqueN(cog_com_22_inf,na.rm = TRUE))]
print(nb_com_AAV) # 26 107 communes.
# on stocke ces probas dans un vecteur (dans l'ordre des colonnes des tableaux 3a et 3b: UU,GD,ZE,BV,AAV,CENTR)
proba_theorique=c(3.116484e-06,0.0625,1.067965e-05,3.431887e-07,3.659629e-06,0.04)  

# On regénère les 2 tableaux précédents "ajustés" de ces proba théoriques permettant de tenir compte des différences
# de taille des différents zonages et ainsi permettre une meilleure comparabilité:

# Tableau 3a_bis: Proportion d'infractions (I) associées à un couple de communes (I,V) présentes
# dans un même zonage d'étude - selon le type d'atteinte
# corrigée des différences de taille des zonages.
tableau3a_bis <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                             (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) &
                             (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) &
                             (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) &
                             (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) &
                             (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) &
                             (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE), .(
  Nb_I_IV = sum(compteur, na.rm = TRUE),
  Nb_I_IV_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui")),na.rm = TRUE),
  Nb_I_IV_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui")),na.rm = TRUE),
  Nb_I_IV_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui")),na.rm = TRUE),
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round((Nb_I_IV_1UU/Nb_I_IV_UU)/proba_theorique[1],1),
                         Prop_I_IV_1GD = round((Nb_I_IV_1GD/Nb_I_IV)/proba_theorique[2],1),
                         Prop_I_IV_1ZE = round((Nb_I_IV_1ZE/Nb_I_IV)/proba_theorique[3],1),
                         Prop_I_IV_1BV = round((Nb_I_IV_1BV/Nb_I_IV)/proba_theorique[4],1),
                         Prop_I_IV_1AAV = round((Nb_I_IV_1AAV/Nb_I_IV_AAV)/proba_theorique[5],1),
                         Prop_I_IV_1CENTR = round((Nb_I_IV_1CENTR/Nb_I_IV_CENTR)/proba_theorique[6],1))
  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
  ][order(classe)]

tableau3a_bis

# Tableau 3b_bis: Proportion d'infractions corporelles (I) associées à un triplet de communes (I,V,M) présentes
# dans un même zonage d'étude - selon le type d'atteinte
# corrigée des différences de taille des zonages.

# on calcule pour chaque zonage la proba que trois communes prises au hasard appartiennent au même zonage
# ZE: proba = 1/306^3 = 3.490083e-08
# BV: proba = 1/1707^3 = 2.010479e-10
# GD: proba = 1/4^3 = 0.015625
# CENTR: proba = 1/5^3 = 0.008
# UU: proba = (1/2565^3)/(7721/34960)^3 = 5.500872e-09
# AAV: proba = (1/700^2)/(26107/34960)^2 = 3.659593e-06

proba_theorique2=c(5.500872e-09,0.015625,3.490083e-08,2.010479e-10,3.659593e-06,0.008)

tableau3b_bis <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
                             (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
                             (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
                             (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
                             (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
                             (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) & (is.na(TYPE_mec)==FALSE) &
                             (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE) & (is.na(P_NP5CLA_mec)==FALSE) &
                            classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"), .(
                                           Nb_I_IVM = sum(compteur, na.rm = TRUE),
                                           Nb_I_IVM_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui") & (M_ds_UU == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui") & (M_ds_AAV == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui") & (M_ds_CENTR == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                       by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round((Nb_I_IVM_1UU/Nb_I_IVM_UU)/proba_theorique2[1],1),
                                              Prop_I_IVM_1GD = round((Nb_I_IVM_1GD/Nb_I_IVM)/proba_theorique2[2],1),
                                              Prop_I_IVM_1ZE = round((Nb_I_IVM_1ZE/Nb_I_IVM)/proba_theorique2[3],1),
                                              Prop_I_IVM_1BV = round((Nb_I_IVM_1BV/Nb_I_IVM)/proba_theorique2[4],1),
                                              Prop_I_IVM_1AAV = round((Nb_I_IVM_1AAV/Nb_I_IVM_AAV)/proba_theorique2[5],1),
                                              Prop_I_IVM_1CENTR = round((Nb_I_IVM_1CENTR/Nb_I_IVM_CENTR)/proba_theorique2[6],1))
                       ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
                       ][order(classe)]

tableau3b_bis



# Zoom sur la crise de la Covid-19: voit-on un effet sur le degré de concentration spatiale des atteintes?
# idée: on s'attendrait à ce que la crise sanitaire ait augmenter la proportion des atteintes associées à 
# un couple (I,V) ou un triplet (I,V,M) de communes présentes dans un même zonage...

# On refait les tableaux 3a et 3b sur les période 2016-2019 et 2020-2021:

# A mettre en Annexe?

# Tableau 3a: 2016-2019 (période pré-Covid)
tableau3a_2016_2019_annexe <- atteintes[annee %between% c(2016,2019) &
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
    (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) &
    (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) &
    (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) &
    (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) &
    (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) &
    (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE), .(
  Nb_I_IV = sum(compteur, na.rm = TRUE),
  Nb_I_IV_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui")),na.rm = TRUE),
  Nb_I_IV_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui")),na.rm = TRUE),
  Nb_I_IV_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui")),na.rm = TRUE),
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV_UU*100,1),
                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV_AAV*100,1),
                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV_CENTR*100,1))
  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
  ][order(classe)]

tableau3a_2016_2019_annexe

# Tableau 3a: 2020-2021 (période Covid)
tableau3a_2020_2021_annexe <- atteintes[annee %between% c(2020,2021) &
                                    (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                                      (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) &
                                      (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) &
                                      (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) &
                                      (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) &
                                      (is.na(TYPE_inf)==FALSE) & (is.na(TYPE_vict)==FALSE) &
                                      (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE), .(
                                      Nb_I_IV = sum(compteur, na.rm = TRUE),
                                      Nb_I_IV_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui")),na.rm = TRUE),
                                      Nb_I_IV_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui")),na.rm = TRUE),
                                      Nb_I_IV_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui")),na.rm = TRUE),
                                      Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                                  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV_UU*100,1),
                                                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                                                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                                                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                                                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV_AAV*100,1),
                                                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV_CENTR*100,1))
                                  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
                                  ][order(classe)]

tableau3a_2020_2021_annexe


# Tableau 3b: 2016-2019 (période pré-Covid)
tableau3b_2016_2019_annexe <- atteintes[annee %between% c(2016,2019) &
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
    (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
    (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
    (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
    (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
    (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) & (is.na(TYPE_mec)==FALSE) &
    (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE) & (is.na(P_NP5CLA_mec)==FALSE) &
                        classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"), .(
                                           Nb_I_IVM = sum(compteur, na.rm = TRUE),
                                           Nb_I_IVM_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui") & (M_ds_UU == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui") & (M_ds_AAV == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui") & (M_ds_CENTR == "oui")),na.rm = TRUE),
                                           Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                       by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM_UU*100,1),
                                              Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM_AAV*100,1),
                                              Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM_CENTR*100,1))
                       ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
                       ][order(classe)]

tableau3b_2016_2019_annexe

# Tableau 3b: 2020_2021 (période Covid)
tableau3b_2020_2021_annexe <- atteintes[annee %between% c(2020,2021) &
                                   (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
                                     (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
                                     (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
                                     (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
                                     (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
                                     (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) & (is.na(GRD_mec)==FALSE) &
                                     (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE) & (is.na(P_NP5CLA_mec)==FALSE) &
                                 classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                   "Coups et blessures volontaires en dehors de la sphère familiale",
                                                   "Homicides",
                                                   "Violences sexuelles"), .(
                                                     Nb_I_IVM = sum(compteur, na.rm = TRUE),
                                                     Nb_I_IVM_UU = sum(compteur*((I_ds_UU == "oui") & (V_ds_UU == "oui") & (M_ds_UU == "oui")),na.rm = TRUE),
                                                     Nb_I_IVM_AAV = sum(compteur*((I_ds_AAV == "oui") & (V_ds_AAV == "oui") & (M_ds_AAV == "oui")),na.rm = TRUE),
                                                     Nb_I_IVM_CENTR = sum(compteur*((I_ds_CENTR == "oui") & (V_ds_CENTR == "oui") & (M_ds_CENTR == "oui")),na.rm = TRUE),
                                                     Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                                 by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM_UU*100,1),
                                                        Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM_AAV*100,1),
                                                        Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM_CENTR*100,1))
                                 ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
                                 ][order(classe)]

tableau3b_2020_2021_annexe

# 1) Statut des communes au sein du bassin de vie (BV):

# Tableau 4a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans un même BV, selon le statut
# respectif de la commune de I et celle de V au sein du BV. 
# Rappel: 00: "Non pôle"; 11: "Pôle partiel"; 12: "Commune associée à un pôle partiel" et 20: "Pôle".

tableau4a <- atteintes[(IV_ds_meme_BV == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                      (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(TYPE_COM_inf)==FALSE) &
                      (is.na(TYPE_COM_vict)==FALSE),
                      .(TYPE_COM_inf,TYPE_COM_vict,compteur)][, type_com_IV_ds_BV := data.table::fcase(
                          (TYPE_COM_inf =="00" & TYPE_COM_vict =="00"), "00 - 00",
                          (TYPE_COM_inf =="00" & TYPE_COM_vict =="11"), "00 - 11",
                          (TYPE_COM_inf =="00" & TYPE_COM_vict =="12"), "00 - 12",
                          (TYPE_COM_inf =="00" & TYPE_COM_vict =="20"), "00 - 20",
                          (TYPE_COM_inf =="11" & TYPE_COM_vict =="00"), "11 - 00",
                          (TYPE_COM_inf =="11" & TYPE_COM_vict =="11"), "11 - 11",
                          (TYPE_COM_inf =="11" & TYPE_COM_vict =="12"), "11 - 12",
                          (TYPE_COM_inf =="11" & TYPE_COM_vict =="20"), "11 - 20",
                          (TYPE_COM_inf =="12" & TYPE_COM_vict =="00"), "12 - 00",
                          (TYPE_COM_inf =="12" & TYPE_COM_vict =="11"), "12 - 11",
                          (TYPE_COM_inf =="12" & TYPE_COM_vict =="12"), "12 - 12",
                          (TYPE_COM_inf =="12" & TYPE_COM_vict =="20"), "12 - 20",
                          (TYPE_COM_inf =="20" & TYPE_COM_vict =="00"), "20 - 00",
                          (TYPE_COM_inf =="20" & TYPE_COM_vict =="11"), "20 - 11",
                          (TYPE_COM_inf =="20" & TYPE_COM_vict =="12"), "20 - 12",
                          (TYPE_COM_inf =="20" & TYPE_COM_vict =="20"), "20 - 20")][
                            , .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IV_ds_BV) 
                          ][order(type_com_IV_ds_BV)][,
                            Total_atteintes := sum(Nb_atteintes)
                          ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IV_ds_BV,Pct)]

# Tableau 4b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans un même BV, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein du BV. 
# Rappel: 00: "Non pôle"; 11: "Pôle partiel"; 12: "Commune associée à un pôle partiel" et 20: "Pôle".
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 4a ;)
# On se restreint ici aux seules atteintes corporelles.

tableau4b <- atteintes[(IVM_ds_meme_BV == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(cog_com_22_mec)==FALSE) &
                         (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
                         (is.na(TYPE_COM_inf)==FALSE) & (is.na(TYPE_COM_vict)==FALSE) & (is.na(TYPE_COM_mec)==FALSE) &
                         classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"),
                       .(TYPE_COM_inf,TYPE_COM_vict,TYPE_COM_mec,compteur)][, type_com_IVM_ds_BV := data.table::fcase(
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="00" & TYPE_COM_mec =="00"), "00 - 00 - 00",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="00" & TYPE_COM_mec =="11"), "00 - 00 - 11",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="00" & TYPE_COM_mec =="12"), "00 - 00 - 12",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="00" & TYPE_COM_mec =="20"), "00 - 00 - 20",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="20" & TYPE_COM_mec =="00"), "00 - 20 - 00",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="20" & TYPE_COM_mec =="11"), "00 - 20 - 11",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="20" & TYPE_COM_mec =="12"), "00 - 20 - 12",
                         (TYPE_COM_inf =="00" & TYPE_COM_vict =="20" & TYPE_COM_mec =="20"), "00 - 20 - 20",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="00" & TYPE_COM_mec =="00"), "20 - 00 - 00",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="00" & TYPE_COM_mec =="11"), "20 - 00 - 11",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="00" & TYPE_COM_mec =="12"), "20 - 00 - 12",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="00" & TYPE_COM_mec =="20"), "20 - 00 - 20",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="20" & TYPE_COM_mec =="00"), "20 - 20 - 00",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="20" & TYPE_COM_mec =="11"), "20 - 20 - 11",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="20" & TYPE_COM_mec =="12"), "20 - 20 - 12",
                         (TYPE_COM_inf =="20" & TYPE_COM_vict =="20" & TYPE_COM_mec =="20"), "20 - 20 - 20",
                         default = "Autres")][
                           , .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IVM_ds_BV) 
                         ][order(type_com_IVM_ds_BV)][,
                                                     Total_atteintes := sum(Nb_atteintes)
                         ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IVM_ds_BV,Pct)]

# Note: On pourra éventuellement regrouper certaines modalités avec des fréquences très faibles... 

# 2) Statut des communes au sein de l'unité urbaine (UU):

# Tableau 5a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans une même UU, selon le statut
# respectif de la commune de I et celle de V au sein de l'UU. 
# Rappel: H: "Hors UU"; C: "Ville-centre"; B: "Banlieue" et I: "Ville isolée".

tableau5a <- atteintes[(IV_ds_meme_UU == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(STATUT_2017_inf)==FALSE) &
                         (is.na(STATUT_2017_vict)==FALSE),
                       .(STATUT_2017_inf,STATUT_2017_vict,compteur)][, type_com_IV_ds_UU := data.table::fcase(
                         (STATUT_2017_inf =="H" & STATUT_2017_vict =="H"), "H - H",
                         (STATUT_2017_inf =="H" & STATUT_2017_vict =="C"), "H - C",
                         (STATUT_2017_inf =="H" & STATUT_2017_vict =="B"), "H - B",
                         (STATUT_2017_inf =="H" & STATUT_2017_vict =="I"), "H - I",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="H"), "C - H",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="C"), "C - C",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="B"), "C - B",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="I"), "C - I",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="H"), "B - H",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="C"), "B - C",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="B"), "B - B",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="I"), "B - I",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="H"), "I - H",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="C"), "I - C",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="B"), "I - B",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="I"), "I - I")][
                           , .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IV_ds_UU) 
                         ][order(type_com_IV_ds_UU)][,
                                                     Total_atteintes := sum(Nb_atteintes)
                         ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IV_ds_UU,Pct)]

# Tableau 5b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même UU, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de l'UU. 
# Rappel: H: "Hors UU"; C: "Ville-centre"; B: "Banlieue" et I: "Ville isolée".
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 5a ;)
# On se restreint ici aux seules atteintes corporelles.

tableau5b <- atteintes[(IVM_ds_meme_UU == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(cog_com_22_mec)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
                         (is.na(STATUT_2017_inf)==FALSE) & (is.na(STATUT_2017_vict)==FALSE) & 
                         (is.na(STATUT_2017_mec)==FALSE) &
                         classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"),
                       .(STATUT_2017_inf,STATUT_2017_vict,STATUT_2017_mec,compteur)][, type_com_IVM_ds_UU := data.table::fcase(
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="B" & STATUT_2017_mec =="H"), "B - B - H",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="B" & STATUT_2017_mec =="C"), "B - B - C",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="B" & STATUT_2017_mec =="B"), "B - B - B",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="B" & STATUT_2017_mec =="I"), "B - B - I",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="C" & STATUT_2017_mec =="H"), "B - C - H",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="C" & STATUT_2017_mec =="C"), "B - C - C",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="C" & STATUT_2017_mec =="B"), "B - C - B",
                         (STATUT_2017_inf =="B" & STATUT_2017_vict =="C" & STATUT_2017_mec =="I"), "B - C - I",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="B" & STATUT_2017_mec =="H"), "C - B - H",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="B" & STATUT_2017_mec =="C"), "C - B - C",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="B" & STATUT_2017_mec =="B"), "C - B - B",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="B" & STATUT_2017_mec =="I"), "C - B - I",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="C" & STATUT_2017_mec =="H"), "C - C - H",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="C" & STATUT_2017_mec =="C"), "C - C - C",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="C" & STATUT_2017_mec =="B"), "C - C - B",
                         (STATUT_2017_inf =="C" & STATUT_2017_vict =="C" & STATUT_2017_mec =="I"), "C - C - I",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="I" & STATUT_2017_mec =="H"), "I - I - H",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="I" & STATUT_2017_mec =="C"), "I - I - C",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="I" & STATUT_2017_mec =="B"), "I - I - B",
                         (STATUT_2017_inf =="I" & STATUT_2017_vict =="I" & STATUT_2017_mec =="I"), "I - I - I",
                         default = "Autres")][
                           , .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IVM_ds_UU) 
                         ][order(type_com_IVM_ds_UU)][,
                                                     Total_atteintes := sum(Nb_atteintes)
                         ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IVM_ds_UU,Pct)]

# 3) Statut des communes au sein de l'aire d'attraction des villes (AAV):

# Tableau 6a: Répartition (en %) des atteintes associées à un couple de communes (I,V) dans une même AAV, selon le statut
# respectif de la commune de I et celle de V au sein de l'AAV. 
# Rappel: 11: "Commune-centre"; 12: "Autre commune du pôle principal"; 13: "Commune d'un pôle secondaire";
# 20: "Commune de la couronne"; 30: "Commune hors AAV".

tableau6a <- atteintes[(IV_ds_meme_AAV == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(CATEAAV2020_inf)==FALSE) &
                         (is.na(CATEAAV2020_vict)==FALSE),
                       .(CATEAAV2020_inf,CATEAAV2020_vict,compteur)][, type_com_IV_ds_AAV := data.table::fcase(
                         (CATEAAV2020_inf =="11" & CATEAAV2020_vict =="11"), "11 - 11",
                         (CATEAAV2020_inf =="11" & CATEAAV2020_vict =="12"), "11 - 12",
                         (CATEAAV2020_inf =="11" & CATEAAV2020_vict =="13"), "11 - 13",
                         (CATEAAV2020_inf =="11" & CATEAAV2020_vict =="20"), "11 - 20",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="11"), "12 - 11",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="12"), "12 - 12",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="13"), "12 - 13",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="20"), "12 - 20",
                         (CATEAAV2020_inf =="13" & CATEAAV2020_vict =="11"), "13 - 11",
                         (CATEAAV2020_inf =="13" & CATEAAV2020_vict =="12"), "13 - 12",
                         (CATEAAV2020_inf =="13" & CATEAAV2020_vict =="13"), "13 - 13",
                         (CATEAAV2020_inf =="13" & CATEAAV2020_vict =="20"), "13 - 20",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="11"), "20 - 11",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="12"), "20 - 12",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="13"), "20 - 13",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="20"), "20 - 20",
                         default = "Autres")][, .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IV_ds_AAV) 
                         ][order(type_com_IV_ds_AAV)][,Total_atteintes := sum(Nb_atteintes)
                         ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IV_ds_AAV,Pct)]

# Tableau 6b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même AAV, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de l'AAV. 
# Rappel: 11: "Commune-centre"; 12: "Autre commune du pôle principal"; 13: "Commune d'un pôle secondaire";
# 20: "Commune de la couronne"; 30: "Commune hors AAV".
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 6a ;)
# On se restreint ici aux seules atteintes corporelles.
tableau6b <- atteintes[(IVM_ds_meme_AAV == "oui") & (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(cog_com_22_mec)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
                         (is.na(CATEAAV2020_inf)==FALSE) & (is.na(CATEAAV2020_vict)==FALSE) & (is.na(CATEAAV2020_mec)==FALSE) &
                         classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"),
                       .(CATEAAV2020_inf,CATEAAV2020_vict,CATEAAV2020_mec,compteur)][, type_com_IVM_ds_AAV := data.table::fcase(
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="11"), "12 - 12 - 11",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="12"), "12 - 12 - 12",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="13"), "12 - 12 - 13",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="20"), "12 - 12 - 20",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="11"), "12 - 20 - 11",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="12"), "12 - 20 - 12",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="13"), "12 - 20 - 13",
                         (CATEAAV2020_inf =="12" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="20"), "12 - 20 - 20",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="11"), "20 - 12 - 11",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="12"), "20 - 12 - 12",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="13"), "20 - 12 - 13",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="12" & CATEAAV2020_mec =="20"), "20 - 12 - 20",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="11"), "20 - 20 - 11",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="12"), "20 - 20 - 12",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="13"), "20 - 20 - 13",
                         (CATEAAV2020_inf =="20" & CATEAAV2020_vict =="20" & CATEAAV2020_mec =="20"), "20 - 20 - 20",
                         default = "Autres")][, .(Nb_atteintes = sum(compteur,na.rm = TRUE)),by = .(type_com_IVM_ds_AAV) 
                         ][order(type_com_IVM_ds_AAV)][,Total_atteintes := sum(Nb_atteintes)
                         ][, Pct := round(Nb_atteintes/Total_atteintes*100,2)][,.(type_com_IVM_ds_AAV,Pct)]

# TODO: on pourrait calculer les pourcentages pour chaque type d'atteinte?

# 3) Type des communes (I,V) (au sens de la GD) qui se retrouvent dans un même zonage:

# Tableau 7a:  Répartition (en %) des atteintes associées à un couple de communes (I,V) dans un même zonage, selon le statut
# respectif de la commune de I et celle de V au sein de la GD 
# Rappel: les différentes modalités de la GD sont:
# Urbain dense (UD); Urbain densité intermédiaire (UDI); Rural périurbain (RP); Rural non périurbain (RNP)

tableau7a <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) &
                         (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) &
                         (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) &
                         (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE),
                       .(GRD_inf,GRD_vict,compteur,IV_ds_meme_ZE,IV_ds_meme_BV,IV_ds_meme_UU,IV_ds_meme_AAV)
                       ][, type_GD_IV_1zonage := data.table::fcase(
                         (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense"), "UD - UD",
                         (GRD_inf =="Urbain dense" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
                         (GRD_inf =="Urbain dense" & GRD_vict =="Rural periurbain"), "UD - RP",
                         (GRD_inf =="Urbain dense" & GRD_vict =="Rural non periurbain"), "UD - RNP",
                         (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense"), "UDI - UD",
                         (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
                         (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain"), "UDI - RP",
                         (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural non periurbain"), "UDI - RNP",
                         (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain dense"), "RP - UD",
                         (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RP - UDI",
                         (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain"), "RP - RP",
                         (GRD_inf =="Rural periurbain" & GRD_vict =="Rural non periurbain"), "RP - RNP",
                         (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain dense"), "RNP - UD",
                         (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RNP - UDI",
                         (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural periurbain"), "RNP - RP",
                         (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain"), "RNP - RNP",
                         default = "Autres")][, .(
                           Nb_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"),na.rm = TRUE),
                           Nb_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"),na.rm = TRUE),
                           Nb_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"),na.rm = TRUE),
                           Nb_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"),na.rm = TRUE)
                           ),by = .(type_GD_IV_1zonage) 
                         ][order(type_GD_IV_1zonage)][,`:=`(Total_IV_1ZE = sum(Nb_IV_1ZE),Total_IV_1BV = sum(Nb_IV_1BV),
                                                          Total_IV_1UU = sum(Nb_IV_1UU),Total_IV_1AAV = sum(Nb_IV_1AAV))
                         ][, `:=`(Pct_IV_1ZE = round(Nb_IV_1ZE/Total_IV_1ZE*100,2),
                                  Pct_IV_1BV = round(Nb_IV_1BV/Total_IV_1BV*100,2),
                                  Pct_IV_1UU = round(Nb_IV_1UU/Total_IV_1UU*100,2),
                                  Pct_IV_1AAV = round(Nb_IV_1AAV/Total_IV_1AAV*100,2))
                           ][,.(type_GD_IV_1zonage,Pct_IV_1ZE,Pct_IV_1BV,Pct_IV_1UU,Pct_IV_1AAV)]


# Tableau 7b: Répartition (en %) des atteintes associées à un triplet de communes (I,V,M) dans une même zonage, selon le statut
# respectif de la commune de I, de celle de V et de celle de M au sein de la GD. 
# Rappel: les différentes modalités de la GD sont:
# Urbain dense (UD); Urbain densité intermédiaire (UDI); Rural périurbain (RP); Rural non périurbain (RNP)
# Remarque: pour construire nos modalités, on ne reprend que les modalités avec les plus grosses freq dans le tableau 7a ;)
# On se restreint ici aux seules atteintes corporelles.
tableau7b <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE) &
                         (is.na(BV2022_inf)==FALSE) & (is.na(BV2022_vict)==FALSE) & (is.na(BV2022_mec)==FALSE) &
                         (is.na(ZE2020_inf)==FALSE) & (is.na(ZE2020_vict)==FALSE) & (is.na(ZE2020_mec)==FALSE) &
                         (is.na(UU2020_inf)==FALSE) & (is.na(UU2020_vict)==FALSE) & (is.na(UU2020_mec)==FALSE) &
                         (is.na(AAV2020_inf)==FALSE) & (is.na(AAV2020_vict)==FALSE) & (is.na(AAV2020_mec)==FALSE) &
                         (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) & (is.na(GRD_mec)==FALSE) &
                         (is.na(P_NP5CLA_inf)==FALSE) & (is.na(P_NP5CLA_vict)==FALSE) & (is.na(P_NP5CLA_mec)==FALSE) &
                         classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"),
                       .(GRD_inf,GRD_vict,GRD_mec,compteur,IVM_ds_meme_ZE,IVM_ds_meme_BV,IVM_ds_meme_UU,IVM_ds_meme_AAV)
][, type_GD_IVM_1zonage := data.table::fcase(
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense" & GRD_mec =="Urbain dense"), "UD - UD - UD",
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense" & GRD_mec =="Urbain densité intermédiaire"), "UD - UD - UDI",
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense" & GRD_mec =="Rural periurbain"), "UD - UD - RP",
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense" & GRD_mec =="Rural non periurbain"), "UD - UD - RNP",
  
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense" & GRD_mec =="Urbain dense"), "UDI - UD - UD",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense" & GRD_mec =="Urbain densité intermédiaire"), "UDI - UD - UDI",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense" & GRD_mec =="Rural periurbain"), "UDI - UD - RP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense" & GRD_mec =="Rural non periurbain"), "UDI - UD - RNP",

  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Urbain dense"), "UDI - UDI - UD",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Urbain densité intermédiaire"), "UDI - UDI - UDI",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Rural periurbain"), "UDI - UDI - RP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Rural non periurbain"), "UDI - UDI - RNP",

  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain" & GRD_mec =="Urbain dense"), "UDI - RP - UD",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain" & GRD_mec =="Urbain densité intermédiaire"), "UDI - RP - UDI",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain" & GRD_mec =="Rural periurbain"), "UDI - RP - RP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain" & GRD_mec =="Rural non periurbain"), "UDI - RP - RNP",
  
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Urbain dense"), "RP - UDI - UD",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Urbain densité intermédiaire"), "RP - UDI - UDI",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Rural periurbain"), "RP - UDI - RP",
  (GRD_inf =="Rural periurbaine" & GRD_vict =="Urbain densité intermédiaire" & GRD_mec =="Rural non periurbain"), "RP - UDI - RNP",
  
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain" & GRD_mec =="Urbain dense"), "RP - RP - UD",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain" & GRD_mec =="Urbain densité intermédiaire"), "RP - RP - UDI",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain" & GRD_mec =="Rural periurbain"), "RP - RP - RP",
  (GRD_inf =="Rural periurbaine" & GRD_vict =="Rural periurbain" & GRD_mec =="Rural non periurbain"), "RP - RP - RNP",

  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain" & GRD_mec =="Urbain dense"), "RNP - RP - UD",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain" & GRD_mec =="Urbain densité intermédiaire"), "RNP - RP - UDI",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain" & GRD_mec =="Rural periurbain"), "RNP - RP - RP",
  (GRD_inf =="Rural non periurbaine" & GRD_vict =="Rural non periurbain" & GRD_mec =="Rural non periurbain"), "RNP - RP - RNP",
  
  default = "Autres")][, .(
    Nb_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"),na.rm = TRUE),
    Nb_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"),na.rm = TRUE),
    Nb_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"),na.rm = TRUE),
    Nb_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"),na.rm = TRUE)
  ),by = .(type_GD_IVM_1zonage) 
  ][order(type_GD_IVM_1zonage)][,`:=`(Total_IVM_1ZE = sum(Nb_IVM_1ZE),Total_IVM_1BV = sum(Nb_IVM_1BV),
                                     Total_IVM_1UU = sum(Nb_IVM_1UU),Total_IVM_1AAV = sum(Nb_IVM_1AAV))
  ][, `:=`(Pct_IVM_1ZE = round(Nb_IVM_1ZE/Total_IVM_1ZE*100,2),
           Pct_IVM_1BV = round(Nb_IVM_1BV/Total_IVM_1BV*100,2),
           Pct_IVM_1UU = round(Nb_IVM_1UU/Total_IVM_1UU*100,2),
           Pct_IVM_1AAV = round(Nb_IVM_1AAV/Total_IVM_1AAV*100,2))
  ][,.(type_GD_IVM_1zonage,Pct_IVM_1ZE,Pct_IVM_1BV,Pct_IVM_1UU,Pct_IVM_1AAV)]

# Une variante des tableaux 7: pour chaque type d'atteinte, répartition des couples de communes (I,V) selon les différentes
# modalités de la grille de densité:

# on se concentre d'abord sur les seules atteintes non corporelles (7a_bis):
tableau7a_bis <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                         (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) &
                           !(classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                           "Coups et blessures volontaires en dehors de la sphère familiale",
                                           "Homicides",
                                           "Violences sexuelles")),
                       .(GRD_inf,GRD_vict,compteur)
][, type_GD_IV := data.table::fcase(
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense"), "UD - UD",
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_inf =="Urbain dense" & GRD_vict =="Rural periurbain"), "UD - RP",
  (GRD_inf =="Urbain dense" & GRD_vict =="Rural non periurbain"), "UD - RNP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense"), "UDI - UD",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain"), "UDI - RP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural non periurbain"), "UDI - RNP",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain dense"), "RP - UD",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RP - UDI",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain"), "RP - RP",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural non periurbain"), "RP - RNP",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain dense"), "RNP - UD",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RNP - UDI",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural periurbain"), "RNP - RP",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain"), "RNP - RNP",
  default = "Autres")][, .(
    Nb_cambr = sum(compteur*(classe == "Cambriolages de logement"),na.rm = TRUE),
    Nb_destr_degrad = sum(compteur*(classe == "Destructions et dégradations"),na.rm = TRUE),
    Nb_vols_arme = sum(compteur*(classe == "Vols avec armes"),na.rm = TRUE),
    Nb_vols_viol_sansarme = sum(compteur*(classe == "Vols violents sans arme"),na.rm = TRUE),
    Nb_vols_sansviol = sum(compteur*(classe == "Vols sans violence contre des personnes"),na.rm = TRUE),
    Nb_vols_vehic = sum(compteur*(classe %chin% c("Vols d'accessoires sur véhicules","Vols dans les véhicules","Vols de véhicules")),na.rm = TRUE)
  ),by = .(type_GD_IV) 
  ][order(type_GD_IV)][,`:=`(Total_cambr = sum(Nb_cambr),Total_destr_degrad = sum(Nb_destr_degrad),
                                     Total_vols_arme = sum(Nb_vols_arme),Total_vols_viol_sansarme = sum(Nb_vols_viol_sansarme),
                             Total_vols_sansviol = sum(Nb_vols_sansviol),Total_vols_vehic = sum(Nb_vols_vehic))
  ][, `:=`(Pct_cambr = round(Nb_cambr/Total_cambr*100,2),
           Pct_destr_degrad = round(Nb_destr_degrad/Total_destr_degrad*100,2),
           Pct_vols_arme = round(Nb_vols_arme/Total_vols_arme*100,2),
           Pct_vols_viol_sansarme = round(Nb_vols_viol_sansarme/Total_vols_viol_sansarme*100,2),
           Pct_vols_sansviol = round(Nb_vols_sansviol/Total_vols_sansviol*100,2),
           Pct_vols_vehic = round(Nb_vols_vehic/Total_vols_vehic*100,2)
           )
  ][,.(type_GD_IV,Pct_cambr,Pct_destr_degrad,Pct_vols_arme,Pct_vols_viol_sansarme,Pct_vols_sansviol,Pct_vols_vehic)]

# puis on se concentre sur les seules atteintes corporelles (7b_bis):
tableau7b_bis <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                             (is.na(GRD_inf)==FALSE) & (is.na(GRD_vict)==FALSE) &
                             (classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                               "Coups et blessures volontaires en dehors de la sphère familiale",
                                               "Homicides",
                                               "Violences sexuelles")),
                           .(GRD_inf,GRD_vict,compteur,classe)
][, type_GD_IV := data.table::fcase(
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain dense"), "UD - UD",
  (GRD_inf =="Urbain dense" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_inf =="Urbain dense" & GRD_vict =="Rural periurbain"), "UD - RP",
  (GRD_inf =="Urbain dense" & GRD_vict =="Rural non periurbain"), "UD - RNP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense"), "UDI - UD",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain"), "UDI - RP",
  (GRD_inf =="Urbain densité intermédiaire" & GRD_vict =="Rural non periurbain"), "UDI - RNP",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain dense"), "RP - UD",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RP - UDI",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural periurbain"), "RP - RP",
  (GRD_inf =="Rural periurbain" & GRD_vict =="Rural non periurbain"), "RP - RNP",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain dense"), "RNP - UD",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RNP - UDI",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural periurbain"), "RNP - RP",
  (GRD_inf =="Rural non periurbain" & GRD_vict =="Rural non periurbain"), "RNP - RNP",
  default = "Autres")][, .(
    Nb_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"),na.rm = TRUE),
    Nb_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"),na.rm = TRUE),
    Nb_homicides = sum(compteur*(classe == "Homicides"),na.rm = TRUE),
    Nb_viol_sex = sum(compteur*(classe == "Violences sexuelles"),na.rm = TRUE)
  ),by = .(type_GD_IV) 
  ][order(type_GD_IV)][,`:=`(Total_bless_famil = sum(Nb_bless_famil),Total_bless_horsfamil = sum(Nb_bless_horsfamil),
                             Total_homicides = sum(Nb_homicides),Total_viol_sex = sum(Nb_viol_sex))
  ][, `:=`(Pct_bless_famil = round(Nb_bless_famil/Total_bless_famil*100,2),
           Pct_bless_horsfamil = round(Nb_bless_horsfamil/Total_bless_horsfamil*100,2),
           Pct_homicides = round(Nb_homicides/Total_homicides*100,2),
           Pct_viol_sex = round(Nb_viol_sex/Total_viol_sex*100,2)
  )
  ][,.(type_GD_IV,Pct_bless_famil,Pct_bless_horsfamil,Pct_homicides,Pct_viol_sex)]

# ultime variante sur les atteintes corporelles: on peut dupliquer le tableau 7b_bis non plus en étudiant la répartition
# selon  le couple de communes (I,V) dans la GD mais selon le couple (M,V) dans la GD:

tableau7b_ter <- atteintes[(is.na(cog_com_22_mec)==FALSE) & (is.na(cog_com_22_vict)==FALSE) &
                             (is.na(GRD_mec)==FALSE) & (is.na(GRD_vict)==FALSE) &
                             (classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                              "Coups et blessures volontaires en dehors de la sphère familiale",
                                              "Homicides",
                                              "Violences sexuelles")),
                           .(GRD_mec,GRD_vict,compteur,classe)
][, type_GD_MV := data.table::fcase(
  (GRD_mec =="Urbain dense" & GRD_vict =="Urbain dense"), "UD - UD",
  (GRD_mec =="Urbain dense" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_mec =="Urbain dense" & GRD_vict =="Rural periurbain"), "UD - RP",
  (GRD_mec =="Urbain dense" & GRD_vict =="Rural non periurbain"), "UD - RNP",
  (GRD_mec =="Urbain densité intermédiaire" & GRD_vict =="Urbain dense"), "UDI - UD",
  (GRD_mec =="Urbain densité intermédiaire" & GRD_vict =="Urbain densité intermédiaire"), "UDI - UDI",
  (GRD_mec =="Urbain densité intermédiaire" & GRD_vict =="Rural periurbain"), "UDI - RP",
  (GRD_mec =="Urbain densité intermédiaire" & GRD_vict =="Rural non periurbain"), "UDI - RNP",
  (GRD_mec =="Rural periurbain" & GRD_vict =="Urbain dense"), "RP - UD",
  (GRD_mec =="Rural periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RP - UDI",
  (GRD_mec =="Rural periurbain" & GRD_vict =="Rural periurbain"), "RP - RP",
  (GRD_mec =="Rural periurbain" & GRD_vict =="Rural non periurbain"), "RP - RNP",
  (GRD_mec =="Rural non periurbain" & GRD_vict =="Urbain dense"), "RNP - UD",
  (GRD_mec =="Rural non periurbain" & GRD_vict =="Urbain densité intermédiaire"), "RNP - UDI",
  (GRD_mec =="Rural non periurbain" & GRD_vict =="Rural periurbain"), "RNP - RP",
  (GRD_mec =="Rural non periurbain" & GRD_vict =="Rural non periurbain"), "RNP - RNP",
  default = "Autres")][, .(
    Nb_bless_famil = sum(compteur*(classe == "Coups et blessures volontaires dans la sphère familiale"),na.rm = TRUE),
    Nb_bless_horsfamil = sum(compteur*(classe == "Coups et blessures volontaires en dehors de la sphère familiale"),na.rm = TRUE),
    Nb_homicides = sum(compteur*(classe == "Homicides"),na.rm = TRUE),
    Nb_viol_sex = sum(compteur*(classe == "Violences sexuelles"),na.rm = TRUE)
  ),by = .(type_GD_MV) 
  ][order(type_GD_MV)][,`:=`(Total_bless_famil = sum(Nb_bless_famil),Total_bless_horsfamil = sum(Nb_bless_horsfamil),
                             Total_homicides = sum(Nb_homicides),Total_viol_sex = sum(Nb_viol_sex))
  ][, `:=`(Pct_bless_famil = round(Nb_bless_famil/Total_bless_famil*100,2),
           Pct_bless_horsfamil = round(Nb_bless_horsfamil/Total_bless_horsfamil*100,2),
           Pct_homicides = round(Nb_homicides/Total_homicides*100,2),
           Pct_viol_sex = round(Nb_viol_sex/Total_viol_sex*100,2)
  )
  ][,.(type_GD_MV,Pct_bless_famil,Pct_bless_horsfamil,Pct_homicides,Pct_viol_sex)]

# TODO: on peut regarder le même genre de chose avec le niveau de fragilité des centralités (étude Inrae...)
# à faire sur le champ des atteintes associées à un couple (I,V) présent dans une centralité (pas forcément la même...)

############################# Fin de la partie "Faits stylisés" sur l'organisation spatiale de la délinquance #############



