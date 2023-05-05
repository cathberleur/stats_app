
# Partie 2 du mémoire Stat'app: 

# Création du tableau 2: Distribution du nombre d'infraction au niveau communal (pour 1000 habitants), 
# selon le type d'atteinte

names(delinquance_com)

classe <- c("Ensemble","Vols sans violence contre des personnes","Destructions et dégradations","Vols dans les véhicules",
            "Cambriolages de logement","Vols de véhicules","Coups et blessures volontaires en dehors de la sphère familiale",
            "Coups et blessures volontaires dans la sphère familiale","Vols d'accessoires sur véhicules",
            "Vols violents sans arme","Violences sexuelles","Vols avec armes","Homicides")

type_atteinte <- data.frame(classe)
type_atteinte <- as_tibble(type_atteinte)

#define quantiles of interest
q = c(.1, .25, .5, .75, .90, .95, .99)

tableau2 <- delinquance_com %>% 
        select(cog_com_22_inf,I,I_cambr,I_bless_famil,I_bless_horsfamil,
               I_destr_degrad,I_homic,I_viol_sex,I_vols_armes,I_vols_acces_vehic,
               I_vols_ds_vehic,I_vols_de_vehic,I_vols_sansviol,I_vols_viol_sansarme) %>%
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
      type_atteinte == "I_vols_sansviol" ~ "01-Vols sans violence contre des personnes",
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

# Création de la graphique 2a: calcul des coefficients de corrélation de Pearson entre le nombre d'infractions (I) et le nombre
# de victimes (pour 1000 habitants et selon le type d'atteinte):

data.graphique2a <-  delinquance_com %>%
                        select(I,I_cambr,I_bless_famil,I_bless_horsfamil,
                        I_destr_degrad,I_homic,I_viol_sex,I_vols_armes,I_vols_acces_vehic,
                        I_vols_ds_vehic,I_vols_de_vehic,I_vols_sansviol,I_vols_viol_sansarme,
                        V,V_cambr,V_bless_famil,V_bless_horsfamil,
                        V_destr_degrad,V_homic,V_viol_sex,V_vols_armes,V_vols_acces_vehic,
                        V_vols_ds_vehic,V_vols_de_vehic,V_vols_sansviol,V_vols_viol_sansarme) %>%
                 filter(I>0 & I_cambr>0 & I_bless_famil>0 & I_bless_horsfamil>0 & I_destr_degrad>0 &
                           I_homic>0 & I_viol_sex>0 & I_vols_armes>0 & I_vols_acces_vehic &
                           I_vols_ds_vehic>0 & I_vols_de_vehic>0 & I_vols_sansviol>0 &
                           I_vols_viol_sansarme>0 & 
                           V>0 & V_cambr>0 & V_bless_famil>0 & V_bless_horsfamil>0 & V_destr_degrad>0 &
                           V_homic>0 & V_viol_sex>0 & V_vols_armes>0 & V_vols_acces_vehic &
                           V_vols_ds_vehic>0 & V_vols_de_vehic>0 & V_vols_sansviol>0 &
                           V_vols_viol_sansarme>0) 

# Matrice de corrélation:
data.graphique2a.cor = cor(data.graphique2a)

corrplot(data.graphique2a.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Graphique 2b: calcul des coefficients de corrélation de Pearson entre le nombre d'infractions (I),le nombre
# de victimes (pour 1000 habitants et selon le type d'atteinte corporelle):

data.graphique2b <-  delinquance_com %>%
  select(I_bless_famil,I_bless_horsfamil,I_homic,I_viol_sex,
         V_bless_famil,V_bless_horsfamil,V_homic,V_viol_sex) %>%
  filter(I_bless_famil>0 & I_bless_horsfamil>0 &
           I_homic>0 & I_viol_sex>0 &
           V_bless_famil>0 & V_bless_horsfamil>0 &
           V_homic>0 & V_viol_sex>0) 

# Matrice de corrélation:
data.graphique2b.cor = cor(data.graphique2b)

corrplot(data.graphique2b.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Tableau 3a: Proportion d'infractions (I) associées à un couple de communes (I,V) présentes
# dans un même zonage d'étude - selon le type d'atteinte

tableau3a <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  Nb_I_IV = sum(compteur, na.rm = TRUE),
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV*100,1),
                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV*100,1),
                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV*100,1))
                  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
                    ][order(classe)]

tableau3a

# Tableau 3b: Proportion d'infractions corporelles (I) associées à un triplet de communes (I,V,M) présentes
# dans un même zonage d'étude - selon le type d'atteinte

tableau3b <- atteintes[(is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                       & classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"), .(
  Nb_I_IVM = sum(compteur, na.rm = TRUE),
  Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM*100,1),
                         Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                         Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                         Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                         Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM*100,1),
                         Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM*100,1))
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
nb_zonages <-communes_zonages_dt[ , .(nb_ZE2020 = uniqueN(ZE2020),
                                      nb_UU2020 = uniqueN(UU2020),
                                      nb_AAV2020 = uniqueN(AAV2020),
                                      nb_BV2022 = uniqueN(BV2022)
                                      )]

# 2 problèmes apparaissent:
# problème 1: contrairement aux autre zonages, UU et AAV ne forment pas une partition du territoire français...
# -> il semble donc difficile de les mettre sur les mêmes plans que les autres en calculant une proba, non?
# problème 2: le zonage "grille de densité" (GD) apparaît atypique, car il ne correspond pas à une zone géographique précise 
# du territoire mais plutôt à une catégorisation d'une commune dans un des 4 types de GD (urbain dense/urbain intermédiaire/
# rural périurbain/ rural non périurbain.). Du coup, difficile de le comparer aux autres zonages!
# le zonage "centralité (CENTR) de l'Inrae ressemble à ce titre à la GD: c'est une catégorisation des communes (en 5 classes)
# mais ne correspond pas à un découpage géographique du territoire au même titre que les ZE, les BV...etc

# A discuter avec Aurélien et Kevin du coup!

# Zoom sur la crise de la Covid-19: voit-on un effet sur le degré de concentration spatiale des atteintes?
# idée: on s'attendrait à ce que la crise sanitaire ait augmenter la proportion des atteintes associées à 
# un couple (I,V) ou un triplet (I,V,M) de communes présentes dans un même zonage...

# On refait les tableaux 3a et 3b sur les période 2016-2019 et 2020-2021:

# A mettre en Annexe?

# Tableau 3a: 2016-2019 (période pré-Covid)
tableau3a_2016_2019 <- atteintes[ annee %between% c(2016,2019) &
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
  Nb_I_IV = sum(compteur, na.rm = TRUE),
  Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
  Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
  Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
  Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
  Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
  Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV*100,1),
                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV*100,1),
                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV*100,1))
  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
  ][order(classe)]

tableau3a_2016_2019

# Tableau 3a: 2020-2021 (période Covid)
tableau3a_2020_2021 <- atteintes[ annee %between% c(2020,2021) &
                                    (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE), .(
                                      Nb_I_IV = sum(compteur, na.rm = TRUE),
                                      Nb_I_IV_1ZE = sum(compteur*(IV_ds_meme_ZE == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1BV = sum(compteur*(IV_ds_meme_BV == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1GD = sum(compteur*(IV_ds_meme_GD == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1UU = sum(compteur*(IV_ds_meme_UU == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1AAV = sum(compteur*(IV_ds_meme_AAV == "oui"), na.rm = TRUE),
                                      Nb_I_IV_1CENTR = sum(compteur*(IV_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                                  by= .(classe)][ , `:=`(Prop_I_IV_1UU = round(Nb_I_IV_1UU/Nb_I_IV*100,1),
                                                         Prop_I_IV_1GD = round(Nb_I_IV_1GD/Nb_I_IV*100,1),
                                                         Prop_I_IV_1ZE = round(Nb_I_IV_1ZE/Nb_I_IV*100,1),
                                                         Prop_I_IV_1BV = round(Nb_I_IV_1BV/Nb_I_IV*100,1),
                                                         Prop_I_IV_1AAV = round(Nb_I_IV_1AAV/Nb_I_IV*100,1),
                                                         Prop_I_IV_1CENTR = round(Nb_I_IV_1CENTR/Nb_I_IV*100,1))
                                  ][,.(classe,Prop_I_IV_1UU,Prop_I_IV_1GD,Prop_I_IV_1ZE,Prop_I_IV_1BV,Prop_I_IV_1AAV,Prop_I_IV_1CENTR)
                                  ][order(classe)]

tableau3a_2020_2021


# Tableau 3b: 2016-2019 (période pré-Covid)
tableau3b_2016_2019 <- atteintes[annee %between% c(2016,2019) &
  (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                       & classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                         "Coups et blessures volontaires en dehors de la sphère familiale",
                                         "Homicides",
                                         "Violences sexuelles"), .(
                                           Nb_I_IVM = sum(compteur, na.rm = TRUE),
                                           Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                           Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                       by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM*100,1),
                                              Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM*100,1))
                       ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
                       ][order(classe)]

tableau3b_2016_2019

# Tableau 3b: 2020_2021 (période Covid)
tableau3b_2020_2021 <- atteintes[annee %between% c(2020,2021) &
                                   (is.na(cog_com_22_inf)==FALSE) & (is.na(cog_com_22_vict)==FALSE) & (is.na(cog_com_22_mec)==FALSE)
                                 & classe %chin% c("Coups et blessures volontaires dans la sphère familiale",
                                                   "Coups et blessures volontaires en dehors de la sphère familiale",
                                                   "Homicides",
                                                   "Violences sexuelles"), .(
                                                     Nb_I_IVM = sum(compteur, na.rm = TRUE),
                                                     Nb_I_IVM_1ZE = sum(compteur*(IVM_ds_meme_ZE == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1BV = sum(compteur*(IVM_ds_meme_BV == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1GD = sum(compteur*(IVM_ds_meme_GD == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1UU = sum(compteur*(IVM_ds_meme_UU == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1AAV = sum(compteur*(IVM_ds_meme_AAV == "oui"), na.rm = TRUE),
                                                     Nb_I_IVM_1CENTR = sum(compteur*(IVM_ds_meme_CENTR == "oui"), na.rm = TRUE)),
                                 by= .(classe)][ , `:=`(Prop_I_IVM_1UU = round(Nb_I_IVM_1UU/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1GD = round(Nb_I_IVM_1GD/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1ZE = round(Nb_I_IVM_1ZE/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1BV = round(Nb_I_IVM_1BV/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1AAV = round(Nb_I_IVM_1AAV/Nb_I_IVM*100,1),
                                                        Prop_I_IVM_1CENTR = round(Nb_I_IVM_1CENTR/Nb_I_IVM*100,1))
                                 ][,.(classe,Prop_I_IVM_1UU,Prop_I_IVM_1GD,Prop_I_IVM_1ZE,Prop_I_IVM_1BV,Prop_I_IVM_1AAV,Prop_I_IVM_1CENTR)
                                 ][order(classe)]

tableau3b_2020_2021







