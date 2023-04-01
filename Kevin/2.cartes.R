source("M:/Commun/BESTRP/Territoires/Données communales/études et publications/interstatanalyse 2023/cartes/scripts/0.environnement.R",encoding = "UTF-8")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Partie 2 : Création des cartes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1. Données et fond de carte ----

  # Données avec la classification construite

load(file = "./études et publications/interstatanalyse 2023/cartes/donnees_cartes_20230120.RData")

ltabUuq
ltabAavq

names(ltabUuq) = paste0("UU - ",names(ltabUuq)) ; names(ltabAavq) = paste0("AAV - ",names(ltabAavq))

ltab = c(ltabUuq,ltabAavq)

  # fond de carte avec les unités urbaines et les aires d'attraction

suuaav = list(st_read("M:/Perso/KMilin/References/contours géographiques/UU_AAV/2022/uu2020-2022.shp"),
              st_read("M:/Perso/KMilin/References/contours géographiques/UU_AAV/2022/aav2020-2022.shp")
)
names(suuaav) = c("UU","AAV")
  
  # Terrtitoires articiciels

    # ajouts éventuels, pour gérer les Drom qui ne sont pas dans la classification de la métropole > permet de contourner le problème de la légende
    # > jusqu'à 5 départements articificiels dans le cas de classes différentes par Drom 

dep_artifice = tibble(dep=c("a","b","c","d","e"),
                      x=c(-0.690546,-0.689591,-0.688110,-0.686436,-0.684945),
                      y=c(45.124182,45.123796,45.122857,45.121706,45.120586)
) %>%
  st_as_sf(coords=c("x","y"),crs=4326) %>%
  st_transform(2154) %>% st_buffer(0.001) %>% st_transform(4326)

### 2. Cartes ----

for(i in 1:length(ltab)){
  
  nom.classe = names(ltab)[i]
  print(nom.classe)
  
  # Objet géographqiue
  
  if(substr(nom.classe,1,2) == "UU"){
    
    tab = suuaav[["UU"]] %>% left_join(ltab[[i]],by=c("uu2020"="UU2020")) %>% mutate(dep = ifelse(substr(uu2020,1,2) == "97",substr(uu2020,1,3), substr(uu2020,1,2)))
    
  }else if(substr(nom.classe,1,3) == "AAV"){
    
    tab = suuaav[["AAV"]] %>% left_join(ltab[[i]],by=c("aav2020"="AAV2020")) %>% mutate(dep = ifelse(substr(aav2020,1,2) == "97",substr(aav2020,1,3), substr(aav2020,1,2)))
    
  }
  
  # Palette de couleurs
  
  # Même couleur que le bilan + 2 couleurs supplémentaires pour la première et dernière classe
  palette.init = brewer.pal((tab %>% filter(ind.n1))$tx.q %>% unique() %>% length(),"Reds")

  if((tab %>% filter(!ind.n1))$min.max %>% unique() %>% length() == 2){
    palette = c(brewer.pal(9,"Reds")[1],palette.init,brewer.pal(9,"Reds")[9])
  }else if((tab %>% filter(!ind.n1))$min.max %>% unique() %>% length() == 1){
    if((tab %>% filter(!ind.n1))$min.max %>% unique() == "min"){
      palette = c(brewer.pal(9,"Reds")[1],palette.init)
    }else if((tab %>% filter(!ind.n1))$min.max %>% unique() == "max"){
      palette = c(palette.init,brewer.pal(9,"Reds")[9])
    }
  }else{
    palette = palette.init
  }

  # # Palette différente du bilan (à cause des des classes supplémentaires)
  # palette = brewer.pal(tab$tx.q %>% unique() %>% length(),"Reds")
  
  # Ordre

  ordre.legende = tab %>% select(tx.q,label.tx.q)
  st_geometry(ordre.legende) = NULL
  ordre.legende = ordre.legende %>% unique() %>% arrange(tx.q) %>% bind_cols("pal" = palette)

  mod.pal.drom = tab %>% filter(dep %in% c("9A","9B","9C","9D","9F","971","972","973","974","976")) %>% 
  left_join(ordre.legende %>% select(tx.q,pal),by = "tx.q") 
  
  st_geometry(mod.pal.drom) = NULL
    
  # Ajout des territoires artificiels si besoin
  
  classesupp = tab$label.tx.q[!tab$label.tx.q %in% (tab %>% filter(!dep %in% c("9A","9B","9C","9D","9F","971","972","973","974","976")))$label.tx.q] %>% unique()
  
  if(length(classesupp)>0){
    tab = bind_rows(
      st_bind_cols(label.tx.q = classesupp,dep_artifice %>% slice(1:length(classesupp))),
      tab
    )
  } 
  
  # Création des titres des légendes

  titre_cercle = case_when(
    nom.classe %in% c("UU - Cambriolages de logement","AAV - Cambriolages de logement") ~ "Nombre d'infractions\n",
    nom.classe %in% c("UU - Violences intrafamiliales","AAV - Violences intrafamiliales",
                      "UU - Autres coups et blessures volontaires","AAV - Autres coups et blessures volontaires",
                      "UU - Coups et blessures volontaires","AAV - Coups et blessures volontaires") ~ "Nombre de victimes\n",
    
    nom.classe %in% c("UU - Vols sans violence contre des personnes","AAV - Vols sans violence contre des personnes") ~ "Nombre de victimes entendues\n",
    nom.classe %in% c("UU - Vols violents sans arme","AAV - Vols violents sans arme",
                      "UU - Destructions et dégradations volontaires","AAV - Destructions et dégradations volontaires") ~ "Nombre d'infractions\n",
    
    nom.classe %in% c("UU - Vols d'accessoires sur véhicules","AAV - Vols d'accessoires sur véhicules",
                      "UU - Vols dans les véhicules","AAV - Vols dans les véhicules",
                      "UU - Vols de véhicules","AAV - Vols de véhicules") ~ "Nombre de véhicules\n",
  )

  titre_choro = case_when(
    nom.classe %in% c("UU - Cambriolages de logement","AAV - Cambriolages de logement") ~ "Nombre d'infractions\npour 1000 logements\n",
    nom.classe %in% c("UU - Violences intrafamiliales","AAV - Violences intrafamiliales",
                      "UU - Autres coups et blessures volontaires","AAV - Autres coups et blessures volontaires",
                      "UU - Coups et blessures volontaires","AAV - Coups et blessures volontaires") ~ "Nombre de victimes\npour 1000 habitants\n",
    
    nom.classe %in% c("UU - Vols sans violence contre des personnes","AAV - Vols sans violence contre des personnes") ~ "Nombre de victimes entendues\npour 1000 habitants\n",
    nom.classe %in% c("UU - Vols violents sans arme","AAV - Vols violents sans arme",
                      "UU - Destructions et dégradations volontaires","AAV - Destructions et dégradations volontaires") ~ "Nombre d'infractions\npour 1000 habitants\n",
    
    nom.classe %in% c("UU - Vols d'accessoires sur véhicules","AAV - Vols d'accessoires sur véhicules",
                      "UU - Vols dans les véhicules","AAV - Vols dans les véhicules",
                      "UU - Vols de véhicules","AAV - Vols de véhicules") ~ "Nombre de véhicules\npour 1000 habitants\n",
  )
  
    
  # Création du fichier d'enregistrement
  
  png(filename=paste("./études et publications/interstatanalyse 2023/cartes/",nom.classe,"_",Sys.Date(),".png",sep=""),width=5600,height=5000)
  
    # France métropole 
  
    opar=par(fig=c(0,1,0,1)) # Paramètres de disposition
    
    # Coloration des polygones
    
    typoLayer(x = tab %>% filter(!dep %in% c("9A","9B","9C","9D","9F","971","972","973","974","976")) %>% st_transform(2154), # Table de données
              var = "label.tx.q", # Variable à cartographier
              col = palette, # Pallette de couleur
              lwd = 2,# Border width
              legend.values.cex = 7,# size of the values in the legend
              legend.pos="right",
              legend.title.txt = titre_choro,
              legend.title.cex = 7,
              legend.values.order = ordre.legende$label.tx.q, # Ordre des modalités de la légende
    ) 
    
    # Ajout des cercles pour le volume
    
    propSymbolsLayer(x = tab %>% filter(!dep %in% c("9A","9B","9C","9D","9F","971","972","973","974","976")) %>% st_transform(2154),
                     var = var.fc,
                     inches=2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE),
                     symbols="circle",
                     col=NA,
                     border = "black",
                     lwd = 7,
                     legend.values.cex = 7,
                     legend.pos="topright" ,
                     legend.title.txt = titre_cercle,
                     legend.title.cex = 7,
                     add = TRUE
    )
  
    # Guadeloupe
    
    opar = par(fig = c(0,0.125,0.5,0.625),new = TRUE)
    
    typoLayer(x = tab %>% filter(dep %in% c("9A","971")) %>% st_transform(2989),
              var="label.tx.q", 
              lwd=2,
              col = (mod.pal.drom %>% filter(dep %in% c("9A","971")) %>% select(tx.q,pal) %>% unique())$pal, 
              legend.pos = "n"
    ) 
    
    propSymbolsLayer(x = tab %>% filter(dep %in% c("9A","971")) %>% st_transform(2989),
                     var = var.fc,
                     inches = 2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE) + 1,
                     symbols = "circle",
                     col = NA,
                     border = "black",
                     lwd = 7,
                     legend.pos = "n",
                     add = TRUE
    )
    
    # Martinique
    
    opar = par(fig = c(0,0.125,0.375,0.5),new = TRUE)
    
    typoLayer(x = tab %>% filter(dep %in% c("9B","972")) %>% st_transform(2989),
              var="label.tx.q", 
              lwd=2,
              col = (mod.pal.drom %>% filter(dep %in% c("9B","972")) %>% select(tx.q,pal) %>% unique())$pal, 
              legend.pos = "n"
    ) 
    
    propSymbolsLayer(x = tab %>% filter(dep %in% c("9B","972")) %>% st_transform(2989),
                     var = var.fc,
                     inches = 2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE) + 1,
                     symbols = "circle",
                     col = NA,
                     border = "black",
                     lwd = 7,
                     legend.pos = "n",
                     add = TRUE
    )
    
    # Guyane
    
    opar = par(fig = c(0,0.125,0.25,0.375),new = TRUE)
    
    typoLayer(x = tab %>% filter(dep %in% c("9C","973")) %>% st_transform(2972),
              var="label.tx.q", 
              lwd=2,
              col = (mod.pal.drom %>% filter(dep %in% c("9C","973")) %>% select(tx.q,pal) %>% unique())$pal, 
              legend.pos = "n"
    ) 
    
    propSymbolsLayer(x = tab %>% filter(dep %in% c("9C","973")) %>% st_transform(2972),
                     var = var.fc,
                     inches = 2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE) + 1,
                     symbols = "circle",
                     col = NA,
                     border = "black",
                     lwd = 7,
                     legend.pos = "n",
                     add = TRUE
    )
    
    # Réunion
    
    opar = par(fig = c(0,0.125,0.125,0.25),new = TRUE)
    
    typoLayer(x = tab %>% filter(dep %in% c("9D","974")) %>% st_transform(2975),
              var="label.tx.q", 
              lwd=2,
              col = (mod.pal.drom %>% filter(dep %in% c("9D","974")) %>% select(tx.q,pal) %>% unique())$pal, 
              legend.pos = "n"
    ) 
    
    propSymbolsLayer(x = tab %>% filter(dep %in% c("9D","974")) %>% st_transform(2975),
                     var = var.fc,
                     inches = 2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE) + 1,
                     symbols = "circle",
                     col = NA,
                     border = "black",
                     lwd = 7,
                     legend.pos = "n",
                     add = TRUE
    )
    
    # Mayotte
    
    opar = par(fig = c(0,0.125,0.0,0.125),new = TRUE)
    
    typoLayer(x = tab %>% filter(dep %in% c("9F","976")) %>% st_transform(2980),
              var="label.tx.q", 
              lwd=2,
              col = (mod.pal.drom %>% filter(dep %in% c("9F","976")) %>% select(tx.q,pal) %>% unique())$pal, 
              legend.pos = "n"
    ) 
    
    propSymbolsLayer(x = tab %>% filter(dep %in% c("9F","976")) %>% st_transform(2980),
                     var = var.fc,
                     inches = 2,
                     fixmax = max(tab[[var.fc]],na.rm = TRUE) + 1,
                     symbols = "circle",
                     col = NA,
                     border = "black",
                     lwd = 7,
                     legend.pos = "n",
                     add = TRUE
    )
    
  dev.off() 
  
}
