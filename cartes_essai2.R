install.packages('sf')
install.packages('ggplot2')
install.packages('ggplot')
library(sf)
library(ggplot2)
library(ggplot)
sf_use_s2(FALSE) #turns off spherical geometry

fr_comm <- st_read("C:\\Users\\marie\\OneDrive\\Documents\\cours\\ensae\\stat app\\contoursGeographiques")
#plot(fr_comm)
head(fr_comm)
#plot(fr_comm['wikipedia'])
ggplot() + geom_sf(data = contours)

## on veut juste la france continentale pour y voir plus clair

# en sélectionnant les données : impossible ?
# on a pas d'info qui permette de séparer continental vs outre mer ?
#fr_continent <- depf[depf$name == 'France',]

#en "coupant" la carte
contours_cropped <- st_crop(contours, xmin = -20, xmax = 40, ymin = 30, ymax = 55)
ggplot() + geom_sf(data = contours_cropped) + theme_bw() + coord_sf(expand = FALSE)

# en coupant le fond de carte mais avec toutes les données (mieux pour garder la projection?, idem pour nous je pense car slmt france)
ggplot() + geom_sf(data = fr_comm) +
  coord_sf(xlim = c(-7, 10), ylim = c(40, 52), expand = FALSE) +
  theme_bw()

# on essaie de joindre le fond de carte et les données
# selon la variable 'insee' (code communes) et 'cog_comm_22_x' selon victime mec ou inf

#on commence par les inf
joint_inf <- merge(x=fr_comm,y=donnees_del,by.x='insee',by.y='cog_com_22_inf', all.x=TRUE)
head(joint_inf)
joint_inf_A <- subset(joint_inf, classe =="A")
head(joint_inf_A)
ggplot() + geom_sf(data = joint_inf_A) +
  coord_sf(xlim = c(-7, 10), ylim = c(40, 52), expand = FALSE) +
  theme_bw()

ggplot() + geom_sf(data = joint_inf_A)

#plot(joint_inf)
