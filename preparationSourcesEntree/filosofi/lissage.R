setwd("U:/AR/ALICE/2_lissage/data")

library(btb)
library(foreign)
library(sp)
library(rgeos)
library(rgdal)
library(cartography)

#debut des parametres
#parametres
carr = 200
rayon = 400
epsg = "2154"

#definition des 5 profils pour la typologie
profil1 = c(50,20,15,10,5)
profil2 = c(30,25,20,15,10)
profil3 = c(20,20,20,20,20)
profil4 = c(10,15,20,25,30)
profil5 = c(5,10,15,20,50)
#fin des parametres

# load(file = "filo.Rdata")

#test sur le departement 31
base <- get(load(file = "dep31.Rdata"))

#####################################
#PRETRAITEMENTS : A AJOUTER EN AMONT#
#####################################

#calcul revenu * population
base$niv_x_pop = base$nivviem * base$pop
base$dis_x_pop = base$revdispm * base$pop
base$dec_x_pop = base$revdecm * base$pop

#exclusion des variables pas à lisser
pourliss <- base[,!names(base) %in% c("REG16","depcom","nivviem","revdispm","revdecm")]


#########
#LISSAGE#
#########

liss <- kernelSmoothing(pourliss, carr, rayon)

#on ne conserve que les carreaux de plus de 11 menages
liss=liss[liss$men>11,]

#creation du carroyage
grid <- smoothingToGrid(liss,epsg=epsg)

##################
#POST TRAITEMENTS#
##################

#calculs de taux
grid@data$tx_mono = round(100 * grid@data$m_mono / grid@data$men)
grid@data$tx_mon_pa = round(100 * grid@data$m_mono_pa / grid@data$men)
grid@data$tx_cosenf = round(100 * grid@data$m_cossenf / grid@data$men)
grid@data$tx_coenf = round(100 * grid@data$m_coenf / grid@data$men)
grid@data$tx_fseul = round(100 * grid@data$m_fseul / grid@data$men)
grid@data$tx_hseul = round(100 * grid@data$m_hseul / grid@data$men)
grid@data$tx_5p = round(100 * grid@data$m_5p / grid@data$men)
grid@data$tx_5p_pa = round(100 * grid@data$m_5p_pa / grid@data$men)
grid@data$tx_rev_sa = round(100 * grid@data$m_rev_sal / grid@data$men)
grid@data$tx_rev_ch = round(100 * grid@data$m_rev_cho / grid@data$men)
grid@data$tx_rev_ns = round(100 * grid@data$m_rev_nsa / grid@data$men)
grid@data$tx_rev_re = round(100 * grid@data$m_rev_ret / grid@data$men)
grid@data$tx_pop_pa = round(100 * grid@data$pop_pa / grid@data$pop)
grid@data$tx_popd1 = round(100 * grid@data$pop_d1 / grid@data$pop)
grid@data$tx_popd10 = round(100 * grid@data$pop_d10 / grid@data$pop)
grid@data$tx_pop_q1 = round(100 * grid@data$pop_q1 / grid@data$pop)
grid@data$tx_pop_q2 = round(100 * grid@data$pop_q2 / grid@data$pop)
grid@data$tx_pop_q3 = round(100 * grid@data$pop_q3 / grid@data$pop)
grid@data$tx_pop_q4 = round(100 * grid@data$pop_q4 / grid@data$pop)
grid@data$tx_pop_q5 = round(100 * grid@data$pop_q5 / grid@data$pop)


#calcul des revenus moyens
grid@data$niv_moy = grid@data$niv_x_pop / grid@data$pop 
grid@data$dis_moy = grid@data$dis_x_pop / grid@data$pop 
grid@data$dec_moy = grid@data$dec_x_pop / grid@data$pop 


#calculs des classes pour la typologie sur la mixite

#calcul de la distance a chacun des profils : distance issue de khi2
grid@data$dist_pro1 = (grid@data$tx_pop_q1 - profil1[1])^2 /  (grid@data$tx_pop_q1 +  profil1[1]) + 
                      (grid@data$tx_pop_q2 - profil1[2])^2 /  (grid@data$tx_pop_q2 +  profil1[2]) + 
                      (grid@data$tx_pop_q3 - profil1[3])^2 /  (grid@data$tx_pop_q3 +  profil1[3]) + 
                      (grid@data$tx_pop_q4 - profil1[4])^2 /  (grid@data$tx_pop_q4 +  profil1[4]) + 
                      (grid@data$tx_pop_q5 - profil1[5])^2 /  (grid@data$tx_pop_q5 +  profil1[5]) 

grid@data$dist_pro2 = (grid@data$tx_pop_q1 - profil2[1])^2 /  (grid@data$tx_pop_q1 +  profil2[1]) + 
                      (grid@data$tx_pop_q2 - profil2[2])^2 /  (grid@data$tx_pop_q2 +  profil2[2]) + 
                      (grid@data$tx_pop_q3 - profil2[3])^2 /  (grid@data$tx_pop_q3 +  profil2[3]) + 
                      (grid@data$tx_pop_q4 - profil2[4])^2 /  (grid@data$tx_pop_q4 +  profil2[4]) + 
                      (grid@data$tx_pop_q5 - profil2[5])^2 /  (grid@data$tx_pop_q5 +  profil2[5]) 

grid@data$dist_pro3 = (grid@data$tx_pop_q1 - profil3[1])^2 /  (grid@data$tx_pop_q1 +  profil3[1]) + 
                      (grid@data$tx_pop_q2 - profil3[2])^2 /  (grid@data$tx_pop_q2 +  profil3[2]) + 
                      (grid@data$tx_pop_q3 - profil3[3])^2 /  (grid@data$tx_pop_q3 +  profil3[3]) + 
                      (grid@data$tx_pop_q4 - profil3[4])^2 /  (grid@data$tx_pop_q4 +  profil3[4]) + 
                      (grid@data$tx_pop_q5 - profil3[5])^2 /  (grid@data$tx_pop_q5 +  profil3[5])  

grid@data$dist_pro4 = (grid@data$tx_pop_q1 - profil3[1])^2 /  (grid@data$tx_pop_q1 +  profil3[1]) + 
                      (grid@data$tx_pop_q2 - profil4[2])^2 /  (grid@data$tx_pop_q2 +  profil4[2]) + 
                      (grid@data$tx_pop_q3 - profil4[3])^2 /  (grid@data$tx_pop_q3 +  profil4[3]) + 
                      (grid@data$tx_pop_q4 - profil4[4])^2 /  (grid@data$tx_pop_q4 +  profil4[4]) + 
                      (grid@data$tx_pop_q5 - profil4[5])^2 /  (grid@data$tx_pop_q5 +  profil4[5]) 

grid@data$dist_pro5 = (grid@data$tx_pop_q1 - profil5[1])^2 /  (grid@data$tx_pop_q1 +  profil5[1]) + 
                      (grid@data$tx_pop_q2 - profil5[2])^2 /  (grid@data$tx_pop_q2 +  profil5[2]) + 
                      (grid@data$tx_pop_q3 - profil5[3])^2 /  (grid@data$tx_pop_q3 +  profil5[3]) + 
                      (grid@data$tx_pop_q4 - profil5[4])^2 /  (grid@data$tx_pop_q4 +  profil5[4]) + 
                      (grid@data$tx_pop_q5 - profil5[5])^2 /  (grid@data$tx_pop_q5 +  profil5[5]) 

#la dist minimale
grid@data<- transform(grid@data, distmin = pmin(grid@data$dist_pro1, grid@data$dist_pro2,
                                                grid@data$dist_pro3, grid@data$dist_pro4, 
                                                grid@data$dist_pro5))

#affectation au profil auquel la dist est minimale
#voir les cas eventuels où on n'affecte aucun profil : nbObs trop faible ...
for (i in 1:nrow(grid@data)) {
  if (grid@data$distmin[i]==grid@data$dist_pro1[i]) 
  {grid@data$classe[i]<- 1
  }else if (grid@data$distmin[i]==grid@data$dist_pro2[i]) {grid@data$classe[i]<- 2
  }else if (grid@data$distmin[i]==grid@data$dist_pro3[i]) {grid@data$classe[i]<- 3
  }else if (grid@data$distmin[i]==grid@data$dist_pro4[i]) {grid@data$classe[i]<- 4
  }else {grid@data$classe[i]<- 5}
}

##############################
#preparation export et export#
##############################

#suppression des variables inutiles
grid@data <- grid@data[, ! names(grid@data) %in% c("niv_x_pop","dec_x_pop","dis_x_pop",
                                                   "dist_pro1","dist_pro2","dist_pro3","dist_pro4",
                                                   "dist_pro5")]

#export
rgdal::writeOGR(grid, paste0("liss_",carr,"_",rayon,".shp"),paste0("liss_",carr,"_",rayon) ,
                driver = "ESRI Shapefile", overwrite_layer = TRUE)


#rappel des variables (pas plus de 9 caracteres pour QGIS)
noms = as.data.frame(sort(names(grid@data)))

