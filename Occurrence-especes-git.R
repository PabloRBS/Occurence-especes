
##Lancer un package: 
library(data.table)
library(sf)
library(readxl)
#library(WriteXLS)
#library(RPostgres)
#library(ETLUtils)

# extra packages 

#library(tidyverse)

library(dplyr)
library(rgeos)
library(tidyr)
library(plyr)
library(ggplot2)
library(spatstat) # to count fragments
library(vegan) # diversity
library(raster)
library(rgdal)
#library(viridis) # color palette
#library(RColorBrewer)
library(terra) # focal function
#library(gridGraphics) # store graphics in objects

library(gridExtra)  # put graphics in a grid

library("grid")
library("ggplotify") # convert plots to ggplot objects
 

#*******************************************************************************
#*******************************************************************************
################################################################################
#
# COMPLETE DATA
#
################################################################################

# IMPORT SPECIES LIST FOR TEST V3

#from laptop
setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Data exports")

tpb10km.v4 <- read.csv("tpb_10km_v3_202209021111.csv") 
tpb5km.v4  <- read.csv("tpb_5km_v3_202209021126.csv")

# select useful columns

tp10km.v4  <- subset(tpb10km.v4, select =c ("id_observation","id_evenement","id_maille_10k","supericie",              
                                 "supericie_x","superficie_recouvrement","classe_recouvrement","id_type_donnees",        
                                 "cd_nom","cd_ref","cd_sig","date_debut",             
                                 "regne","phylum","classe","ordre",                  
                                "famille","group2_inpn"))

 tpb5km.v4  <- subset(tpb5km.v4, select =c ("id_observation","id_evenement","id_maille_5k","supericie",              
                                 "supericie_x","superficie_recouvrement","classe_recouvrement","id_type_donnees",        
                                 "cd_nom","cd_ref","cd_sig","date_debut",             
                                 "regne","phylum","classe","ordre",                  
                                 "famille","group2_inpn"))

#### CHANGE NAMES 

colnames(tpb10km.v4) <- c("id_observation","id_evenement","id_maille","supericie","supericie_x",            
                       "superficie_recouvrement","classe_recouvrement","id_type_donnees","cd_nom","cd_ref",                 
                       "cd_sig","date_debut","regne","phylum","classe",                 
                       "ordre","famille","group2_inpn")

colnames(tpb5km.v4)  <- c("id_observation","id_evenement","id_maille","supericie","supericie_x",            
                       "superficie_recouvrement","classe_recouvrement","id_type_donnees","cd_nom","cd_ref",                 
                       "cd_sig","date_debut","regne","phylum","classe",                 
                       "ordre","famille","group2_inpn") 


### Filtre classe recouvrement = 1 

tpb10km <- filter(tpb10km.v4, classe_recouvrement == 1 | classe_recouvrement == 2)  ### before it was 1 (100 %), 2 is 80 - 99.99 %
 
tpb5km <- filter(tpb5km.v4, classe_recouvrement == 1 | classe_recouvrement == 2)  ### before it was 1 (100 %), 2 is 80 - 99.99 %


##############################################################################
# Import list of species
##############################################################################

setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/data")

### import species reference list
ListRef  <- read.csv("ListRef_CDUE_2019.csv", sep = ";")

### filter ListRef EVAL = Present
listref.p <- filter (ListRef, EVAL == "Present")

#### filter only the species of reference list

# changing names to match the database

colnames(listref.p) <- c("Code_N2000","Prio","Nom.cit?.dans.la.Directive","cd_nom","Nom_valide","Nom_vern",
                         "ALP","ATL","CONT","MED","Nom_cite","A1","CDUE_2019","cd_ref","LB_NOM_VALIDE",
                         "GROUP2_INPN","EVAL")

# save list

#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")
#save(listref.p, file = "listref.p.RData" )

tpb10km.sp <- right_join (tpb10km, listref.p, by = "cd_ref")  # right_join in order to keep just the species present

# make the same for 5km

tpb5km.sp <- right_join (tpb5km, listref.p, by = "cd_ref")  # right_join in order to keep just the species present

################################################################################
# IMPORT list of species with knowledge indicator
################################################################################

setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/data")

listconn  <- read.csv("Liste_Esp_Grp_Test.csv", sep = ";")

colnames(listconn) <- c("cd_ref","cd_nom","Code_N2000","CDUE_2019"             
                       ,"Nom_cite_Directive","Nom_vern","ALP","ATL"                        
                       ,"CONT","MED","GROUP2_INPN","Regroupement"               
                       ,"Indicateurs","Expert","Commentaire")

# save list connues

#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")
#save(listconn, file = "listconn.RData")


## join data

tpb10km.sp.c <- right_join (tpb10km.sp, listconn, by = "cd_ref")  # right_join in order to keep just the species present

# make the same for 5km

tpb5km.sp.c <- right_join (tpb5km.sp, listconn, by = "cd_ref")  # right_join in order to keep just the species present


# FILTER NA VALUES

tpb10km.sp.cf <-  filter(tpb10km.sp.c, !is.na(id_observation))

tpb5km.sp.cf <-  filter(tpb5km.sp.c, !is.na(id_observation))

# filter blank values (only for 10km)

tpb10km.sp.cf <-  filter(tpb10km.sp.cf, cd_sig != "")

#tpb5km.sp.cf2 <-  filter(tpb5km.sp.cf1, cd_sig != "")

## save data objects
#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")
#save(tpb5km.sp.cf, file = "tpb5km.sp.cf.RData")
#save(tpb10km.sp.cf, file = "tpb10km.sp.cf.RData")

#******************************************************
#************************START*************************
#******************************************************

setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")
load("tpb5km.sp.cf.RData")
load("tpb10km.sp.cf.RData")

# list of species
load("listref.p.RData")

# list of connaisance

load("listconn.RData")

## MAILLES

# Import France/LAEA grids 10km and 5km 

setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/shape files/ETRS89_LAEA_10K_FR")
sh10 <- st_read("Grid_ETRS89_LAEA_10K_FR.shp")

# Couche France/LAEA 5km
setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/shape files/mailles")
sh5 <- st_read("la5x5fr.shp")

 #******************************************************
#* raster maille France sans aire marine
#******************************************************

   #import rasters mv
  
  setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/rasters from R/tiff")
   
  raster10_tot.1 <- readGDAL("salida10tif.tif")
  raster5_tot.1 <- readGDAL("salida5tif.tif")

  raster10_tot.2 <- raster(raster10_tot.1)
  raster5_tot.2 <- raster(raster5_tot.1)
   
#______________________________________________________________________________
#*******************************************************************************
################################################################################
# list of species
################################################################################
#_______________________________________________________________________________

## complete list  
  list.esp <- unique(as.integer(tpb5km.sp.cf$cd_ref))           
  list.esp <- as.character(list.esp)    # complete list (307 species)   

## test with some species
  
#  list.esp <- c("89567","84699","717782","82376","86380","132159","717179","103008","3885","123672")

#  list.esp <- "89567"
  
################################################################################
################################################################################
# objects to store data
#_______________________________________________________________________________

# list of tables by species
tpb10km.spl <- split(tpb10km.sp.cf, f = tpb10km.sp.cf$cd_ref)   ### 
tpb5km.spl <- split(tpb5km.sp.cf, f = tpb5km.sp.cf$cd_ref)      ### 

# lists to store raw maps and K index
map_raw <- list()

EspK10_plot <- list()
EspK5_plot <- list()

# list of dataframes to store calculations

values   <- data.frame(cd_ref = list.esp)

values.spl5 <- split(values, f = values$cd_ref)       
  
values.spl10 <- split(values, f = values$cd_ref) 

values.spl <- list()


# lists to store maps

map10 <- list()
map5 <- list()

# list to store graphs

graph <- list()

################################################################################

for(esp in list.esp){               
 
  ### ************************************************************************************
  ### choosing the shape for each species
  ### ************************************************************************************
    shpEsp10 <-sh10[sh10$CD_SIG %in% tpb10km.spl[[esp]]$id_maille,] 
    shpEsp5 <-sh5[sh5$CD_SIG %in% tpb5km.spl[[esp]]$id_maille,] 
  ### ******************************************************************
  ### intersection area
  ### ******************************************************************
  ### centroids 10km in grid 5km
  centroid_10  <- st_centroid(shpEsp10, of_largest_polygon = TRUE) 
  #int.g5_c10 <- st_intersects((shpEsp5),(centroid_10), sparse = FALSE) 
 
  centroid_5  <- st_centroid(shpEsp5, of_largest_polygon = TRUE) 
  int.g10_c5 <- st_intersects((shpEsp10),(centroid_5), sparse = FALSE)
  
  nb_mai_int <- table(int.g10_c5)["TRUE"]  # COUNTS THE NUMBER OF GRIDS OF INTERSECTION
  area_int.g10_c5 <- nb_mai_int*25 # multiply by the area of grids of 5km
  
  #***************************************************************************************
  #### Convertion into a SpatialPolygonsDataFrame in order to calculate the area and perimeter
  #***************************************************************************************
  
     sp10 <- as(shpEsp10, "Spatial")  
     sp5 <- as(shpEsp5, "Spatial")                               
     
    #*********************************************************
    # calculates area and perimeter for the species
    #*********************************************************
    # result in km
     
     # surface and perimeter for 10km
     
     sp10@data$Surface<-round(gArea(sp10, byid=T)/1e6)   
     sp10@data$Perimetre<-round(gLength(sp10, byid=T),0)/1e3           

     # surface and perimeter for 10km
     
     sp5@data$Surface<-round(gArea(sp5, byid=T)/1e6)   
     sp5@data$Perimetre<-round(gLength(sp5, byid=T),0)/1e3      
     
    ### areas
     
     area_10  <- sum(sp10@data$Surface)  
     area_5   <- sum(sp5@data$Surface)  

    # densite de remplisage de 10x10 par 5x5
     
     dens_rempl <-  (sum(area_int.g10_c5)/area_10) * 100
     
  #************************************************************************
  #     Raw map
  #************************************************************************
        
  shpEsp10_g <- st_geometry(shpEsp10)
  shpEsp5_g <- st_geometry(shpEsp5)


rawmap  <- ggplot() +
     geom_sf(data = shpEsp10_g, color="#1D928F", fill = "#3A9E98", size = 0.05) +
     geom_sf(data = shpEsp5_g, color="#5D0C0D", fill = alpha("darksalmon",0.8), size = 0.05) +
     xlab("Longitude") + ylab ("Latitude") +
     ggtitle(paste0("Occurence de l'esp?ce", " ", esp)) +
               theme(plot.title = element_text(hjust = 0.5)) +
     scale_color_manual(values = colors) 

#####################################################################################
# neighboring
#######################################################################################

  # Preparing rasters 
  # Standarizing the extensions and crs (coordinate reference system) 

    # calculating centroid_5 (centroid_10 was done)
    centroid_5  <- st_centroid(shpEsp5, of_largest_polygon = TRUE) # temp centroid_5

    # extensions of the maps
    ext10 <- extent(sh10)
    ext5 <- extent(sh5)
    
    # the resolution due to the sides of 10,000 m 
    raster_templ_10 = raster(shpEsp10, ext = ext10,
                             resolution = 10000,  
                             crs = crs(sh10))                   

    # sides of 5,000 m
    raster_templ_5 = raster(shpEsp5, ext = ext5, resolution = 5000, 
                          crs = crs(sh5)) 
    
    raster10 <- rasterize(centroid_10, raster_templ_10, field = 1)  # no 
    raster5 <- rasterize(centroid_5, raster_templ_5, field = 1)
  
  
    #************************************************
    # neighboring calculation (voisinage)
    #************************************************
  
  r_10_focal = focal(raster10, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)#, NAonly = FALSE) #, NAonly = TRUE) #na.rm = TRUE,   # pad = FALSE) 

  r_5_focal = focal(raster5, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)#, NAonly = FALSE) #, NAonly = TRUE) #na.rm = TRUE,   # pad = FALSE) 

  ## masking with NA values
  
  r_10_focal_m <- mask(r_10_focal, raster10, inverse = FALSE)
  r_5_focal_m <- mask(r_5_focal, raster5, inverse = FALSE)

  ## remooving the value of the focal cell
  
  r_10_focal_m_1 <- (r_10_focal_m)-1
  r_5_focal_m_1 <- (r_5_focal_m)-1
  
  
    #************************************************
    # Ripley K index
    #************************************************
  
  ## buffer
  ## try changing the buffer dist
       
   buff_10 <- st_buffer(shpEsp10, dist = 0) 
   buff_5 <- st_buffer(shpEsp5, dist = 0)   
  
  ## 5km   
  ## dataframe of centroids
   
  XY<-data.frame(st_coordinates(st_centroid(buff_5, byid=T)), row.names=buff_5$CD_SIG)
  owinesp<-convexhull.xy(XY)
  
  # join centroid coordinates to list 
  
  XY$id_maille <- row.names(XY)  # put row names as another column
  tpb5km.sp2  <-   left_join(tpb5km.spl[[esp]], XY, by = "id_maille")  
  
  # creating a ppp object
  # the condition avoids creating a window when there is less than four cells with occurrence
  
  if ((n_distinct(tpb5km.sp2$id_maille )) > 4) {
     Esppp5<-ppp(x=tpb5km.sp2$X, 
     y=tpb5km.sp2$Y,
     marks=as.character(tpb5km.sp2$id_evenement),
     window=as.owin(owinesp))
     } else {
    Esppp5<-ppp(x=tpb5km.sp2$X, 
    y=tpb5km.sp2$Y,
    marks=as.character(tpb5km.sp2$id_evenement))           
     }

  EspK5<-Kest((Esppp5), correction = "border")
  
  #### Plot Ripley clustering index (K) 5km

EspK5_gg <- ggplot(EspK5, aes(x = r)) + 
         geom_line (aes(y= theo, col = "theo"), size = 1.1) +
         geom_line (aes(y= border, col = "border"), size = 1.1) +            
                         scale_color_manual(name = "",
                         labels = c("K th?orique", "K esp?ce"),
                         values = c("theo" = "darkgray",
                         "border" = "darksalmon")) +
         labs(title = "Indice de Ripley (K)", 
         subtitle = "Maille 5x5 km",
         y = "K(r)")
    
  
   ## 10km   
  ## dataframe of centroids
   
  XY<-data.frame(st_coordinates(st_centroid(buff_10, byid=T)), row.names=buff_10$CD_SIG)
  owinesp<-convexhull.xy(XY)
  
  # join centroid coordinates to list 
  
  XY$id_maille <- row.names(XY)  # put row names as another column
  tpb10km.sp2  <-   left_join(tpb10km.spl[[esp]], XY, by = "id_maille")
  
  # creating a ppp object
  # the condition avoid creating a window when there is just one square with occurrence

  if ((n_distinct(tpb10km.sp2$id_maille )) > 4) {
     Esppp10<-ppp(x=tpb10km.sp2$X, 
     y=tpb10km.sp2$Y,
     marks=as.character(tpb10km.sp2$id_evenement),
     window=as.owin(owinesp))
     } else {
    Esppp10<-ppp(x=tpb10km.sp2$X, 
    y=tpb10km.sp2$Y,
    marks=as.character(tpb10km.sp2$id_evenement))           
     }
  

  EspK10 <-Kest((Esppp10), correction = "border")                                            ### temp K 10km
  
  #### Plot Ripley clustering index (K) 10km
  
EspK10_gg    <- ggplot(EspK10, aes(x = r)) + 
         geom_line (aes(y= theo, col = "theo"), size = 1.1) +
         geom_line (aes(y= border, col = "border"), size = 1.1) +
         scale_color_manual(name = "",
                         labels = c("K th?orique", "K esp?ce"),
                         values = c("theo" = "darkgray",
                         "border" = "#3A9E98")) +
         labs(title = "", 
         subtitle = "Maille 10x10 km",
         y = "K(r)")
  
  
  # ____________________________________________________________________
  #  voisinage especes
  # ____________________________________________________________________
  
  #class(r_10_focal@data)
    
  vois_10km <- cellStats(r_10_focal_m_1, mean) # 3 "130784"  moyenne voisinage espece
  vois_5km  <- cellStats(r_5_focal_m_1, mean) # 12 "130784"  moyenne voisinage espece
  
  # ____________________________________________________________________
  #  voisinage mailles completes
  # ____________________________________________________________________

  #***********************************************************************************
  #***********************************************************************************
  #***********************************************************************************

  raster10_tot.3 <- crop(raster10_tot.2, extent(raster10))
  raster5_tot.3 <- crop(raster5_tot.2, extent(raster5)) 

  # reproject to reference data crs
  raster10_pj <- projectRaster(raster10_tot.3, crs = crs(sh10))
  raster5_pj <- projectRaster(raster5_tot.3, crs = crs(sh5))
  
  # resample to reference data extent
  raster10_tot.3 <- resample (raster10_pj, raster10, method = "bilinear") ## 
  raster5_tot.3 <- resample (raster5_pj, raster5, method = "bilinear")
  
  # Focal voisinage total FR 
  
  r_10_focal_tot = focal(raster10_tot.3, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE) 
  r_5_focal_tot = focal(raster5_tot.3, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)
 
  # Focal voisinage chaque espece
  
  r_10_focal_tot_m <- mask(r_10_focal_tot, raster10_tot.3, inverse = FALSE)
  r_5_focal_tot_m <- mask(r_5_focal_tot, raster5_tot.3, inverse = FALSE)

  r_10_focal_tot_m_1 <- (r_10_focal_tot_m)-1 # this subtracts the value of each cell itself
  r_5_focal_tot_m_1 <- (r_5_focal_tot_m)-1 # this subtracts the value of each cell itself
  
  r_10_focal_tot_m_esp <- mask(r_10_focal_tot_m_1, raster10, inverse = FALSE) # mask voisinage total with shape of species
  r_5_focal_tot_m_esp <- mask(r_5_focal_tot_m_1, raster5, inverse = FALSE) # mask voisinage total with shape of species
  
  # calcul de voisinage FR total
  
  nvois10_tot.2 <- cellStats(r_10_focal_tot_m_esp, sum) # quantite de voicines total
  nvois5_tot.2  <- cellStats(r_5_focal_tot_m_esp, sum) # quantite de voicines
  
  # calcul de voisinage especes
  
  NVois_10 <- (r_10_focal_tot_m_esp@nrows)*(r_10_focal_tot_m_esp@ncols) # quantit? de mailles 10x10
  NVois_5 <- (r_5_focal_tot_m_esp@nrows)*(r_5_focal_tot_m_esp@ncols) # quantit? de mailles 5x5 
  
  # calcul de nombre de voisines NVois
  
  NVois_10.a <- count(values(r_10_focal_tot_m_esp >=1))
  NVois_10  <- NVois_10.a[1,2]
  
  NVois_5.a <- count(values(r_5_focal_tot_m_esp >=1))
  NVois_5  <- NVois_5.a[1,2]
  
  #********
  # 2] taux moyen de voisinage de l'espece 
  # NVoisEsp = r_10_focal_m_1
  # NVois = r_10_focal_tot_m_esp
  # n = NVois_10
  #********
 
  
  #************************************************
  # Average neighboring rate 
  #************************************************
  
  TVois_10 <- r_10_focal_m_1/r_10_focal_tot_m_esp
  TVois_5 <- r_5_focal_m_1/r_5_focal_tot_m_esp
  
  TVoisEsp_10 <- (cellStats(TVois_10, sum))/NVois_10
  TVoisEsp_5 <- (cellStats(TVois_5, sum))/NVois_5
  
  # ____________________________________________________________________
  # fragment diversity
  # ____________________________________________________________________
  
  InterF<-gIntersection(gBuffer(sp10,width=.1),gBuffer(sp5,width=.1))
  InterF10<-gIntersection(gBuffer(sp10,width=.1),gBuffer(sp10,width=.1))
  InterF5<-gIntersection(gBuffer(sp5,width=.1),gBuffer(sp5,width=.1))
 
  Frag<-disaggregate(InterF)  
  Frag10<-disaggregate(InterF10)    
  Frag5<-disaggregate(InterF5)

  ### variability of fragments
  #** 1 large fragment = 0
  #** several small fragments = 1

  ### for the intersection

  Frag<-SpatialPolygonsDataFrame(Frag, data.frame(ID=1:length(Frag)))
  Frag@data$Surface<-round(gArea(Frag, byid=T)/1e6)
  Frag@data$Perimetre<-round(gLength(Frag, byid=T),0)/1e3
  divInt <- diversity(Frag@data$Surface)/log(nrow(Frag@data))

  ### for 10x10

  Frag10<-SpatialPolygonsDataFrame(Frag10, data.frame(ID=1:length(Frag10)))
  Frag10@data$Surface<-round(gArea(Frag10, byid=T)/1e6)
  Frag10@data$Perimetre<-round(gLength(Frag10, byid=T),0)/1e3
  div10 <- diversity(Frag10@data$Surface)/log(nrow(Frag10@data))

  ### for 5x5

  Frag5<-SpatialPolygonsDataFrame(Frag5, data.frame(ID=1:length(Frag5)))
  Frag5@data$Surface<-round(gArea(Frag5, byid=T)/1e6)
  Frag5@data$Perimetre<-round(gLength(Frag5, byid=T),0)/1e3
  div5 <- diversity(Frag5@data$Surface)/log(nrow(Frag5@data))

  ### distance entre fragments

  dist_frag10<-round(gDistance(Frag10, Frag10, byid=T)/1e3); diag(dist_frag10)<-NA 
  dist_frag10<-apply(dist_frag10, 1, min, na.rm=T)
  dist_frag10_moy <- mean(dist_frag10)

  dist_frag5<-round(gDistance(Frag5, Frag5, byid=T)/1e3); diag(dist_frag5)<-NA 
  dist_frag5<-apply(dist_frag5, 1, min, na.rm=T)
  dist_frag5_moy <- mean(dist_frag5)
  #dist_frag5_et <- sd(dist_frag5)
  
  ### moyenne surface de fragments
  
  surf_frag5_moy <- mean(Frag5@data$Surface) 
  surf_frag10_moy <- mean(Frag10@data$Surface) 
  
  ### **********
  ### Adding results
  ### **********
     
 # 10km
  
  values.spl10[[esp]]$maille <- "10km"
  values.spl10[[esp]]$cd_ref <- as.numeric(esp)
  values.spl10[[esp]]$area_10 <- area_10
  values.spl10[[esp]]$area_int <- area_int.g10_c5
  values.spl10[[esp]]$vois_10km <- round(vois_10km,2)   
  values.spl10[[esp]]$frag_div_int <- round(divInt,2) 
  values.spl10[[esp]]$frag_div_10 <- round(div10,2)  
  values.spl10[[esp]]$dist_frag10_moy <- round(dist_frag10_moy,2)   
  values.spl10[[esp]]$surf_frag10_moy <- round(surf_frag10_moy,2)   
  values.spl10[[esp]]$dens_rempl <- round(dens_rempl,2)   
  values.spl10[[esp]]$TVoisEsp_10 <- round(TVoisEsp_10,2)
  values.spl10[[esp]] <- left_join(values.spl10[[esp]], listref.p, by = "cd_ref")  
  values.spl10[[esp]] <- left_join(values.spl10[[esp]], listconn, by = "cd_ref")
    
  values.spl10[[esp]] <- subset(values.spl10[[esp]], select =  c("LB_NOM_VALIDE","GROUP2_INPN.x","Regroupement","cd_ref","Indicateurs","dens_rempl","area_int","frag_div_int","maille","area_10",
                                                                 "vois_10km","TVoisEsp_10","frag_div_10","surf_frag10_moy",
                                                                 "dist_frag10_moy"))
                              
  colnames(values.spl10[[esp]]) <- c("nom_valide","group_2_INPN","regroupement","cd_ref","connaissance","dens_rempl","area_int","frag_div_int","maille","area",
                                                                 "vois","TVoisEsp","frag_div","surf_frag_moy",
                                                                 "dist_frag_moy")
  
  # 5km
  
  values.spl5[[esp]]$maille <- "5km"
  values.spl5[[esp]]$cd_ref <- as.numeric(esp)
  values.spl5[[esp]]$area_5 <- area_5
  values.spl5[[esp]]$area_int <- area_int.g10_c5
  values.spl5[[esp]]$vois_5km <- round(vois_5km,2)
  values.spl5[[esp]]$frag_div_int <- round(divInt,2)
  values.spl5[[esp]]$frag_div_5 <- round(div5,2)
  values.spl5[[esp]]$dist_frag5_moy <- round(dist_frag5_moy,2) 
  values.spl5[[esp]]$surf_frag5_moy <- round(surf_frag5_moy,2)     
  values.spl5[[esp]]$dens_rempl <- round(dens_rempl,2) 
  values.spl5[[esp]]$TVoisEsp_5 <- round(TVoisEsp_5,2)
  values.spl5[[esp]] <- left_join(values.spl5[[esp]], listref.p, by = "cd_ref")  
  values.spl5[[esp]] <- left_join(values.spl5[[esp]], listconn, by = "cd_ref")
    
  values.spl5[[esp]] <- subset(values.spl5[[esp]], select =  c("LB_NOM_VALIDE","GROUP2_INPN.x","Regroupement","cd_ref","Indicateurs","dens_rempl","area_int","frag_div_int","maille","area_5",
                                                                 "vois_5km","TVoisEsp_5","frag_div_5","surf_frag5_moy",
                                                                 "dist_frag5_moy"))
  colnames(values.spl5[[esp]]) <- c("nom_valide","group_2_INPN","regroupement","cd_ref","connaissance","dens_rempl","area_int","frag_div_int","maille","area",
                                                                 "vois","TVoisEsp","frag_div","surf_frag_moy",
                                                                 "dist_frag_moy")
  
  # bind two tables 10km and 5km
  
  values.spl[[esp]] <- rbind(values.spl5[[esp]], values.spl10[[esp]])
  
  #***********************************************
  # GRAPHIC LAYOUT
  #***********************************************
     
  # setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R/test2")
  
    gl <- list(rawmap, EspK5_gg, EspK10_gg)
    
  graph[[esp]] <-  grid.arrange(
    grobs = gl, 
    widths = c(1,1),
    heights = c(1,1),
    layout_matrix = rbind(c(1, 2),c(1, 3))
     )

  }
 
## Put all values in one table  

#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/species_summary_exports")

values.total <- ldply(values.spl)
values.total <- arrange(values.total, .id)

# save results

setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")

save(graph, file = "graph.RData" )
#save(values.spl, file = "values.spl.RData")

#write.csv(values.total, file = "values.total.csv", row.names = FALSE)

# load results
#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R")
#load("values.spl.RData")








