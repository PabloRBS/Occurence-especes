
# Packages 

library(data.table)
library(sf)
library(readxl)
library(dplyr)
library(rgeos)
library(tidyr)
library(plyr)
library(ggplot2)
library(spatstat) # to count fragments
library(vegan) # diversity
library(raster)
library(rgdal)
library(terra) # focal function
library(gridExtra)  # put graphics in a grid
library("grid")
library("ggplotify") # convert plots to ggplot objects

#*******************************************************************************
#*******************************************************************************
################################################################################
# DATA EDITION FROM DATABASE EXPORT
################################################################################

username  <- rstudioapi::askForPassword(prompt = "username")  #ums_p..s (change with your credentials)
password  <- rstudioapi::askForPassword(prompt = "password*") #S...1*   (change with your credentials)
conex <- dbConnect(dbDriver("Postgres"), dbname = "db_NAME", host = "data-gest-prod.patnat.mnhn.fr", port = XXX, user = username, password = password)

options(scipen = 50)

# import of the list from database

tpb10km.v4 <-as.data.table(dbGetQuery(conex, "select * FROM temp_doe.tpb_10km_v3")) # this takes some minutes
tpb5km.v4 <-as.data.table(dbGetQuery(conex, "select * FROM temp_doe.tpb_5km_v3")) 

# if you already have the data located in your computer
# change to your current folder
#setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Data mailles")
# this is the original data from the database
# [temp_doe].[tpb_5km_v3]
# [temp_doe].[tpb_10km_v3]
#tpb10km.v4 <- read.csv("./Original data/tpb_10km_v3_202211301400.csv") 
#tpb5km.v4  <- read.csv("./Original data/tpb_5km_v3_202211301427.csv")

# select useful columns

tp10km.v4  <- subset(tpb10km.v4, select =c ("id_observation","id_evenement","id_maille_10k","supericie",              
                                 "supericie_x","superficie_recouvrement","classe_recouvrement","id_type_donnees",        
                                 "cd_nom","cd_ref","cd_sig","date_debut",             
                                 "regne","phylum","classe","ordre",                  
                                "famille","group2_inpn","iscontinental","ismarine"))

tpb5km.v4  <- subset(tpb5km.v4, select =c ("id_observation","id_evenement","id_maille_5k","supericie",              
                                 "supericie_x","superficie_recouvrement","classe_recouvrement","id_type_donnees",        
                                 "cd_nom","cd_ref","cd_sig","date_debut",             
                                 "regne","phylum","classe","ordre",                  
                                 "famille","group2_inpn","iscontinental","ismarine"))

#### CHANGE NAMES 

colnames(tpb10km.v4) <- c("id_observation","id_evenement","id_maille","supericie","supericie_x",            
                       "superficie_recouvrement","classe_recouvrement","id_type_donnees","cd_nom","cd_ref",                 
                       "cd_sig","date_debut","regne","phylum","classe",                 
                       "ordre","famille","group2_inpn","iscontinental","ismarine")

colnames(tpb5km.v4)  <- c("id_observation","id_evenement","id_maille","supericie","supericie_x",            
                       "superficie_recouvrement","classe_recouvrement","id_type_donnees","cd_nom","cd_ref",                 
                       "cd_sig","date_debut","regne","phylum","classe",                 
                       "ordre","famille","group2_inpn","iscontinental","ismarine")

##############################################################################
# list of species
##############################################################################

### import species reference list
ListRef  <- read.csv("./Original data/ListRef_CDUE_2019.csv", sep = ";")
### filter ListRef EVAL = Present
listref.p <- filter (ListRef, EVAL == "Present")
#### filter only the species of reference list
# changing names to match the database
colnames(listref.p) <- c("Code_N2000","Prio","Nom.cité.dans.la.Directive","cd_nom","Nom_valide","Nom_vern",
                         "ALP","ATL","CONT","MED","Nom_cite","A1","CDUE_2019","cd_ref","LB_NOM_VALIDE",
                         "GROUP2_INPN","EVAL")
# save list
#save(listref.p, file = "listref.p.RData" )

################################################################################
# list of species with knowledge indicator
################################################################################

listconn  <- read.csv(".Original data/Liste_Esp_Grp_Test.csv", sep = ";")
colnames(listconn) <- c("cd_ref","cd_nom","Code_N2000","CDUE_2019"             
                       ,"Nom_cite_Directive","Nom_vern","ALP","ATL"                        
                       ,"CONT","MED","GROUP2_INPN","Regroupement"               
                       ,"Indicateurs","Expert","Commentaire")

#save(listconn, file = "listconn.RData")

## join data
tpb10km.sp.c <- left_join (tpb10km, listconn, by = "cd_ref")  
tpb5km.sp.c <- left_join (tpb5km, listconn, by = "cd_ref")   
# filter NA values
tpb10km.sp.cf <-  filter(tpb10km.sp.c, !is.na(id_observation))
tpb5km.sp.cf <-  filter(tpb5km.sp.c, !is.na(id_observation))

## save prepared data

#save(tpb5km.sp.cf, file = "tpb5km.sp.cf2.RData")
#save(tpb10km.sp.cf, file = "tpb10km.sp.cf2.RData")



#**********************************************************************************************
#
#                    START FROM HERE, LOADING THE DATA ALREADY PREPARED
#
# Note: it is recomended to prepare the data before, and then upload it in a new R sesion.
#       Preparing the data requires a high amount of RAM, then the second part of the script
#       requires as well a heavy load of memory).
#
#***********************************************************************************************

# Loading data already prepared with the last script

# change with your folder location
# setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Data mailles")

load("tpb5km.sp.cf2.RData")
load("tpb10km.sp.cf2.RData")

# list of species
load("listref.p.RData")
# list of connaisance
load("listconn.RData")

#*******************************************************
# Shape files of grids and departments of France
#*******************************************************

# Import France/LAEA grids 10km and 5km 
sh10 <- st_read("Grid_ETRS89_LAEA_10K_FR.shp")
# Couche France/LAEA 5km
sh5 <- st_read("la5x5fr.shp")
# couche France departements
#fr <- st_read("copie_loc_depts.shp")
#fr_simpl <- st_simplify(fr, preserveTopology = FALSE, dTolerance = 1000) # to make graph faster
fr_simpl <- st_read("fr_simpl.shp")

#******************************************************
#* Rasters maille France sans aire marine
#******************************************************

raster10_tot.1 <- readGDAL("salida10tif.tif")
raster5_tot.1 <- readGDAL("salida5tif.tif")
raster10_fr <- raster(raster10_tot.1)
raster5_fr <- raster(raster5_tot.1)

#******************************************************
#* raster maille France AVEC aire marine
#******************************************************

raster10_frmar_1 <- readGDAL("raster10_tot.tif")  # continental et marine
raster5_frmar_1 <- readGDAL("raster5_tot.tif")
raster10_frmar <- raster(raster10_frmar_1)
raster5_frmar <- raster(raster5_frmar_1)

#******************************************************
#* raster maille France JUSTE marine
#******************************************************

raster10_tot.cr <- crop(raster10_fr, extent(raster10_frmar))
raster5_tot.cr <- crop(raster5_fr, extent(raster5_frmar)) 

  # reproject to reference data crs
raster10_fr.pj <- projectRaster(raster10_tot.cr,  crs = crs(sh10))
raster5_fr.pj <- projectRaster(raster5_tot.cr, crs = crs(sh5))
  
  # resample to reference data extent
raster10_fr.ext <- resample (raster10_fr.pj, raster10_frmar, method = "bilinear") ## 
raster5_fr.ext <- resample (raster5_fr.pj, raster5_frmar, method = "bilinear")

raster10_mar <- mask(raster10_frmar, raster10_fr.ext, inverse = TRUE)  #  OK
raster5_mar <- mask(raster5_frmar, raster5_fr.ext, inverse = TRUE)  # OK

#*******************************************************
#* preparation des donnees pour la question mailles 10x10 vides
#*******************************************************  
t10.sb   <- subset(tpb10km.sp.cf, select = c("id_observation","id_evenement","id_maille","cd_ref","Regroupement"))
t5.sb   <- subset(tpb5km.sp.cf, select = c("id_observation","id_evenement","id_maille","cd_ref","Regroupement"))
colnames(t10.sb) <- c("id_observation","id_evenement","id_maille_10","cd_ref","Regroupement")
colnames(t5.sb) <- c("id_observation","id_evenement","id_maille_5","cd_ref","Regroupement")
t10.sb$occurrence_10 <- "True"
t5.sb$occurrence_5 <- "True"
t.sb <- full_join(t10.sb, t5.sb) # , by = "id_evenement")
# 1. table with all the combinations of 5 and 10 km
t.sb <- subset(t.sb, select = c("id_maille_10","id_maille_5","occurrence_10","occurrence_5")) #before t.sb.1
t.sb <- unique(t.sb)                                                                        #before t.sb.2
comb_maille_10_5 <- arrange(t.sb, id_maille_10, id_maille_5)
# 1.2 prepare species tables in separated lists
t105.sb.1 <- left_join(t10.sb, t5.sb,  by = c("id_evenement","cd_ref"))
t105.sb.1 <- subset(t105.sb.1, select = c("id_maille_10","id_maille_5","cd_ref","Regroupement.x","occurrence_10","occurrence_5"))

# lists
esp_maille <- split(t105.sb.1, f = t105.sb.1$cd_ref)   
#list.esp <- as.character(names(esp_maille))  # check if the lists are the same

# objects to store data

comb_esp <- list()
maille_10_vides <- list()
esp_maille_ref <- list()

# list of species for the loop

## complete list  
  list.esp.1 <- unique(as.integer(tpb5km.sp.cf$cd_ref))           
  list.esp.2 <- sort.int(list.esp.1)
  list.esp <- as.character(list.esp.2) 

# if you only needs to test with some species
#list.esp <- c("2836","2840")
#list.esp  <- "608254"
  
# OBJECTS TO STORE DATA

# list of tables by species
tpb10km.spl <- split(tpb10km.sp.cf, f = tpb10km.sp.cf$cd_ref)   
tpb5km.spl <- split(tpb5km.sp.cf, f = tpb5km.sp.cf$cd_ref)      

# lists to store raw maps and K index
#map_raw <- list()

# list of dataframes to store calculations

values   <- data.frame(cd_ref = list.esp)
values.spl5 <- split(values, f = values$cd_ref)       
values.spl10 <- split(values, f = values$cd_ref) 
values.spl <- list()

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

exttot <- extent(shpEsp10)

rawmap1  <-
 ggplot() +
  geom_sf(data = fr_simpl) +   
  geom_sf(data = shpEsp10_g, color= alpha("#1D928F", 0.5), fill = alpha("#3A9E98", 0.8), size = 0.05) +
     geom_sf(data = shpEsp5_g, color= alpha("#b5745e", 0.5), fill = alpha("darksalmon",0.6), size = 0.05) +
     geom_rect(aes(xmin = exttot[1], xmax = exttot[2], ymin = exttot[3], ymax = exttot[4]),
               color = "red", fill = NA) +
     xlab("Longitude") + ylab ("Latitude") +
     ggtitle(paste0("Occurrence")) +
               theme(plot.title = element_text(hjust = 0.5)) +
     scale_color_manual(values = colors) 

rawmap  <- 
     ggplot() +
     geom_sf(data = fr_simpl) +  
     geom_sf(data = shpEsp10_g, color= alpha("#1D928F",0.6), fill = alpha("#3A9E98", 0.7), size = 0.05) +
     geom_sf(data = shpEsp5_g, color=alpha("#b5745e", 0.6), fill = alpha("darksalmon",0.7), size = 0.05) +  ##5D0C0D" = border, #E9967A = darksalmon
     geom_rect(aes(xmin = exttot[1], xmax = exttot[2], ymin = exttot[3], ymax = exttot[4]),
               color = "red", fill = NA) +   
     xlab("Longitude") + ylab ("Latitude") +
     #ggtitle(paste0("Occurrence de l'espece", " ", esp)) +
        scale_x_continuous(limits = c(exttot[1], exttot[2]))+
        scale_y_continuous(limits = c(exttot[3], exttot[4]))+
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
                         labels = c("K théorique", "K espèce"),
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
                         labels = c("K théorique", "K espèce"),
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

  #raster10_tot.3 <- crop(raster10_fr, extent(raster10))
  #raster5_tot.3 <- crop(raster5_fr, extent(raster5)) 

  if (tpb10km.sp2$ismarine[1] == 'false' & tpb10km.sp2$iscontinental[1] == 'true'){
  raster10_tot.3 <- crop(raster10_fr, extent(raster10))
  raster5_tot.3 <- crop(raster5_fr, extent(raster5)) 
 } else if (tpb10km.sp2$ismarine[1] == 'true' & tpb10km.sp2$iscontinental[1] == 'false'){
  raster10_tot.3 <- crop(raster10_mar, extent(raster10))
  raster5_tot.3 <- crop(raster5_mar, extent(raster5))
 } else if (tpb10km.sp2$ismarine[1] == 'true' & tpb10km.sp2$iscontinental[1] == 'true'){
  raster10_tot.3 <- crop(raster10_frmar, extent(raster10))
  raster5_tot.3 <- crop(raster5_frmar, extent(raster5))
 }
  
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
  
  NVois_10 <- (r_10_focal_tot_m_esp@nrows)*(r_10_focal_tot_m_esp@ncols) # quantité de mailles 10x10
  NVois_5 <- (r_5_focal_tot_m_esp@nrows)*(r_5_focal_tot_m_esp@ncols) # quantité de mailles 5x5 
  
  # calcul de nombre de voisines NVois
  
  NVois_10.a <- count(values(r_10_focal_tot_m_esp >=1))
  NVois_10.b <- NVois_10.a[NVois_10.a$x=="TRUE",] 
  NVois_10  <- NVois_10.b[1,2]
  
  NVois_5.a <- count(values(r_5_focal_tot_m_esp >=1))
  NVois_5.b <- NVois_5.a[NVois_5.a$x=="TRUE",] 
  NVois_5  <- NVois_5.b[1,2]
  
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
  
  TVois_5[!is.finite(TVois_5)] <- NA    # to avoid errors due to INF values
  TVois_10[!is.finite(TVois_10)] <- NA 

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
  
  #############################################
  # nombre de mailles 10x10 vides par espece 
  #############################################
  
  esp_maille_ref[[esp]] <- subset(esp_maille[[esp]], select = c("id_maille_10","id_maille_5","occurrence_10","occurrence_5","Regroupement.x"))
  colnames(esp_maille_ref[[esp]]) <- c("id_maille_10","id_maille_5", "occurrence_10", "occurrence_5","group")
  esp_maille_ref[[esp]]$id_maille_10_sp <- esp_maille_ref[[esp]]$id_maille_10
  esp_maille_ref[[esp]]$id_maille_5_sp <- esp_maille_ref[[esp]]$id_maille_5
  esp_maille_ref[[esp]] <- unique(esp_maille_ref[[esp]])
  # join the full list of combinations with all the species
  comb_esp[[esp]] <- left_join(comb_maille_10_5, esp_maille_ref[[esp]], by = c("id_maille_10","id_maille_5"))
  comb_esp[[esp]] <- subset(comb_esp[[esp]], select = c("id_maille_10","id_maille_5","occurrence_10.y","occurrence_5.y", "group"))
  colnames(comb_esp[[esp]]) <- c("id_maille_10","id_maille_5","occurrence_10.y","occurrence_5.y", paste(esp))
  # count cells
  tmp  <- filter(comb_esp[[esp]], occurrence_10.y == "True" & is.na(occurrence_5.y))
  #tmp2 <- left_join(tmp, comb_esp[[esp]], by = "id_maille_10")
  tmp_true  <- filter(comb_esp[[esp]], occurrence_10.y == "True" & occurrence_5.y == "True")
  list10 <- unique(tmp_true$id_maille_10)
  vides <- nrow(tmp)
  remplies <- sum(tmp$id_maille_10 %in% list10)
  maille_10_vides[[esp]]  <- vides - remplies
  
  cel_esp_tot1  <- filter(comb_esp[[esp]], occurrence_10.y == "True" | occurrence_5.y == "True")
  cel_esp_tot <- n_distinct(cel_esp_tot1$id_maille_10)
  
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
  values.spl10[[esp]]$'vid/remp' <- round((maille_10_vides[[esp]]/cel_esp_tot),2) 
  values.spl10[[esp]]$frag_div_10 <- round(div10,2)  
  values.spl10[[esp]]$dist_frag10_moy <- round(dist_frag10_moy,2)   
  values.spl10[[esp]]$surf_frag10_moy <- round(surf_frag10_moy,2)   
  values.spl10[[esp]]$dens_rempl <- round(dens_rempl,2)   
  values.spl10[[esp]]$TVoisEsp_10 <- round(TVoisEsp_10,2)
  values.spl10[[esp]] <- left_join(values.spl10[[esp]], listref.p, by = "cd_ref")  
  values.spl10[[esp]] <- left_join(values.spl10[[esp]], listconn, by = "cd_ref")
    
  values.spl10[[esp]] <- subset(values.spl10[[esp]], select =  c("LB_NOM_VALIDE","GROUP2_INPN.x","Regroupement","cd_ref","Indicateurs","dens_rempl","area_int","frag_div_int",
                                                                 #"mailles_10_tot",
                                                                 #"mailles_10_vid",
                                                                 "vid/remp",
                                                                 "maille","area_10",
                                                                 "vois_10km","TVoisEsp_10","frag_div_10","surf_frag10_moy",
                                                                 "dist_frag10_moy"))
                              
  colnames(values.spl10[[esp]]) <- c("nom_valide","group_2_INPN","regroupement","cd_ref","connaissance","dens_rempl","area_int","frag_div_int",
                                                                 #"mailles_10_tot",
                                                                 #"mailles_10_vid",
                                                                 "vid/remp",
                                                                 "maille","area",
                                                                 "vois","TVoisEsp","frag_div","surf_frag_moy",
                                                                 "dist_frag_moy")
  
  # 5km
  
  values.spl5[[esp]]$maille <- "5km"
  values.spl5[[esp]]$cd_ref <- as.numeric(esp)
  values.spl5[[esp]]$area_5 <- area_5
  values.spl5[[esp]]$area_int <- area_int.g10_c5
  values.spl5[[esp]]$vois_5km <- round(vois_5km,2)
  values.spl5[[esp]]$frag_div_int <- round(divInt,2)
  values.spl5[[esp]]$'vid/remp' <- round((maille_10_vides[[esp]]/cel_esp_tot),2) 
  values.spl5[[esp]]$frag_div_5 <- round(div5,2)
  values.spl5[[esp]]$dist_frag5_moy <- round(dist_frag5_moy,2) 
  values.spl5[[esp]]$surf_frag5_moy <- round(surf_frag5_moy,2)     
  values.spl5[[esp]]$dens_rempl <- round(dens_rempl,2) 
  values.spl5[[esp]]$TVoisEsp_5 <- round(TVoisEsp_5,2)
  values.spl5[[esp]] <- left_join(values.spl5[[esp]], listref.p, by = "cd_ref")  
  values.spl5[[esp]] <- left_join(values.spl5[[esp]], listconn, by = "cd_ref")
    
  values.spl5[[esp]] <- subset(values.spl5[[esp]], select =  c("LB_NOM_VALIDE","GROUP2_INPN.x","Regroupement","cd_ref","Indicateurs","dens_rempl","area_int","frag_div_int",
                                                                 #"mailles_10_tot",
                                                                 #"mailles_10_vid",
                                                                 "vid/remp",
                                                                 "maille","area_5",
                                                                 "vois_5km","TVoisEsp_5","frag_div_5","surf_frag5_moy",
                                                                 "dist_frag5_moy"))
  colnames(values.spl5[[esp]]) <- c("nom_valide","group_2_INPN","regroupement","cd_ref","connaissance","dens_rempl","area_int","frag_div_int",
                                                                 #"mailles_10_tot",
                                                                 #"mailles_10_vid",
                                                                 "vid/remp",
                                                                 "maille","area",
                                                                 "vois","TVoisEsp","frag_div","surf_frag_moy",
                                                                 "dist_frag_moy")
  
  # bind two tables 10km and 5km
  
  values.spl[[esp]] <- rbind(values.spl5[[esp]], values.spl10[[esp]])
  
  #***********************************************
  # GRAPHIC LAYOUT
  #***********************************************
     
  # setwd("C:/Users/pbolanos01/Dropbox/ETC-BD MNHN/2022/Patrinat/Graphics from R/test2")
  
gl4<-list(rawmap1, rawmap, EspK5_gg, EspK10_gg)
gl3<-list(rawmap1, EspK5_gg, EspK10_gg)

  # this condition put just one map if the species is spread accross FR and two graphics if their distribution is small  

if (((exttot[2]-exttot[1]) > 400000) & ((exttot[4] - exttot[3]) > 400000)) {
  graph[[esp]]<-grid.arrange(grobs = gl3, widths = c(1,1),
                             heights = c(1,1),
                             layout_matrix = rbind(c(1, 2),c(1, 3))) 
    } else {
  graph[[esp]]<-grid.arrange(grobs = gl4, widths = c(1,1),
                               heights = c(1,1),
                               layout_matrix = rbind(c(1, 3),c(2, 4)))
    }

  }
 
## If you want to export a table with all the data 

values.total <- ldply(values.spl)
values.total <- arrange(values.total, .id)
# save results
#save(graph, file = "graph8.RData" )
#save(values.spl, file = "values.spl8.RData")

#write.csv(values.total, file = "values.total14.csv", row.names = FALSE)

#*******************************************************************************
#* Objects graph and values.spl will be used in next script to export the graphics
#* and results to a Word document
#*******************************************************************************



