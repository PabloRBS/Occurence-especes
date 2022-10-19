# Échelle de rapportage, données d'observation d'especes sur les Directives Nature

---
Changement des cartes d'occurrence d''espèces de mailles de 10x10 km à 5x5 km : étude de cas en France.
<br />
Author: "Collaboration CTE / PatriNat, Muséum National d'Histoire Naturelle"
<br />
date: '2022-10-13'
---

## Travail en colaboration CTE / BD - Patrinat : 

Analyste ETC / BD : Pablo Bolaños
<br />

## Contexte 

Dans la perspective de la nécessité de fournir des cartes de distribution en mailles 5x5 km (au lieu de 10x10 actuellement) pour les prochains rapportages communautaires au titre des directives Habitats-Faune-Flore et Oiseaux. 

Ce travail est une collaboration de deux mois durant l'année 2022, entre le "Centre Thématique Européen sur la Diversité Biologique (CTE/BD) et le Patrinat, MNHN.

Les résultats ci-dessous est un exemple pour l’espèce 67179 "Barbus meridionalis". Le script complet effectue l’analyse pour toutes les espèces (158), selon les critères de sélection. 


**Questions**

Quel serait l’impact en termes de complétude ?

Sur quels taxons ?

Ou sur quels territoires plus d'informations seront perdues ?

**Métriques de comparaison entre les mailles**

1. Densité de remplissage des mailles 10x10 par les mailles 5x5.

2. Taux de voisinage des mailles.

3. Clusters : nombre de clusters, distance moyenne entre clusters, surface moyenne des clusters.

4. Nombre d'observations des espèces du même groupe taxonomique dans les mailles 5x5 "vides" qui sont dans des mailles 10x10 de présence.

## Méthodologie

**Données et informations techniques** 

Sources des données à tester : INPN, données rapportages. 
Concernant : espèces DHFF, oiseaux nicheurs DO.
Liste des taxons concernés, avec réconciliation Taxref

**Critères de sélection des données**

Données d’observation depuis 2012
Classe recouvrement 1 et 2 conservées (80 – 100%)
Données de synthèse retirées


**DATA** 


```{r include = FALSE}

# données de rapportage pour mailles de 5 et 10 km

tpb5km.sp.cf
tpb10km.sp.cf

# liste des especes avec l'information pour les groups

listref.p

# liste avec l'information du connaisance

listconn

# mailles de France de 10 et 5 km

sh10 <- sf::st_read("Grid_ETRS89_LAEA_10K_FR.shp")

sh5 <- sf::st_read("la5x5fr.shp")

# mailles de France de superficie terrestre, en raster pour le calcul de voisinage complet
  
  raster10_tot.1 <- readGDAL("salida10tif.tif")
  raster5_tot.1 <- readGDAL("salida5tif.tif")

  raster10_tot.2 <- raster(raster10_tot.1)
  raster5_tot.2 <- raster(raster5_tot.1)
    
```

```{r include = FALSE}
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
```


**objets pour enregistrer les résultats du script et listes pour le boucle**


```{r include = FALSE}

# liste complete d'especes

  list.esp.1 <- unique(as.integer(tpb5km.sp.cf$cd_ref))           
  list.esp.2 <- sort.int(list.esp.1)
  list.esp <- as.character(list.esp.2) 

# liste juste pour une espece (pour tester)

# list.esp <- "67179"
# esp <- "67179"

# liste de tables de chaque espece

tpb10km.spl <- split(tpb10km.sp.cf, f = tpb10km.sp.cf$cd_ref)   ### 
tpb5km.spl <- split(tpb5km.sp.cf, f = tpb5km.sp.cf$cd_ref)      ### 

# liste pour les cartes de chaque espece
map_raw <- list()

# liste pour les resultats de l'indice K de Ripley

EspK10_plot <- list()
EspK5_plot <- list()

# liste de dataframes pour enregistrer les calculs des metriques

values   <- data.frame(cd_ref = list.esp)
values.spl5 <- split(values, f = values$cd_ref)       
values.spl10 <- split(values, f = values$cd_ref) 
values.spl <- list()

# listes pour garder les cartes

map10 <- list()
map5 <- list()

# listes pour garder les graphiques

graph <- list()

```

## Densité de remplissage des mailles 10x10 par les mailles 5x5 km

Calcul des zones d'occurrence de chaque espèce, périmètres, densité et zone d'intersection.


**Filtrage de chaque espèce**

Pour faire le calcul pour chaque espèce, un filtre a d'abord été réalisé pour n'obtenir que les cellules avec la présence de l'espèce. Cela a été fait avec les données d'observation et la couche de mailles pour la France, à 5 et 10 km.


```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}

    shpEsp10 <-sh10[sh10$CD_SIG %in% tpb10km.spl[[esp]]$id_maille,] 
    shpEsp5 <-sh5[sh5$CD_SIG %in% tpb5km.spl[[esp]]$id_maille,] 
```

**Calcul de la surface d'intersection**

Avec le résultat de la présence de l'espèce dans les mailles de 5 et 10 km, la surface de l'intersection a été calculée. 


```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}
    
  centroid_10  <- sf::st_centroid(shpEsp10, of_largest_polygon = TRUE) 
  centroid_5  <- sf::st_centroid(shpEsp5, of_largest_polygon = TRUE) 
  int.g10_c5 <- st_intersects((shpEsp10),(centroid_5), sparse = FALSE)
  
  nb_mai_int <- table(int.g10_c5)["TRUE"]  # nombre de mailles dans l'intersection
  area_int.g10_c5 <- nb_mai_int*25 # multiplié par la surface d'intersection
```

**Calcul de la surface et du périmètre de chaque fragment (résultat en km)**

  
```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}

     sp10 <- as(shpEsp10, "Spatial")  
     sp5 <- as(shpEsp5, "Spatial")                               
     
     # surface and perimeter 
     
     sp10@data$Surface<-round(gArea(sp10, byid=T)/1e6)        
     sp10@data$Perimetre<-round(gLength(sp10, byid=T),0)/1e3           

     sp5@data$Surface<-round(gArea(sp5, byid=T)/1e6)   
     sp5@data$Perimetre<-round(gLength(sp5, byid=T),0)/1e3      
     
     area_10  <- sum(sp10@data$Surface)  
     area_5   <- sum(sp5@data$Surface)  

    # densité de remplisage de 10x10 par 5x5
     
     dens_rempl <-  (sum(area_int.g10_c5)/area_10) * 100
     
  shpEsp10_g <- st_geometry(shpEsp10)
  shpEsp5_g <- st_geometry(shpEsp5)

```
  
**Carte de présence de l'espèce dans les mailles de 10 et 5 km**
  

```{r, echo=FALSE}
  
rawmap  <- ggplot() +
     geom_sf(data = shpEsp10_g, color="#1D928F", fill = "#3A9E98", size = 0.05) +
     geom_sf(data = shpEsp5_g, color="#5D0C0D", fill = alpha("darksalmon",0.8), size = 0.05) +
     xlab("Longitude") + ylab ("Latitude") +
     ggtitle(paste0("Occurence de l'espèce", " ", esp)) +
               theme(plot.title = element_text(hjust = 0.5)) +
     scale_color_manual(values = colors) 

ggplot() +
     geom_sf(data = shpEsp10_g, color="#1D928F", fill = "#3A9E98", size = 0.05) +
     geom_sf(data = shpEsp5_g, color="#5D0C0D", fill = alpha("darksalmon",0.8), size = 0.05) +
     xlab("Longitude") + ylab ("Latitude") +
     ggtitle(paste0("Occurence de l'espèce", " ", esp)) +
               theme(plot.title = element_text(hjust = 0.5)) +
     scale_color_manual(values = colors) 


```

## Taux de voisinage des mailles

Les métriques de voisinage sont basées sur la quantité des mailles de la même espèce voisines par rapport a la quantité des mailles voisines existantes.

**Tout d'abord, il faut standardiser les extensions des données pour chacun des maillages**
<br />
**Ensuite, des fichiers raster ont été créés, pour ensuite appliquer une fonction de calcul qui quantifie le nombre de voisins de chaque cellule**

```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}

    centroid_5  <- st_centroid(shpEsp5, of_largest_polygon = TRUE) # temp centroid_5
    ext10 <- extent(sh10)
    ext5 <- extent(sh5)
    
    raster_templ_10 = raster(shpEsp10, ext = ext10,
                             resolution = 10000,    # resolution pour les mailles de 10km
                             crs = crs(sh10))                   

    raster_templ_5 = raster(shpEsp5, ext = ext5, resolution = 5000, # resolution pour les mailles de 5km
                          crs = crs(sh5)) 
    
    raster10 <- rasterize(centroid_10, raster_templ_10, field = 1)  
    raster5 <- rasterize(centroid_5, raster_templ_5, field = 1)


  r_10_focal = focal(raster10, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)

  r_5_focal = focal(raster5, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)
```

**pour exclure les calculs de voisinage aux cellules vides, la fonction de masque a été appliquée avec le raster de chaque espèce**
  
```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}

  r_10_focal_m <- mask(r_10_focal, raster10, inverse = FALSE)
  r_5_focal_m <- mask(r_5_focal, raster5, inverse = FALSE)

  ## cela sert à soustraire la valeur des voisins donnée par la cellule centrale (la fonction calcule le nombre total de cellules dans la fenêtre de calcul)
  r_10_focal_m_1 <- (r_10_focal_m)-1
  r_5_focal_m_1 <- (r_5_focal_m)-1

```


**Calcul des indices de voisinage**

  Pour le cas du calcul du nombre de voisins totaux pour chaque cellule de l'espèce, les extensions de l'espèce ont dû être standardisées avec l'extension totale pour la France.
  Un masque a ensuite été appliqué au voisinage total sous la forme de chaque espèce. Ainsi, le calcul du voisinage total pour chaque espèce a été obtenu.
  
```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}
  # moyenne voisinage espece  

  vois_10km <- cellStats(r_10_focal_m_1, mean)
  vois_5km  <- cellStats(r_5_focal_m_1, mean) 
  
  # voisinage mailles completes

  raster10_tot.3 <- crop(raster10_tot.2, extent(raster10))
  raster5_tot.3 <- crop(raster5_tot.2, extent(raster5)) 

  raster10_pj <- projectRaster(raster10_tot.3, crs = crs(sh10))
  raster5_pj <- projectRaster(raster5_tot.3, crs = crs(sh5))
  
  raster10_tot.3 <- resample (raster10_pj, raster10, method = "bilinear") 
  raster5_tot.3 <- resample (raster5_pj, raster5, method = "bilinear")
  
  # voisinage total FR 
  
  r_10_focal_tot = focal(raster10_tot.3, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE) 
  r_5_focal_tot = focal(raster5_tot.3, w = matrix(1, nrow = 3, ncol = 3), fun = sum, na.policy = "omit", na.rm = TRUE)
 
  r_10_focal_tot_m <- mask(r_10_focal_tot, raster10_tot.3, inverse = FALSE)
  r_5_focal_tot_m <- mask(r_5_focal_tot, raster5_tot.3, inverse = FALSE)

  r_10_focal_tot_m_1 <- (r_10_focal_tot_m)-1 
  r_5_focal_tot_m_1 <- (r_5_focal_tot_m)-1 
  # masque du voisinage total avec forme pour l'espèce
  
  r_10_focal_tot_m_esp <- mask(r_10_focal_tot_m_1, raster10, inverse = FALSE) 
  r_5_focal_tot_m_esp <- mask(r_5_focal_tot_m_1, raster5, inverse = FALSE)
  
  # calcul de voisinage FR total
  
  nvois10_tot.2 <- cellStats(r_10_focal_tot_m_esp, sum) # quantite de voicines total
  nvois5_tot.2  <- cellStats(r_5_focal_tot_m_esp, sum) # quantite de voicines
  
  # nombre total de voisins pour l'espèce
  
  #NVois_10 <- (r_10_focal_tot_m_esp@nrows)*(r_10_focal_tot_m_esp@ncols) # quantité de mailles 10x10
  #NVois_5 <- (r_5_focal_tot_m_esp@nrows)*(r_5_focal_tot_m_esp@ncols) # quantité de mailles 5x5 
  
  # calcul de nombre de voisines NVois
  
  NVois_10.a <- count(values(r_10_focal_tot_m_esp >=1))
  NVois_10  <- NVois_10.a[1,2]
  
  NVois_5.a <- count(values(r_5_focal_tot_m_esp >=1))
  NVois_5  <- NVois_5.a[1,2]
  
  #********
  # taux moyen de voisinage de l'espece 
  # NVoisEsp = r_10_focal_m_1
  # NVois = r_10_focal_tot_m_esp
  # n = NVois_10
  #********

  TVois_10 <- r_10_focal_m_1/r_10_focal_tot_m_esp 
  TVois_5 <- r_5_focal_m_1/r_5_focal_tot_m_esp
  
  TVoisEsp_10 <- (cellStats(TVois_10, sum))/NVois_10
  TVoisEsp_5 <- (cellStats(TVois_5, sum))/NVois_5
  
```

## Clusters : nombre de clusters, distance moyenne entre clusters, surface moyenne des clusters

**Indice de Ripley (K)**

Cet indice sert à visualiser si la distribution d’une espèce est aléatoire, agrégée ou sur dispersé.  L’indice réponds à la quantité points d’occurrence dans de différents radius concentriques.  Dans le graphique, si la ligne bleue ou orange (espèce) est au-dessus de la ligne grise (aléatoire), la distribution de l’espèce est agrégée, et si la ligne est en au-dessus, la distribution est sur dispersé.
  
  
```{r include = TRUE, message=FALSE, warning=FALSE, comment=NA}
  
   # dans cet exemple, aucun tampon n'a été appliqué, mais cela peut être modifié
   buff_10 <- st_buffer(shpEsp10, dist = 0) 
   buff_5 <- st_buffer(shpEsp5, dist = 0)   
  
  XY<-data.frame(st_coordinates(st_centroid(buff_5, byid=T)), row.names=buff_5$CD_SIG)
  owinesp<-convexhull.xy(XY)
  
  XY$id_maille <- row.names(XY)  
  tpb5km.sp2  <-   left_join(tpb5km.spl[[esp]], XY, by = "id_maille")  
  
  # création d'un objet ppp
  # la condition évite de créer une fenêtre lorsqu'il y a moins de quatre cellules avec occurrence
  
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
  
  XY<-data.frame(st_coordinates(st_centroid(buff_10, byid=T)), row.names=buff_10$CD_SIG)
  owinesp<-convexhull.xy(XY)
  
  XY$id_maille <- row.names(XY)  # put row names as another column
  tpb10km.sp2  <-   left_join(tpb10km.spl[[esp]], XY, by = "id_maille")

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
  

```
  

  
```{r, echo=FALSE}
  
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

ggplot(EspK10, aes(x = r)) + 
         geom_line (aes(y= theo, col = "theo"), size = 1.1) +
         geom_line (aes(y= border, col = "border"), size = 1.1) +
         scale_color_manual(name = "",
                         labels = c("K théorique", "K espèce"),
                         values = c("theo" = "darkgray",
                         "border" = "#3A9E98")) +
         labs(title = "", 
         subtitle = "Maille 10x10 km",
         y = "K(r)")


```


```{r, echo = FALSE}
  
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
    
ggplot(EspK5, aes(x = r)) + 
         geom_line (aes(y= theo, col = "theo"), size = 1.1) +
         geom_line (aes(y= border, col = "border"), size = 1.1) +            
                         scale_color_manual(name = "",
                         labels = c("K théorique", "K espèce"),
                         values = c("theo" = "darkgray",
                         "border" = "darksalmon")) +
         labs(title = "Indice de Ripley (K)", 
         subtitle = "Maille 5x5 km",
         y = "K(r)")


```

**Diversité de fragments** 
Pour le calcul de la diversité des fragments, l'indice de Shannon a été utilisé. Dans ce cas, un résultat de 1 indiquerait qu'il existe de nombreux petits fragments et 0 indiquerait qu'il n'y a qu'un seul grand fragment.


```{r, include=TRUE, message=FALSE, warning=FALSE, comment=NA}
  
  InterF<-gIntersection(gBuffer(sp10,width=.1),gBuffer(sp5,width=.1))
  InterF10<-gIntersection(gBuffer(sp10,width=.1),gBuffer(sp10,width=.1))
  InterF5<-gIntersection(gBuffer(sp5,width=.1),gBuffer(sp5,width=.1))
 
  Frag<-disaggregate(InterF)  
  Frag10<-disaggregate(InterF10)    
  Frag5<-disaggregate(InterF5)

  # fragments dans l'intersection
  
  Frag<-SpatialPolygonsDataFrame(Frag, data.frame(ID=1:length(Frag)))
  Frag@data$Surface<-round(gArea(Frag, byid=T)/1e6)
  Frag@data$Perimetre<-round(gLength(Frag, byid=T),0)/1e3
  divInt <- diversity(Frag@data$Surface)/log(nrow(Frag@data))

  # fragments 10x10

  Frag10<-SpatialPolygonsDataFrame(Frag10, data.frame(ID=1:length(Frag10)))
  Frag10@data$Surface<-round(gArea(Frag10, byid=T)/1e6)
  Frag10@data$Perimetre<-round(gLength(Frag10, byid=T),0)/1e3
  div10 <- diversity(Frag10@data$Surface)/log(nrow(Frag10@data))

  # fragments 5x5

  Frag5<-SpatialPolygonsDataFrame(Frag5, data.frame(ID=1:length(Frag5)))
  Frag5@data$Surface<-round(gArea(Frag5, byid=T)/1e6)
  Frag5@data$Perimetre<-round(gLength(Frag5, byid=T),0)/1e3
  div5 <- diversity(Frag5@data$Surface)/log(nrow(Frag5@data))

  # distance entre fragments

  dist_frag10<-round(gDistance(Frag10, Frag10, byid=T)/1e3); diag(dist_frag10)<-NA 
  dist_frag10<-apply(dist_frag10, 1, min, na.rm=T)
  dist_frag10_moy <- mean(dist_frag10)

  dist_frag5<-round(gDistance(Frag5, Frag5, byid=T)/1e3); diag(dist_frag5)<-NA 
  dist_frag5<-apply(dist_frag5, 1, min, na.rm=T)
  dist_frag5_moy <- mean(dist_frag5)
  #dist_frag5_et <- sd(dist_frag5)
  
  # moyenne surface de fragments
  
  surf_frag5_moy <- mean(Frag5@data$Surface) 
  surf_frag10_moy <- mean(Frag10@data$Surface) 

  
```

```{r, include=FALSE}
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
  
```

## Résultats

Les mesures ont été incluses dans un tableau récapitulatif. Les graphiques ont été placés dans une grille et enregistrés dans une liste pour être utilisés dans un autre script pour les exporter vers un document au format Word.


```{r, echo=FALSE}  
  
  #***********************************************
  # GRAPHIC LAYOUT
  #***********************************************
     
  
    gl <- list(rawmap, EspK5_gg, EspK10_gg)
    
  graph[[esp]] <-  grid.arrange(
    grobs = gl, 
    widths = c(1,1),
    heights = c(1,1),
    layout_matrix = rbind(c(1, 2),c(1, 3))
     )


```

```{r, echo=FALSE}
knitr::kable(values.spl[[esp]], caption = "Métriques de comparaison pour l'espece")
```



