
# import packages

library(magrittr)      # script improvement
library(tidyverse)     # db manipulation
library(data.table)    # db manipulation
library(tidyr)         # db manipulation
library(flextable)     # table edition
library(condformat)    # table edition
library(gdtools)       # graphics
library(formattable)   # table edition
library(officer)       # export documents to Word
library(ggpubr)


# this script uses the objects generated previously graph.RData and values.spl.RData

#load("graph.RData" )
#load("values.spl.RData")


# font types

font.type  <-  fp_text(color = "black", font.size = 11, bold = FALSE, font.family = "Calibri")
font.type.6 <- fp_text(color = "black", font.size = 8, bold = FALSE, font.family = "Calibri")
font.type.h <- fp_text(color = "black", font.size = 12, bold = FALSE, italic = TRUE, font.family = "Calibri")

# table header text
fpar.h <- fpar(ftext("Résultats des métriques pour chaque maille", prop = font.type))

# table bottom comment
fpar.b <- fpar(ftext("NaN : Impossible à calculer / un seul fragment, Inf : Distance nulle entre fragments", prop = font.type.6))

# white rows
fpar.space <- fpar(ftext("", prop = font.type.6)) 

# border formating
out_bord = fp_border(color="#DEDEDE", width = 1)   
inn_bord = fp_border(color="#DEDEDE", width = 1)    
hea_bord = fp_border(color="#DEDEDE", width = 1)   

# optional if you want to test some species
#esp <- "139"
#list.esp <- c("139","212","223")


# total list of especies in order
graph_ord  <- graph[order(as.numeric(names(graph)))]
list.esp  <- names(graph_ord)

# working directory to save document
setwd("CHOSE YOUR DIRECTORY")  # CHANGED TO THE NEW FOLDER

#*********************************************************************
# Loop to create the document with results for all the species (299)
#*********************************************************************

my_doc <- read_docx() # object to store the document data

for (esp in list.esp){
  
fpar.h <- fpar(ftext("Résultats des métriques pour chaque maille", prop = font.type))
  
sp_header  <- paste(values.spl[[esp]][1,]$nom_valide, " ","(",esp,")", sep = "")
fpar.h.gen <- fpar(ftext(sp_header, prop = font.type.h)) 

graph.esp <- as_ggplot(graph[[esp]])

values.df <- data.frame(values.spl[[esp]])

ft  <- flextable(values.df) 

  ft.mer <- merge_v(ft, j = c("nom_valide","group_2_INPN","regroupement","cd_ref","connaissance",
                              "area_int","frag_div_int","vid.remp","dens_rempl"))

  ft.rem <- border_remove(x = ft.mer)

  ft.out <- border_outer(ft.rem, part="all", border = out_bord)
  ft.inn <- border_inner(ft.out, part="body", border = inn_bord)
  ft.hea <- border_inner(ft.inn, part="header", border = hea_bord)
  ft.hea.l <- hline(ft.hea, part="header", border= inn_bord)
  
    # add color to header
  ft.hea.c1 <- bg(ft.hea.l, bg = "#F2F2F2", part = "header")
  
    # add color to first row 5km
  ft.row.c1  <- bg(ft.hea.c1, i = 1, j = 10:16, bg =  "#ffd7c7") # lightsalmon 
  
    # add color to second row 10km
  ft.row.c2  <- bg(ft.row.c1, i = 2,  j = 10:16, bg = "#b1d8d8") # green
  
  # change font color and size
  ft.font.c <- color(ft.row.c2, color = "#000000", part = "all") #text in black
  ft.font.s <- fontsize(ft.font.c, size = 6, part = "all" ) 

  # add bold style to headher, 1st and 9th columns
  # add italic style to species name
  ft.font.s.b <- bold(ft.font.s, bold = TRUE, part = "header") 
  ft.font.s.b2 <- bold(ft.font.s.b, j = 1, bold = TRUE)
  ft.font.s.b3 <- bold(ft.font.s.b2, j = 9, bold = FALSE)
  ft.font.s.i <- italic(ft.font.s.b3, j = 1, italic = TRUE)
  
  # align text
  ft.font.a <- align(ft.font.s.i, align = "center", part= "all")

  # edit columns widths
  ft.font.w <- width(ft.font.a, j = 1, width = 1.3) # LB_NOM_VALIDE
  ft.font.w <- width(ft.font.w, j = 2, width = 0.8) # GROUP2_INPN
  ft.font.w <- width(ft.font.w, j = 3, width = 0.8) # Regroupement
  ft.font.w <- width(ft.font.w, j = 4, width = 0.5) # cd_ref
  ft.font.w <- width(ft.font.w, j = 5, width = 0.73) # connaissance
  ft.font.w <- width(ft.font.w, j = 6, width = 0.66) # dens_rempl
  ft.font.w <- width(ft.font.w, j = 7, width = 0.5) # area_int
  ft.font.w <- width(ft.font.w, j = 8, width = 0.65) # frag_div_int 
  ft.font.w <- width(ft.font.w, j = 9, width = 0.57) # vid.remp
  ft.font.w <- width(ft.font.w, j = 10, width = 0.4) # maille 
  ft.font.w <- width(ft.font.w, j = 11, width = 0.55) # area
  ft.font.w <- width(ft.font.w, j = 12, width = 0.35) # vois 
  ft.font.w <- width(ft.font.w, j = 13, width = 0.55) # TVoisEsp 
  ft.font.w <- width(ft.font.w, j = 14, width = 0.5)  # frag_div 
  ft.font.w <- width(ft.font.w, j = 15, width = 0.74) # surf_frag_moy 
  ft.font.w <- width(ft.font.w, j = 16, width = 0.74) # dist_frag_moy
  
  # add parts to document
    body_add_fpar(my_doc, fpar.h.gen)
    body_add_fpar(my_doc, fpar.space)
    body_add_gg(my_doc, value = graph.esp, style = "centered", width = 10.3, height = 4.32, res = 100, pointsize = 12)
    body_add_fpar(my_doc, fpar.h) 
    body_add_flextable(my_doc, ft.font.w)
    body_add_fpar(my_doc, fpar.b)
    body_add_break(my_doc, pos = "after")
  }
  # End of section    
  body_end_section_landscape(my_doc)
  print(my_doc, target = paste("results_analyse_mailles_PBolanos.docx",sep =""))
  
  #save(my_doc, file = "results_analyse_mailles_PBolanos.RData") if you want to save the object
  
  
  
  
