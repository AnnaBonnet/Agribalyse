library(tidyverse)
library(readxl)

url1<-'https://data.ademe.fr/data-fair/api/v1/datasets/agribalyse-31-synthese/metadata-attachments/AGRIBALYSE3.1.1_produits%20alimentaires.xlsx'
p1f <- tempfile()

download.file(url1, p1f, mode="wb")
agritot<-read_excel(path = p1f,sheet = 3, skip=2 ,col_names=TRUE, .name_repair = "minimal") %>% 
  select(c(5,3:4,16:22)) %>% rename(nom =`Nom du Produit en Français`, groupe = `Groupe d'aliment`, sous_groupe=`Sous-groupe d'aliment`)



caf <- agritot$nom[grepl('caf',agritot$nom)][1:7]

chocolat_boisson <- agritot$nom[grepl('chocolat',agritot$nom)][2:8]

the <- agritot$nom[grepl('thé',agritot$nom)]
yaourt <- agritot$nom[grepl('Yaourt',agritot$nom)]
agri <- agritot  %>% 
  mutate(sg = case_when(
    sous_groupe%in% c("viandes cuites","viandes crues")~"viandes",
    sous_groupe=="charcuterie"~"charcuterie",
    grepl("poisson",sous_groupe)~"poisson",
    grepl("mollusque",sous_groupe)~"mollusque",
    grepl("substituts",sous_groupe)~"substituts",
    sous_groupe=="œufs"~"oeufs",
    sous_groupe =="légumes"~"légumes",
    sous_groupe =="fruits"~"fruits",
    sous_groupe =="glaces"~"glaces", 
    sous_groupe =="gâteaux et pâtisseries"~"gâteaux et pâtisseries",
    sous_groupe =="chocolats et produits à base de chocolat"~"chocolats et produits à base de chocolat",
    sous_groupe =="confiseries non chocolatées"~"confiseries non chocolatées",
    sous_groupe =="céréales de petit-déjeuner et biscuits"~"céréales de petit-déjeuner et biscuits",
    sous_groupe %in% the ~ "thé",
    sous_groupe %in% chocolat_boisson ~ "chocolat_chaud",
    sous_groupe %in% caf ~ "café",
    sous_groupe %in% yaourt ~ "yaourt",
    sous_groupe =="pâtes, riz et céréales"~ "pâtes, riz et céréales",
    groupe=="lait et produits laitiers"~"produits laitiers",
    TRUE~"supr"
  )) %>% 
  filter(sg!="supr") %>% 
  dplyr::select(!c(groupe,sous_groupe)) %>% 
  relocate(sg) 

write.csv(agri,file="agri_tot.csv")