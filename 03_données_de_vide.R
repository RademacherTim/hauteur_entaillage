# charger les dépendances ------------------------------------------------------
if (!existsFunction('read_excel')) library ('readxl')
if (!existsFunction('%>%')) library ('tidyverse')

# clé --------------------------------------------------------------------------
# il avait quatre hauteurs différentes dans les traitements :
#     24" au dessus  du latéral  = _h2        # Contrôle A, C et E 
#      4" au dessus  du latéral  = _h1        # Traitement C et E1 
#      4" en dessous du latéral  = _b1        # Contrôle B et Traitement E2
#     24" en dessous du latéral  = _b2        # Traitement A, B, E3

# détermine le dossier avec les données
dir <- '../données/vacuum/'

# lire le fichier avec la structure du réseau ----------------------------------
struc <- read_excel(path = paste0(dir,'structure_du_réseau.xlsx'))
systèmes <- strsplit(struc$systèmes, ', ')[[1]]
lignes <- strsplit(struc$lignes, ', ')[[1]]
dates <-  as.character(strsplit(struc$dates, ', ')[[1]])
a <- struc$année
  
# boucle des systèmes ----------------------------------------------------------
for (s in systèmes) {
  
  # fait une liste des fichiers à lire -----------------------------------------
  noms_dossiers <- list.files(paste0(dir,s,'_Vacuum_',a,'/'))
  
  # réduit la liste des dossiers -----------------------------------------------
  noms_dossiers <- noms_dossiers[substr(noms_dossiers, 1, 10) %in% dates]
  
  # vérifie qu'il y a un dossier par date --------------------------------------
  if (length(noms_dossiers) > length(dates)) break ("Erreur : Trop de dossiers!")
  
  # boucle des dates -----------------------------------------------------------
  for (date_fin in dates) {
    
    # obtenir les noms des fichiers --------------------------------------------
    noms_fichiers <- list.files(paste0(dir,s,'_Vacuum_',a,'/',date_fin,'/'))
    
    # boucle des fichiers ------------------------------------------------------
    for (f in noms_fichiers){
      
      # par défault il n'y qu'une mesure dans le fichier -----------------------
      double <- FALSE
      
      # décompose le nom du fichier --------------------------------------------
      composantes <- strsplit(f, split = '_')[[1]]
      ligne <- composantes[1]
      if (!(ligne %in% lignes)) break ("Erreur : La ligne n'existe pas!")
      t1 <- composantes[2]
      
      # défini deuxième traitement, le cas échéant -----------------------------
      if (substr(composantes[3], 1, 4) != a) {
        t2 <- composantes[3]
        double <- TRUE
      }
      
      # lire les données du fichier et ajoute ----------------------------------
      tmp <- read_excel(path = paste0(dir,s,'_Vacuum_',a,'/',date_fin,'/',f), 
                        skip = 1, 
                        sheet = "Ark1")
        
      # combien de mesures dans le fichier -----------------------------------
      if (double) {
        tmp <- tmp %>% 
          pivot_longer(cols = c(Pression, `Pression 2`), 
                       names_to = 'traitement', 
                       values_to = 'vide') %>% 
          rename(datetime = DateTime, t_capteur = Température) %>%
          mutate(traitement = case_when(
            traitement == 'Pression' ~ t1,
            traitement == 'Pression 2' ~ t2,
          ))
      } else {
        tmp <- tmp %>% 
          rename(datetime = DateTime, 
                 t_capteur = Température, 
                 vide = Pression) %>%
          add_column(traitement = t1)
          
      }
      
      # ajoute la ligne et le système aux données ------------------------------
      tmp <-  tmp %>% add_column(ligne = ligne, système = s) %>% 
        relocate(datetime, système, ligne, t_capteur, vide)
      
      # combine les données ----------------------------------------------------
      if (f == noms_fichiers[1] & date_fin == dates[1]) {
        d <- tmp
      } else {
        d <- rbind (d, tmp)
      }
      
      # supprime les variables de traitement -----------------------------------
      if (exists(t2)) rm(t2)
      
    } # fin boucles fichiers
  } # fin boucle dates
  
} # fin boucle systèmes
