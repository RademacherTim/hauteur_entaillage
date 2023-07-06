# charger les dépendances ------------------------------------------------------
if (!existsFunction('read_excel')) library ('readxl')
if (!existsFunction('%>%')) library ('tidyverse')
if (!existsFunction('str_squish')) library('stringr')
if (existsFunction('openxlsx')) library('createWorkbook')

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

# lire les données de vide ----------------------------------------------------- 
#=============================================================================== 

# boucle des systèmes ----------------------------------------------------------
for (s in systèmes) {
  
  # fait une liste des fichiers à lire -----------------------------------------
  if (s != 'PompesVide') {
    noms_dossiers <- list.files(paste0(dir,s,'_Vacuum_',a,'/'))
  
    # réduit la liste des dossiers ---------------------------------------------
    noms_dossiers <- noms_dossiers[substr(noms_dossiers, 1, 10) %in% dates]
  } else {
    noms_dossiers <- list.files(paste0(dir,s,'/'))
    
    # réduit la liste des dossiers ---------------------------------------------
    noms_dossiers <- noms_dossiers[substr(noms_dossiers, 8, 17) %in% dates]
  }
  
  # vérifie qu'il y a un dossier par date --------------------------------------
  if (length(noms_dossiers) > length(dates)) break ("Erreur : Trop de dossiers!")
  
  # boucle des dates -----------------------------------------------------------
  for (date_fin in dates) {
    
    # obtenir les noms des fichiers --------------------------------------------
    if (s != 'PompesVide') {
      noms_fichiers <- list.files(paste0(dir,s,'_Vacuum_',a,'/',date_fin,'/'))
    } else {
      noms_fichiers <- noms_dossiers
    }
    
    # boucle des fichiers ------------------------------------------------------
    for (f in noms_fichiers){
      
      # par défault il n'y qu'une mesure dans le fichier -----------------------
      double <- FALSE
      
      # décompose le nom du fichier --------------------------------------------
      composantes <- strsplit(f, split = '_')[[1]]
      ligne <- composantes[1]
      if (!(ligne %in% lignes)) break ("Erreur : La ligne n'existe pas!")
      t1 <- ifelse(s != 'PompesVide', composantes[2], "POMPES_1")
      
      # défini deuxième traitement, le cas échéant -----------------------------
      if (s != 'PompesVide') {
        if (substr(composantes[3], 1, 4) != a) {
          t2 <- composantes[3]
          double <- TRUE
        }
      } else {
        t2 <- "POMPES-2"
        double <- TRUE
      }
      
      # lire les données du fichier et ajoute ----------------------------------
      tmp <- read_excel(path = ifelse(s != 'PompesVide', 
                                      paste0(dir,s,'_Vacuum_',a,'/',date_fin,'/',f),
                                      paste0(dir,s,'/',f)),
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
      if (s == systèmes[1] & date_fin == dates[1] & f == noms_fichiers[1]) {
        d1 <- tmp
      } else {
        d1 <- rbind(d1, tmp)
      }
      
      # supprime les variables de traitement -----------------------------------
      if (exists(t2)) rm(t2)
      
    } # fin boucles fichiers
  } # fin boucle dates
  
} # fin boucle systèmes

# brouiller les données --------------------------------------------------------
d1 <- d1 %>% mutate(datetime = as_datetime(datetime))

# créer des moyennes horaires --------------------------------------------------
d1_horaire <- d1 %>% 
  group_by(système, 
           ligne, 
           traitement, 
           heure = floor_date(datetime, "1 hour")) %>%
  summarise(t_capteur = mean(t_capteur),
            vide = mean(vide))

# lire les données de température ----------------------------------------------
#===============================================================================

# obtient les noms des fichiers ------------------------------------------------
noms_fichiers <- list.files(paste0(dir,'TEMP/'))[-1]

# extraire les jours julien et dates des noms ----------------------------------
jj <- as.numeric(sapply(strsplit(sapply(strsplit(noms_fichiers, '_'), "[[", 2), split = "[.]"), "[[", 1))
dates_temp <- as_date(jj, origin = as_date(paste(a,'-01-01')))

# boucle fichiers --------------------------------------------------------------
for (f in noms_fichiers){
  
  # lire les données brutes ----------------------------------------------------
  tmp2 <- read_tsv(file = paste0(dir,'TEMP/',f),
                   col_names = FALSE, show_col_types = FALSE)
  
  tmp2 <- as_tibble(t(as_tibble(sapply(sapply(tmp2, str_squish), strsplit, split = ' +')))) %>%
    rename(jj = V1, h = V2, t_min = V3, t_max = V4, temp = V5) %>% 
    mutate(jj = as.integer(jj),
           h = as.integer(h),
           t_min = as.numeric(t_min),
           t_max = as.numeric(t_max),
           temp = as.numeric(temp)) %>%
    mutate(date = as_date(jj, origin = paste0(a, '-01-01')),
           h = ifelse(h < 10, 
                      paste0('000', h), 
                      ifelse(h < 100, 
                             paste0('00', h), 
                             ifelse(h < 1000, paste0('0', h), h))),
           datetime = as_datetime(paste(date, h), format = 
                                    '%Y-%m-%d %H%M', 
                                  tz = 'EST')) %>%
    select(-h) %>%
    relocate(jj, date, datetime, t_min, t_max, temp)
  
  # ajoute les données de température ------------------------------------------
  if (f == noms_fichiers[1]) {
    d2 <- tmp2
  } else {
    d2 <- rbind(d2, tmp2)
  }
}

# ajouter un colonne pour les plages horaires ----------------------------------
d2 <- d2 %>% 
  group_by(jj, date, datetime, heure = floor_date(datetime, "1 hour")) %>%
  summarise(t_min = min(t_min),
            t_max = max(t_max),
            temp = mean(temp), .groups = 'keep')

# fusionner les deux jeux de données -------------------------------------------
d_horaire <- left_join(d1_horaire, d2, by = 'heure') %>%
  relocate(date, jj, heure, système, ligne, traitement, t_capteur, t_min, 
           t_max, temp, vide) %>% 
  select(-datetime) %>% 
  mutate(vide_ligne = case_when(
    temp <= -2 ~ NA, # TR - Il faut change les seuils ici. La pompe s'arrête à 2C et re-démarre à -0.5C
    temp >  -2 ~ vide,
  ))

# calculer les moyennes journalières -------------------------------------------
d_journalier <- d_horaire %>% 
  group_by(système, ligne, traitement, date) %>%
  summarise(vide_ligne = mean(vide_ligne, na.rm = TRUE), 
            t_capteur = mean(t_capteur, na.rm = TRUE),
            t_min = min(t_min, na.rm = TRUE),
            t_max = max(t_max, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            .groups = 'keep')

# crée un nouveau fichier excel ------------------------------------------------
OUT <- openxlsx::createWorkbook()

# ajoute des feuilles au fichier excel -----------------------------------------
openxlsx::addWorksheet(OUT, "Données brutes")
openxlsx::addWorksheet(OUT, "Données horaires")
openxlsx::addWorksheet(OUT, "Données journalières")

# écrit les données dans le fichier --------------------------------------------
openxlsx::writeData(OUT, sheet = "Données brutes", x = as.data.frame(d1))
openxlsx::writeData(OUT, sheet = "Données horaires", x = as.data.frame(d_horaire))
openxlsx::writeData(OUT, sheet = "Données journalières", x = as.data.frame(d_journalier))

# exporte les données dans un fichier excel ------------------------------------
openxlsx::saveWorkbook(OUT, paste0('Compilation_données_vide_',a,'.xlsx'),
                       overwrite = TRUE)

# début de la saison était le 2023-03-21 et le fin était le 2023-04-16 ---------

# calcule les moyennes pour la saison des sucres -------------------------------
d_horaire %>% group_by(système, ligne, traitement) %>% 
  summarise(vide_m = mean(vide_ligne, na.rm = TRUE),
            vide_et = sd(vide, na.rm = TRUE)) %>% print(n = 40)

