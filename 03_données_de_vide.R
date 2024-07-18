# ============================================================================ #
# Lire les données de vide
# ---------------------------------------------------------------------------- #

# charger les dépendances ------------------------------------------------------
if (!existsFunction('read_excel')) library ('readxl')
if (!existsFunction('%>%')) library ('tidyverse')
if (!existsFunction('str_squish')) library('stringr')
if (existsFunction('openxlsx')) library('createWorkbook')

# Clé pour les traitements -----------------------------------------------------
# il avait quatre hauteurs différentes dans les traitements en 2023 et 2024:

# 2023 
#-------------------------------------------------------------------------------
#     24" au dessus  du latéral  = _h3        # Contrôle A, C et E 
#      4" au dessus  du latéral  = _h1        # Traitement C et E1 
#      4" en dessous du latéral  = _b1        # Contrôle B et Traitement E2
#     24" en dessous du latéral  = _b3        # Traitement A, B, E3

# 2024
#-------------------------------------------------------------------------------
#     24" au dessus  du latéral  = _h3        # Contrôle B, Traitement C et E1 
#     12" au dessus  du latéral  = _h2        # Traitement A et Contrôle E 
#     12" en dessous du latéral  = _b2        # Contrôle C et Traitement E2
#     24" en dessous du latéral  = _b3        # Contrôle A, Traitement B et E3

# initier les noms des fichiers ------------------------------------------------
file_name_2023 <- '../données/vacuum/vac_CDL_2023_15mars_14avril_TDC.xlsx'
file_name_2024 <- '../données/vacuum/vac_CDL_2024_27février_12avril_TDC.xlsx'

# lire les données du système Smartrek en 2023 et 2024 -------------------------
tmp_23 <- read_excel(path = file_name_2023, sheet = "Données Brutes") %>% 
  rename(datetime = `Date et Heure UTC`, 
         vide = `Vacuum Entaille`, 
         temp = Température) 
tmp_23 <- tmp_23 %>%
  mutate(année = 2023,
         ligne   = unlist(str_split(Capteur, pattern = ' '))[seq(1, dim(tmp_23)[1]*4, 4)],
         endroit = unlist(str_split(Capteur, pattern = ' '))[seq(2, dim(tmp_23)[1]*4, 4)],
         t       = unlist(str_split(Capteur, pattern = ' '))[seq(3, dim(tmp_23)[1]*4, 4)],
         arbre   = unlist(str_split(Capteur, pattern = ' '))[seq(4, dim(tmp_23)[1]*4, 4)]) %>%
  select(année, ligne, endroit, t, arbre, datetime, temp, vide) %>%
  mutate(t = case_when(t == "24+" ~ "h3",
                       t == "4+" ~ "h1",
                       t == "4-" ~ "b1",
                       t == "24-" ~ "b3")) %>%
  mutate(h = case_when(t == "h3" ~ 60.96,
                       t == "h1" ~ 10.16,
                       t == "b1" ~ -10.16,
                       t == "b3" ~ -60.96))
tmp_24 <- read_excel(path = file_name_2024, sheet = "Données Brutes") %>% 
  rename(datetime = `Date et Heure UTC`, 
         vide = `Vacuum Entaille`, 
         temp = Température) 
tmp_24 <- tmp_24 %>%
  mutate(année = 2024,
         ligne   = unlist(str_split(Capteur, pattern = ' '))[seq(1, dim(tmp_24)[1]*4, 4)],
         endroit = unlist(str_split(Capteur, pattern = ' '))[seq(2, dim(tmp_24)[1]*4, 4)],
         t       = unlist(str_split(Capteur, pattern = ' '))[seq(3, dim(tmp_24)[1]*4, 4)],
         arbre   = unlist(str_split(Capteur, pattern = ' '))[seq(4, dim(tmp_24)[1]*4, 4)]) %>%
  select(année, ligne, endroit, t, arbre, datetime, temp, vide) %>%
  mutate(t = case_when(t == "24+" ~ "h3",
                       t == "12+" ~ "h2",
                       t == "12-" ~ "b2",
                       t == "24-" ~ "b3")) %>%
  mutate(h = case_when(t == "h3" ~ 60.96,
                       t == "h2" ~ 30.48,
                       t == "b2" ~ -30.48,
                       t == "b3" ~ -60.96))

# exclure les données avant et après la récolte (2023-03-21 à 2023-04-16 ainsi 
# que 2023-02-27 et 2024-04-11) ------------------------------------------------
tmp_23 <- tmp_23 %>% filter(datetime > as_datetime("2023-03-21") & 
                          datetime < as_datetime("2023-04-16"))
tmp_24 <- tmp_24 %>% filter(datetime > as_datetime("2024-02-27") & 
                              datetime < as_datetime("2024-04-11"))
#vide <- rbind(tmp_23, tmp_24); rm(tmp_23, tmp_24)
vide_p <- rbind(tmp_23, tmp_24); rm(tmp_23, tmp_24)

# Nous avons essayer de séléctionner les données de trois façons différentes:
# 1) En utilisant toutes les données
# 2) En excluant les données en bas de -26 "Hg
# 3) En excluant les données en bas de -26 "Hg et des changements de plus de 1 "Hg entre mesures
# Le code qui suit produit ces sélections et il doit être commenté au besoin. 
# N.B.: Si je n'exclu pas les données en bas de -26 "Hg, je devrait utiliser une 
# distribution lognormale plutôt que normale.
# enlève tout les données avec un sous-vide inférieur à -26 "Hg ----------------
#vide$vide[vide$vide > -26] <- NA

# enlève les données avec un changement de vide plus hauts que 1" Hg -----------
#vide_p <- vide_p %>%
#  group_by(année, ligne, endroit, t, arbre) %>%
#  arrange(datetime) %>%
#  mutate(vide_p = ifelse(c(0, abs(diff(vide))) > 1, NA, vide))

# calcul du temps de coulée effectif (tce) -------------------------------------
#===============================================================================

# calcul le temps depuis la dernière mesure ------------------------------------
vide_p <- vide_p %>% group_by(année, ligne, endroit, t, arbre) %>%
  arrange(datetime) %>%
  mutate(diff = datetime - lag(datetime)) 
vide_p$diff[which(vide_p$diff > 4000)] <- NA
vide_p$diff[which(vide_p$temp < -2)] <- NA

tce <- vide_p %>%
  filter(!is.na(vide)) %>% 
  group_by(année, ligne, t, arbre, endroit) %>% 
  summarise(tce = sum(as.numeric(diff), na.rm = TRUE) / (60 * 60 * 24), 
            .groups = "drop") %>% group_by(année, t) %>%
  summarise(mean = mean(tce),
            sd = sd(tce), .groups = "drop")

# calcule les moyennes horaires ------------------------------------------------
v_h <- vide_p %>% 
  group_by(année,
           ligne, 
           t,
           endroit,
           heure = floor_date(datetime, "1 hour")) %>%
  summarise(vide = mean(vide), .groups = 'drop')

# ============================================================================ #
# lire les données des têtes de ligne, des extracteurs, etc. -------------------
# ============================================================================ #
# détermine le dossier avec les données
dir <- '../données/vacuum/'

# lire le fichier avec la structure du réseau ----------------------------------
struc <- read_excel(path = paste0(dir,'structure_du_réseau.xlsx'))

# lire les données de vide des têtes de ligne, des extracteurs, etc. -----------
#=============================================================================== 

# boucle des années ------------------------------------------------------------
for (a in struc$année) {
  i <- which(struc$année == a)
  systèmes <- strsplit(struc$systèmes, ', ')[[i]]
  lignes <- strsplit(struc$lignes, ', ')[[i]]
  dates <-  as.character(strsplit(struc$dates, ', ')[[i]])
  if(a == 2024) dates <- substr(dates, 2, 11) 

  # boucle des systèmes --------------------------------------------------------
  for (s in systèmes) {
  
    # fait une liste des fichiers à lire -----------------------------------------
    if (s != 'PompesVide') {
      noms_dossiers <- list.files(paste0(dir,s,'_Vacuum_',a,'/'))
    
      # réduit la liste des dossiers ---------------------------------------------
      noms_dossiers <- noms_dossiers[substr(noms_dossiers, 1, 10) %in% dates]
    } else {
      noms_dossiers <- list.files(paste0(dir,s,"_",a,"/"))
      
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
          t2 <- "POMPES_2"
          double <- TRUE
        }
        
        # lire les données du fichier et ajoute ----------------------------------
        tmp <- read_excel(path = ifelse(s != 'PompesVide', 
                                        paste0(dir,s,'_Vacuum_',a,'/',date_fin,'/',f),
                                        paste0(dir,s,"_",a,"/",f)),
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
        if (s == systèmes[1] & date_fin == dates[1] & f == noms_fichiers[1] & 
            a == 2023) {
          d1 <- tmp
        } else {
          d1 <- rbind(d1, tmp)
        }
        
        # supprime les variables de traitement -----------------------------------
        if (exists(t2)) rm(t2)
        
      } # fin boucles fichiers
    } # fin boucle dates
    
  } # fin boucle systèmes
} # fin boucle de l'année

# brouiller les données --------------------------------------------------------
d1 <- d1 %>% mutate(datetime = as_datetime(datetime))

# graphique des données brutes du système A ------------------------------------
par(mar = c(5, 5, 1, 1))
par(mfrow = c(2, 2))
plot(y = -d1$vide[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A1"],
     x = d1$datetime[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A1"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A1"],
       x = d1$datetime[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A1"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne A1")
plot(y = -d1$vide[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A2"],
     x = d1$datetime[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A2"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A2"],
       x = d1$datetime[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A2"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne A2")
plot(y = -d1$vide[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A3"],
     x = d1$datetime[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A3"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A3"],
       x = d1$datetime[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A3"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne A3")
plot(y = -d1$vide[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A4"],
     x = d1$datetime[d1$système == "A" & d1$traitement == "CTRL" & d1$ligne == "A4"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A4"],
       x = d1$datetime[d1$système == "A" & d1$traitement == "TTM" & d1$ligne == "A4"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne A4")

# graphique des données brutes du système B ------------------------------------
par(mar = c(5, 5, 1, 1))
par(mfrow = c(2, 2))
plot(y = -d1$vide[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B1"],
     x = d1$datetime[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B1"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#40004b")
lines (y = -d1$vide[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B1"],
       x = d1$datetime[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B1"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne B1")
plot(y = -d1$vide[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B2"],
     x = d1$datetime[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B2"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#40004b")
lines (y = -d1$vide[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B2"],
       x = d1$datetime[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B2"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne B2")
plot(y = -d1$vide[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B3"],
     x = d1$datetime[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B3"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#40004b")
lines (y = -d1$vide[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B3"],
       x = d1$datetime[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B3"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne B3")
plot(y = -d1$vide[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B4"],
     x = d1$datetime[d1$système == "B" & d1$traitement == "CTRL" & d1$ligne == "B4"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#40004b")
lines (y = -d1$vide[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B4"],
       x = d1$datetime[d1$système == "B" & d1$traitement == "TTM" & d1$ligne == "B4"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne B4")

# graphique des données brutes du système C ------------------------------------
par(mar = c(5, 5, 1, 1))
par(mfrow = c(2, 2))
plot(y = -d1$vide[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C1"],
     x = d1$datetime[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C1"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C1"],
       x = d1$datetime[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C1"], 
       col = "#00441b")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne C1")
plot(y = -d1$vide[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C2"],
     x = d1$datetime[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C2"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C2"],
       x = d1$datetime[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C2"], 
       col = "#00441b")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne C2")
plot(y = -d1$vide[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C3"],
     x = d1$datetime[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C3"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C3"],
       x = d1$datetime[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C3"], 
       col = "#00441b")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne C3")
plot(y = -d1$vide[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C4"],
     x = d1$datetime[d1$système == "C" & d1$traitement == "CTRL" & d1$ligne == "C4"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C4"],
       x = d1$datetime[d1$système == "C" & d1$traitement == "TTM" & d1$ligne == "C4"], 
       col = "#00441b")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne C4")

# graphique des données brutes du système E ------------------------------------
par(mar = c(5, 5, 1, 1))
par(mfrow = c(2, 1))
plot(y = -d1$vide[d1$système == "E" & d1$traitement == "CTRL" & d1$ligne == "E1"],
     x = d1$datetime[d1$système == "E" & d1$traitement == "CTRL" & d1$ligne == "E1"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#a6dba0")
lines (y = -d1$vide[d1$système == "E" & d1$traitement == "TTM1" & d1$ligne == "E1"],
       x = d1$datetime[d1$système == "E" & d1$traitement == "TTM1" & d1$ligne == "E1"], 
       col = "#00441b")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne E1")
plot(y = -d1$vide[d1$système == "E" & d1$traitement == "TTM2" & d1$ligne == "E2"],
     x = d1$datetime[d1$système == "E" & d1$traitement == "TTM2" & d1$ligne == "E2"], typ = "l",
     ylim = c(-29, -26),
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     col = "#40004b")
lines (y = -d1$vide[d1$système == "E" & d1$traitement == "TTM3" & d1$ligne == "E2"],
       x = d1$datetime[d1$système == "E" & d1$traitement == "TTM3" & d1$ligne == "E2"], 
       col = "#c2a5cf")
text(x = as_datetime("2023-04-14"), y = -28.8, labels = "Ligne E2")


# créer des moyennes horaires --------------------------------------------------
d1_horaire <- d1 %>% 
  group_by(système, 
           ligne, 
           traitement, 
           heure = floor_date(datetime, "1 hour")) %>%
  summarise(t_capteur = mean(t_capteur),
            vide = mean(vide), .groups = 'drop')

# TR - Need to get temperature files for 2024 to include them in the analysis for the report.
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
    temp <= -2 ~ NA, # TR - Il faut changer les seuils ici. La pompe s'arrête à 2C et re-démarre à -0.5C
    temp >  -2 ~ vide,
  ))

# ajoute les traitements et hauteurs relatives aux données de vide -------------
d_horaire <- d_horaire %>% mutate(t = case_when(
  système == "A" & traitement == "CTRL" ~ "h2",
  système == "A" & traitement == "TTM"  ~ "b2",
  système == "B" & traitement == "CTRL" ~ "b1",
  système == "B" & traitement == "TTM"  ~ "b2",
  système == "C" & traitement == "CTRL" ~ "h2",
  système == "C" & traitement == "TTM"  ~ "h1",
  système == "E" & traitement == "CTRL" ~ "h2",
  système == "E" & traitement == "TTM1" ~ "h1",
  système == "E" & traitement == "TTM2" ~ "b1",
  système == "E" & traitement == "TTM3" ~ "b2",
),
                     h = case_when(
  système == "A" & traitement == "CTRL" ~  60.96,
  système == "A" & traitement == "TTM"  ~ -60.96,
  système == "B" & traitement == "CTRL" ~ -10.16,
  système == "B" & traitement == "TTM"  ~ -60.96,
  système == "C" & traitement == "CTRL" ~  60.96,
  système == "C" & traitement == "TTM"  ~  10.16,
  système == "E" & traitement == "CTRL" ~  60.96,
  système == "E" & traitement == "TTM1" ~  10.16,
  système == "E" & traitement == "TTM2" ~ -10.16,
  système == "E" & traitement == "TTM3" ~ -60.96,
))


# calculer les moyennes journalières -------------------------------------------
d_journalier <- d_horaire %>% 
  group_by(système, ligne, traitement, date) %>%
  summarise(vide_ligne = mean(vide_ligne, na.rm = TRUE), 
            t_capteur = mean(t_capteur, na.rm = TRUE),
            t_min = min(t_min, na.rm = TRUE),
            t_max = max(t_max, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            .groups = 'drop')
# TR - Ajouter les dégrès jours et l'indice SBB --------------------------------


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
d_horaire %>% group_by(h) %>% 
  filter(date > as_date("2023-03-21") & date < as_date("2023-04-16")) %>%
  filter(système != "PompesVide") %>%
  summarise(vide_m = mean(vide_ligne, na.rm = TRUE),
            vide_et = sd(vide, na.rm = TRUE),
            .groups = "drop")

# faire le ménage --------------------------------------------------------------
rm(tmp, struc, a, composantes, date_fin, dates, dir, double, f, file_name_2023, 
   file_name_2024, ligne, lignes, noms_dossiers, noms_fichiers, s, systèmes, 
   t1, t2)
