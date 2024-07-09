# ============================================================================ #
# Lire les données de volume et de la composition de sève
# ---------------------------------------------------------------------------- #

# charger les dépendances ------------------------------------------------------
if(!existsFunction('read_excel')) library('readxl')
if(!existsFunction('%>%')) library('tidyverse')

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


# lire les données des systèmes ------------------------------------------------
systeme <- c("A","A","B","B","C","C","E","E","E","E")
traitement <- c("C","T","C","T","C","T","C","T","T","T")
ligne <- c("AC", "AT", "BC", "BT", "CC", "CT", "EC", "E1", "E2", "E3")

# Couleurs pour les graphiques -------------------------------------------------
#    t         h (cm)       couleur             hexcode         pch
#    h3        +60.96       vert très foncé     "#a6dba0"       24
#    h2        +30.48       vert foncé          "#5aae61"       2
#    h1.5      +20.32       vert                "#1b7837"       21
#    h1        +10.16       vert pale           "#00441b"       1
#    b1        -10.16       mauve pale          "#40004b"       5
#    b1.5      -20.32       mauve               "#762a83"       23
#    b2        -30.48       mauve foncé         "#9970ab"       6
#    b3        -60.96       mauve très foncé    "#c2a5cf"       25

# initialiser les noms des fichiers --------------------------------------------
nom_fichier_SN_2023 <- "../données/Compilation donnée érablière - 2023.xlsm"
nom_fichier_SN_2024 <- "../données/Compilation donnée érablière - 2024.xlsm"
nom_fichier_CE_2022 <- "../données/Projet_chalumeau_2022.xlsx"
nom_fichier_CE_2023 <- "../données/Projet_chalumeau_2023.xlsx"

# extraire les metadonnées -----------------------------------------------------
info_SN1 <- readxl::read_excel(path = nom_fichier_SN_2023,
                               sheet = "Paramètres", range = "B19:K19",
                               col_types = c(rep("numeric", 10)),
                               col_names = FALSE) %>% 
  pivot_longer(cols = 1:10, values_to = "n_arbres") %>%
  add_column(systeme, traitement, ligne, .before = 1) %>% 
  select(-name) %>% 
  add_column(t = c("h3", "b3", "b1", "b3", "h3", "h1", "h3", "h1", "b1", "b3"),
             colour = c("#a6dba0", "#c2a5cf", "#40004b", "#c2a5cf", "#a6dba0", 
                        "#00441b", "#a6dba0", "#00441b", "#40004b", "#c2a5cf"),
             sym = c(24, 25, 5, 25, 24, 1, 24, 1, 5, 25)) %>%
  mutate(année = 2023)
info_SN2 <- readxl::read_excel(path = nom_fichier_SN_2024,
                               sheet = "Paramètres", range = "B19:K19",
                               col_types = c(rep("numeric", 10)),
                               col_names = FALSE) %>% 
  pivot_longer(cols = 1:10, values_to = "n_arbres") %>%
  add_column(systeme, traitement, ligne, .before = 1) %>% 
  select(-name) %>% 
  add_column(t = c("b3", "h2", "h3", "b3", "b2", "h3", "h2", "h3", "b2", "b3"),
             colour = c("#c2a5cf", "#1b7837", "#a6dba0", "#c2a5cf", "#762a83", 
                        "#a6dba0", "#1b7837", "#a6dba0", "#762a83", "#c2a5cf"),
             sym = c(25, 2, 24, 25, 6, 24, 2, 24, 6, 25)) %>%
  mutate(année = 2024)
info <- rbind(info_SN1, info_SN2); rm(info_SN1, info_SN2)

# initialise column names and column types -------------------------------------
noms_col_SN <- c(
  'date','time', 'compteur_1', 'coulee_m3_1', 'coulee_l_1', 'coulee_cum_1',
  'rendement_1', 'compteur_2', 'coulee_m3_2', 'coulee_l_2', 'coulee_cum_2',
  'rendement_2', 'compteur_3', 'lecture', 'coulee_m3_3', 'coulee_l_3',
  'coulee_cum_3', 'rendement_3', 'brix', 'commentaires', 'diff_compteur',
  'diff_compteur_1', 'debit', 'debit_cum', 'rendement', 'rendement_cum')
types_col_SN <- c('date', 'text', rep('numeric', 17), 'text', rep('numeric', 6))

# lire les données du système A contrôle de 2023 (h3; 24" au dessus) -----------
A_h3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(1)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'A', ligne = 'AC', h = 60.96, année = 2023)

# lire les données du système A traitement de 2023 (b3; 24" en dessous) --------
A_b3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(2)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN,
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'A', ligne = 'AT', h = -60.96, année = 2023)

# lire les données du système B contrôle de 2023 (b1; 4" en dessous) -----------
B_b1_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(3)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN,
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b1', systeme = 'B', ligne = 'BC', h = -10.16, année = 2023)

# lire les données du système B traitement de 2023 (b3; 24" en dessous) --------
B_b3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(4)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN,
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'B', ligne = 'BT', h = -60.96, année = 2023)

# lire les données du système C contrôle de 2023 (h3; 24" au dessus) -----------
C_h3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(5)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN,
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'C', ligne = 'CC', h = 60.96, année = 2023)

# lire les données du système C traitement de 2023 (h1; 4" au dessus) ----------
C_h1_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(6)', skip = 4, na = '-', n_max = 28,
                                col_types = types_col_SN,
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h1', systeme = 'C', ligne = 'CT', h = 10.16, année = 2023)

# lire les données du système E contrôle de 2023 (h3; 24" au dessus) -----------
E_h3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(7)', skip = 4, na = '-', n_max = 26,
                                col_types = types_col_SN[-c(13:18, 22)],
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'E', ligne = 'EC', h = 60.96, année = 2023)

# lire les données du système E traitement 1 de 2023 (h1; 4" au dessus) --------
E_h1_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(8)', skip = 4, na = '-', n_max = 26,
                                col_types = types_col_SN[-c(13:18, 22)],
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h1', systeme = 'E', ligne = 'E1', h = 10.16, année = 2023)

# lire les données du système E traitement 2 de 2023 (b1; 4" en dessous) -------
E_b1_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(9)', skip = 4, na = '-', n_max = 26,
                                col_types = types_col_SN[-c(13:18, 22)],
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b1', systeme = 'E', ligne = 'E2', h = -10.16, année = 2023)

# lire les données du système E traitement 3 de 2023 (b3; 24" en dessous) ------
E_b3_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = '(10)', skip = 4, na = '-', n_max = 26,
                                col_types = types_col_SN[-c(13:18, 22)],
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'E', ligne = 'E3', h = -60.96, année = 2023)

# lire les données du système A contrôle de 2024 (b3; 24" en dessous) ----------
A_b3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(1)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'A', ligne = 'AC', h = -60.96, année = 2024)

# lire les données du système A traitement de 2024 (h2; 12" au dessus) ---------
A_h2_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(2)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h2', systeme = 'A', ligne = 'AT', h = 30.48, année = 2024)

# lire les données du système B contrôle de 2024 (h3; 24" au dessus) -----------
B_h3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(3)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'B', ligne = 'BC', h = 60.96, année = 2024)

# lire les données du système B traitement de 2024 (b3; 24" en dessous) --------
B_b3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(4)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'B', ligne = 'BT', h = -60.96, année = 2024)

# lire les données du système C contrôle de 2024 (b2; 12" en dessous) ----------
C_b2_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(5)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b2', systeme = 'C', ligne = 'CC', h = -30.48, année = 2024)

# lire les données du système C traitement de 2024 (h3; 24" au dessus) ---------
C_h3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(6)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN, 
                                col_names = noms_col_SN) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'C', ligne = 'CT', h = 60.96, année = 2024)

# lire les données du système E contrôle de 2024 (h2; 12" au dessus) -----------
E_h2_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(7)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN[-c(13:18, 22)], 
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h2', systeme = 'E', ligne = 'EC', h = 30.48, année = 2024)

# lire les données du système E traitement 1 de 2024 (h3; 24" au dessus) -------
E_h3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(8)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN[-c(13:18, 22)], 
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'h3', systeme = 'E', ligne = 'E1', h = 60.96, année = 2024)

# lire les données du système E traitement 2 de 2024 (b2; 12" en dessous) ------
E_b2_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(9)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN[-c(13:18, 22)], 
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b2', systeme = 'E', ligne = 'E2', h = -30.48, année = 2024)

# lire les données du système E traitement 3 de 2024 (b3; 24" en dessous) ------
E_b3_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = '(10)', skip = 4, na = '-', n_max = 46,
                                col_types = types_col_SN[-c(13:18, 22)], 
                                col_names = noms_col_SN[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(t = 'b3', systeme = 'E', ligne = 'E3', h = -60.96, année = 2024)

# combiner tous les données pertinentes ----------------------------------------
d <- rbind(A_h3_2023, A_b3_2023, B_b1_2023, B_b3_2023, C_h3_2023, C_h1_2023, 
           E_h3_2023, E_h1_2023, E_b1_2023, E_b3_2023, A_b3_2024, A_h2_2024,
           B_h3_2024, B_b3_2024, C_b2_2024, C_h3_2024, E_h2_2024, E_h3_2024,
           E_b2_2024, E_b3_2024) %>% 
  select(systeme, ligne, t, h, année, date, datetime, rendement, brix) %>% 
  filter(rendement > 0)

# supprime les lignes sans date, car elles sont les moyennes -------------------
d <- d %>% filter(!is.na(date)) 

# ajouter le site et convertir les caractères en facteurs ----------------------
d <- d %>% mutate(t = factor(t, ordered = TRUE, 
                             levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
                  systeme = factor(systeme, levels = c('A', 'B', 'C', 'E')),
                  ligne = factor(ligne, 
                                 levels = c("AC", "AT", "BC", "BT", "CC", "CT", 
                                            "EC", "E1", "E2", "E3")),
                  année = factor(année, levels = c("2023", "2024"))) %>% 
  add_column(site = 'SN') %>% 
  relocate(année, site, systeme, ligne, t, h, date, datetime, rendement, brix)

# initialiser les noms des colonnes --------------------------------------------
noms_col_CE <- c('date', 'comp1.1', 'récolte1.1', 'rendement1.1', 'rien1',
                         'comp1.2', 'récolte1.2', 'rendement1.2', 'var1', 
                         'comp2.1', 'récolte2.1', 'rendement2.1', 'rien2',
                         'comp2.2', 'récolte2.2', 'rendement2.2', 'var2',
                         'comp3.1', 'récolte3.1', 'rendement3.1', 'rien3',
                         'comp3.2', 'récolte3.2', 'rendement3.2', 'var3')

# lire les données de 2022 du CE (au-dessus de latéral) ------------------------
F1_h1_2022 <- readxl::read_excel(path = nom_fichier_CE_2022,
                                 sheet = 'Prise de données', range = 'B14:Z52',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement1.1, rendement1.2) %>% 
  filter(!is.na(rendement1.1) | !is.na(rendement1.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")), # TR - Midi ? À vérifier avec Andréanne
         date = as_date(date),
         t = factor("h1.5"),
         h = 22.86, # Entre 6 et 12", donc j'utilise la moyenne de 9" ou 22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor("2022"),
         systeme = factor("F"),
         ligne = factor("F1"),
         site = factor('CE'),
         rendement = rowMeans(select(., rendement1.1, rendement1.2))) %>%
  select(-c(rendement1.1, rendement1.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, ligne, t, h, date, datetime, rendement, brix)

# lire les données de 2023 du CE (en dessus de latéral) ------------------------
F1_b1_2023 <- readxl::read_excel(path = nom_fichier_CE_2023,
                                 sheet = 'Prise de données', range = 'B14:Z44',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement1.1, rendement1.2) %>% 
  filter(!is.na(rendement1.1) | !is.na(rendement1.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")),
         date = as_date(date),
         t = factor("b1.5"),
         h = -22.86, # Entre 6 et 12", donc j'utilise la moyenne de -9" ou -22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor('2023'),
         systeme = factor('F'),
         ligne = factor('F1'),
         site = factor('CE'),
         rendement = rowMeans(select(., rendement1.1, rendement1.2))) %>%
  select(-c(rendement1.1, rendement1.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, ligne, t, h, date, datetime, rendement, brix)

# lire les données de 2022 du CE (au dessous de latéral) -----------------------
F2_h1_2022 <- readxl::read_excel(path = nom_fichier_CE_2022,
                                 sheet = 'Prise de données', range = 'B14:Z52',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement2.1, rendement2.2) %>% 
  filter(!is.na(rendement2.1) | !is.na(rendement2.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")), 
         date = as_date(date),
         t = factor("h1.5"),
         h = 22.86, # Entre 6 et 12", donc j'utilise la moyenne de 9" ou 22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor("2022"),
         systeme = factor("F"),
         ligne = factor("F2"),
         site = factor("CE"),
         rendement = rowMeans(select(., rendement2.1, rendement2.2))) %>%
  select(-c(rendement2.1, rendement2.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, t, h, date, datetime, rendement, brix)

# lire les données de 2023 du CE (au dessous de latéral) -----------------------
F2_h1_2023 <- readxl::read_excel(path = nom_fichier_CE_2023,
                                 sheet = 'Prise de données', range = 'B14:Z44',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement2.1, rendement2.2) %>% 
  filter(!is.na(rendement2.1) | !is.na(rendement2.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")), 
         date = as_date(date),
         t = factor("h1.5"),
         h = 22.86,  # Entre 6 et 12", donc j'utilise la moyenne de 9" ou 22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor("2023"),
         systeme = factor('F'),
         ligne = factor("F2"),
         site = factor("CE"),
         rendement = rowMeans(select(., rendement2.1, rendement2.2))) %>%
  select(-c(rendement2.1, rendement2.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, ligne, t, h, date, datetime, rendement, brix)

# lire les données de 2022 du CE (en-dessous de latéral) -----------------------
F3_b1_2022 <- readxl::read_excel(path = nom_fichier_CE_2022,
                                 sheet = 'Prise de données', range = 'B14:Z52',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement3.1, rendement3.2) %>% 
  filter(!is.na(rendement3.1) | !is.na(rendement3.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")), 
         date = as_date(date),
         t = factor("b1.5"),
         h = -22.86, # Entre 6 et 12", donc j'utilise la moyenne de 9" ou -22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor("2022"),
         systeme = factor("F"),
         ligne = factor("F3"),
         site = factor("CE"),
         rendement = rowMeans(select(., rendement3.1, rendement3.2))) %>%
  select(-c(rendement3.1, rendement3.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, t, h, date, datetime, rendement, brix)

# lire les données de 2023 du CE (en-dessous de latéral) -----------------------
F3_b1_2023 <- readxl::read_excel(path = nom_fichier_CE_2023,
                                 sheet = 'Prise de données', range = 'B14:Z44',
                                 col_types = c('date', rep('numeric', 24)),
                                 col_names = noms_col_CE) %>% 
  select(date, rendement3.1, rendement3.2) %>% 
  filter(!is.na(rendement3.1) | !is.na(rendement3.2)) %>% 
  mutate(datetime = ymd_hms(paste(date, " 12:00:00")), 
         date = as_date(date),
         t = factor("b1.5"),
         h = -22.86,  # Entre 6 et 12", donc j'utilise la moyenne de 9" ou -22.86 cm
         # TR - Je devrais inclure une erreur dans l'analyse
         année = factor("2023"),
         systeme = factor('F'),
         ligne = factor("F3"),
         site = factor("CE"),
         rendement = rowMeans(select(., rendement3.1, rendement3.2))) %>%
  select(-c(rendement3.1, rendement3.2)) %>% add_column(brix = NA) %>% 
  relocate(année, site, systeme, ligne, t, h, date, datetime, rendement, brix)

# combiner les données de St-Norbert (SN) et du Club d'encadrement téchnique en 
# acériculture de l'est (CE) ---------------------------------------------------
d <- rbind(d, F1_h1_2022, F1_b1_2023, F2_h1_2022, F2_h1_2023, F3_b1_2022, F3_b1_2023)

# re-arranger l'ordre du facteur traitement ------------------------------------
d <- d %>% mutate(t = ordered(t, levels = c("h3", "h2", "h1.5", "h1", "b1", 
                                            "b1.5", "b2", "b3")))

# re-définir les noms des colonnes ---------------------------------------------
noms_col_SN_2023 <- c("date", "temps", "responsable", "periode", "A_AC_h3", 
                      "C_CC_h3", "C_CT_h1", "B_BC_b1", "A_AT_b3", "B_BT_b3")
noms_col_SN_2024 <- c("date", "temps", "responsable", "periode", "B_BC_h3", 
                      "C_CT_h3", "A_AT_h2", "C_CC_b2", "A_AC_b3", "B_BT_b3")

# noms des fichier -------------------------------------------------------------
nom_fichier_SN_2023 <- "../données/Copie de 4010408_RécolteSN_V2JH.xlsx"
nom_fichier_SN_2024 <- "../données/4010408_RécolteSN_2024.xlsx"

# lire les données pour le brix ------------------------------------------------
brix_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = 'CompilationDonnées', range = 'A4:J14',
                                col_types = c('date', 'text', 'text', 'text', 
                                              rep('numeric', 6)),
                                col_names = noms_col_SN_2023) %>% 
  pivot_longer(cols = c("A_AC_h3", "C_CC_h3", "C_CT_h1", "B_BC_b1", "A_AT_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "brix") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2023")) %>%
  select(-c(responsable, periode, temps, heure, minute))
brix_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = 'Compilation_Données_2024', range = 'A4:J15',
                                col_types = c('date', 'text', 'text', 'text', 
                                              rep('numeric', 6)),
                                col_names = noms_col_SN_2024) %>% 
  pivot_longer(cols = c("B_BC_h3", "C_CT_h3", "A_AT_h2", "C_CC_b2", "A_AC_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "brix") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 4, 5)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2024")) %>%
  select(-c(responsable, periode, temps, heure, minute))
brix <- rbind(brix_2023, brix_2024) %>% 
  mutate(t = factor(t, levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
         h = case_when(
           t == "h3" ~ 60.96,
           t == "h2" ~ 30.48,
           t == "h1" ~ 20.32,
           t == "b1" ~ -20.32,
           t == "b2" ~ -30.48,
           t == "b3" ~ -60.96,
         )) %>% 
  mutate(systeme = factor(systeme, levels = c("A", "B", "C")),
         ligne = factor(ligne, levels = c("AC", "AT", "BC", "BT", "CC", "CT"))) %>%
  relocate(année, date, datetime, systeme, ligne, t, h, brix)

# lire les données pour l'atp --------------------------------------------------
atp_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                                sheet = 'CompilationDonnées', range = 'A25:J35',
                                col_types = c('date', 'text', 'text', 'text', 
                                              rep('numeric', 6)),
                                col_names = noms_col_SN_2023) %>% 
  pivot_longer(cols = c("A_AC_h3", "C_CC_h3", "C_CT_h1", "B_BC_b1", "A_AT_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "atp") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2023")) %>%
  select(-c(responsable, periode, temps, heure, minute))
atp_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                                sheet = 'Compilation_Données_2024', range = 'A27:J38',
                                col_types = c('date', 'text', 'text', 'text', 
                                              rep('numeric', 6)),
                                col_names = noms_col_SN_2024) %>% 
  pivot_longer(cols = c("B_BC_h3", "C_CT_h3", "A_AT_h2", "C_CC_b2", "A_AC_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "atp") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 4, 5)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2024")) %>%
  select(-c(responsable, periode, temps, heure, minute))
atp <- rbind(atp_2023, atp_2024) %>% 
  mutate(t = factor(t, levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
         h = case_when(
           t == "h3" ~ 60.96,
           t == "h2" ~ 30.48,
           t == "h1" ~ 20.32,
           t == "b1" ~ -20.32,
           t == "b2" ~ -30.48,
           t == "b3" ~ -60.96,
         )) %>% 
  mutate(systeme = factor(systeme, levels = c("A", "B", "C")),
         ligne = factor(ligne, 
                        levels = c("AC", "AT", "BC", "BT", "CC", "CT"))) %>%
  relocate(année, date, datetime, systeme, ligne, t, h, atp)

# lire les données du ph -------------------------------------------------------
ph_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                               sheet = 'CompilationDonnées', range = 'A47:J57',
                               col_types = c('date', 'text', 'text', 'text', 
                                             rep('numeric', 6)),
                               col_names = noms_col_SN_2023) %>% 
  pivot_longer(cols = c("A_AC_h3", "C_CC_h3", "C_CT_h1", "B_BC_b1", "A_AT_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "ph") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2023")) %>%
  select(-c(responsable, periode, temps, heure, minute))
ph_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                               sheet = 'Compilation_Données_2024', range = 'A50:J61',
                               col_types = c('date', 'text', 'text', 'text', 
                                             rep('numeric', 6)),
                               col_names = noms_col_SN_2024) %>% 
  pivot_longer(cols = c("B_BC_h3", "C_CT_h3", "A_AT_h2", "C_CC_b2", "A_AC_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "ph") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 4, 5)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2024")) %>%
  select(-c(responsable, periode, temps, heure, minute))
ph <- rbind(ph_2023, ph_2024) %>% 
  mutate(t = factor(t, levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
         h = case_when(
           t == "h3" ~ 60.96,
           t == "h2" ~ 30.48,
           t == "h1" ~ 20.32,
           t == "b1" ~ -20.32,
           t == "b2" ~ -30.48,
           t == "b3" ~ -60.96,
         )) %>% 
  mutate(systeme = factor(systeme, levels = c("A", "B", "C")),
         ligne = factor(ligne, levels = c("AC", "AT", "BC", "BT", "CC", "CT"))) %>%
  relocate(année, date, datetime, systeme, ligne, t, h, ph)

# lire les données de la concentration en sacchrose ----------------------------
sc_2023 <- readxl::read_excel(path = nom_fichier_SN_2023,
                               sheet = 'CompilationDonnées', range = 'A92:J102',
                               col_types = c('date', 'text', 'text', 'text', 
                                             rep('numeric', 6)),
                               col_names = noms_col_SN_2023) %>% 
  pivot_longer(cols = c("A_AC_h3", "C_CC_h3", "C_CT_h1", "B_BC_b1", "A_AT_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "sc") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2023")) %>%
  select(-c(responsable, periode, temps, heure, minute))
sc_2024 <- readxl::read_excel(path = nom_fichier_SN_2024,
                               sheet = 'Compilation_Données_2024', range = 'A96:J107',
                               col_types = c('date', 'text', 'text', 'text', 
                                             rep('numeric', 6)),
                               col_names = noms_col_SN_2024) %>% 
  pivot_longer(cols = c("B_BC_h3", "C_CT_h3", "A_AT_h2", "C_CC_b2", "A_AC_b3", 
                        "B_BT_b3"), 
               names_to = c("systeme", "ligne", "t"), names_sep = '_', 
               values_to = "sc") %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(temps, 1, 2)),
         minute = as.numeric(substr(temps, 4, 5)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " ")),
         année = factor("2024")) %>%
  select(-c(responsable, periode, temps, heure, minute))
sc <- rbind(sc_2023, sc_2024) %>% 
  mutate(t = factor(t, levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
         h = case_when(
           t == "h3" ~ 60.96,
           t == "h2" ~ 30.48,
           t == "h1" ~ 20.32,
           t == "b1" ~ -20.32,
           t == "b2" ~ -30.48,
           t == "b3" ~ -60.96,
         )) %>% 
  mutate(systeme = factor(systeme, levels = c("A", "B", "C")),
         ligne = factor(ligne, 
                        levels = c("AC", "AT", "BC", "BT", "CC", "CT"))) %>%
  relocate(année, date, datetime, systeme, ligne, t, h, sc)

# unir les données -------------------------------------------------------------
tmp1 <- full_join(brix, atp, by = c("année", "date", "datetime", "systeme", 
                                    "ligne", "t", "h"))
tmp2 <- full_join(ph, sc, by = c("année", "date", "datetime", "systeme", 
                                 "ligne", "t", "h"))
d1 <- full_join(tmp1, tmp2, by = c("année", "date", "datetime", "systeme", 
                                   "ligne", "t", "h"))

# nettoyer l'espace de travail -------------------------------------------------
rm(A_b3_2023, A_b3_2024, A_h2_2024, A_h3_2023, atp, atp_2023, atp_2024, 
   B_b1_2023, B_b3_2023, B_b3_2024, B_h3_2024, brix, brix_2023, brix_2024, 
   C_b2_2024, C_h1_2023, C_h3_2023, C_h3_2024, E_b1_2023, E_b2_2024, E_b3_2023, 
   E_b3_2024, E_h1_2023, E_h2_2024, E_h3_2023, E_h3_2024, F1_h1_2022, 
   F1_b1_2023, F2_h1_2022, F2_h1_2023, F3_b1_2022, F3_b1_2023, ph, ph_2023, 
   ph_2024, sc, sc_2023, sc_2024, tmp1, tmp2, nom_fichier_CE_2022, 
   nom_fichier_CE_2023, nom_fichier_SN_2023, nom_fichier_SN_2024, 
   noms_col_CE, noms_col_SN, noms_col_SN_2023, noms_col_SN_2024, ligne, systeme, 
   traitement, types_col_SN)
#===============================================================================