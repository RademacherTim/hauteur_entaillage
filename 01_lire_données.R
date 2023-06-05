# charger les dépendances ------------------------------------------------------
if(!existsFunction('read_excel')) library('readxl')
if(!existsFunction('%>%')) library('tidyverse')

# clé --------------------------------------------------------------------------
# il avait quatre hauteurs différentes dans les traitements :
#     24" au dessus  du latéral  = _h2        # Contrôle A, C et E 
#      4" au dessus  du latéral  = _h1        # Traitement C et E1 
#      4" en dessous du latéral  = _b1        # Contrôle B et Traitement E2
#     24" en dessous du latéral  = _b2        # Traitement A, B, E3


# lire les données des systèmes ------------------------------------------------
systeme <- c('A','A','B','B','C','C','E','E','E','E')
traitement <- c('C','T','C','T','C','T','C','T','T','T')

# initialiser le nom du fichier ------------------------------------------------
file_name <- '../données/Compilation donnée érablière - 2023.xlsm'

# extraire les metadonnées -----------------------------------------------------
info <- readxl::read_excel(path = file_name,
                           sheet = 'Paramètres', range = 'B19:K19',
                           col_types = c(rep('numeric', 10)),
                           col_names = FALSE) %>% 
  pivot_longer(cols = 1:10, values_to = 'n_arbres') %>%
  add_column(systeme, traitement, .before = 1) %>% 
  select(-name) %>% 
  add_column(h = c('h2', 'b2', 'b1', 'b2', 'h2', 'h1', 'h2', 'h1', 'b1', 'b2'),
             colour = c('#008837','#7b3294','#c2a5cf','#7b3294','#008837',
                        '#a6dba0','#008837','#a6dba0','#c2a5cf','#7b3294'),
             sym = c(21, 21, 22, 22, 23, 23, 24, 24, 24, 24))

# initialise column names and column types -------------------------------------
column_names <- c(
  'date','time', 'compteur_1', 'coulee_m3_1', 'coulee_l_1', 'coulee_cum_1',
  'rendement_1', 'compteur_2', 'coulee_m3_2', 'coulee_l_2', 'coulee_cum_2',
  'rendement_2', 'compteur_3', 'lecture', 'coulee_m3_3', 'coulee_l_3',
  'coulee_cum_3', 'rendement_3', 'brix', 'commentaires', 'diff_compteur',
  'diff_compteur_1', 'debit', 'debit_cum', 'rendement', 'rendement_cum')
column_types <- c('date', 'text', rep('numeric', 17), 'text', rep('numeric', 6))

# lire les données du système A contrôle (24" au dessus) -----------------------
A_h2 <- readxl::read_excel(path = file_name,
                           sheet = '(1)', skip = 4, na = '-',
                           col_types = column_types, 
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'h2', systeme = 'A')

# lire les données du système A traitement (24" en dessous) --------------------
A_b2 <- readxl::read_excel(path = file_name,
                           sheet = '(2)', skip = 4, na = '-',
                           col_types = column_types,
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'b2', systeme = 'A')

# lire les données du système B contrôle (4" en dessous) -----------------------
B_b1 <- readxl::read_excel(path = file_name,
                           sheet = '(3)', skip = 4, na = '-',
                           col_types = column_types,
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'b1', systeme = 'B')

# lire les données du système B traitement (24" en dessous) --------------------
B_b2 <- readxl::read_excel(path = file_name,
                           sheet = '(4)', skip = 4, na = '-',
                           col_types = column_types,
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'b2', systeme = 'B')

# lire les données du système C contrôle (24" au dessus) -----------------------
C_h2 <- readxl::read_excel(path = file_name,
                           sheet = '(5)', skip = 4, na = '-',
                           col_types = column_types,
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'h2', systeme = 'C')

# lire les données du système C traitement (4" au dessus) ----------------------
C_h1 <- readxl::read_excel(path = file_name,
                           sheet = '(6)', skip = 4, na = '-',
                           col_types = column_types,
                           col_names = column_names) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'h1', systeme = 'C')

# lire les données du système E contrôle (24" au dessus) -----------------------
E_h2 <- readxl::read_excel(path = file_name,
                           sheet = '(7)', skip = 4, na = '-',
                           col_types = column_types[-c(13:18, 22)],
                           col_names = column_names[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'h2', systeme = 'E')

# lire les données du système E traitement 1 (4" au dessus) --------------------
E_h1 <- readxl::read_excel(path = file_name,
                           sheet = '(8)', skip = 4, na = '-',
                           col_types = column_types[-c(13:18, 22)],
                           col_names = column_names[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'h1', systeme = 'E')

# lire les données du système E traitement 2 (4" en dessous) -------------------
E_b1 <- readxl::read_excel(path = file_name,
                           sheet = '(9)', skip = 4, na = '-',
                           col_types = column_types[-c(13:18, 22)],
                           col_names = column_names[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'b1', systeme = 'E')

# lire les données du système E traitement 3 (24" en dessous) ------------------
E_b2 <- readxl::read_excel(path = file_name,
                           sheet = '(10)', skip = 4, na = '-',
                           col_types = column_types[-c(13:18, 22)],
                           col_names = column_names[-c(13:18, 22)]) %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(date, datetime, debit, debit_cum, rendement, rendement_cum, brix) %>%
  add_column(h = 'b2', systeme = 'E')

# combiner tous les données pertinentes ----------------------------------------
d <- rbind(A_h2, A_b2, B_b1, B_b2, C_h2, C_h1, E_h2, E_h1, E_b1, E_b2) %>% 
  select(systeme, h, date, datetime, rendement, brix) %>% filter(rendement > 0)

# supprime les lignes sans date, car elles sont les moyennes -------------------
d <- d %>% filter(!is.na(date))

# re-définir les noms des colonnes ---------------------------------------------
column_names <- c('date', 'time', 'responsable', 'periode', 'A_h2', 'C_h2', 
                  'C_h1', 'B_b1', 'A_b2', 'B_b2')

# lire les données pour atp ----------------------------------------------------
brix <- readxl::read_excel(path = '../données/Copie de 4010408_RécolteSN_V2JH.xlsx',
                           sheet = 'CompilationDonnées', range = 'A4:J14',
                           col_types = c('date', 'text', 'text', 'text', rep('numeric', 6)),
                           col_names = column_names) %>% 
  pivot_longer(cols = c('A_h2', 'C_h2', 'C_h1', 'B_b1', 'A_b2', 'B_b2'), 
               names_to = c('systeme', 'h'), names_sep = '_') %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>%
  select(-c(responsable, periode, time, heure, minute))
atp <- readxl::read_excel(path = '../données/Copie de 4010408_RécolteSN_V2JH.xlsx',
                          sheet = 'CompilationDonnées', range = 'A25:J35',
                          col_types = c('date', 'text', 'text', 'text', rep('numeric', 6)),
                          col_names = column_names) %>% 
  pivot_longer(cols = c('A_h2', 'C_h2', 'C_h1', 'B_b1', 'A_b2', 'B_b2'), 
               names_to = c('systeme', 'h'), names_sep = '_') %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>%
  select(-c(responsable, periode, time, heure, minute))
ph <-  readxl::read_excel(path = '../données/Copie de 4010408_RécolteSN_V2JH.xlsx',
                          sheet = 'CompilationDonnées', range = 'A47:J57',
                          col_types = c('date', 'text', 'text', 'text', rep('numeric', 6)),
                          col_names = column_names) %>% 
  pivot_longer(cols = c('A_h2', 'C_h2', 'C_h1', 'B_b1', 'A_b2', 'B_b2'), 
               names_to = c('systeme', 'h'), names_sep = '_') %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>% 
  select(-c(responsable, periode, time, heure, minute))
sc <-  readxl::read_excel(path = '../données/Copie de 4010408_RécolteSN_V2JH.xlsx',
                          sheet = 'CompilationDonnées', range = 'A92:J102',
                          col_types = c('date', 'text', 'text', 'text', rep('numeric', 6)),
                          col_names = column_names) %>% 
  pivot_longer(cols = c('A_h2', 'C_h2', 'C_h1', 'B_b1', 'A_b2', 'B_b2'), 
               names_to = c('systeme', 'h'), names_sep = '_') %>%
  mutate(date = as_date(date),
         heure = as.numeric(substr(time, 1, 2)),
         minute = as.numeric(substr(time, 6, 7)),
         datetime = ymd_hm(str_c(date, heure, minute, sep = " "))) %>%
  select(-c(responsable, periode, time, heure, minute)) 

# unir les données -------------------------------------------------------------
tmp1 <- full_join(brix, atp, by = c('date', 'datetime', 'systeme', 'h')) %>% 
  rename(brix = value.x, atp = value.y) %>%
  relocate(systeme, h, date, datetime, brix, atp)
tmp2 <- full_join(ph, sc, by = c('date', 'datetime', 'systeme', 'h')) %>% 
  rename(ph = value.x, sc = value.y) %>%
  relocate(systeme, h, date, datetime, ph, sc)
d1 <- full_join(tmp1, tmp2, by = c('systeme', 'h', 'date', 'datetime'))

# nettoyer l'espace de travail -------------------------------------------------
rm(A_b2, A_h2, atp, B_b1, B_b2, brix, C_h1, C_h2, column_names, column_types, 
   E_b1, E_b2, E_h1, E_h2, file_name, ph, sc, systeme, tmp1, tmp2, traitement)
