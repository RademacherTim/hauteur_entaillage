# ============================================================================ #
# Analyse des données de vide à l'entaille
#
# Je pourrais eventuellement ajouter les données à l'extracteur, aux têtes de 
# ligne et à la pompe. Étrangement les têtes de ligne semble avoir ....
# ---------------------------------------------------------------------------- #

# dépendances ------------------------------------------------------------------
if(!existsFunction('%>%')) library('tidyverse')
if(!existsFunction('brms')) library('brms')
if(!existsFunction('pp_check')) library('rstanarm')

# lire les données de vide -----------------------------------------------------
source("03_données_de_vide.R")

# faire une graphique des données du vide à l'entaille -------------------------
par(mfrow = c(1, 1))
par(mar = c (5, 5, 1, 1))
plot(x = v_h$heure[v_h$ligne == "A1" & v_h$endroit == "début" & v_h$t == "h2"],
     y = v_h$vide[v_h$ligne == "A1" & v_h$endroit == "début" & v_h$t == "h2"], typ = "l",
     xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
     ylim = c(-29, -26), col = "#008837")
lines(v_h$heure[v_h$ligne == "A1" & v_h$endroit == "fin" & v_h$t == "h2"],
      y = v_h$vide[v_h$ligne == "A1" & v_h$endroit == "fin" & v_h$t == "h2"],
      col = "#008837", lty = 2)
lines(v_h$heure[v_h$ligne == "A1" & v_h$endroit == "début" & v_h$t == "b2"],
      y = v_h$vide[v_h$ligne == "A1" & v_h$endroit == "début" & v_h$t == "b2"],
      col = "#7b3294")
lines(v_h$heure[v_h$ligne == "A1" & v_h$endroit == "fin" & v_h$t == "b2"],
      y = v_h$vide[v_h$ligne == "A1" & v_h$endroit == "fin" & v_h$t == "b2"],
      col = "#7b3294", lty = 2)
lines(v_h$heure[v_h$ligne == "B1" & v_h$endroit == "début" & v_h$t == "b1"],
      y = v_h$vide[v_h$ligne == "B1" & v_h$endroit == "début" & v_h$t == "b1"],
      col = "#c2a5cf")
lines(v_h$heure[v_h$ligne == "B1" & v_h$endroit == "fin" & v_h$t == "b1"],
      y = v_h$vide[v_h$ligne == "B1" & v_h$endroit == "fin" & v_h$t == "b1"],
      col = "#c2a5cf", lty = 2)
lines(v_h$heure[v_h$ligne == "C1" & v_h$endroit == "début" & v_h$t == "h1"],
      y = v_h$vide[v_h$ligne == "C1" & v_h$endroit == "début" & v_h$t == "h1"],
      col = "#a6dba0")
lines(v_h$heure[v_h$ligne == "C1" & v_h$endroit == "fin" & v_h$t == "h1"],
      y = v_h$vide[v_h$ligne == "C1" & v_h$endroit == "fin" & v_h$t == "h1"],
      col = "#a6dba0", lty = 2)
lines(v_h$heure[v_h$ligne == "Ectr" & v_h$endroit == "fin" & v_h$t == "h2"],
      y = v_h$vide[v_h$ligne == "Ectr" & v_h$endroit == "fin" & v_h$t == "h2"],
      col = "#008837", lty = 2)
lines(v_h$heure[v_h$ligne == "Ettr1" & v_h$endroit == "fin" & v_h$t == "h1"],
      y = v_h$vide[v_h$ligne == "Ettr1" & v_h$endroit == "fin" & v_h$t == "h1"],
      col = "#a6dba0", lty = 2)
lines(v_h$heure[v_h$ligne == "Ettr2" & v_h$endroit == "fin" & v_h$t == "b1"],
      y = v_h$vide[v_h$ligne == "Ettr2" & v_h$endroit == "fin" & v_h$t == "b1"],
      col = "#c2a5cf", lty = 2)
lines(v_h$heure[v_h$ligne == "Ettr3" & v_h$endroit == "fin" & v_h$t == "b2"],
      y = v_h$vide[v_h$ligne == "Ettr3" & v_h$endroit == "fin" & v_h$t == "b2"],
      col = "#7b3294", lty = 2)

# change le traitement pour que ce soit un facteur ----
v_h %>% mutate(ligne   = factor(ligne),
               t       = factor(t),
               endroit = factor(endroit),
               heure   = factor(heure))

# TR - Quelle est la différence entre vide et vide_ligne dans les données???
# relation entre l'hauteur de l'entaille et le niveau de vide ------------------
mod_v <- brms::brm(brms::bf(vide ~ 
                              (1 | t) +             # traitement
                              (1 | endroit) + # début et fin de ligne
                              (1 | heure)),# +   # différence par heure
                              #(1 | ligne)), # différence entre systèmes
                      data = v_h,
                      family = gaussian(), 
                      prior = c(set_prior('normal(-27, 4)', class = 'Intercept'),
                                set_prior('exponential(1)', class = 'sigma')),
                                #set_prior('normal(0, 2)', class = 'b')),
                      cores = 4, chains = 4,
                      control = list(max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_v)
pp_check(mod_v, ndraws = 100)
pp_check(mod_v, type = 'error_hist',  ndraws = 10)
pp_check(mod_v, type = 'scatter_avg', ndraws = 100)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_v)
ranef(mod_v)$ligne [, , 'Intercept']
ranef(mod_v)$t [, , 'Intercept']
ranef(mod_v)$heure [, , 'Intercept']
ranef(mod_v)$endroit [, , 'Intercept']

# ============================================================================ #
# Ce qui suit traite les données des têtes de ligne, des extracteurs et des 
# pompes -----------------------------------------------------------------------

# faire le tri dans les données de vide ----------------------------------------
d_vide <- d_horaire %>% 
  # exclure les données quand les lignes étaient gelées
  filter(temp > -2.0) %>%
  group_by(système, t, heure) %>% 
  summarise(vide = mean(vide, na.rm = TRUE), .groups = "drop") %>% 
  mutate(vide = ifelse(vide >= 26.0, vide, NA))
  
# faire une graphique des données horaires -------------------------------------
par(mar = c (5, 5, 1, 1))
plot(x = d_vide$heure[d_vide$t == "h2" & d_vide$système == "E"], 
     y = -d_vide$vide[d_vide$t == "h2" & d_vide$système == "E"], 
     typ = "l", xlab = "Date", ylab = "Sous-vide (mm Hg)", 
     xlim = c(as_datetime("2023-03-21"), as_datetime("2023-04-16")), 
     ylim = c(-28.2, -26), 
     axes = FALSE, col = "white", lwd = 2)
axis(side = 1, at = c(as_datetime("2023-03-20"), as_datetime("2023-03-27"), 
                      as_datetime("2023-04-03"), as_datetime("2023-04-10"), 
                      as_datetime("2023-04-17")),
     labels = c("20 mar", "27 mar", "3 avr", "10 avr", "17 avr"))
axis(side = 2, las = 1)
#for (s in c("A", "B", "C", "E")) {
for (s in c("E")) {

  if (s == "A") {
    ts <- c("h2", "b2")
  } else if (s == "B") {
    ts <- c("b1", "b2")
  } else if (s == "C") {
    ts <- c("h2", "h1")
  } else if (s == "E") {
    ts <- c("h2", "h1", "b1", "b2")
  }
  for (t in ts){
    lines(x = d_vide$heure[d_vide$t == t & d_vide$système == s], 
          y = -d_vide$vide[d_vide$t == t & d_vide$système == s], 
          col = ifelse(t == "h2", "#008837", 
                       ifelse(t == "h1", "#a6dba0", 
                              ifelse(t == "b1", "#c2a5cf", "#7b3294"))),
          lwd = 2)  
  }
}

# faire le tri dans les données de vide ----------------------------------------
d_vide <- d_horaire %>% 
  # exclure les données avant et après la récolte (2023-03-21 à 2023-04-16)
  filter(date > as_date("2023-03-21") & date < as_date("2023-04-16")) %>%
  # exclure les données qui n'appartiennent à aucune traitement
  filter(système != "PompesVide") %>%
  # exclure les données quand les lignes étaient gelées
  filter(temp > -2.0) %>%
  group_by(t, heure) %>% 
  summarise(vide = mean(vide, na.rm = TRUE), .groups = "drop") %>% 
  mutate(vide = ifelse(vide >= 26.0, vide, NA))

# faire une graphique des données horaires par traitement ----------------------
par(mar = c (5, 5, 1, 1))
plot(x = d_vide$heure[d_vide$t == "h2"], 
     y = -d_vide$vide[d_vide$t == "h2"], 
     typ = "l", xlab = "Date", ylab = "Vide (\" Hg)", 
     #xlim = c(as_datetime("2023-03-21"), as_datetime("2023-04-16")), 
     xlim = c(as_datetime("2023-03-26"), as_datetime("2023-03-30")), 
     ylim = c(-28.2, -26), 
     axes = FALSE, col = "white", lwd = 2)
axis(side = 1, at = c(as_datetime("2023-03-20"), as_datetime("2023-03-27"), 
                      as_datetime("2023-04-03"), as_datetime("2023-04-10"), 
                      as_datetime("2023-04-17")),
     labels = c("20 mar", "27 mar", "3 avr", "10 avr", "17 avr"))
axis(side = 2, las = 1)
for (t in c("h2", "h1", "b1", "b2")){
  lines(x = d_vide$heure[d_vide$t == t], 
        y = -d_vide$vide[d_vide$t == t], 
        col = ifelse(t == "h2", "#008837", 
                     ifelse(t == "h1", "#a6dba0", 
                            ifelse(t == "b1", "#c2a5cf", "#7b3294"))),
        lwd = 2)  
}

# analyse des données en fonction du traitement --------------------------------
d_vide <- d_horaire %>% 
  # exclure les données avant et après la récolte (2023-03-21 à 2023-04-16)
  filter(date > as_date("2023-03-21") & date < as_date("2023-04-16")) %>%
  # exclure les données qui n'appartiennent à aucune traitement
  filter(système != "PompesVide") %>%
  # exclure les données quand les lignes étaient gelées
  filter(temp > -2.0) %>% 
  # exclure les données qui sont trop haut
  mutate(vide = ifelse(vide >= 26.0, vide, NA)) %>%
  # standardiser l'hauteur relative de l'entaille (moyenne est déjà zéro)
  mutate(h = h / sd(c(60.96, -60.96, 10.16, -10.16))) %>%
  select(heure, système, h, vide) %>%
  filter(!is.na(vide)) %>%
  # standardiser le vide
  mutate(v = (vide - mean(vide, na.rm = TRUE)) / sd (vide, na.rm = TRUE)) %>%
  # convertir l'heure et le système en facteur
  mutate(heure = factor(heure),
         système = factor(système))

# TR - Quelle est la différence entre vide et vide_ligne dans les données???
# relation entre l'hauteur de l'entaille et le niveau de vide ------------------
mod_vide <- brms::brm(brms::bf(vide ~ 
                                h +             # hauteur de l'entaille
                                (1 | heure) +   # différence par heure
                                (1 | système)), # différence entre systèmes
                     data = d_vide,
                     family = gaussian(), 
                     prior = c(set_prior('normal(0, 2)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(max_treedepth = 11),
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vide)
pp_check(mod_vide, ndraws = 100)
pp_check(mod_vide, type = 'error_hist',  ndraws = 10)
pp_check(mod_vide, type = 'scatter_avg', ndraws = 100)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vide)
ranef(mod_vide)$système [, , 'Intercept']
ranef(mod_vide)$heure [, , 'Intercept']

# Si on n'utilise pas l'heure le vide est très similaires entres les systèmes, 
# mais le vide est clairement plus grand, le plus qu'on est haut (beta = -0.24 [-0.25; -0.24]).  
# Pourtant, le modèle n'est pas bien convergé.

# Si on n'utilise pas le système le modèle converge mieux et l'effet de l'
# hauteur relative de l'entaille est similaire (beta = -0.20 [-0.20; -0.19]). 