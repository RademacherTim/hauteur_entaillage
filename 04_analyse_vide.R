# ============================================================================ #
# Analyse des données de vide à l'entaille
#
# Je pourrais eventuellement ajouter les données à l'extracteur, aux têtes de 
# ligne et à la pompe. Étrangement les têtes de ligne semble avoir ....
# ---------------------------------------------------------------------------- #

# dépendances ------------------------------------------------------------------
if (!existsFunction("%>%")) library("tidyverse")
if (!existsFunction("brms")) library("brms")
if (!existsFunction("pp_check")) library("rstanarm")
if (!existsFunction("panel_border")) library("cowplot")

# lire les données de vide -----------------------------------------------------
if(!exists("v_h")) source("03_données_de_vide.R")

# Graphique des données du vide à l'entaille en 2023 ---------------------------
PLOT <- FALSE
if (PLOT) {
  par(mfrow = c(2, 2))
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2023 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "début" & 
    v_h$t == "h3"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#a6dba0")
  con <- v_h$année == 2023 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#a6dba0", lty = 2)
  con <- v_h$année == 2023 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "début" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf")
  con <- v_h$année == 2023 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "fin" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf", lty = 2)
  text(x = as_datetime("2023-04-14"), y = -29, labels = "Ligne A")
  
  # Système B 2023 ---------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2023 & 
    v_h$ligne == "B1" & 
    v_h$endroit == "début" & 
    v_h$t == "b1"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#40004b")
  con <- v_h$année == 2023 & 
    v_h$ligne == "B1" & 
    v_h$endroit == "fin" & 
    v_h$t == "b1"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#40004b", lty = 2)
  text(x = as_datetime("2023-04-14"), y = -29, labels = "Ligne B")
  legend(x = as_datetime("2023-03-27"), y = -27.7,
         box.lty = 0, legend = rep(" ", 6), 
         col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         lty = 1, title = "début")
  legend(x = as_datetime("2023-03-31"), y = -27.7,
         box.lty = 0, legend = c("+24\"",  "+12\"",  "  +4\"", "   -4\"", "-12\"", 
                                 " -24\""), 
         col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         lty = 2, title = "fin")
  
  # Système C 2023 ---------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2023 & 
    v_h$ligne == "C1" & 
    v_h$endroit == "début" & 
    v_h$t == "h1"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#a6dba0")
  con <- v_h$année == 2023 & 
    v_h$ligne == "C1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h1"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#a6dba0", lty = 2)
  text(x = as_datetime("2023-04-14"), y = -29, labels = "Ligne C")
  
  # Système E 2023 ---------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2023 & 
    v_h$ligne == "Ectr" & 
    v_h$endroit == "fin" & 
    v_h$t == "h3"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#a6dba0", lty = 2)
  con <- v_h$année == 2023 & 
    v_h$ligne == "Ettr1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h1"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#00441b", lty = 2)
  con <- v_h$année == 2023 & 
    v_h$ligne == "Ettr2" & 
    v_h$endroit == "fin" & 
    v_h$t == "b1"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#40004b", lty = 2)
  con <- v_h$année == 2023 & 
    v_h$ligne == "Ettr3" & 
    v_h$endroit == "fin" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf", lty = 2)
  text(x = as_datetime("2023-04-14"), y = -29, labels = "Ligne E")
  
  
  # Graphique des données du vide à l'entaille en 2024 ---------------------------
  par(mfrow = c(2, 2))
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2024 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "début" & 
    v_h$t == "h2"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#1b7837")
  con <- v_h$année == 2024 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h2"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#1b7837", lty = 2)
  con <- v_h$année == 2024 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "début" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf")
  con <- v_h$année == 2024 & 
    v_h$ligne == "A1" & 
    v_h$endroit == "fin" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf", lty = 2)
  text(x = as_datetime("2024-04-07"), y = -29, labels = "Ligne A")
  legend(x = as_datetime("2024-02-27"), y = -27.8,
         box.lty = 0, legend = rep(" ", 6), bg = "transparent",
         col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         lty = 1, title = "début")
  legend(x = as_datetime("2024-03-04"), y = -27.8, bg = "transparent",
         box.lty = 0, legend = c("+24\"",  "+12\"",  "  +4\"", "   -4\"", "-12\"", 
                                 " -24\""), 
         col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
         lty = 2, title = "fin")
  
  # Système B (2024) -------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2024 & 
    v_h$ligne == "B1" & 
    v_h$endroit == "début" & 
    v_h$t == "h3"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#a6dba0")
  con <- v_h$année == 2024 & 
    v_h$ligne == "B1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#a6dba0", lty = 2)
  text(x = as_datetime("2024-04-07"), y = -29, labels = "Ligne B")
  
  # Système C (2024) -------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2024 & 
    v_h$ligne == "C1" & 
    v_h$endroit == "début" & 
    v_h$t == "b2"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#762a83")
  con <- v_h$année == 2024 & 
    v_h$ligne == "C1" & 
    v_h$endroit == "fin" & 
    v_h$t == "b2"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#762a83", lty = 2)
  text(x = as_datetime("2024-04-07"), y = -29, labels = "Ligne C")
  
  
  # Système E (2024) -------------------------------------------------------------
  par(mar = c (5, 5, 1, 1))
  con <- v_h$année == 2024 & 
    v_h$ligne == "Ectr" & 
    v_h$endroit == "fin" & 
    v_h$t == "h2"
  plot(x = v_h$heure[con],
       y = v_h$vide[con], typ = "l",
       xlab = "Date et heure", ylab = "Sous-vide (\" Hg)", las = 1,
       ylim = c(-29, -26), col = "#1b7837", lty = 2)
  con <- v_h$année == 2024 & 
    v_h$ligne == "Ettr1" & 
    v_h$endroit == "fin" & 
    v_h$t == "h3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#a6dba0", lty = 2)
  con <- v_h$année == 2024 & 
    v_h$ligne == "Ettr2" & 
    v_h$endroit == "fin" & 
    v_h$t == "b2"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#762a83", lty = 2)
  con <- v_h$année == 2024 & 
    v_h$ligne == "Ettr3" & 
    v_h$endroit == "fin" & 
    v_h$t == "b3"
  lines(x = v_h$heure[con], y = v_h$vide[con], col = "#c2a5cf", lty = 2)
  text(x = as_datetime("2024-04-07"), y = -29, labels = "Ligne E")
}

# Change le traitement pour que ce soit un facteur -----------------------------
v_h <- v_h %>% 
  mutate(année   = factor(année),
         ligne   = factor(ligne),
         t       = factor(t, ordered = TRUE, 
                          levels = c("h3", "h2", "h1", "b1", "b2", "b3")),
         endroit = factor(endroit),
         heure   = factor(heure))

# relation entre l'hauteur de l'entaille et le niveau de vide ------------------
mod_v <- brms::brm(brms::bf(log(vide) ~ 
                               (1 | t) +       # traitement
                               (1 | heure) +   # différence par heure
                               (1 | ligne)),   # différence entre systèmes
                    data = v_h %>% select(ligne, t, heure, vide) %>% 
                      filter(!is.na(vide)),
                    family = gaussian(), 
                    prior = c(set_prior('normal(-27, 4)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.9, max_treedepth = 12),
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_v)
pp_check(mod_v, ndraws = 100)
pp_check(mod_v, type = 'error_hist',  ndraws = 10)
pp_check(mod_v, type = 'scatter_avg', ndraws = 100)
# Need to explore better distributions to model the vacuum as the real 
# distribution is much more skewed. Cannot use lognormal due to the negative 
# numbers.

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_v)
ranef(mod_v)$t [, , 'Intercept']
ranef(mod_v)$ligne [, , 'Intercept']
ranef(mod_v)$heure [, , 'Intercept']

# Extraire les distributions postérieures --------------------------------------
theme_set(theme_tidybayes() + panel_border())
# mod_v %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
#   mutate(t_mean = b_Intercept + r_t) %>%
#   ggplot(aes(y = t, x = t_mean)) + 
#   scale_x_continuous(name ="Sous-vide (\" Hg)") +
#   scale_y_discrete(name ="Hauteur relative au latéral", 
#                    labels=c("-24\"", "-12\"","-4\"","+4\"","+12\"","+24\"")) +
#   stat_halfeye()
mod_v %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Vacuum level (\" Hg)") +
  scale_y_discrete(name ="Relative spout height (inches)", 
                   labels=c("-24\"", "-12\"","-4\"","+4\"","+12\"","+24\"")) +
  stat_halfeye()

# relation entre l'hauteur de l'entaille et le niveau de vide en 2024 ----------
mod_v23 <- brms::brm(brms::bf(log(vide) ~ 
                                (1 | t) +       # traitement
                                (1 | heure) +   # différence par heure
                                (1 | ligne)),   # différence entre systèmes
                     data = v_h %>% select(ligne, t, heure, vide, année) %>% 
                       filter(!is.na(vide) & année == 2023) %>%
                       mutate(vide = -vide),
                     family = gaussian(), 
                     prior = c(set_prior('normal(-27, 4)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 12),
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_v23)
pp_check(mod_v23, ndraws = 100)
pp_check(mod_v23, type = 'error_hist',  ndraws = 10)
pp_check(mod_v23, type = 'scatter_avg', ndraws = 100)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_v23)
ranef(mod_v23)$t [, , 'Intercept']
ranef(mod_v23)$ligne [, , 'Intercept']
ranef(mod_v23)$ligne:endroit [, , 'Intercept']
ranef(mod_v23)$heure [, , 'Intercept']

# Extraire les distributions postérieures --------------------------------------
theme_set(theme_tidybayes() + panel_border())
# mod_v23 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
#   mutate(t_mean = b_Intercept + r_t) %>%
#   ggplot(aes(y = t, x = -exp(t_mean))) + 
#   scale_x_continuous(name ="Sous-vide (\" Hg)") +
#   scale_y_discrete(name ="Hauteur relative au latéral", 
#                    labels=c("-24\"", "-4\"","+4\"","+24\"")) +
#   stat_halfeye()
mod_v23 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = -exp(t_mean))) + 
  scale_x_continuous(name ="Vacuum level (\" Hg)") +
  scale_y_discrete(name ="Relative spout height", 
                   labels=c("-24\"", "-4\"","+4\"","+24\"")) +
  stat_halfeye()

# relation entre l'hauteur de l'entaille et le niveau de vide en 2024 ----------
mod_v24 <- brms::brm(brms::bf(log(vide) ~ 
                              (1 | t) +       # traitement
                              (1 | heure) +   # différence par heure
                              (1 | ligne)),   # différence entre systèmes
                   data = v_h %>% select(ligne, t, heure, vide, année) %>% 
                     filter(!is.na(vide) & année == 2024) %>%
                     mutate(vide = -vide),
                   family = gaussian(), 
                   prior = c(set_prior('normal(-27, 4)', class = 'Intercept'),
                             set_prior('exponential(1)', class = 'sigma')),
                   cores = 4, chains = 4,
                   control = list(adapt_delta = 0.9, max_treedepth = 12),
                   iter = 6000,
                   seed = 1353,
                   backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_v24)
pp_check(mod_v24, ndraws = 100)
pp_check(mod_v24, type = 'error_hist',  ndraws = 10)
pp_check(mod_v24, type = 'scatter_avg', ndraws = 100)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_v24)
ranef(mod_v24)$t [, , 'Intercept']
ranef(mod_v24)$ligne [, , 'Intercept']
ranef(mod_v24)$ligne:endroit [, , 'Intercept']
ranef(mod_v24)$heure [, , 'Intercept']

# Extraire les distributions postérieures --------------------------------------
theme_set(theme_tidybayes() + panel_border())
mod_v24 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = -exp(t_mean))) + 
  scale_x_continuous(name ="Sous-vide (\" Hg)") +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-24\"", "-12\"","+12\"","+24\"")) +
  stat_halfeye()

# relation entre l'hauteur de l'entaille et le niveau de vide ------------------
# Compared to mod_v we add the nested effect of 'endroit' here, which is 
# probably only complicating the parameter exploration. I could do a formal 
# model comparison, but I don't see the advantages of using this model.
mod_v2 <- brms::brm(brms::bf(vide ~ 
                              (1 | t) +       # traitement
                              (1 | heure) +   # différence par heure
                              (1 | ligne / endroit)),   # différence entre systèmes
                    data = v_h %>% select(ligne, t, endroit, heure, vide) %>% 
                     filter(!is.na(vide)),
                    family = gaussian(), 
                    prior = c(set_prior('normal(-27, 4)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.9, max_treedepth = 12),
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_v2)
pp_check(mod_v2, ndraws = 100)
pp_check(mod_v2, type = 'error_hist',  ndraws = 10)
pp_check(mod_v2, type = 'scatter_avg', ndraws = 100)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_v2)
ranef(mod_v2)$t [, , 'Intercept']
ranef(mod_v2)$ligne [, , 'Intercept']
ranef(mod_v2)$ligne:endroit [, , 'Intercept']
ranef(mod_v2)$heure [, , 'Intercept']


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