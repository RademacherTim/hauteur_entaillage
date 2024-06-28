# Dépendances ------------------------------------------------------------------
if (!existsFunction("brms")) library("brms")
if (!existsFunction("pp_check")) library("rstanarm")
if (!existsFunction("%>%")) library("tidyverse")
if (!existsFunction("spread_draws")) library("tidybayes")
if (!existsFunction("panel_border")) library("cowplot")


# Lire les données -------------------------------------------------------------
if (!exists('d')) source('01_lire_données.R')


# Erreur des instruments -------------------------------------------------------
atp_se   <- 5      # Hygiena SureSystem II 5% ou 5 RLUs
ph_se    <- 0.01   # Extech ExStick 
brix1_se <- 0.2    # Atago Pal-Maple (0-85%)


# Rendement par hauteur et système ---------------------------------------------
par(mar = c(5, 5, 1, 2), mfrow = c(2, 1))
plot(x = d$datetime, y = d$rendement, 
     xlab = "Date", ylab = "Rendement par entaille (litres)", axes = FALSE, 
     col = "white", xlim = as_datetime(c("2023-02-27", "2023-04-17")), 
     ylim = c(0, 12))
axis(side = 1, at = as_datetime(c("2023-02-28", "2023-03-06", "2023-03-13", 
                                  "2023-03-20", "2023-03-27", "2023-04-03", 
                                  "2023-04-10", "2023-04-17")),
     labels = c("28 feb", "6 mar", "13 mar", "20 mar", "27 mar", "3 avr", 
                "10 avr", "17 avr"))
axis(side = 2, las = 1)
for (i in 1:10){
  con <- d$t == info$t[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$rendement[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
text(x = as_datetime("2023-04-17"), y = 12, labels = "2023", cex = 1)
legend(x = as_datetime("2023-03-01 12:00:00"), y = 12, bg = "transparent", 
       box.lty = 0, legend = c("+24\"",  "+12\"",  "  +4\"", "   -4\"", "-12\"", 
                               " -24\""), 
       col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
       pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
       pch = c(24, 2, 1, 5, 6, 25))
plot(x = d$datetime, y = d$rendement, 
     xlab = "Date", ylab = "Rendement par entaille (litres)", axes = FALSE, 
     col = "white", xlim = as_datetime(c("2024-02-27", "2024-04-17")), 
     ylim = c(0, 12))
axis(side = 1, at = as_datetime(c("2024-02-28", "2024-03-06", "2024-03-13", 
                                  "2024-03-20", "2024-03-27", "2024-04-03", 
                                  "2024-04-10", "2024-04-17")),
     labels = c("28 feb", "6 mar", "13 mar", "20 mar", "27 mar", "3 avr", 
                "10 avr", "17 avr"))
axis(side = 2, las = 1)
for (i in 11:dim(info)[1]){
  con <- d$t == info$t[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$rendement[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
text(x = as_datetime("2024-04-17"), y = 12, labels = "2024", cex = 1)


# L'effet de l'hauteur de l'entaille sur le rendement (modèle complet h) -------
mod_vol <- brms::brm(brms::bf(rendement ~
                              h +                     # effet de l'hauteur de l'entaille 
                              (1 | site / année / date) +    # différence entre année et date
                              (1 | systeme / ligne)), #+ # différence entre systèmes et ligne
                              #(1 | site)),            # effet du site
                      data = d %>% filter(rendement > 0),
                      family = lognormal(), 
                      prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                set_prior('exponential(1)', class = 'sigma'),
                                set_prior('normal(0, 2)', class = 'b')),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.9, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol)
pp_check(mod_vol, ndraws = 100)
pp_check(mod_vol, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Effet de l'hauteur relative de l'entaille ---------------------------––-------
theme_set(theme_tidybayes() + panel_border())
plot(conditional_effects(mod_vol)) [[1]] +
  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
                     limits = c(0, 25)) +
  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol)
ranef(mod_vol)$site [, , 'Intercept']
ranef(mod_vol)$`site:année` [, , 'Intercept'] 
ranef(mod_vol)$`site:année:date`[, , 'Intercept']
ranef(mod_vol)$systeme [, , 'Intercept']
ranef(mod_vol)$`systeme:ligne`[, , "Intercept"]
summary(mod_vol)$fixed

# Notes (modèle complet h) : 
# ------------------------------------------------------------------------------
# L'effet est petit et plutôt insignificant. Chaque centimetres, entaille et 
# coulée ont un effet. En résumé, l'effet est de 0.0004176738 L par entaille 
# par coulée par cm est important. Par exemple, pour une saison de 15 coulée 
# faire l'entaille 50 cm plus haut donne 0.3 L plus de sève par entaille. 

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# h) ---------------------------------------------------------------------------
mod_vol_SN <- brms::brm(brms::bf(rendement ~ 
                                h +                     # effet de l'hauteur de l'entaille 
                                (1 | année / date) +    # différence entre année et date
                                (1 | systeme / ligne)),# + # différence entre systèmes et ligne
                     data = d %>% filter(site == "SN" & rendement > 0),
                     family = lognormal(), 
                     prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 11),
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol_SN)

pp_check(mod_vol_SN, ndraws = 100)
pp_check(mod_vol_SN, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_SN, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_vol_SN)) [[1]] +
  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
                     limits = c(0, 5)) +
  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_SN)
ranef(mod_vol_SN)$année [, , 'Intercept']
ranef(mod_vol_SN)$`année:date`[, , 'Intercept']
ranef(mod_vol_SN)$systeme [, , 'Intercept']
ranef(mod_vol_SN)$`systeme:ligne`[, , "Intercept"]
summary(mod_vol_SN)$fixed
# Notes (modèle Saint-Norbert seulement h) -------------------------------------
# L'effet n'est similaire à Saint-Norbert comparé au modèle global.

# L'effet de l'hauteur de l'entaille sur le rendement (Club de l'Est seulement 
# h) ---------------------------------------------------------------------------
mod_vol_CE <- brms::brm(brms::bf(rendement ~ 
                                   h +                     # effet de l'hauteur de l'entaille 
                                   (1 | année / date) +    # différence entre année et date
                                   (1 | ligne)),           # différence entre lignes
                        data = d %>% filter(site == "CE" & rendement > 0),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma'),
                                  set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 11),
                        iter = 6000,
                        seed = 1353,
                        backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol_CE)

pp_check(mod_vol_CE, ndraws = 100)
pp_check(mod_vol_CE, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_CE, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_vol_CE))[[1]] +
  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
                     limits = c(0, 3)) +
  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_CE)
ranef(mod_vol_CE)$année [, , 'Intercept']
ranef(mod_vol_CE)$`année:date`[, , 'Intercept']
ranef(mod_vol_CE)$ligne[, , "Intercept"]
summary(mod_vol_CE)$fixed
# Notes (modèle Club de l'Est seulement h) -------------------------------------
# L'effet n'est pas significatif pour les données du Club de l'Est. 

# L'effet de l'hauteur de l'entaille sur le rendement (modèle complet) t -------
mod_vol2 <- brms::brm(brms::bf(rendement ~ 
                                (1 | t) +               # effet du traitement 
                                (1 | site / année / date) +    # différence entre année et date
                                (1 | systeme / ligne)),# + # différence entre systèmes et ligne
                                #(1 | site)),            # effet du site
                     data = d %>% select(rendement, année, site, systeme, ligne, t, date) %>% filter(rendement > 0),
                     family = lognormal(), 
                     prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma')),
                               #set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 11),
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vol2)

pp_check(mod_vol2, ndraws = 100)
pp_check(mod_vol2, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Extraire les distributions postérieures --------------------------------------
mod_vol2 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
                     limits = c(0, 6)) +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
                            "+12\"", "+24\"")) +
  stat_halfeye()

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol2)
ranef(mod_vol2)$t[, ,"Intercept"]
ranef(mod_vol2)$site[, ,"Intercept"]
ranef(mod_vol2)$`site:année`[, , 'Intercept']
ranef(mod_vol2)$`site:année:date`[, , 'Intercept']
ranef(mod_vol2)$systeme [, , 'Intercept']
ranef(mod_vol2)$`systeme:ligne`[, , "Intercept"]
# Notes (modèle complet t) -----------------------------------------------------
# Aucuns des effets catégorique est clairement positifs ou négatifs dans un 
# modèle global.

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# t) ---------------------------------------------------------------------------
mod_vol2_SN <- brms::brm(brms::bf(rendement ~ 
                                   (1 | t) +                     # effet du traitement
                                   (1 | année / date) +    # différence entre année et date
                                   (1 | systeme / ligne)),# + # différence entre systèmes et ligne
                        data = d %>% filter(site == "SN" & rendement > 0),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma')),
                                  #set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 11),
                        iter = 6000,
                        seed = 1353,
                        backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol2_SN)

pp_check(mod_vol2_SN, ndraws = 100)
pp_check(mod_vol2_SN, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol2_SN, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# extraire les distributions postérieures --------------------------------------
mod_vol2_SN %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
                     limits = c(0, 2.5)) +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-24\"", "-12\"", "-4\"", "+4\"", "+12\"", "+24\"")) +
  stat_halfeye()

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol2_SN)
ranef(mod_vol2_SN)$t [, , 'Intercept']
ranef(mod_vol2_SN)$année [, , 'Intercept']
ranef(mod_vol2_SN)$`année:date`[, , 'Intercept']
ranef(mod_vol2_SN)$systeme [, , 'Intercept']
ranef(mod_vol2_SN)$`systeme:ligne`[, , "Intercept"]
# Notes : Pas d'effet claire 

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# t en 2023) -------------------------------------------------------------------
mod_vol23_SN <- brms::brm(brms::bf(rendement ~ 
                                    (1 | t) +                     # effet du traitement
                                    (1 | date) +    # différence entre année et date
                                    (1 | systeme / ligne)),# + # différence entre systèmes et ligne
                         data = d %>% filter(site == "SN" & rendement > 0 & année == 2023),
                         family = lognormal(), 
                         prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                   set_prior('exponential(1)', class = 'sigma')),
                         cores = 4, chains = 4,
                         control = list(adapt_delta = 0.9, max_treedepth = 11),
                         iter = 6000,
                         seed = 1353,
                         backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol23_SN)

pp_check(mod_vol23_SN, ndraws = 100)
pp_check(mod_vol23_SN, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol23_SN, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# extraire les distributions postérieures --------------------------------------
mod_vol23_SN %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
                     limits = c(0, 2)) +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-24\"", "-4\"", "+4\"", "+24\"")) +
  stat_halfeye()

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol23_SN)
ranef(mod_vol23_SN)$t [, , 'Intercept']
ranef(mod_vol23_SN)$date[, , 'Intercept']
ranef(mod_vol23_SN)$systeme [, , 'Intercept']
ranef(mod_vol23_SN)$`systeme:ligne`[, , "Intercept"]
# Notes : 

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# t en 2024) -------------------------------------------------------------------
mod_vol24_SN <- brms::brm(brms::bf(rendement ~ 
                                     (1 | t) +                     # effet du traitement
                                     (1 | date) +    # différence entre année et date
                                     (1 | systeme / ligne)),# + # différence entre systèmes et ligne
                          data = d %>% filter(site == "SN" & rendement > 0 & année == 2024),
                          family = lognormal(), 
                          prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                    set_prior('exponential(1)', class = 'sigma')),
                          cores = 4, chains = 4,
                          control = list(adapt_delta = 0.9, max_treedepth = 11),
                          iter = 6000,
                          seed = 1353,
                          backend = 'cmdstanr')

# Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol24_SN)

pp_check(mod_vol24_SN, ndraws = 100)
pp_check(mod_vol24_SN, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol24_SN, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# extraire les distributions postérieures --------------------------------------
mod_vol24_SN %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
                     limits = c(0, 2)) +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-24\"", "-12\"", "+12\"", "+24\"")) +
  stat_halfeye()

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol24_SN)
ranef(mod_vol24_SN)$t [, , 'Intercept']
ranef(mod_vol24_SN)$date[, , 'Intercept']
ranef(mod_vol24_SN)$systeme [, , 'Intercept']
ranef(mod_vol24_SN)$`systeme:ligne`[, , "Intercept"]
# Notes : 

# L'effet de l'hauteur de l'entaille sur le rendement (Club de l'Est seulement 
# t) ---------------------------------------------------------------------------
mod_vol2_CE <- brms::brm(brms::bf(rendement ~ 
                                   (1 | t) +            # effet du traitement
                                   (1 | année / date) + # différence entre année et date
                                   (1 | ligne)),# + # différence entre systèmes et ligne
                        data = d %>% filter(site == "CE" & rendement > 0),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma')),
                                  #set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 11),
                        iter = 6000,
                        seed = 1353,
                        backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vol2_CE)

pp_check(mod_vol2_CE, ndraws = 100)
pp_check(mod_vol2_CE, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol2_CE, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Extraire les distributions postérieures --------------------------------------
mod_vol2_CE %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = t, x = t_mean)) + 
  scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
                     limits = c(0, 5)) +
  scale_y_discrete(name ="Hauteur relative au latéral", 
                   labels=c("-8\"", "+8\"")) +
  stat_halfeye()

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol2_CE)
ranef(mod_vol2_CE)$t [, , 'Intercept']
ranef(mod_vol2_CE)$année [, , 'Intercept']
ranef(mod_vol2_CE)$`année:date`[, , 'Intercept']
ranef(mod_vol2_CE)$ligne[, , "Intercept"]
# Notes : Pas d'effet claire

# teneur en sucre par hauteur et système ---------------------------------------
par(mar = c(5, 5, 1, 2), mfrow = c (2, 1))
plot(x = d$datetime, y = d$brix, 
     xlab = "Date", 
     ylab = expression(paste("Teneur en sucre (",degree,"Brix)", sep = "")), 
     axes = FALSE, 
     col = "white", 
     xlim = as_datetime(c("2023-02-27", "2023-04-17")), ylim = c(1, 3))
axis(side = 1, at = as_datetime(c("2023-02-28", "2023-03-06", "2023-03-13", 
                                  "2023-03-20", "2023-03-27", "2023-04-03", 
                                  "2023-04-10", "2023-04-17")),
     labels = c("28 feb", "6 mar", "13 mar", "20 mar", "27 mar", "3 avr", "10 avr", "17 avr"))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$t == info$t[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$brix[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
legend(x = as_datetime("2023-02-28 12:00:00"), y = 3, bg = "transparent", 
       box.lty = 0,
       legend = c("+24\"",  "  +12\"",  "  +4\"", "   -4\"", "-12\"", "-24\""), 
       col = c("#00441b", "#1b7837", "#a6dba0", "#c2a5cf", "#762a83", "#40004b"),
       pt.bg = c("#00441b", "#1b7837", "#a6dba0", "#c2a5cf", "#762a83","#40004b"),
       pch = c(2, 24, 1, 5, 25, 6))
plot(x = d$datetime, y = d$brix, 
     xlab = "Date", 
     ylab = expression(paste("Teneur en sucre (",degree,"Brix)", sep = "")), 
     axes = FALSE, 
     col = "white", 
     xlim = as_datetime(c("2024-02-27", "2024-04-17")), ylim = c(1, 3))
axis(side = 1, at = as_datetime(c("2024-02-28", "2024-03-06", "2024-03-13", 
                                  "2024-03-20", "2024-03-27", "2024-04-03", 
                                  "2024-04-10", "2024-04-17")),
     labels = c("28 feb", "6 mar", "13 mar", "20 mar", "27 mar", "3 avr", "10 avr", "17 avr"))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$t == info$t[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$brix[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}

# effet de l'hauteur de l'entaille sur le teneur en sucre (seulement disponible 
# pour St-Norbert-d'Arthabaska) ------------------------------------------------
mod_brix1 <- brms::brm(brms::bf(brix ~ 
                             t +                     # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                             (1 | année / date) +    # différence entre année et date
                             (1 | systeme / ligne)), # difference entre systèmes et lignes
                       data = d %>% #add_column(brix1_se = brix1_se) %>% 
                         filter(site == "SN") %>%
                         select(-rendement, -datetime, -site, -h),
                       family = gaussian(), 
                       prior = c(set_prior('normal(2, 5)', class = 'Intercept'),
                                 set_prior('exponential(1)', class = 'sigma')),
                                 #set_prior('normal(0, 2)', class = 'b')),
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.99),
                       iter = 6000,
                       seed = 1353,
                       backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix1)
pp_check(mod_brix1, ndraws = 100)
pp_check(mod_brix1, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix1, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_brix1)) [[1]]
# Il semble avoir un petit effet de l'hauteur sur le brix, ce qui n'est pas à 
# négliger. Ceci est en concordance avec des résultats antérieur qui ont montré 
# qu'il y a une relation entre hauteur sur le tronc et brix (Rademacher et al., 
# 2023). L'effet serait d'environ 0.06 degré brix par metre ici, donc 
# relativement petit. 

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix1)
summary(mod_brix1_h)$fixed
ranef(mod_brix1_t)$t [, , 'Intercept']
#ranef(mod_brix1)$année [, , 'Intercept']
ranef(mod_brix1)$`année:date`[, , 'Intercept']
ranef(mod_brix1)$systeme [, , 'Intercept']
ranef(mod_brix1)$`systeme:ligne`[, , "Intercept"]

# effet de l'hauteur de l'entaille sur le teneur en sucre ----------------------
mod_brix2 <- brms::brm(brms::bf(brix ~ 
                                 h +                     # hauteur de l'entaille 
                                 (1 | année / date) +    # différence par date
                                 (1 | systeme / ligne)), # difference antre systèmes
                       data = d1,
                       family = gaussian(), 
                       prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                                 set_prior('exponential(1)', class = 'sigma'),
                                 set_prior('normal(0, 2)', class = 'b')),
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.9),
                       iter = 6000,
                       seed = 1353,
                       backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix2)
pp_check(mod_brix2, ndraws = 100)
pp_check(mod_brix2, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix2)
summary(mod_brix2)$fixed
ranef(mod_brix2)$systeme [, , 'Intercept']
ranef(mod_brix2)$systeme:ligne [, , 'Intercept']
ranef(mod_brix2)$année [, , 'Intercept']
ranef(mod_brix2)$`année:date` [, , 'Intercept']

# effet de l'hauteur de l'entaille sur la contamination microbienne ------------
# TR - Need to look at whether a lognormal distribution is really the best fit.
# It almost looks uniformly distributed.
mod_atp <- brms::brm(brms::bf(log(atp) ~ 
                                  h +                    # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                                  (1 | année / date) +   # différence par date
                                  (1 | systeme / ligne)), # difference antre systèmes
                     data = d1,
                     family = gaussian(), 
                     prior = c(#set_prior('normal(2, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9),
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_atp)
pp_check(mod_atp, ndraws = 100)
pp_check(mod_atp, type = 'error_hist',  ndraws = 10)
pp_check(mod_atp, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_atp)
summary(mod_atp)$fixed
ranef(mod_atp)$systeme [, , "Intercept"]
ranef(mod_atp)$systeme:ligne [, , "Intercept"]
ranef(mod_atp)$année [, , "Intercept"]
ranef(mod_atp)$`année:date` [, , "Intercept"]

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_ph <- brms::brm(brms::bf(#ph | mi(ph_se) ~ 
                             ph ~
                               h +                     # hauteur de l'entaille 
                               (1 | année / date) +    # différence par date
                               (1 | systeme / ligne)), # difference entre systèmes
                    data = d1 %>% select(-c(brix, datetime, atp, sc)), #%>% 
                      #add_column(ph_se = ph_se),
                    family = gaussian(), 
                    prior = c(set_prior('normal(5, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma'),
                              set_prior('normal(0, 2)', class = 'b')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99),
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_ph)
pp_check(mod_ph, ndraws = 100)
pp_check(mod_ph, type = 'error_hist',  ndraws = 10)
pp_check(mod_ph, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 
# il n'y a pas de données de ph entre 6,4 et 7,0, ce que semble bizarre.

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_ph)
summary(mod_ph)$fixed
ranef(mod_ph)$systeme [, , 'Intercept']
ranef(mod_ph)$date [, , 'Intercept']

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_sc <- brms::brm(brms::bf(sc ~ 
                             h +             # hauteur de l'entaille 
                             (1 | année / date) +    # différence par date
                             (1 | systeme / ligne)), # difference antre systèmes
                    data = d1,
                    family = gaussian(), 
                    prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma'),
                              set_prior('normal(0, 2)', class = 'b')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99),
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_sc)
pp_check(mod_sc, ndraws = 100)
pp_check(mod_sc, type = 'error_hist',  ndraws = 10)
pp_check(mod_sc, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 
# Pourquoi la distribution est bi-modal?

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_sc)
summary(mod_sc)$fixed
ranef(mod_sc)$systeme [, , 'Intercept']
ranef(mod_sc)$systeme:ligne [, , 'Intercept']
ranef(mod_sc)$`année:date` [, , 'Intercept']
# TR - Il manque encore les données de 2024 pour cet analyse. 