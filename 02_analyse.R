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
                              h +                         # effet de l'hauteur de l'entaille 
                              (1 | site / année / date) + # différence entre sites, années et dates
                              (1 | systeme / ligne)),     # différence entre systèmes et ligne
                      data = d %>% filter(rendement > 0),
                      family = lognormal(), 
                      prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                set_prior('exponential(1)', class = 'sigma'),
                                set_prior('normal(0, 2)', class = 'b')),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.9, max_treedepth = 12),
                      iter = 6000,
                      warmup = 2000,
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
#plot(conditional_effects(mod_vol)) [[1]] +
#  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
#                     limits = c(1, 3)) +
#  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")
plot(conditional_effects(mod_vol)) [[1]] +
  scale_y_continuous(name ="Sap volume per tap per day of flow (litres)",
                     limits = c(0, 25)) +
  scale_x_continuous(name ="Relative spout height (cm)")

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
# La tendance est petit et n'est pas très clair. Pourtant, chaque centimetres,
# entaille et coulée ont un effet, donc il y a un effet cumulatif plus 
# important. En résumé, l'effet est de 0.0004376034 L [-0.00016; 0.00112] par 
# entaille par coulée par cm est non-négligable. Par exemple, pour une saison de
# 15 coulées et des entailles, qui sont 50 cm plus haut donne 0.3 L plus de sève 
# par entaille. 

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# h) ---------------------------------------------------------------------------
mod_vol_SN <- brms::brm(brms::bf(rendement ~ 
                                h +                     # effet de l'hauteur de l'entaille 
                                (1 | année / date) +    # différence entre année et date
                                (1 | systeme / ligne)), # différence entre systèmes et ligne
                     data = d %>% filter(site == "SN" & rendement > 0),
                     family = lognormal(), 
                     prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 12),
                     iter = 6000,
                     warmup = 2000,
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
# L'effet n'est similaire à Saint-Norbert comparé au modèle global, mais 
# légèrement plus large 0.0004562404 [-0.00021; 0.00113]. Donc, il y aurait 0.34 
# L de sève de plus par entaille dans une saison avec 15 evènements de coulée.

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert pour 2023 
# seulement h) -----------------------------------------------------------------
mod_vol_SN23 <- brms::brm(brms::bf(rendement ~ 
                                   h +                     # effet de l'hauteur de l'entaille 
                                   (1 | date) +            # différence entre année et date
                                   (1 | systeme / ligne)), # différence entre systèmes et ligne
                        data = d %>% filter(site == "SN" & rendement > 0 & 
                                              année == 2023),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma'),
                                  set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 12),
                        iter = 6000,
                        warmup = 2000,
                        seed = 1353,
                        backend = 'cmdstanr')

# # Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol_SN23)
 
pp_check(mod_vol_SN23, ndraws = 100)
pp_check(mod_vol_SN23, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_SN23, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_vol_SN23)) [[1]] +
  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
                     limits = c(0, 5)) +
  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_SN23)
ranef(mod_vol_SN23)$date[, , 'Intercept']
ranef(mod_vol_SN23)$systeme [, , 'Intercept']
ranef(mod_vol_SN23)$`systeme:ligne`[, , "Intercept"]
summary(mod_vol_SN23)$fixed
# Notes (modèle Saint-Norbert seulement h) -------------------------------------
# L'effet est beaucoup plus fort en 2023 à Saint-Norbert avec 0.001647183 
# [0.00019; 0.00307]. Donc, il y aurait 1.24 L de sève de plus par entaille 
# pour des entaille à 50 cm plus haut dans une saison avec 15 évènements de 
# coulée.

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert pour 2024 
# seulement h) -----------------------------------------------------------------
mod_vol_SN24 <- brms::brm(brms::bf(rendement ~ 
                                     h +                     # effet de l'hauteur de l'entaille 
                                     (1 | date) +            # différence entre année et date
                                     (1 | systeme / ligne)), # différence entre systèmes et ligne
                          data = d %>% filter(site == "SN" & rendement > 0 & 
                                                année == 2024),
                          family = lognormal(), 
                          prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                    set_prior('exponential(1)', class = 'sigma'),
                                    set_prior('normal(0, 2)', class = 'b')),
                          cores = 4, chains = 4,
                          control = list(adapt_delta = 0.9, max_treedepth = 12),
                          iter = 6000,
                          warmup = 2000,
                          seed = 1353,
                          backend = 'cmdstanr')

# # Vérifier la distribution postérieur ------------------------------------------
plot(mod_vol_SN24)

pp_check(mod_vol_SN24, ndraws = 100)
pp_check(mod_vol_SN24, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_SN24, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# Effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_vol_SN24)) [[1]] +
  scale_y_continuous(name ="Rendement en sève par entaille par coulée (litres)",
                     limits = c(0, 5)) +
  scale_x_continuous(name ="Hauteur realtive de l'entaille (cm)")

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_SN24)
ranef(mod_vol_SN24)$date[, , 'Intercept']
ranef(mod_vol_SN24)$systeme [, , 'Intercept']
ranef(mod_vol_SN24)$`systeme:ligne`[, , "Intercept"]
summary(mod_vol_SN24)$fixed
# Notes (modèle Saint-Norbert seulement h) -------------------------------------
# L'effet n'est pas clair du tout en 2024 à Saint-Norbert avec -0.0003386373 
# [-0.001576262; 0.0009157717]. Donc, il y aurait 0.25 L de sève de moins par entaille 
# dans une saison avec 15 évènements de coulée.

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
                        control = list(adapt_delta = 0.9, max_treedepth = 12),
                        warmup = 2000,
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
# L'effet n'est pas significatif pour les données du Club de l'Est avec 
# 0.0002402153 L par coulée par entaille [-0.0019; 0.0015].

# L'effet de l'hauteur de l'entaille sur le rendement (modèle complet) t -------
mod_vol2_t <- brms::brm(brms::bf(rendement ~ 
                                (1 | t) +                   # effet du traitement 
                                (1 | site / année / date) + # différence entre année et date
                                (1 | systeme / ligne)),     # différence entre systèmes et ligne
                     data = d %>% select(rendement, année, site, systeme, ligne, t, date) %>% filter(rendement > 0),
                     family = lognormal(), 
                     prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma')),
                               #set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 12),
                     warmup = 2000,
                     iter = 6000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vol2)

pp_check(mod_vol2, ndraws = 100)
pp_check(mod_vol2, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# extract intercept ------------------------------------------------------------
intercept <- posterior_summary(mod_vol2, variable = "b_Intercept")

# Extraire les distributions postérieures --------------------------------------
# mod_vol2 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
#   mutate(t_mean = b_Intercept + r_t) %>%
#   ggplot(aes(y = t, x = t_mean)) + 
#   scale_x_continuous(name ="Rendement en sève par entaille par coulée (litres)", 
#                      limits = c(0, 6)) +
#   scale_y_discrete(name ="Hauteur relative au latéral", 
#                    labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
#                             "+12\"", "+24\"")) +
#   stat_halfeye()
mod_vol2 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = exp(b_Intercept + r_t)) %>%
  ggplot(aes(y = factor (t, levels = c("b3", "b2", "b1.5", "b1", "h1", "h1.5", "h2", "h3")), 
             x = t_mean)) + 
  scale_x_continuous(name = expression(paste("Sap yield (liters ", tap^-1," ",day^-1,")")), 
                     limits = c(0, 9)) +
  # scale_y_discrete(name ="Relative spout height (Inches)", 
  #                  labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
  #                           "+12\"", "+24\"")) +
  scale_y_discrete(name = "Relative spout height (cm)", 
                   labels=c("-60", "-30", "-20", "-10", "+10", "+20", "+30", 
                            "+60")) +
  stat_halfeye(aes(fill = t, color = t), 
               adjust = 0.5, 
               width = 0.6, 
               .width = c(0.5, 0.8, 0.95)) +
  scale_fill_manual(values = c("b1" = "#40004b99", "b1.5" = "#762a8399", 
                               "b2" = "#9970ab99", "b3" = "#c2a5cf99", 
                               "h1" = "#00441b99", "h1.5" = "#1b783799", 
                               "h2" = "#5aae6199", "h3" = "#a6dba099")) +
  scale_color_manual(values = c("b1" = "#333333", "b1.5" = "#333333", 
                                "b2" = "#333333", "b3" = "#333333", 
                                "h1" = "#333333", "h1.5" = "#333333", 
                                "h2" = "#333333", "h3" = "#333333")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = exp(intercept[1, 1]), 
             color = "darkgrey", linetype = "dashed", size = 1)

# Regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol2)$fixed
ranef(mod_vol2)$t[, ,"Intercept"]
ranef(mod_vol2)$site[, ,"Intercept"]
ranef(mod_vol2)$`site:année`[, , 'Intercept']
ranef(mod_vol2)$`site:année:date`[, , 'Intercept']
ranef(mod_vol2)$systeme [, , 'Intercept']
ranef(mod_vol2)$`systeme:ligne`[, , "Intercept"]
# Notes (modèle complet t) -----------------------------------------------------
# Aucuns des effets catégorique est clairement positifs ou négatifs dans un 
# modèle global, mais surtout les extrême ont plus (24" en haut) et moins (24" 
# en bas) de volume. Le résultats ne change pas si j'utilise un effet 
# catégorique structurelle "t" plutôt que (des intercepts (1 | t).

# comparer modèle mod_vol1 et mod_vol2 -----------------------------------------
loo_vol1 <- loo(mod_vol)
loo_vol2 <- loo(mod_vol2)
loo_compare(loo_vol1, loo_vol2, loo_vol3, loo_vol4)
# Le deuxième modèle avec un facteur catégorique décrit mieux les données. Ceci 
# est aussi vrai quand on inclut un modèle avec un effet structurelle 
# catégorique "t" et on change le à priori pour l'intercept à 2. Donc, ce 
# résultat semble robuste.

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# t) ---------------------------------------------------------------------------
mod_vol2_SN <- brms::brm(brms::bf(rendement ~ 
                                   (1 | t) +               # effet du traitement
                                   (1 | année / date) +    # différence entre année et date
                                   (1 | systeme / ligne)), # différence entre systèmes et ligne
                        data = d %>% filter(site == "SN" & rendement > 0),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma')),
                                  #set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 12),
                        warmup = 2000,
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
  ggplot(aes(y = factor(t, levels = c("b3", "b2", "b1", "h1", "h2", "h3")), 
             x = t_mean)) + 
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
# Notes : Encore une fois les plus gros effets sont 24" en haut et en bas, mais 
# il n'y pas d'effet clair.

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
                         control = list(adapt_delta = 0.9, max_treedepth = 12),
                         iter = 6000,
                         warmup = 2000,
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
  ggplot(aes(y = factor(t, levels = c("b3", "b1", "h1", "h3")), 
             x = t_mean)) + 
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
# Notes : Tendance de plus de rendement plus haut avec l'exception de -24" en 
# bas, mais la tendance est subtile. Il ne semble pas avoir de différence entre 
# la ligne de 4" et de -4".

# L'effet de l'hauteur de l'entaille sur le rendement (Saint-Norbert seulement 
# t en 2024) -------------------------------------------------------------------
mod_vol24_SN <- brms::brm(brms::bf(rendement ~ 
                                     (1 | t) +                # effet du traitement
                                     (1 | date) +             # différence entre année et date
                                     (1 | systeme / ligne)),  # différence entre systèmes et ligne
                          data = d %>% filter(site == "SN" & rendement > 0 & année == 2024),
                          family = lognormal(), 
                          prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                    set_prior('exponential(1)', class = 'sigma')),
                          cores = 4, chains = 4,
                          control = list(adapt_delta = 0.9, max_treedepth = 12),
                          iter = 6000,
                          warmup = 2000,
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
  ggplot(aes(y = factor (t, levels = c("b3", "b2", "h2", "h3")), 
             x = t_mean)) + 
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
# Notes : Pas de tendance clair. -12" a le plus gros rendements.

# L'effet de l'hauteur de l'entaille sur le rendement (Club de l'Est seulement 
# t) ---------------------------------------------------------------------------
mod_vol2_CE <- brms::brm(brms::bf(rendement ~ 
                                   (1 | t) +            # effet du traitement
                                   (1 | année / date) + # différence entre année et date
                                   (1 | ligne)),        # différence entre systèmes et ligne
                        data = d %>% filter(site == "CE" & rendement > 0),
                        family = lognormal(), 
                        prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma')),
                                  #set_prior('normal(0, 2)', class = 'b')),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9, max_treedepth = 12),
                        iter = 6000,
                        warmup = 2000,
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
# Notes : Vraiment pas d'effet claire. La moyenne est peu différent et il y a 
# une large marge d'erreur.

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
       col = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
       pt.bg = c("#a6dba0", "#1b7837", "#00441b", "#40004b", "#762a83", "#c2a5cf"),
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
mod_brix1 <- brms::brm(brms::bf(brix ~ # | mi(brix1_se) ~ 
                             (1 | t) +               # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                             (1 | année / date) +    # différence entre année et date
                             (1 | systeme / ligne)), # difference entre systèmes et lignes
                       data = d %>%# add_column(brix1_se = brix1_se) %>% 
                         filter(site == "SN") %>%
                         select(-rendement, -datetime, -site, -h) %>%
                         filter(!is.na(brix)),
                       family = gaussian(), 
                       prior = c(set_prior('normal(2, 5)', class = 'Intercept'),
                                 set_prior('exponential(1)', class = 'sigma')),
                                 #set_prior('normal(0, 2)', class = 'b')),
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.99, max_treedepth = 13),
                       warmup = 2000,
                       iter = 6000,
                       seed = 1353,
                       backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix1)
pp_check(mod_brix1, ndraws = 100)
pp_check(mod_brix1, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix1, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement
# TR - Le modèle a de la misère de converger. Je dois encore jouer un peu avec les 
# paramètres "à prior" pour voir si les résultats sont rigoureux.

# extract intercept ------------------------------------------------------------
intercept <- posterior_summary(mod_brix1_se, variable = "b_Intercept")

# effet de l'hauteur relative de l'entaille ---------------------------––-------
mod_brix1 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = factor(t, levels = c("b3", "b2", "b1", "h1", "h2", "h3")), 
             x = t_mean)) + 
  scale_x_continuous(name = expression(paste("Sap sugar concentration (", degree," Brix)")), 
                     limits = c(0.7, 3.0)) +
  # scale_y_discrete(name ="Relative spout height (Inches)", 
  #                  labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
  #                           "+12\"", "+24\"")) +
  scale_y_discrete(name = "Relative spout height (cm)", 
                   labels=c("-60", "-30", "-10", "+10", "+30", "+60")) +
  stat_halfeye(aes(fill = t, color = t), 
               adjust = 0.5, 
               width = 0.6, 
               .width = c(0.5, 0.8, 0.95)) +
  scale_fill_manual(values = c("b1" = "#40004b99", "b1.5" = "#762a8399", 
                               "b2" = "#9970ab99", "b3" = "#c2a5cf99", 
                               "h1" = "#00441b99", "h1.5" = "#1b783799", 
                               "h2" = "#5aae6199", "h3" = "#a6dba099")) +
  scale_color_manual(values = c("b1" = "#333333", "b1.5" = "#333333", 
                                "b2" = "#333333", "b3" = "#333333", 
                                "h1" = "#333333", "h1.5" = "#333333", 
                                "h2" = "#333333", "h3" = "#333333")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = intercept[1, 1], 
             color = "darkgrey", linetype = "dashed", size = 1)


# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix1)
ranef(mod_brix1)$t
ranef(mod_brix1)$année [, , 'Intercept']
ranef(mod_brix1)$`année:date`[, , 'Intercept']
ranef(mod_brix1)$systeme [, , 'Intercept']
ranef(mod_brix1)$`systeme:ligne`[, , "Intercept"]
# Notes ------------------------------------------------------------------------
# Il semble avoir un petit effet de l'hauteur sur le brix, ce qui n'est pas à 
# négliger. Ceci est en concordance avec des résultats antérieur qui ont montré 
# qu'il y a une relation entre hauteur sur le tronc et brix (Rademacher et al., 
# 2023). 
# Inclure la précision de la mesure cause des problèmes de convergence, mais le 
# résultats est qualitativement le même. Pourtant, l'effet entre la hauteur la 
# plus haute et la plus basse est un peu plus large. 

# effet de l'hauteur de l'entaille sur le teneur en sucre ----------------------
mod_brix2 <- brms::brm(brms::bf(brix ~ #| mi(brix2_se) ~ 
                                 h +                     # hauteur de l'entaille 
                                 (1 | année / date) +    # différence par date
                                 (1 | systeme / ligne)), # difference antre systèmes
                       data = d %>% #add_column(brix2_se = brix1_se) %>% 
                         filter(site == "SN") %>%
                         select(-rendement, -datetime, -site, -t),,
                       family = gaussian(), 
                       prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                                 set_prior('exponential(1)', class = 'sigma'),
                                 set_prior('normal(0, 2)', class = 'b')),
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.9, max_treedepth = 12),
                       warmup = 2000,
                       iter = 6000,
                       seed = 1353,
                       backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix2)
pp_check(mod_brix2, ndraws = 100)
pp_check(mod_brix2, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_brix2)) [[1]] + 
  scale_x_continuous(name = "Relative spout height (cm)") +
  scale_y_continuous(name = "Sap sugar concentration (°Brix)", 
                     limits = c(0, 3)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
# Notes :  
# Il semble avoir un petit effet de l'hauteur sur le brix, ce qui n'est pas à 
# négliger. Ceci est en concordance avec des résultats antérieur qui ont montré 
# qu'il y a une relation entre hauteur sur le tronc et brix (Rademacher et al., 
# 2023). 
# Le résultats est qualitativement le même si on inclut l'erreur de mesure, mais 
# le modèle a des problèmes de convergence. 

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix2)
summary(mod_brix2)$fixed
ranef(mod_brix2)$systeme [, , 'Intercept']
ranef(mod_brix2)$systeme:ligne [, , 'Intercept']
ranef(mod_brix2)$année [, , 'Intercept']
ranef(mod_brix2)$`année:date` [, , 'Intercept']

# comparer modèle mod_brix1 et mod_brix2 -----------------------------------------
loo_brix1 <- loo(mod_brix1)
loo_brix2 <- loo(mod_brix2)
loo_compare(loo_brix1, loo_brix2)

# effet de l'hauteur de l'entaille sur la contamination microbienne ------------
mod_atp <- brms::brm(brms::bf(log(atp) | mi(atp_se) ~ 
                                  h +                    # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                                  (1 | année / date) +   # différence par date
                                  (1 | systeme / ligne)), # difference antre systèmes
                     data = d1 %>% add_column (atp_se = atp_se),
                     family = gaussian(), 
                     prior = c(#set_prior('normal(2, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 12),
                     iter = 6000,
                     warmup = 2000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_atp)
pp_check(mod_atp, ndraws = 100)
pp_check(mod_atp, type = 'error_hist',  ndraws = 10)
pp_check(mod_atp, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# effet de l'hauteur relative de l'entaille ---------------------------––------- 
plot(conditional_effects(mod_atp)) [[1]] + 
  scale_x_continuous(name = "Relative spout height (cm)") +
  scale_y_continuous(name = "Log (ATP)", 
                     limits = c(0, 10)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
  
# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_atp)
summary(mod_atp)$fixed
ranef(mod_atp)$systeme [, , "Intercept"]
ranef(mod_atp)$systeme:ligne [, , "Intercept"]
ranef(mod_atp)$année [, , "Intercept"]
ranef(mod_atp)$`année:date` [, , "Intercept"]

# effet de l'hauteur de l'entaille sur la contamination microbienne ------------
mod_atp2 <- brms::brm(brms::bf(log(atp) | mi(atp_se) ~ 
                                (1 | t) +               # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                                (1 | année / date) +    # différence par date
                                (1 | systeme / ligne)), # difference antre systèmes
                     data = d1 %>% add_column (atp_se = atp_se),
                     family = gaussian(), 
                     prior = c(#set_prior('normal(2, 10)', class = 'Intercept'),
                       set_prior('exponential(1)', class = 'sigma')),
                     cores = 4, chains = 4,
                     control = list(adapt_delta = 0.9, max_treedepth = 12),
                     iter = 6000,
                     warmup = 2000,
                     seed = 1353,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_atp2)
pp_check(mod_atp2, ndraws = 100)
pp_check(mod_atp2, type = 'error_hist',  ndraws = 10)
pp_check(mod_atp2, type = 'scatter_avg', ndraws = 100)
# l'erreur de la distribution postérieur ne semble pas être distribuée normalement

# extract intercept ------------------------------------------------------------
intercept_atp <- posterior_summary(mod_atp2, variable = "b_Intercept")

# effet de l'hauteur relative de l'entaille ---------------------------––-------
mod_atp2 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = exp(b_Intercept + r_t)) %>%
  ggplot(aes(y = factor(t, levels = c("b3", "b2", "b1", "h1", "h2", "h3")), 
             x = t_mean)) + 
  scale_x_continuous(name = "ATP (count)", 
                     limits = c(0, 7000)) +
  # scale_y_discrete(name ="Relative spout height (Inches)", 
  #                  labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
  #                           "+12\"", "+24\"")) +
  scale_y_discrete(name = "Relative spout height (cm)", 
                   labels = c("-60", "-30", "-10", "+10", "+30", "+60")) +
  stat_halfeye(aes(fill = t, color = t), 
               adjust = 0.5, 
               width = 0.6, 
               .width = c(0.5, 0.8, 0.95)) +
  scale_fill_manual(values = c("b1" = "#40004b99", "b2" = "#9970ab99", 
                               "b3" = "#c2a5cf99", "h1" = "#00441b99",  
                               "h2" = "#5aae6199", "h3" = "#a6dba099")) +
  scale_color_manual(values = c("b1" = "#333333", "b2" = "#333333", 
                                "b3" = "#333333", "h1" = "#333333",
                                "h2" = "#333333", "h3" = "#333333")) +
  theme(legend.position = "none")  +
  geom_vline(xintercept = exp(intercept_atp[1, 1]), 
             color = "darkgrey", linetype = "dashed", size = 1)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_atp2)
summary(mod_atp2)$fixed
ranef(mod_atp2)$t [, , "Intercept"]
ranef(mod_atp2)$systeme [, , "Intercept"]
ranef(mod_atp2)$systeme:ligne [, , "Intercept"]
ranef(mod_atp2)$année [, , "Intercept"]
ranef(mod_atp2)$`année:date` [, , "Intercept"]

# compare les deux modèles -----------------------------------------------------
loo_atp1 <- loo(mod_atp)
loo_atp2 <- loo(mod_atp2)
loo_compare(loo_atp1, loo_atp2)

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_ph <- brms::brm(brms::bf(ph | mi(ph_se) ~ 
                             #ph ~
                               h +                     # hauteur de l'entaille 
                               (1 | année / date) +    # différence par date
                               (1 | systeme / ligne)), # difference entre systèmes
                    data = d1 %>% select(-c(brix, datetime, atp, sc)) %>% 
                      add_column(ph_se = ph_se),
                    family = gaussian(), 
                    prior = c(set_prior('normal(5, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma'),
                              set_prior('normal(0, 2)', class = 'b')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12),
                    iter = 6000,
                    warmup = 2000,
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
ranef(mod_ph)$année [, , 'Intercept']
# Notes : Le pH semble legèrement plus haut avec des entailles plus hauts. 
# Pourtant, ce n'est pas significatif. Je n'aurais pas de bonne explication 
# pour une telle tendance, non plus. 

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_ph2 <- brms::brm(brms::bf(ph | mi(ph_se) ~ 
                               #ph ~
                               (1 | t ) +              # hauteur de l'entaille 
                               (1 | année / date) +    # différence par date
                               (1 | systeme / ligne)), # difference entre systèmes
                    data = d1 %>% select(-c(brix, datetime, atp, sc)) %>% 
                      add_column(ph_se = ph_se),
                    family = gaussian(), 
                    prior = c(set_prior('normal(5, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12),
                    iter = 6000,
                    warmup = 2000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_ph2)
pp_check(mod_ph2, ndraws = 100)
pp_check(mod_ph2, type = 'error_hist',  ndraws = 10)
pp_check(mod_ph2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 
# il n'y a pas de données de ph entre 6,4 et 7,0, ce que semble bizarre.

# extract intercept ------------------------------------------------------------
intercept_ph <- posterior_summary(mod_ph2, variable = "b_Intercept")

# effet de l'hauteur relative de l'entaille ---------------------------––-------
mod_ph2 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>%
  ggplot(aes(y = factor (t, levels = c ("b3", "b2", "b1", "h1", "h2", "h3")), 
             x = t_mean)) + 
  scale_x_continuous(name = "pH", 
                     limits = c(3.5, 8.5)) +
  # scale_y_discrete(name ="Relative spout height (Inches)", 
  #                  labels=c("-24\"", "-12\"", "-8\"", "-4\"", "+4\"", "+8\"",
  #                           "+12\"", "+24\"")) +
  scale_y_discrete(name = "Relative spout height (cm)", 
                   labels = c("-60", "-30", "-10", "+10", "+30", "+60")) +
  stat_halfeye(aes(fill = t, color = t), 
               adjust = 0.5, 
               width = 0.6, 
               .width = c(0.5, 0.8, 0.95)) +
  scale_fill_manual(values = c("b1" = "#40004b99", "b2" = "#9970ab99", 
                               "b3" = "#c2a5cf99", "h1" = "#00441b99",
                               "h2" = "#5aae6199", "h3" = "#a6dba099")) +
  scale_color_manual(values = c("b1" = "#333333", "b2" = "#333333", 
                                "b3" = "#333333", "h1" = "#333333", 
                                "h2" = "#333333", "h3" = "#333333")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = intercept_ph[1, 1], 
             color = "darkgrey", linetype = "dashed", size = 1)
  

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_ph2)
summary(mod_ph2)$fixed
ranef(mod_ph2)$t [, , 'Intercept']
ranef(mod_ph2)$systeme [, , 'Intercept']
ranef(mod_ph)$année [, , 'Intercept']
# Notes :  Il ne semble pas avoir un effect systématique avec la hauteur 
# relative de l'entaille.

# compare les deux modèles -----------------------------------------------------
loo_ph1 <- loo(mod_ph)
loo_ph2 <- loo(mod_ph2)
loo_compare(loo_ph1, loo_ph2)

# effet de l'hauteur de l'entaille sur le profile des sucres -------------------
mod_sc <- brms::brm(brms::bf(sc ~ 
                             h +                     # hauteur de l'entaille 
                             (1 | année / date) +    # différence par date
                             (1 | systeme / ligne)), # difference antre systèmes
                    data = d1,
                    family = gaussian(), 
                    prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma'),
                              set_prior('normal(0, 2)', class = 'b')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 13),
                    warmup = 2000, 
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_sc)
pp_check(mod_sc, ndraws = 100)
pp_check(mod_sc, type = 'error_hist',  ndraws = 10)
pp_check(mod_sc, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_sc)
summary(mod_sc)$fixed
ranef(mod_sc)$systeme [, , 'Intercept']
ranef(mod_sc)$`systeme:ligne` [, , 'Intercept']
ranef(mod_sc)$`année:date` [, , 'Intercept']

# commentaires -----------------------------------------------------------------
# L'effet est de 0.00061 [0.00033; 0.00088], donc relativement petit.

# effet de l'hauteur de l'entaille sur le profile des sucres -------------------
mod_sc2 <- brms::brm(brms::bf(sc ~ 
                               (1 | t) +                     # hauteur de l'entaille 
                               (1 | année / date) +    # différence par date
                               (1 | systeme / ligne)), # difference antre systèmes
                    data = d1,
                    family = gaussian(), 
                    prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                              set_prior('exponential(1)', class = 'sigma')),
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 13),
                    warmup = 2000, 
                    iter = 6000,
                    seed = 1353,
                    backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_sc2)
pp_check(mod_sc2, ndraws = 100)
pp_check(mod_sc2, type = 'error_hist',  ndraws = 10)
pp_check(mod_sc2, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_sc2)
summary(mod_sc2)$fixed
ranef(mod_sc2)$t [, , 'Intercept']
ranef(mod_sc2)$systeme [, , 'Intercept']
ranef(mod_sc2)$`systeme:ligne` [, , 'Intercept']
ranef(mod_sc2)$`année:date` [, , 'Intercept']

# compare les deux modèles -----------------------------------------------------
loo_sc1 <- loo(mod_sc)
loo_sc2 <- loo(mod_sc2)
loo_compare(loo_sc1, loo_sc2)
