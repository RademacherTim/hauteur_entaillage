# dépendances ------------------------------------------------------------------
if (!existsFunction('brms')) library('brms')
if (!existsFunction('pp_check')) library('rstanarm')
if (!existsFunction('%>%')) library('tidyverse')

# lire les données -------------------------------------------------------------
if (!exists('d')) source('01_lire_données.R')

# erreur des instruments -------------------------------------------------------
atp_se   <- 5      # Hygiena SureSystem II 5% ou 5 RLUs
ph_se    <- 0.01   # Extech ExStick 
brix1_se <- 0.2    # Atago Pal-Maple (0-85%)

# rendement par hauteur et systeme ---------------------------------------------
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
for (i in 1:dim(info)[1]){
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
       col = c("#00441b", "#1b7837", "#a6dba0", "#c2a5cf", "#762a83", "#40004b"),
       pt.bg = c("#00441b", "#1b7837", "#a6dba0", "#c2a5cf", "#762a83","#40004b"),
       pch = c(2, 24, 1, 5, 25, 6))
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
for (i in 1:dim(info)[1]){
  con <- d$t == info$t[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$rendement[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
text(x = as_datetime("2024-04-17"), y = 12, labels = "2024", cex = 1)

# L'effet de l'hauteur de l'entaille sur le rendement --------------------------
mod_vol <- brms::brm(brms::bf(rendement ~ 
                              h +                     # effet de l'hauteur de l'entaille 
                              (1 | année / date) +    # différence entre année et date
                              (1 | systeme / ligne) + # différence entre systèmes et ligne
                              (1 | site)),            # effet du site
                      data = d, #%>% filter(site == "SN"),
                      family = gaussian(), 
                      prior = c(set_prior('normal(3, 10)', class = 'Intercept'),
                                set_prior('exponential(1)', class = 'sigma'),
                                set_prior('normal(0, 2)', class = 'b')),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.9),
                      iter = 6000,
                      seed = 1353,
                      backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vol)
pp_check(mod_vol, ndraws = 100)
pp_check(mod_vol, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# effet de l'hauteur relative de l'entaille ---------------------------––-------
plot(conditional_effects(mod_vol)) [[1]]
# L'effet est petit mais important, car chaque centimetres, entaille et coulée 
# ont un effet. En somme, l'effet de 0.002 L par entaille par coulée par cm est 
# important. Par exemple, pour une saison de 15 coulée faire l'entaille 50 cm 
# plus haut donne 1.5L plus de sève par entaille. 

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol)
ranef(mod_vol)$site
ranef(mod_vol)$année [, , 'Intercept']
ranef(mod_vol)$`année:date`[, , 'Intercept']
ranef(mod_vol)$systeme [, , 'Intercept']
ranef(mod_vol)$`systeme:ligne`[, , "Intercept"]

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
mod_brix1 <- brms::brm(brms::bf(brix | mi(brix1_se) ~ 
                             h +                     # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                             (1 | année / date) +    # différence entre année et date
                             (1 | systeme / ligne)), # difference entre systèmes et lignes
                       data = d %>% add_column(brix1_se = brix1_se) %>% 
                         filter(site == "SN") %>%
                         select(-rendement, -datetime),
                       family = gaussian(), 
                       prior = c(set_prior('normal(2, 5)', class = 'Intercept'),
                                 set_prior('exponential(1)', class = 'sigma'),
                                 set_prior('normal(0, 2)', class = 'b')),
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

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix1)
ranef(mod_brix1)$site
ranef(mod_brix1)$année [, , 'Intercept']
ranef(mod_brix1)$`année:date`[, , 'Intercept']
ranef(mod_brix1)$systeme [, , 'Intercept']
ranef(mod_brix1)$`systeme:ligne`[, , "Intercept"]

# effet de l'hauteur de l'entaille sur le teneur en sucre ----------------------
mod_brix2 <- brms::brm(brms::bf(brix ~ 
                                 h +             # hauteur de l'entaille 
                                 (1 | date) +    # différence par date
                                 (1 | systeme)), # difference antre systèmes
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
ranef(mod_brix2)$systeme [, , 'Intercept']
ranef(mod_brix2)$date [, , 'Intercept']

# effet de l'hauteur de l'entaille sur la contamination microbienne ------------
# TR - Need to look at whether a lognormal distribution is really the best fit.
# It almost looks uniformly distributed.
mod_atp <- brms::brm(brms::bf(log(atp) ~ 
                                  t +             # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                                  (1 | date) +    # différence par date
                                  (1 | systeme)), # difference antre systèmes
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
ranef(mod_atp)$systeme [, , 'Intercept']
ranef(mod_atp)$date [, , 'Intercept']

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_ph <- brms::brm(brms::bf(ph | mi(ph_se) ~ 
                               t +             # hauteur de l'entaille 
                               (1 | date) +    # différence par date
                               (1 | systeme)), # difference antre systèmes
                    data = d1 %>% select(-c(brix, datetime, atp, sc)) %>% 
                      add_column(ph_se = ph_se),
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
plot(mod_ph)
pp_check(mod_ph, ndraws = 100)
pp_check(mod_ph, type = 'error_hist',  ndraws = 10)
pp_check(mod_ph, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement, mais 
# il n'y a pas de données de ph entre 6,4 et 7,0, ce que semble bizarre

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_ph)
ranef(mod_ph)$systeme [, , 'Intercept']
ranef(mod_ph)$date [, , 'Intercept']

# effet de l'hauteur de l'entaille sur le pH -----------------------------------
mod_sc <- brms::brm(brms::bf(sc ~ 
                               t +             # hauteur de l'entaille 
                               (1 | date) +    # différence par date
                               (1 | systeme)), # difference antre systèmes
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
ranef(mod_sc)$systeme [, , 'Intercept']
ranef(mod_sc)$date [, , 'Intercept']
# Il semble avoir approximativement le même effet que avec le brix mesuré avec 
# un refractomètre.