# dépendances ------------------------------------------------------------------
if(!existsFunction('brms')) library('brms')
if(!existsFunction('pp_check')) library('rstanarm')
if(!existsFunction('%>%')) library('tidyverse')

# lire les données -------------------------------------------------------------
if (!exists('d')) source('01_lire_données.R')

# erreur des instruments -------------------------------------------------------
atp_se   <- 5      # Hygiena SureSystem II 5% ou 5 RLUs
ph_se    <- 0.01   # Atago Pal-Maple (0-85%)
brix1_se <- 0.2    # Extech ExStick 

# rendement par hauteur et systeme ---------------------------------------------
par(mar = c(5, 5, 1, 2))
plot(x = d$datetime, y = d$rendement, 
     xlab = 'Date', ylab = 'Rendement par entaille (litres)', axes = FALSE, 
     col = 'white', xlim = as_datetime(c('2023-03-19', '2023-04-17')), ylim = c(0, 12))
axis(side = 1, at = as_datetime(c('2023-03-20', '2023-03-27', '2023-04-03', 
                                  '2023-04-10', '2023-04-17')),
     labels = c('20 mar', '27 mar', '3 avr', '10 avr', '17 avr'))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$h == info$h[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$rendement[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
legend(x = as_datetime('2023-04-11 12:00:00'), y = 12, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('#008837', 'white', 'white', '#7b3294'),
       pt.bg = c('#008837', 'white', 'white', '#7b3294'),
       pch = 21, title = 'A')
legend(x = as_datetime('2023-04-12 12:00:00'), y = 12, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('white', 'white', '#c2a5cf', '#7b3294'),
       pt.bg = c('white', 'white', '#c2a5cf', '#7b3294'),
       pch = 22, title = 'B')
legend(x = as_datetime('2023-04-13 12:00:00'), y = 12, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('#008837', '#a6dba0', 'white', 'white'),
       pt.bg = c('#008837', '#a6dba0', 'white', 'white'),
       pch = 23, title = 'C')
legend(x = as_datetime('2023-04-14 12:00:00'), y = 12, bg = 'transparent', 
       box.lty = 0, legend = c('+24"',  '  +4"', '   -4"', ' -24"'), 
       col = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pt.bg = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pch = 24, title = 'E')

# L'effet de l'hauteur de l'entaille sur le rendement --------------------------
mod_vol <- brms::brm(brms::bf(rendement ~ 
                             t +             # hauteur de l'entaille (t pour catégorique et h pour gradient)
                             (1 | date) +    # différence par date
                             (1 | systeme)), # difference antre systèmes
                      data = d,
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

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol)
ranef(mod_vol)$systeme [, , 'Intercept']
ranef(mod_vol)$date [, , 'Intercept']

# teneur en sucre par hauteur et système ---------------------------------------
par(mar = c(5, 5, 1, 2))
plot(x = d$datetime, y = d$brix, 
     xlab = 'Date', 
     ylab = expression(paste('Teneur en sucre (',degree,'Brix)', sep = '')), 
     axes = FALSE, 
     col = 'white', 
     xlim = as_datetime(c('2023-03-19', '2023-04-17')), ylim = c(0, 4))
axis(side = 1, at = as_datetime(c('2023-03-20', '2023-03-27', '2023-04-03', 
                                  '2023-04-10', '2023-04-17')),
     labels = c('20 mar', '27 mar', '3 avr', '10 avr', '17 avr'))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$h == info$h[i] & d$systeme == info$systeme[i]
  points(x = d$datetime[con] + 
           hours(sample(-4:4, size = length(d$datetime[con]), replace = TRUE)), 
         y = d$brix[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
legend(x = as_datetime('2023-04-12 12:00:00'), y = 4, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('#008837', 'white', 'white', '#7b3294'),
       pt.bg = c('#008837', 'white', 'white', '#7b3294'),
       pch = 21, title = 'A')
legend(x = as_datetime('2023-04-13 12:00:00'), y = 4, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('white', 'white', '#c2a5cf', '#7b3294'),
       pt.bg = c('white', 'white', '#c2a5cf', '#7b3294'),
       pch = 22, title = 'B')
legend(x = as_datetime('2023-04-14 12:00:00'), y = 4, bg = 'transparent', 
       box.lty = 0, legend = rep('',  4), 
       col = c('#008837', '#a6dba0', 'white', 'white'),
       pt.bg = c('#008837', '#a6dba0', 'white', 'white'),
       pch = 23, title = 'C')
legend(x = as_datetime('2023-04-15 12:00:00'), y = 4, bg = 'transparent', 
       box.lty = 0,
       legend = c('+24\"',  '  +4"', '   -4"', ' -24"'), 
       col = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pt.bg = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pch = 24, title = 'E')

# effet de l'hauteur de l'entaille sur le teneur en sucre ----------------------
mod_brix1 <- brms::brm(brms::bf(brix | mi(brix1_se) ~ 
                             t +             # hauteur de l'entaille  (t pour catégorique et h pour gradient)
                             (1 | date) +    # différence par date
                             (1 | systeme)), # difference antre systèmes
                       data = d %>% add_column(brix1_se = brix1_se) %>% 
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

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix1)
ranef(mod_brix1)$systeme [, , 'Intercept']
ranef(mod_brix1)$date [, , 'Intercept']

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