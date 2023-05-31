# dépendances ------------------------------------------------------------------
if(!existsFunction('brms')) library('brms')
if(!existsFunction('pp_check')) library('rstanarm')
if(!existsFunction('%>%')) library('tidyverse')

# lire les données -------------------------------------------------------------
if (!exists('d')) source('01_lire_données.R')

# rendement par hauteur et systeme ---------------------------------------------
par(mar = c(5, 5, 1, 2))
plot(x = d$date, y = d$rendement, 
     xlab = 'Date', ylab = 'Rendement par entaille (litres)', axes = FALSE, 
     col = 'white', ylim = c(0, 12))
axis(side = 1, at = c(as_date('2023-03-20'), as_date('2023-03-27'), 
                      as_date('2023-04-03'), as_date('2023-04-10'), 
                      as_date('2023-04-17')),
     labels = c('20 mar', '27 mar', '3 avr', '10 avr', '17 avr'))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$h == info$h[i] & d$systeme == info$systeme[i]
  points(d$date[con], y = d$rendement[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
legend(x = as_date('2023-04-12'), y = 12, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('#008837', 'white', 'white', '#7b3294'),
       pt.bg = c('#008837', 'white', 'white', '#7b3294'),
       pch = 21, title = 'A')
legend(x = as_date('2023-04-13'), y = 12, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('white', 'white', '#c2a5cf', '#7b3294'),
       pt.bg = c('white', 'white', '#c2a5cf', '#7b3294'),
       pch = 22, title = 'B')
legend(x = as_date('2023-04-14'), y = 12, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('#008837', '#a6dba0', 'white', 'white'),
       pt.bg = c('#008837', '#a6dba0', 'white', 'white'),
       pch = 23, title = 'C')
legend(x = as_date('2023-04-15'), y = 12, bg = 'transparent', box.lty = 0,
       legend = c('+24"',  '  +4"', '   -4"', ' -24"'), 
       col = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pt.bg = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pch = 24, title = 'E')

# L'effet de l'hauteur de l'entaille sur le rendement --------------------------
mod_vol <- brms::brm(brms::bf(rendement ~ 
                             h +             # hauteur de l'entaille 
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

# additional posterior distribution checks -------------------------------------
plot(mod_vol)
pp_check(mod_vol, ndraws = 100)
pp_check(mod_vol, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol, type = 'scatter_avg', ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod_vol)
ranef(mod_vol)$systeme [, , 'Intercept']
ranef(mod_vol)$date [, , 'Intercept']

# rendement par hauteur et systeme ---------------------------------------------
par(mar = c(5, 5, 1, 2))
plot(x = d$date, y = d$brix, 
     xlab = 'Date', 
     ylab = expression(paste('Teneur en sucre (',degree,'Brix)', sep = '')), 
     axes = FALSE, 
     col = 'white', ylim = c(0, 4))
axis(side = 1, at = c(as_date('2023-03-20'), as_date('2023-03-27'), 
                      as_date('2023-04-03'), as_date('2023-04-10'), 
                      as_date('2023-04-17')),
     labels = c('20 mar', '27 mar', '3 avr', '10 avr', '17 avr'))
axis(side = 2, las = 1)
for (i in 1:dim(info)[1]){
  con <- d$h == info$h[i] & d$systeme == info$systeme[i]
  points(d$date[con], y = d$brix[con], pch = info$sym[i], col = info$colour[i], 
         bg = info$colour[i])
}
legend(x = as_date('2023-04-12'), y = 4, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('#008837', 'white', 'white', '#7b3294'),
       pt.bg = c('#008837', 'white', 'white', '#7b3294'),
       pch = 21, title = 'A')
legend(x = as_date('2023-04-13'), y = 4, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('white', 'white', '#c2a5cf', '#7b3294'),
       pt.bg = c('white', 'white', '#c2a5cf', '#7b3294'),
       pch = 22, title = 'B')
legend(x = as_date('2023-04-14'), y = 4, bg = 'transparent', box.lty = 0,
       legend = rep('',  4), col = c('#008837', '#a6dba0', 'white', 'white'),
       pt.bg = c('#008837', '#a6dba0', 'white', 'white'),
       pch = 23, title = 'C')
legend(x = as_date('2023-04-15'), y = 4, bg = 'transparent', box.lty = 0,
       legend = c('+24"',  '  +4"', '   -4"', ' -24"'), 
       col = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pt.bg = c('#008837', '#a6dba0', '#c2a5cf', '#7b3294'),
       pch = 24, title = 'E')

# L'effet de l'hauteur de l'entaille sur le rendement --------------------------
mod_brix <- brms::brm(brms::bf(brix ~ 
                             h +             # hauteur de l'entaille 
                             (1 | date) +    # différence par date
                             (1 | systeme)), # difference antre systèmes
                  data = d,
                  family = gaussian(), 
                  prior = c(set_prior('normal(2, 10)', class = 'Intercept'),
                            set_prior('exponential(1)', class = 'sigma'),
                            set_prior('normal(0, 2)', class = 'b')),
                  cores = 4, chains = 4,
                  control = list(adapt_delta = 0.99),
                  iter = 6000,
                  seed = 1353,
                  backend = 'cmdstanr')

# additional posterior distribution checks -------------------------------------
plot(mod_brix)
pp_check(mod_brix, ndraws = 100)
pp_check(mod_brix, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix, type = 'scatter_avg', ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod_brix)
ranef(mod_brix)$systeme [, , 'Intercept']
ranef(mod_brix)$date [, , 'Intercept']
