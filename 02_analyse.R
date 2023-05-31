# dépendances ------------------------------------------------------------------
if(!existsFunction('brms')) library('brms')
if(!existsFunction('%>%')) library('tidyverse')

# lire les données -------------------------------------------------------------
if (!exists('d')) source('01_lire_données.R')

# rendement par systeme --------------------------------------------------------
par(mar = c(5, 5, 1, 1))
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
mod1 <- brms::brm(brms::bf(rendement ~ 
                             h +             # hauteur de l'entaille 
                             (1 | date) +    # différence par date
                             (1 | systeme)), # difference antre systèmes
                      data = d,
                      family = gaussian(), 
                      prior = c(set_prior("normal(3, 10)", class = "Intercept"),
                                set_prior("exponential(1)", class = "sigma")),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.9), #max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")
summary(mod1)

