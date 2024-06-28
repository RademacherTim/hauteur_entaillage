# Dépendances ------------------------------------------------------------------
if (!existsFunction("spread_draws")) library("tidybayes")

# Plot posterior distributions of vaccum against volume ------
vol23 <- mod_vol23_SN %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>% 
  group_by(t) %>% 
  summarise(mean = mean(t_mean),
            qi_l = qi(t_mean)[1],
            qi_u = qi(t_mean)[2]) %>% 
  add_column(année = 2023)
vol24 <-  mod_vol24_SN %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>% 
  group_by(t) %>% 
  summarise(mean = mean(t_mean),
            qi_l = qi(t_mean)[1],
            qi_u = qi(t_mean)[2]) %>% 
  add_column(année = 2024)
vide23 <-  mod_v23 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>% 
  group_by(t) %>% 
  summarise(mean = mean(t_mean),
            qi_l = qi(t_mean)[1],
            qi_u = qi(t_mean)[2]) %>% 
  add_column(année = 2023)
vide24 <- mod_v24 %>% spread_draws(b_Intercept, r_t[t, ]) %>% 
  mutate(t_mean = b_Intercept + r_t) %>% 
  group_by(t) %>% 
  summarise(mean = mean(t_mean),
            qi_l = qi(t_mean)[1],
            qi_u = qi(t_mean)[2]) %>% 
  add_column(année = 2024)

par(mar=c(5, 5, 1, 1))
plot(x = 1, y = 1,
     xlim = c(-20.5, -25.5), ylim = c(0, 4.5), pch = 23, 
     col = "white", axes = "FALSE",
     bg = "white", lwd = 2, 
     xlab = "Sous-vide (\" Hg)", 
     ylab = "Rendement par coulée par entaille (litres)", cex = 1.5)
axis(side = 1)
axis (side = 2, las = 1)
# erreur sous-vide ---------
arrows(x0 = -exp(vide23$qi_l), 
       y0 = exp(vol23$mean), 
       x1 = -exp(vide23$qi_u), 
       y1 = exp(vol23$mean), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"))
# erreur rendement ---------
arrows(x0 = -exp(vide23$mean), 
       y0 = exp(vol23$qi_l), 
       x1 = -exp(vide23$mean), 
       y1 = exp(vol23$qi_u), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"))
points(x = -exp(vide23$mean), y = exp(vol23$mean), pch = 23, 
     col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"), 
     bg = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"), lwd = 2, 
     cex = 2)
# erreur sous-vide -----
arrows(x0 = -exp(vide24$qi_l), 
       y0 = exp(vol24$mean), 
       x1 = -exp(vide24$qi_u), 
       y1 = exp(vol24$mean), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"))
# erreur rendement -----
arrows(x0 = -exp(vide24$mean), 
       y0 = exp(vol24$qi_l), 
       x1 = -exp(vide24$mean), 
       y1 = exp(vol24$qi_u), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"))
# points
points(x = -exp(vide24$mean), y = exp(vol24$mean), pch = 21, lwd = 2, 
       bg = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"),
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"), cex = 2)
legend(x = -23.6, y = 1.2, bg = "transparent",
       box.lty = 0, lty = 0, cex = 1.3,
       legend = c("+24\"",  "  +4\"", "   -4\"", " -24\""), 
       col = c("#c2a5cf","#40004b", "#00441b", "#a6dba0"),
       pt.bg = c("#c2a5cf","#40004b", "#00441b", "#a6dba0"),
       lwd = 2, pch = 23, title = "2023")
legend(x = -24.5, y = 1.2, bg = "transparent",
       box.lty = 0, lty = 0, cex = 1.3,
       legend = c("+24\"",  "+12\"", " -12\"", " -24\""), 
       col = c("#c2a5cf","#762a83", "#1b7837", "#a6dba0"),
       pt.bg = c("#c2a5cf","#762a83", "#1b7837", "#a6dba0"),
       lwd = 2, pch = 21, title = "2024")

# graphique du temps de coulée effectif --------------------
par(mfrow = c(1, 1))
plot(x = 1, y = 1,
     xlim = c(10, 32), ylim = c(0, 4.5), pch = 23, 
     col = "white", axes = "FALSE",
     bg = "white", lwd = 2, 
     xlab = "Temps de coulée effectif (Jours)", 
     ylab = "Rendement par coulée par entaille (litres)", cex = 1.5)
axis(side = 1)
axis (side = 2, las = 1)
# erreur sous-vide ---------
arrows(x0 = tce$mean[tce$année == 2023] - tce$sd[tce$année == 2023], 
       y0 = exp(vol23$mean), 
       x1 = tce$mean[tce$année == 2023] + tce$sd[tce$année == 2023], 
       y1 = exp(vol23$mean), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"))
# erreur rendement ---------
arrows(x0 = tce$mean[tce$année == 2023], 
       y0 = exp(vol23$qi_l), 
       x1 = tce$mean[tce$année == 2023], 
       y1 = exp(vol23$qi_u), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"))
points(x = tce$mean[tce$année == 2023], y = exp(vol23$mean), pch = 23, 
       col = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"), 
       bg = c("#40004b", "#c2a5cf", "#00441b", "#a6dba0"), lwd = 2, 
       cex = 2)
# erreur sous-vide ---------
arrows(x0 = tce$mean[tce$année == 2024] - tce$sd[tce$année == 2024], 
       y0 = exp(vol24$mean), 
       x1 = tce$mean[tce$année == 2024] + tce$sd[tce$année == 2024], 
       y1 = exp(vol24$mean), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"))
# erreur rendement ---------
arrows(x0 = tce$mean[tce$année == 2024], 
       y0 = exp(vol24$qi_l), 
       x1 = tce$mean[tce$année == 2024], 
       y1 = exp(vol24$qi_u), angle = 90, code = 3, length = 0.1, lwd = 2,
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"))
points(x = tce$mean[tce$année == 2024], y = exp(vol24$mean), pch = 21, 
       col = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"), 
       bg = c("#762a83", "#c2a5cf", "#1b7837", "#a6dba0"), lwd = 2, 
       cex = 2)
legend(x = 24, y = 1.2, bg = "transparent",
       box.lty = 0, lty = 0, cex = 1.3,
       legend = c("+24\"",  "  +4\"", "   -4\"", " -24\""), 
       col = c("#a6dba0", "#00441b", "#40004b", "#c2a5cf"),
       pt.bg = c("#a6dba0", "#00441b", "#40004b", "#c2a5cf"),
       lwd = 2, pch = 23, title = "2023")
legend(x = 28, y = 1.2, bg = "transparent",
       box.lty = 0, lty = 0, cex = 1.3,
       legend = c("+24\"",  "+12\"", " -12\"", " -24\""), 
       col = c("#a6dba0", "#1b7837", "#762a83", "#c2a5cf"),
       pt.bg = c("#a6dba0", "#1b7837", "#762a83", "#c2a5cf"),
       lwd = 2, pch = 21, title = "2024")
