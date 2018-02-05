
## DATENAUFBEREITUNG DPLYR() ####
tb_subset <- 
  tb_jets_month %>%
  filter(method == "Dijkstra" & class == "STJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra_STJ = lat) %>%
  arrange(dts, lon)

tb_subset[,7] <- 
  tb_jets_month %>%
  filter(method == "Dijkstra" & class == "PFJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra_PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra_PFJ)

tb_subset[,8] <- 
  tb_jets_month %>%
  filter(method == "Dijkstra" & class == "SJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra_SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra_SJ)

tb_subset[,9] <- 
  tb_jets_month %>%
  filter(method == "Chebyshev" & class == "STJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev_STJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev_STJ)

tb_subset[,10] <- 
  tb_jets_month %>%
  filter(method == "Chebyshev" & class == "PFJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev_PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev_PFJ)

tb_subset[,11] <- 
  tb_jets_month %>%
  filter(method == "Chebyshev" & class == "SJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev_SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev_SJ)

tb_subset[,12] <- 
  tb_jets_month %>%
  filter(method == "Chebyshev" & class == "MJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev_MJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev_MJ)

tb_subset[,13] <- 
  tb_jets_month %>%
  filter(method == "Maximum" & class == "MJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Maximum_MJ = lat) %>%
  arrange(dts, lon) %>%
  select(Maximum_MJ)

## KORRELATIONEN ZWISCHEN CHEBYSHEV- UND DIJKSTRA-METHODE ####
## 

ks.test(tb_subset$Dijkstra_STJ, tb_subset$Chebyshev_STJ) %>% tidy

ks.test(tb_subset$Dijkstra_STJ, "pnorm") %>% tidy
# Gesamt
cor.test(x = tb_subset$Chebyshev_PFJ,
         y = tb_subset$Dijkstra_PFJ) %>%
  tidy

# MAM
tb_subset %>% # Polarfront-Jetstream
  filter(season == "MAM") %$%
  cor.test(Chebyshev_PFJ, Dijkstra_PFJ) %>%
  tidy
tb_subset %>% # Subtropen-Jetstream
  filter(season == "MAM") %$%
  cor.test(Chebyshev_STJ, Dijkstra_STJ) %>%
  tidy

# JJA
tb_subset %>% # Polarfront-Jetstream
  filter(season == "JJA") %$%
  cor.test(Chebyshev_PFJ, Dijkstra_PFJ) %>%
  tidy
tb_subset %>% # Subtropen-Jetstream
  filter(season == "JJA") %$%
  cor.test(Chebyshev_STJ, Dijkstra_STJ) %>%
  tidy

# SON
tb_subset %>% # Polarfront-Jetstream
  filter(season == "SON") %$%
  cor.test(Chebyshev_PFJ, Dijkstra_PFJ) %>%
  tidy
tb_subset %>% # Subtropen-Jetstream
  filter(season == "SON") %$%
  cor.test(Chebyshev_STJ, Dijkstra_STJ) %>%
  tidy

# DJF
tb_subset %>% # Polarfront-Jetstream
  filter(season == "DJF") %$%
  cor.test(Chebyshev_PFJ, Dijkstra_PFJ) %>%
  tidy
tb_subset %>% # Subtropen-Jetstream
  filter(season == "DJF") %$%
  cor.test(Chebyshev_STJ, Dijkstra_STJ) %>%
  tidy


## VERGLEICH VERSCHIEDENER JETS NACH METHODEN ####
##

## Maximum Jetstream
ggp_comp_mj_pt <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_MJ, y = Maximum_MJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Maximum}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(5, 85), ylim = c(5,85)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp_comp_mj_cont <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_MJ, y = Maximum_MJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Maximum}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 60), ylim = c(15,60)) + 
  theme_bw() + theme(legend.position = "bottom")


## Polarfrontjetstreams
## 
ggp_comp_pfj_pt <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_PFJ, y = Dijkstra_PFJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                              direction = "horizontal",
                              label.position = "bottom",
                              nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp_comp_pfj_cont <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_PFJ, y = Dijkstra_PFJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw() + theme(legend.position = "bottom")

## Subtropenjetstreams
## 
ggp_comp_stj_pt <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_STJ, y = Dijkstra_STJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp_comp_stj_cont <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_STJ, y = Dijkstra_STJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")


## Single-Jetstreams
## 
ggp_comp_sj_pt <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_SJ, y = Dijkstra_SJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp_comp_sj_cont <-
  ggplot(data = tb_subset, mapping = aes(x = Chebyshev_SJ, y = Dijkstra_SJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")
  

# ## SPEICHERN DER PLOTS ####
# ## 
# 
# save_plot(plt = ggp_comp.mj.pt, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "MJ_scat")
# save_plot(plt = ggp_comp.mj.cont, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "MJ_cont")
# 
# save_plot(plt = ggp_comp.pfj.pt, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "PFJ_scat")
# save_plot(plt = ggp_comp.pfj.cont, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "PFJ_cont")
# 
# save_plot(plt = ggp_comp.stj.pt, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "STJ_scat")
# save_plot(plt = ggp_comp.stj.cont, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "STJ_cont")
# 
# save_plot(plt = ggp_comp.sj.pt, 
#          width = 90, height = 65, pointsize = 11,
#          filepath = paste0(save_dir, "03-comp/"), 
#          filename = "SJ_scat")
# save_plot(plt = ggp_comp.sj.cont, 
#          width = 100, height = 100, pointsize = 11,
#          filepath = paste0(save_dir, "/03-comp"), 
#          filename = "SJ_cont")

## Jahreszeitweise Plot mit points/jitter + density_2d
## 
ssn <- c("DJF", "MAM", "JJA", "SON")
colour_season <- c("#2c7bb6", "#fdae61", "#d7191c", "#abd9e9")
for (i_season in seq_along(ssn)) {
  print(i_season)
  
  ## Polarfrontjetstreams
  ## 
  ggp_comp_pfj_pt_cont <-
    ggplot(data = tb_subset %>%
             filter(season == ssn[i_season]), 
           mapping = aes(x = Chebyshev_PFJ, y = Dijkstra_PFJ)) + 
    geom_jitter(colour = colour_season[i_season], size = 0.2) + geom_density_2d(colour = "black") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
    labs(colour = "Jahreszeit") +
    guides(colour = guide_legend(title.position = "top",
                                 direction = "horizontal",
                                 label.position = "bottom",
                                 nrow = 1)) +
    scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
    scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
    coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_comp_pfj_pt_cont)
  ## Subtropenjetstreams
  ## 
  ggp_comp_stj_pt_cont <-
    ggplot(data = tb_subset %>%
             filter(season == ssn[i_season]), 
           mapping = aes(x = Chebyshev_STJ, y = Dijkstra_STJ)) + 
    geom_jitter(colour = colour_season[i_season], size = 0.2) + geom_density_2d(colour = "black") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
    labs(colour = "Jahreszeit") +
    guides(colour = guide_legend(title.position = "top",
                                 direction = "horizontal",
                                 label.position = "bottom",
                                 nrow = 1)) +
    scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
    scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
    coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_comp_stj_pt_cont)
  
  # ## Speichern der Plots
  # save_plot(plt = ggp_comp.pfj.pt.cont, 
  #          width = 100, height = 100, pointsize = 11,
  #          filepath = paste0(save_dir, "03-comp/"), 
  #          filename = paste0("PFJ_ptcont_", ssn[i_season]))
  # save_plot(plt = ggp_comp.stj.pt.cont, 
  #          width = 100, height = 100, pointsize = 11,
  #          filepath = paste0(save_dir, "03-comp/"), 
  #          filename = paste0("STJ_ptcont_", ssn[i_season]))
}
rm(tb_subset)
