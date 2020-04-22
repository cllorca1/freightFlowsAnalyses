pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf)

counts = read.table("clipboard", sep = "\t", header = T)

counts = counts %>% rowwise() %>%
  mutate(to_from_abroad = if_else(grepl("AUSLAND", direction1) | grepl("AUSLAND", direction2), "yes", "no"))


ggplot(counts, aes(x= observed, y = simulated, color = to_from_abroad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
  geom_abline(slope = 1.5, intercept = 0, color = "black", size = 1, linetype = "dashed") +
  geom_abline(slope = 0.5, intercept = 0, color = "black", size = 1, linetype = "dashed") +
  xlim(0,15000) + ylim(0,15000)
  


ggplot(counts, aes(x= observed, y = simulated, color = to_from_abroad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 2) +
  scale_x_log10() + scale_y_log10()


counts = counts %>% mutate(diff = simulated - observed) %>% mutate(sq_diff = diff^2) %>% mutate(sq_diff = if_else(is.na(sq_diff), 0, sq_diff))

sqrt(mean(counts$sq_diff))

counts %>% group_by(to_from_abroad) %>% summarize(rmse = sqrt(mean(sq_diff)), counts = mean(observed, na.rm = T))

counts %>% group_by(Land) %>% summarize(rmse = sqrt(mean(sq_diff)), counts = mean(observed, na.rm = T))

obse_trucks = sum(counts$observed, na.rm = T)
simu_trucks = sum(counts$simulated, na.rm = T)

ratio = obse_trucks / simu_trucks

ggplot(counts, aes(x= observed, y = simulated * ratio, color = to_from_abroad)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
  geom_abline(slope = 1.5, intercept = 0, color = "black", size = 1, linetype = "dashed") +
  geom_abline(slope = 0.5, intercept = 0, color = "black", size = 1, linetype = "dashed") +
  xlim(0,15000) + ylim(0,15000)


counts = counts %>% mutate(diff = simulated*ratio - observed) %>% mutate(sq_diff = diff^2) %>% mutate(sq_diff = if_else(is.na(sq_diff), 0, sq_diff))

sqrt(mean(counts$sq_diff))

counts %>% group_by(to_from_abroad) %>% summarize(rmse = sqrt(mean(sq_diff)), counts = mean(observed, na.rm = T))
