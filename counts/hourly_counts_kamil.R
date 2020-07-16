pacman::p_load(readr, ggplot2, dplyr, tidyr)

path_to_hourly_data  = "c:/projects/radLast/analysis/counts/"

path_to_my = paste(path_to_hourly_data, "Autobahnen/daily_comparison_output_AS.csv", sep  ="")

#or
#path_to_my = paste(path_to_hourly_data, "Bundesstrassen/daily_comparison_output_Bundesstrassen.csv", sep  ="")


my_data = read_csv(path_to_my)


my_comparison1 = my_data %>%
  select(station = Zst, hour, state = Land, sim = count_matsim1, obs = sim_obs_average_dir1)

my_comparison1$direction = 1

my_comparison2 = my_data %>%
  select(station = Zst, hour, state = Land, sim = count_matsim2, obs = sim_obs_average_dir2)

my_comparison2$direction = 2

my_comparison = rbind(my_comparison1, my_comparison2)

upper_value = 1.1 * max(my_comparison$obs)


ggplot(my_comparison, aes(x = obs, y = sim)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) + 
  scale_x_continuous(expand  =c(0,0), limits = c(0, upper_value)) + 
  scale_y_continuous(expand  =c(0,0), limits = c(0, upper_value)) + 
  ylab("Simulated traffic volume (veh/h)") + 
  xlab("Observed traffic volume (veh/h)") + 
  theme_bw()


my_comparison_daily = my_comparison %>% group_by(station, state) %>% summarize(sim = sum(sim), obs = sum(obs))

upper_value_daily = 1.1 * max(my_comparison_daily$obs)

ggplot(my_comparison_daily, aes(x = obs, y = sim)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  scale_x_continuous(expand  =c(0,0), limits = c(0, upper_value_daily)) + 
  scale_y_continuous(expand  =c(0,0), limits = c(0, upper_value_daily)) + 
  ylab("Simulated traffic volume (veh/day)") + 
  xlab("Observed traffic volume (veh/day)") + 
  theme_bw()

ggsave("counts/AADT.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
ggsave("counts/AADT.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")


ggplot(my_comparison, aes(x = obs, y = sim)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) + 
  scale_x_continuous(expand  =c(0,0), limits = c(0, upper_value)) + 
  scale_y_continuous(expand  =c(0,0), limits = c(0, upper_value)) + 
  ylab("Simulated traffic volume (veh/h)") + 
  xlab("Observed traffic volume (veh/h)") + 
  theme_bw() + facet_wrap(.~state)


my_comparison_long = my_comparison %>% pivot_longer(cols = c("sim", "obs"))
my_comparison_long_a = my_comparison_long %>% group_by(hour, name) %>% summarise(value = mean(value))

#rename categories
my_comparison_long_a$name[my_comparison_long_a$name == "sim"] = "simulated"
my_comparison_long_a$name[my_comparison_long_a$name == "obs"] = "observed"

ggplot(my_comparison_long_a, aes(x=hour, y = value, linetype = name, group  = name)) +
  scale_linetype_manual(values = c("dotted", "solid"), name = "") + 
  geom_line(stat = "identity", size = 1) + 
  scale_x_continuous(expand  =c(0,0)) + 
  scale_y_continuous(expand  =c(0,50)) + 
  xlab("Time of day (h)") + 
  ylab("Average traffic volume (veh/h)") + 
  theme_bw() 

ggsave("counts/hourly.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
ggsave("counts/hourly.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")


my_comparison_long_b = my_comparison_long %>% group_by(hour, name, state) %>% summarise(value = mean(value))

ggplot(my_comparison_long_b, aes(x=hour, y = value, color = name, group  = name)) +
  geom_line(stat = "identity") + 
  scale_x_continuous(expand  =c(0,0)) + 
  scale_y_continuous(expand  =c(0,50)) + 
  xlab("Time of day (h)") + 
  ylab("Traffic volume (average) (veh/h)") + 
  theme_bw() +
  facet_wrap(.~state)


rmse = my_data %>% group_by(Zst, longitude, latitude) %>% summarize(n = n(), d1 = sum(count_difference1^2),
                                                                    d2 = sum(count_difference2^2), count = mean(sim_obs_average_dir1) + mean(sim_obs_average_dir2))


rmse = rmse %>% mutate(rmse = (sqrt((d1 + d2)/n)))
rmse = rmse %>% mutate(prmse = rmse/count)



write.csv(rmse, "rmse_by_station.csv", row.names = F)

##global rmse


my_comparison_daily$dif = my_comparison_daily$sim - my_comparison_daily$obs

my_comparison_daily$sq_dif = my_comparison_daily$dif^2

sqrt(sum(my_comparison_daily$sq_dif)/nrow(my_comparison_daily))/mean(my_comparison_daily$obs) * 100



summary(lm(data  =my_comparison_daily, formula = sim ~obs))
