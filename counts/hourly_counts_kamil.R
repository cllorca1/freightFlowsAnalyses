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

ggplot(my_comparison_long_a, aes(x=hour, y = value, color = name, group  = name)) +
  geom_line(stat = "identity") + 
  scale_x_continuous(expand  =c(0,0)) + 
  scale_y_continuous(expand  =c(0,50)) + 
  xlab("Time of day (h)") + 
  ylab("Traffic volume (average) (veh/h)") + 
  theme_bw() 

my_comparison_long_b = my_comparison_long %>% group_by(hour, name, state) %>% summarise(value = mean(value))

ggplot(my_comparison_long_b, aes(x=hour, y = value, color = name, group  = name)) +
  geom_line(stat = "identity") + 
  scale_x_continuous(expand  =c(0,0)) + 
  scale_y_continuous(expand  =c(0,50)) + 
  xlab("Time of day (h)") + 
  ylab("Traffic volume (average) (veh/h)") + 
  theme_bw() +
  facet_wrap(.~state)

