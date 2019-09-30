pacman::p_load(readr, dplyr, ggplot2, tidyr)


folder = "c:/models/freightFlows/output/"

preffix = "ld_all_"

years = c(2010, 2020, 2030, 2040, 2050)

suffix = "/truckFlows.csv"

truck_flows = data.frame()

for (year in years){
  
  file_name = paste(folder,preffix,year, suffix, sep = "")

  data = read_csv(file_name) %>% mutate(year = year)
  truck_flows = truck_flows %>% bind_rows(data)
  rm(data)  
  
}


rm(preffix, suffix, year)

non_empty_flows = truck_flows %>% filter(trucks > 0)


summary = truck_flows %>%
  group_by(year) %>%
  summarize(trucks = sum(trucks), volume = sum(volume_tn))

ggplot(summary, aes(x=year, y = trucks)) + geom_line()

summary = truck_flows %>%
  group_by(year, commodity) %>%
  summarize(trucks = sum(trucks), volume = sum(volume_tn))

ggplot(summary, aes(x=year, y = trucks, color = commodity)) + geom_line(size = 1)


