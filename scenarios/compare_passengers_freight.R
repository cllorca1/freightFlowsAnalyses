pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr)


upper_folder = "C:/models/freightFlows/output/"

scenario_folders = c("base")
scenario_names = c("base")
scales = c(.20)

counts = data.frame()

for (i in 1:length(scenario_folders)){
  this_counts = read_csv(paste(upper_folder, scenario_folders[i], "/counts.csv", sep  =""))
  this_counts$scenario = scenario_names[i]
  this_counts$scale  = scales[i]
  counts = counts %>% bind_rows(this_counts)
  rm(this_counts)
}

sum(counts$van)

link_stats = read_delim(gzfile(paste(upper_folder, scenario_folders[1], "/matsim/ITERS/it.50/", scenario_names[1], ".50.linkstats.txt.gz", sep ="")), delim = "\t")

link_stats = link_stats %>% select(LINK, LENGTH, CAPACITY, FREESPEED)


counts = counts %>% mutate(freight = (cargoBike + van + lDTruck + sDTruck)/scale)  %>%
  mutate(passenger = car/scale)


counts = counts %>% left_join(link_stats, by = c("link" = "LINK")) 

counts = counts %>% mutate(freight_vkt = freight * LENGTH, passenger_vkt = passenger * LENGTH)

counts %>% group_by(scenario) %>%
  summarize(freight = sum(freight), passenger = sum(passenger),
            freight_vkt = sum(freight_vkt), car_vkt = sum(passenger_vkt))


hourly = counts %>% group_by(scenario, hour) %>%
  summarize(freight = sum(freight), passenger = sum(passenger),
            freight_vkt = sum(freight_vkt), car_vkt = sum(passenger_vkt))

write.table(hourly, "clipboard", sep = "\t", row.names = F)

by_link = counts %>% group_by(scenario, link )%>%
  summarize(freight = sum(freight), passenger = sum(passenger),
            freight_vkt = sum(freight_vkt), car_vkt = sum(passenger_vkt))

write.table(by_link, "clipboard-1000kb", sep = "\t", row.names = F)


subset_of_links = read_csv("C:/projects/radLast/analysis/scenario_with_passenger/dc_close_links_test.csv", col_names = F)

hourly2 = counts %>% filter(link %in% subset_of_links$X1) %>% group_by(scenario, hour) %>%
  summarize(freight = sum(freight), passenger = sum(passenger),
            freight_vkt = sum(freight * LENGTH), car_vkt = sum(passenger * LENGTH))
write.table(hourly2, "clipboard", sep = "\t", row.names = F)


counts$share = counts$freight / (counts$freight + counts$passenger)
counts$share_vkt = counts$freight * counts$LENGTH / (counts$freight * counts$LENGTH + counts$passenger * counts$LENGTH)


ggplot(counts %>% filter( hour < 25), aes(x=share, color = hour, group = hour)) + geom_freqpoly()
