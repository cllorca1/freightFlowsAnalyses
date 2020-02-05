pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr)


upper_folder = "C:/models/freightFlows/output/"

scenario_folders = c("test_passengers", "muc_hd_100")
scenario_names = c("passenger", "freight")
scales = c(.05, .25)

counts = data.frame()

for (i in 1:2){
  this_counts = read_csv(paste(upper_folder, scenario_folders[i], "/matsim/counts.csv", sep  =""))
  this_counts$scenario = scenario_names[i]
  this_counts$scale  = scales[i]
  counts = counts %>% bind_rows(this_counts)
  rm(this_counts)
}



counts = counts %>% mutate(all = (car + cargoBike + van + lDTruck + sDTruck)/scale)

counts %>% group_by(scenario) %>% summarize(all = sum(all))


hourly = counts %>% group_by(scenario, hour) %>% summarize(all = sum(all))


ggplot(hourly, aes(x=hour, y = all, fill = scenario)) + geom_bar(stat  ="identity") + xlim(0,24)
ggplot(hourly, aes(x=hour, y = all, fill = scenario)) + geom_bar(stat  ="identity", position = "fill") + xlim(0,24)


by_link = counts %>% group_by(scenario, link) %>% summarize(all = sum(all))

subset_of_links = read_csv("C:/projects/radLast/analysis/scenario_with_passenger/dc_close_links_test.csv", col_names = F)

hourly2 = counts %>% filter(link %in% subset_of_links$X1) %>% group_by(scenario, hour) %>% summarize(all = sum(all))

ggplot(hourly2, aes(x=hour, y = all, fill = scenario)) + geom_bar(stat  ="identity") + xlim(0,24)
ggplot(hourly2, aes(x=hour, y = all, fill = scenario)) + geom_bar(stat  ="identity", position = "fill") + xlim(0,24)
