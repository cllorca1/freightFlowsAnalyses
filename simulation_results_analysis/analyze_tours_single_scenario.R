pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, sf, tmap)

folders = c("c:/models/freightFlows/output/base/")

scenario_folders = c("base")

scenarios = scenario_folders
capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder"), capacity = c(100,20,100))

tours_all = data.frame()

for (i in 1:length(folders)){
  folder = folders[[i]]
  scenario = scenarios[[i]]
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
  rm(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  = tours_all %>% left_join(capacities, by = "vehicle_type")

tours_all$vehicle_first = if_else(tours_all$service == 0, 1, 0)

tours_all$is_service = if_else(tours_all$type == "service", 1, 0)

tours = tours_all %>% group_by(tour) %>% summarise(stops = mean(number_of_services), distance = sum(distance),
                                           total_time = sum(time),
                                           service_time = sum(time * is_service),
                                           leg_time = sum(time * (1-is_service)))
