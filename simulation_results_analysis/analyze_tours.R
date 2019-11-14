pacman::p_load(data.table, dplyr, tidyr, sf, ggplot2, readr, extrafont)

folders = c("c:/models/freightFlows/output/muc_scenario_zero_c/",
            "c:/models/freightFlows/output/muc_scenario_3km/",
            "c:/models/freightFlows/output/muc_scenario_1km/",
            "c:/models/freightFlows/output/muc_scenario_paketbox/")

scenarios = c("Base (urban)", "a (urban)", "b (urban)","c (urban)" )

capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder"), capacity = c(200,20,200))

v_c_ratio_average = data.frame()
v_c_ratio_summary = data.frame()
service_leg_ratio = data.frame()

for (i in 1:length(folders)){
  folder = folders[[i]]
  scenario = scenarios[[i]]
  
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  
  legs = tour_analysis %>%
    filter(type == "leg")
  
  legs = left_join(legs, capacities, by="vehicle_type")
  legs = legs %>% 
    mutate(v_c_ratio = parcels / capacity) %>% 
    mutate(v_c_ratio_bin = cut(v_c_ratio, breaks = seq(-1,5)/5))
  
  tmp = legs %>% group_by(vehicle_type) %>%
    summarize(v_c_ratio = weighted.mean(v_c_ratio, time))
  tmp$scenario = scenario
  
  
  
  
  v_c_ratio_average = rbind(as.data.frame(tmp), v_c_ratio_average)
  
  tmp = legs %>% group_by(vehicle_type, v_c_ratio_bin) %>%
    summarize(time = sum(time)) 
  tmp$scenario = scenario
  v_c_ratio_summary = rbind(as.data.frame(tmp), v_c_ratio_summary)
  
  tmp = tour_analysis %>%
    group_by(type, vehicle_type) %>% 
    summarize(time = sum(time)) %>%
    spread(type, time) %>% mutate(rate = service/leg)
  
  tmp$scenario = scenario
  service_leg_ratio = rbind(as.data.frame(tmp), service_leg_ratio)
}

v_c_ratio_summary$scenario = factor(v_c_ratio_summary$scenario, levels = scenarios)
v_c_ratio_average$scenario = factor(v_c_ratio_average$scenario, levels = scenarios)
service_leg_ratio$scenario = factor(service_leg_ratio$scenario, levels = scenarios)

colors = c("#36a332","#407dd8", "#193256")

ggplot(v_c_ratio_summary, aes(x=vehicle_type, y = time, fill = v_c_ratio_bin)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer() + facet_grid(.~scenario)

ggplot(v_c_ratio_average, aes(x=vehicle_type, y = v_c_ratio, fill = vehicle_type)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = colors) +
 facet_grid(.~scenario)

ggplot(service_leg_ratio, aes(x=vehicle_type, y = rate, fill = vehicle_type)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = colors) +
  facet_grid(.~scenario) + ylab("ratio service_time/travel_time")
