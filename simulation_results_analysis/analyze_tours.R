pacman::p_load(data.table, dplyr, tidyr, sf, ggplot2, readr, extrafont)

folders = c("c:/models/freightFlows/output/muc_hd_0/",
            "c:/models/freightFlows/output/muc_hd_20/",
            "c:/models/freightFlows/output/muc_hd_40/",
            "c:/models/freightFlows/output/muc_hd_60/",
            "c:/models/freightFlows/output/muc_hd_80/",
            "c:/models/freightFlows/output/muc_hd_100/")

scenarios = c(0,20,40,60,80,100)



folders = c("c:/models/freightFlows/output/muc_densities_1000/",
            "c:/models/freightFlows/output/muc_densities_2000/",
            "c:/models/freightFlows/output/muc_densities_3000/")

scenarios = c(1000,2000,3000)




capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder"), capacity = c(200,20,200))

tours_all = data.frame()


for (i in 1:length(folders)){
  folder = folders[[i]]
  scenario = scenarios[[i]]
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  # legs = tour_analysis %>%
  #   filter(type == "leg")
  # legs = left_join(legs, capacities, by="vehicle_type")
  # legs = legs %>% 
  #   mutate(v_c_ratio = parcels / capacity) %>% 
  #   mutate(v_c_ratio_bin = cut(v_c_ratio, breaks = seq(-1,5)/5))
  # 
  # tmp = legs %>% group_by(vehicle_type) %>%
  #   summarize(v_c_ratio = weighted.mean(v_c_ratio, time))
  # tmp$scenario = scenario
  # 
  # v_c_ratio_average = rbind(as.data.frame(tmp), v_c_ratio_average)
  # 
  # tmp = legs %>% group_by(vehicle_type, v_c_ratio_bin) %>%
  #   summarize(time = sum(time)) 
  # tmp$scenario = scenario
  # v_c_ratio_summary = rbind(as.data.frame(tmp), v_c_ratio_summary)
  # 
  # tmp = tour_analysis %>%
  #   group_by(type, vehicle_type) %>% 
  #   summarize(time = sum(time)) %>%
  #   spread(type, time) %>% mutate(rate = service/leg)
  # 
  # tmp$scenario = scenario
  # service_leg_ratio = rbind(as.data.frame(tmp), service_leg_ratio)
  # 
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  =tours_all %>% left_join(capacities, by = "vehicle_type")


#empty time
empty_time = tours_all %>%
  filter(type == "leg") %>%
  mutate(v_c = if_else(parcels/capacity < 0.2, "under20",if_else(parcels/capacity < 0.8, "between20and80", "over80"))) %>%
  group_by(scenario, vehicle_type, v_c) %>%
  summarize(time = sum(time), distance = sum(distance))
empty_time$v_c = factor(empty_time$v_c, levels = c("over80", "between20and80","under20" ))


ggplot(empty_time, aes(y=time, x = scenario, fill = v_c)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type)


ggplot(empty_time, aes(y=distance, x = scenario, fill = v_c)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type)


leg_vs_service = tours_all %>% group_by(type, vehicle_type, scenario) %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(leg_vs_service, aes(y=time, x = scenario, fill = type)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type)

