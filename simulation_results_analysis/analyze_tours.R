pacman::p_load(data.table, dplyr, tidyr, sf, ggplot2, readr, extrafont)

# different shares ----

folders = c("c:/models/freightFlows/output/muc_hd_0/",
            "c:/models/freightFlows/output/muc_hd_20/",
            "c:/models/freightFlows/output/muc_hd_40/",
            "c:/models/freightFlows/output/muc_hd_60/",
            "c:/models/freightFlows/output/muc_hd_80/",
            "c:/models/freightFlows/output/muc_hd_100/")

scenarios = c(0,20,40,60,80,100)




folders = c("c:/models/freightFlows/output/muc_demand_1.5/",
            "c:/models/freightFlows/output/muc_demand_2/",
            "c:/models/freightFlows/output/muc_demand_2.5/")

scenarios = c(150,200,250)




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

tours_all$vehicle_type = factor(tours_all$vehicle_type, levels=c("truck", "cargoBike", "truck_feeder"), labels = c("Van", "Cargo bike", "truck_feeder"))
#empty time
empty_time = tours_all %>%
  filter(type == "leg", vehicle_type != "truck_feeder") %>%
  mutate(v_c = if_else(parcels/capacity < 0.2, "under20",if_else(parcels/capacity < 0.8, "between20and80", "over80"))) %>%
  group_by(scenario, vehicle_type, v_c) %>%
  summarize(time = sum(time), distance = sum(distance))
 

empty_time$v_c = factor(empty_time$v_c, levels = c("over80", "between20and80","under20" ), labels = c("rate > 80%", "20 < rate <80%", "rate < 20%"))

colors_two = c("#407dd8", "#36a332")

ggplot(empty_time, aes(y=distance, x = scenario, fill = vehicle_type, alpha = as.factor(v_c))) +
  scale_alpha_manual(values = c(1,0.7,0.5)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) +
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  xlab("Share of cargo bikes (%)") + 
  ylab("Share of distance") +
  labs(alpha = "Occupation rate (%)") + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Sc.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)


leg_vs_service = tours_all %>% group_by(type, vehicle_type, scenario) %>%
  filter(vehicle_type != "truck_feeder") %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(leg_vs_service, aes(y=time, x = scenario, fill = vehicle_type, alpha = type)) +
  scale_alpha_manual(values = c(1,0.7)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) + 
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  labs(alpha = "Action (%)") + 
  xlab("Share of cargo bikes (%)") + 
  ylab("Share of time") +
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Se.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)

# different mds -----

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
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  =tours_all %>% left_join(capacities, by = "vehicle_type")

tours_all$vehicle_type = factor(tours_all$vehicle_type, levels=c("truck", "cargoBike", "truck_feeder"), labels = c("Van", "Cargo bike", "truck_feeder"))
#empty time
empty_time = tours_all %>%
  filter(type == "leg", vehicle_type != "truck_feeder") %>%
  mutate(v_c = if_else(parcels/capacity < 0.2, "under20",if_else(parcels/capacity < 0.8, "between20and80", "over80"))) %>%
  group_by(scenario, vehicle_type, v_c) %>%
  summarize(time = sum(time), distance = sum(distance))


empty_time$v_c = factor(empty_time$v_c, levels = c("over80", "between20and80","under20" ), labels = c("rate > 80%", "20 < rate <80%", "rate < 20%"))

colors_two = c("#407dd8", "#36a332")

ggplot(empty_time, aes(y=distance, x = scenario, fill = vehicle_type, alpha = as.factor(v_c))) +
  scale_alpha_manual(values = c(1,0.7,0.5)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) +
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  xlab("Micro depot grid spacing (m)") +
  ylab("Share of distance") +
  labs(alpha = "Occupation rate (%)") + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Me.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)


leg_vs_service = tours_all %>% group_by(type, vehicle_type, scenario) %>%
  filter(vehicle_type != "truck_feeder") %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(leg_vs_service, aes(y=time, x = scenario, fill = vehicle_type, alpha = type)) +
  scale_alpha_manual(values = c(1,0.7)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) + 
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  labs(alpha = "Action (%)") + 
  xlab("Micro depot grid spacing (m)") +
  ylab("Share of time") +
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Mf.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)


# different demands -----

folders = c("c:/models/freightFlows/output/muc_demand_1/",
            "c:/models/freightFlows/output/muc_demand_1.5/",
            "c:/models/freightFlows/output/muc_demand_2/",
            "c:/models/freightFlows/output/muc_demand_2.5/")

scenarios = c("100%","150%","200%","250%")


capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder"), capacity = c(200,20,200))

tours_all = data.frame()


for (i in 1:length(folders)){
  folder = folders[[i]]
  scenario = scenarios[[i]]
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  =tours_all %>% left_join(capacities, by = "vehicle_type")

tours_all$vehicle_type = factor(tours_all$vehicle_type, levels=c("truck", "cargoBike", "truck_feeder"), labels = c("Van", "Cargo bike", "truck_feeder"))
#empty time
empty_time = tours_all %>%
  filter(type == "leg", vehicle_type != "truck_feeder") %>%
  mutate(v_c = if_else(parcels/capacity < 0.2, "under20",if_else(parcels/capacity < 0.8, "between20and80", "over80"))) %>%
  group_by(scenario, vehicle_type, v_c) %>%
  summarize(time = sum(time), distance = sum(distance))


empty_time$v_c = factor(empty_time$v_c, levels = c("over80", "between20and80","under20" ), labels = c("rate > 80%", "20 < rate <80%", "rate < 20%"))

colors_two = c("#407dd8", "#36a332")

ggplot(empty_time, aes(y=distance, x = scenario, fill = vehicle_type, alpha = as.factor(v_c))) +
  scale_alpha_manual(values = c(1,0.7,0.5)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) +
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  xlab("Micro depot grid spacing (m)") +
  ylab("Share of distance") +
  labs(alpha = "Occupation rate (%)") + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "De.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)


leg_vs_service = tours_all %>% group_by(type, vehicle_type, scenario) %>%
  filter(vehicle_type != "truck_feeder") %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(leg_vs_service, aes(y=time, x = scenario, fill = vehicle_type, alpha = type)) +
  scale_alpha_manual(values = c(1,0.7)) + 
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) + 
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  labs(alpha = "Action (%)") + 
  xlab("Micro depot grid spacing (m)") +
  ylab("Share of time") +
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Df.bmp", sep  ="")

ggsave(fileName,  width = 8, height = 10, units = "cm", dpi = 300)



# different dcs -----

folders = c("c:/models/freightFlows/output/muc_dc_20/",
            "c:/models/freightFlows/output/muc_dc_19/",
            "c:/models/freightFlows/output/muc_dc_16/",
            "c:/models/freightFlows/output/muc_dc_24/",
            "c:/models/freightFlows/output/muc_dc_22/")

scenario_folders = c("muc_dc_20", "muc_dc_19", "muc_dc_16", "muc_dc_24", "muc_dc_22")

scenarios = scenario_folders
capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder"), capacity = c(200,20,200))

tours_all = data.frame()


for (i in 1:length(folders)){
  folder = folders[[i]]
  scenario = scenarios[[i]]
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  = tours_all %>% left_join(capacities, by = "vehicle_type")

tours_all$vehicle_first = if_else(tours_all$service == 0, 1, 0)

tour_summary = tours_all %>%
  group_by(scenario, tour, carrier, vehicle_type) %>%
  summarize(parcels = sum(number_of_services*vehicle_first), distance  = sum(distance), time = sum(time), tours = sum(vehicle_first))

#get area of the catchment areas

catchment_areas = read_csv("c:/projects/radLast/analysis/scenarios_paper_ists_2020/microDepotCatchmentAreas_multiple_dc.csv")
micro_depot_catchment_areas = catchment_areas %>%
  group_by(md_and_zones_microDepot, md_and_zones_distributionCenter) %>%
  summarize(area= sum(Area))

micro_depot_catchment_areas$microDepot = paste(micro_depot_catchment_areas$md_and_zones_microDepot, "_microDepot", sep = "")
micro_depot_catchment_areas = micro_depot_catchment_areas %>% ungroup() %>% select(carrier = microDepot, area = area)

dc_catchment_areas = catchment_areas %>%
  group_by(md_and_zones_distributionCenter) %>%
  summarize(area= sum(Area))


dc_catchment_areas = dc_catchment_areas %>% ungroup() %>% select(carrier = md_and_zones_distributionCenter, area = area)
dc_catchment_areas$carrier = paste("muc_dc_" , dc_catchment_areas$carrier, sep = "")


all_catchment_areas = micro_depot_catchment_areas %>% bind_rows(dc_catchment_areas)

#merge with summary

tour_summary = tour_summary %>% rowwise() %>% 
  mutate(carrier = if_else(!grepl("microDepot", carrier), scenario, carrier))

tour_summary = tour_summary %>% left_join(all_catchment_areas, by = c("carrier"))


ggplot(tour_summary, aes(x=parcels, y = distance, color = vehicle_type)) +
  scale_color_manual(values = colors) +
  geom_point(size = 3)


ggplot(tour_summary %>% filter(!is.na(area), vehicle_type == "cargoBike"), aes(x=parcels/area * 1e6, y = time/parcels/60, color = vehicle_type)) +
  scale_color_manual(values = colors) +
  xlab("Parcel density (parcels/km2)") + 
  ylab("Time by parcel (min)") + 
  geom_point(size = 3) + 
  xlim(0,750) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "DCsb.bmp", sep  ="")

ggsave(fileName,  width = 16, height = 10, units = "cm", dpi = 300)

ggplot(tour_summary %>% filter(!is.na(area), vehicle_type == "cargoBike"), aes(x=parcels/area * 1e6, y = distance/parcels, color = vehicle_type)) +
  scale_color_manual(values = colors) +
  xlab("Parcel density (parcels/km2)") + 
  ylab("Distance by parcel (m)") + 
  geom_point(size = 3) + 
  xlim(0,750) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "DCsa.bmp", sep  ="")

ggsave(fileName,  width = 16, height = 10, units = "cm", dpi = 300)

length(unique(tour_summary$carrier))


# ggplot(tour_summary %>% filter(!is.na(area)), aes(x=parcels/area * 1e6, y = time/tours/60, color = vehicle_type)) +
#   scale_color_manual(values = colors) +
#   xlab("Parcel density (parcels/km2)") + 
#   ylab("Time by tour (min)") + 
#   geom_point(size = 3) + 
#   facet_wrap(.~vehicle_type, scales = "free")
