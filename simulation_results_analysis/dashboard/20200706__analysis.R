pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard,
               plotly, processx, leaflet, st, tmap, rgdal, data.table)

this_folder = "C:/code/freightFlowsR/simulation_results_analysis/dashboard/"

source(paste(this_folder, "read_data_fun_2.R", sep =""))
source(paste(this_folder, "read_counts_fun_2.R", sep =""))
source(paste(this_folder, "read_networks.R", sep =""))

upper_folder = "C:/models/freightFlows/output/"

scenario_folders = c(
  "0_cargo_bike_dc20",
  "40_cargo_bike_dc20",
  "60_cargo_bike_dc20",
  "100_cargo_bike_dc20"
)

scenarios = c(
  0,
  40,
  60,
  100
  )

scenario_pretty_names = c(
  "Munich 0%",
  "Munich 40% (high density)",
  "Munich 60% (high density)",
  "Munich 100% (high density)"
)

distribution_centers = c(
  20,
  20,
  20,
  20
  )

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

input = list()

input$selected_scenarios = scenarios

shares_of_ev = c(0)

summary = data.frame()

for (share_of_ev in shares_of_ev){
  this_summary = read_model_results_2(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers, share_of_ev)
  this_summary$share_of_ev = share_of_ev
  summary = summary %>% bind_rows(this_summary)
  rm(this_summary)
}


summary$vehicle = factor(summary$vehicle, levels = c("Feeder (shop)", "Van", "Feeder", "Cargo bike"))

colors_two = c("#407dd8", "#36a332", "grey")
colors_4_vehicles = c("black","#407dd8", "grey66", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")

# ggplot(summary %>% filter(share_of_ev == 0), aes(y=n, x=scenario, fill = vehicle, group = vehicle)) +
#   scale_fill_manual(values = colors_4_vehicles) + 
#   geom_bar(stat = "identity", position = "dodge", size = 2) +
#   ylab("Number of tours") +
#   xlab("Share of cargo bikes (%)") +
#   theme(text=element_text(size=14)) + 
#   theme_bw() 

ggplot(summary %>% filter(share_of_ev == 0), aes(y=n, x=scenario, linetype = vehicle, color = vehicle, group = vehicle)) +
  scale_color_manual(values = colors_4_vehicles, name = "Vehicle") + 
  geom_point(size = 3) + 
  geom_line(stat = "identity", size = 1) +
  ylab("Number of tours") +
  xlab("Share of cargo bikes (%)") +
  scale_y_continuous(limits = c(0,400), expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  theme_bw() + 
  theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))





#ggsave("simulation_results_analysis/tours.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
#ggsave("simulation_results_analysis/tours.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")

ggplot(summary %>% filter(vehicle != "Feeder", vehicle != "Feeder (shop)", share_of_ev == 0), aes(y=parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = c("#6b719f", "#a5c594"), name = "Vehicle") +
  geom_bar(stat = "identity", position =  "stack", color  ="black") +
  ylab("Number ofparcels")  +
  xlab("Share of parcels by cargo bike (%)") +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() + 
  theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))

#ggsave("simulation_results_analysis/share.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
#ggsave("simulation_results_analysis/share.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")

# ggplot(summary %>% filter(vehicle != "Feeder"), aes(y=parcels, x=scenario, fill = vehicle)) +
#   scale_fill_manual(values = colors_two) +
#   geom_bar(stat = "identity", position =  "fill") +
#   ylab("Number ofparcels")  +
#   xlab("Share of parcels by cargo bike (%)") +
#   theme(text=element_text(size=14), axis.text.x = element_text(angle = 90)) +
#   theme_bw()

summary_aux = summary

summary_aux$vehicle = factor(summary_aux$vehicle, levels = c("Cargo bike", "Feeder", "Van","Feeder (shop)" ))

ggplot(summary_aux %>% filter(share_of_ev == 0, vehicle != "Feeder (shop)"), aes(y=distance/1000, x=scenario, fill = vehicle, group = vehicle)) +
  scale_fill_manual(values = colors_4_vehicles, name = "Vehicle") + 
  geom_bar(stat = "identity", position =  "stack", color = "black") +
  ylab("Distance travelled (km)") + 
  xlab("Share of cargo bikes (%)") +
  #scale_y_continuous(expand = c(0,0), limits = c(0,6000)) +
  theme_bw() + 
  theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))

#ggsave("simulation_results_analysis/vkt.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
#ggsave("simulation_results_analysis/vkt.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")

# ggplot(summary %>% filter(share_of_ev == 0), aes(y=distance/1000, x=scenario, color = vehicle, group = vehicle)) +
#   scale_color_manual(values = colors_4_vehicles) + 
#   geom_line(stat = "identity", size  =2) +
#   ylab("Distance travelled (km)") + 
#   xlab("Share of cargo bikes (%)") +
#   theme(text=element_text(size=14)) + 
#   theme_bw()

ggplot(summary %>% filter(share_of_ev == 0), aes(y=distance/n/1000, x=scenario, color = vehicle, group = vehicle)) +
  scale_color_manual(values = colors_4_vehicles) + 
  geom_line(stat = "identity", size  = 2) +
  ylab("Distance by tour (m)") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) +
  theme_bw()


ggplot(summary %>% filter(share_of_ev == 0), aes(y=parcels/n, x=scenario, color = vehicle, group = vehicle)) +
  scale_color_manual(values = colors_4_vehicles) + 
  geom_line(stat = "identity", size  = 2) +
  ylab("Parcels by tour") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) +
  theme_bw() + ylim(0,120)

ggplot(summary %>% filter(share_of_ev == 0,), aes(y=operatingTime/60/n, x=scenario, color = vehicle, group = vehicle)) +
  scale_color_manual(values = colors_4_vehicles) + 
  geom_line(stat = "identity", size  = 2) +
  ylab("Time by tour (min)") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) +
  theme_bw()

summary_aux$share_of_ev = factor(summary_aux$share_of_ev, levels = c(0, 0.25, 0.50), labels = c("0% EV", "25% EV", "50% EV"))


ggplot(summary_aux %>% filter(vehicle !="Feeder (shop)"), aes(y= CO2, x=scenario, fill = vehicle, group = vehicle)) +
  scale_fill_manual(values = c("gray80", "gray66", "gray25"), name = "Vehicle") + 
  geom_bar(stat = "identity", position = "stack", color = "black") +
  ylab("CO2 emissions (g)") + 
  xlab("Share of cargo bikes (%)") +
  facet_wrap(.~share_of_ev) +
  theme_bw() + theme() +scale_y_continuous(expand = c(0, 50))



# ggplot(summary, aes(y= NOx, x=scenario, fill = vehicle, group = vehicle)) +
#   scale_fill_manual(values = colors_4_vehicles) + 
#   geom_area(stat = "identity", position = "stack") +
#   ylab("NOx emissions (g)") + 
#   xlab("Share of cargo bikes (%)") +
#   theme(text=element_text(size=14)) +
#   facet_wrap(.~share_of_ev) +
#   theme_bw()


simplified_emission_factors = data.frame(vehicle = c("Feeder (shop)", "Van", "Feeder", "Cargo bike"), 
                                         co2_factor_combustion = c(634,634,634,0), co2_factor_electric = c(259,259,259,15.54))


summary$effective_share_of_ev = if_else(summary$vehicle != "Cargo bike", summary$share_of_ev, 1)

summary_aux2 = summary

summary_aux2$share_of_ev = factor(summary_aux2$share_of_ev, levels = c(0, 0.25, 0.50), labels = c("0% EV", "25% EV", "50% EV"))

summary_aux2 = summary_aux2 %>% left_join(simplified_emission_factors, by = "vehicle")

summary_aux2 = summary_aux2 %>%
  mutate(co2_simple = distance / 1000 * effective_share_of_ev * co2_factor_electric + distance / 1000 * (1 - effective_share_of_ev) * co2_factor_combustion)


ggplot(summary_aux2 %>% filter(vehicle !="Feeder (shop)"), aes(y= co2_simple/1000, x=scenario, fill = vehicle, group = vehicle)) +
  scale_fill_manual(values = c("#a5c594", "#70928c","#6b719f" ), name = "Vehicle") + 
  geom_bar(stat = "identity", position = "stack", color = "black") +
  ylab("CO2 emissions (kg)") + 
  xlab("Share of cargo bikes (%)") +
  #scale_y_continuous(expand = c(0, 0), limits = c(0,3500)) +
  facet_wrap(.~share_of_ev) +
  theme_bw() + theme() 


#ggsave("simulation_results_analysis/co2.pdf", device = "pdf", scale = 0.75, width = 300, height = 150, units = "mm")
#ggsave("simulation_results_analysis/co2.bmp", device = "bmp", scale = 0.75, width = 300, height = 150, units = "mm")



ggplot(summary_aux2, aes(x=CO2, y = co2_simple, color = share_of_ev)) + geom_point() + geom_abline(intercept = 0, slope = 1)



####tours####

capacities = data.frame(vehicle_type = c("truck", "cargoBike", "truck_feeder","truck_shop"), capacity = c(100,20,100,100))

tours_all = data.frame()


for (i in 1:length(scenario_folders)){
  folder = paste(upper_folder, scenario_folders[[i]], "/", sep = "")
  scenario = scenarios[[i]]
  tour_analysis = read.csv(paste(folder, "carriers_analysis.csv", sep =""))
  tour_analysis$vehicle_type = as.character(tour_analysis$vehicle_type)
  tour_analysis$carrier = as.character(tour_analysis$carrier)
  tour_analysis$scenario = scenario
  tours_all= tours_all %>% bind_rows(tour_analysis)
}

colors = c("#36a332","#407dd8", "#193256")

tours_all  =tours_all %>% left_join(capacities, by = "vehicle_type")

tours_all$vehicle_type = factor(tours_all$vehicle_type, levels=c( "cargoBike", "truck_feeder","truck_shop","truck"),
                                labels = c("Cargo bike", "Feeder", "Feeder (shop)","Van"))
#empty time
empty_time = tours_all %>%
  filter(type == "leg", vehicle_type != "truck_feeder") %>%
  mutate(v_c = if_else(parcels/capacity < 0.2, "under20",if_else(parcels/capacity < 0.8, "between20and80", "over80"))) %>%
  group_by(scenario, vehicle_type, v_c) %>%
  summarize(time = sum(time), distance = sum(distance))


empty_time$v_c = factor(empty_time$v_c, levels = c("over80", "between20and80","under20" ), labels = c("rate > 80%", "20 < rate <80%", "rate < 20%"))

# colors_two = c("#407dd8", "#36a332")
# 
# ggplot(empty_time, aes(y=distance, x = scenario, fill = vehicle_type, alpha = as.factor(v_c))) +
#   scale_alpha_manual(values = c(1,0.7,0.5)) +
#   scale_fill_manual(values = colors_4_vehicles) +
#   geom_bar(stat = "identity", position = "fill") +
#   facet_wrap(.~vehicle_type) +
#   theme_bw() +
#   labs(fill = "Vehicle")+
#   xlab("Share of cargo bikes (%)") +
#   ylab("Share of distance") +
#   labs(alpha = "Occupation rate (%)") +
#   theme(legend.position = "bottom")




operating_time = tours_all %>% group_by(vehicle_type, scenario) %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(operating_time%>% filter(vehicle_type !="Feeder (shop)"), aes(y=time/3600, x = as.factor(scenario), fill = vehicle_type)) +
  scale_fill_manual(values = c("#a5c594", "#70928c", "#6b719f"), name = "Vehicle") + 
  geom_bar(stat = "identity", position = "stack", color = "black") +
  theme_bw() +
  labs(fill = "Vehicle")+
  labs(alpha = "Action (%)") +
  xlab("Share of cargo bikes (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +
  ylab("Time (h)") +
  theme(legend.position = "bottom")



ggsave("simulation_results_analysis/operating_time.pdf", device = "pdf", scale = 0.75, width = 150, height = 150, units = "mm")
ggsave("simulation_results_analysis/operating_time.bmp", device = "bmp", scale = 0.75, width = 150, height = 150, units = "mm")


tours_aggr = tours_all %>% group_by(carrier, tour, scenario, vehicle_type) %>%
  mutate(time_if_leg = if_else(type == "leg", time, 0)) %>%
  summarize(time_total = sum(time)/60, time_travel = sum(time_if_leg)/60)


tours_summary = tours_aggr %>% group_by(scenario, vehicle_type) %>%
  summarize(time_total = mean(time_total), time_travel = mean(time_travel))


ggplot(tours_summary, aes(x = scenario, y = time_travel/60, color = vehicle_type)) +
  geom_line(size  =2) +
  scale_color_manual(values = colors_4_vehicles)


ggplot(tours_summary, aes(x = scenario, y = time_total/60, color = vehicle_type)) +
  geom_line(size  =2) +
  scale_color_manual(values = colors_4_vehicles)


tours_all %>% filter(type  == "service") %>% group_by(vehicle_type) %>% summarize(mean(time)/60)




##counts
counts = read_model_counts2(upper_folder, scenarios, scenario_folders ,scenarios)

links_in_catchment_area = read_csv("C:/projects/radLast/analysis/scenarios_new_paper_2020/sub_networks/catchment_area.csv")$ID
links_in_md_buffer = read_csv("C:/projects/radLast/analysis/scenarios_new_paper_2020/sub_networks/500_m_microDepot.csv")$ID

##load the old passenger count file
pax_count = read_csv("C:/projects/radLast/analysis/scenario_with_passenger/results/counts_link_mode.csv")
pax_count = pax_count %>% select(link, passenger)

counts = counts %>% left_join(pax_count, by = "link")
counts$passenger = counts$passenger/24 ##passenger links cannot be disagregated by hour - the file was already aggregated to days

aux = counts %>% group_by(link, length, type) %>% summarize()
aux = aux %>% group_by(type) %>% summarize(sum(length))
write.table(aux, "clipboard", sep  ="\t")


counts_long = counts %>% pivot_longer(cols = c("passenger", "cargoBike", "van", "lDTruck", "sDTruck", "feeder"))

counts_long = counts_long %>% 
  filter(link %in% links_in_catchment_area) %>%
  mutate(link_in_md = link %in% links_in_md_buffer)


summary_counts_1 = counts_long %>%
  group_by(scenario,name) %>%
  summarize(count = sum(value)) %>%
  pivot_wider(id_cols = "scenario", values_from = "count")

summary_counts_2 = counts_long %>%
  group_by(scenario,type, name) %>%
  summarize(count = sum(value))

selected_types = c("motorway", "trunk","primary", "secondary",  "tertiary", "residential")

summary_counts_3 = summary_counts_2 %>% filter(type %in% selected_types)
summary_counts_3$type = factor(summary_counts_3$type , levels = selected_types)

summary_counts_3$name[summary_counts_3$name == "sDTruck"] = "truck"
summary_counts_3$name[summary_counts_3$name == "lDTruck"] = "truck"

summary_counts_3$name = factor(summary_counts_3$name , 
                               levels = c("passenger", "cargoBike", "feeder",  "van", "truck"), 
                               labels= c("Passenger", "Cargo bike", "Feeder", "Van", "Truck"))

summary_counts_3$type = as.character(summary_counts_3$type)

summary_counts_3$type[summary_counts_3$type == "motorway"] = "Major road (28% in length)"
summary_counts_3$type[summary_counts_3$type == "primary"] = "Major road (28% in length)"
summary_counts_3$type[summary_counts_3$type == "secondary"] = "Major road (28% in length)"
summary_counts_3$type[summary_counts_3$type == "tertiary"] = "Major road (28% in length)"
summary_counts_3$type[summary_counts_3$type == "trunk"] = "Major road (28% in length)"
summary_counts_3$type[summary_counts_3$type == "residential"] = "Minor road (72% in length)"

summary_counts_3= summary_counts_3 %>% group_by(scenario, type, name) %>% summarize(count = sum(count))

summary_counts_3 = summary_counts_3 %>% filter(name != "Passenger")

ggplot(summary_counts_3, aes(x=as.factor(scenario), y = count, fill  = name)) +
  scale_fill_manual(values = c("gray80","gray66","gray25" , "black", "gray5")) + 
  geom_bar(stat = "identity", position = "fill", color = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(.~type, scales = "free") + 
  xlab("Share of cargo bikes (%)") + ylab("Share of traffic volume") +
  theme_bw()

ggsave("simulation_results_analysis/share_freight.pdf", device = "pdf", scale = 0.75, width = 300, height = 100, units = "mm")
ggsave("simulation_results_analysis/share_freight.bmp", device = "bmp", scale = 0.75, width = 300, height = 100, units = "mm")

ggplot(summary_counts_3, aes(x=as.factor(scenario), y = count, fill  = name)) +
  scale_fill_manual(values = c("#a5c594", "#70928c","#6b719f" , "gray25", "gray25")) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(.~type, scales = "free") + 
  xlab("Share of cargo bikes (%)") + ylab("Sum of traffic volume (veh/day)") +
  theme_bw()



ggsave("simulation_results_analysis/counts_freight.pdf", device = "pdf", scale = 0.75, width = 300, height = 150, units = "mm")
ggsave("simulation_results_analysis/counts_freight.bmp", device = "bmp", scale = 0.75, width = 300, height = 150, units = "mm")


#write.table(summary_counts_3, "clipboard", sep  ="\t")

summary_counts_4 = counts_long %>%
  filter(link_in_md) %>%
  group_by(scenario, type, name) %>%
  summarize(count = sum(value * length))

selected_types = c("motorway", "trunk","primary", "secondary",  "tertiary", "residential", "unclassified")

summary_counts_4 = summary_counts_4 %>% filter(type %in% selected_types)
summary_counts_4$type = factor(summary_counts_4$type , levels = selected_types)
summary_counts_4$name = factor(summary_counts_4$name , levels = c("car", "cargoBike", "van", "sDTruck", "lDTruck"))

summary_counts_4 = summary_counts_4 %>% filter(name != "passenger")

ggplot(summary_counts_4, aes(x=as.factor(scenario), y = count, fill  = name)) +
  scale_fill_manual(values = c("gray80", "gray66", "gray25", "gray20")) + 
  geom_bar(stat = "identity", position = "fill", color = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(.~type, scales = "free") + 
  xlab("Share of cargo bikes (%)") + ylab("Share of traffic flows") +
  theme_bw()

ggplot(summary_counts_4, aes(x=as.factor(scenario), y = count, fill  = name)) +
  scale_fill_manual(values = c("gray80", "gray66", "gray25", "gray20")) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(.~type) + 
  xlab("Share of cargo bikes (%)") + ylab("Sum of traffic counts") +
  theme_bw()


daily_counts = counts %>% group_by(link, length, type, scenario) %>% summarise_all(sum)

write_csv(daily_counts, "C:/projects/radLast/analysis/scenarios_new_paper_2020/counts_daily.csv")
