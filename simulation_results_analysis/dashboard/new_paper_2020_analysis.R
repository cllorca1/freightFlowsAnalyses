pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard,
               plotly, processx, leaflet, st, tmap, rgdal, data.table)

this_folder = "C:/code/freightFlowsR/simulation_results_analysis/dashboard/"

source(paste(this_folder, "read_data_fun_2.R", sep =""))
source(paste(this_folder, "read_counts_fun.R", sep =""))
source(paste(this_folder, "read_networks.R", sep =""))

upper_folder = "C:/models/freightFlows/output/"

scenario_folders = c(
  "0_cargo_bike_dc20_v3",
  "20_cargo_bike_dc20_v3",
  "40_cargo_bike_dc20",
  "60_cargo_bike_dc20_v3",
  "80_cargo_bike_dc20_v3",
  "100_cargo_bike_dc20_v3"
)

scenarios = c(
  0,
  20,
  40,
  60,
  80,
  100
  )

scenario_pretty_names = c(
  "Munich 0%",
  "Munich 20% (high density)",
  "Munich 40% (high density)",
  "Munich 60% (high density)",
  "Munich 80% (high density)",
  "Munich 100% (high density)"
)

distribution_centers = c(
  20,
  20,
  20,
  20,
  20,
  20
  )

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

input = list()

input$selected_scenarios = scenarios

shares_of_ev = c(0, 0.25, 0.5)

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

ggplot(summary %>% filter(share_of_ev == 0, vehicle != "Feeder (shop)"), aes(y=n, x=scenario, linetype = vehicle, color = vehicle, group = vehicle)) +
  scale_color_manual(values = c("grey25", "grey66", "grey80"), name = "Vehicle") + 
  scale_linetype_manual(values = c("solid", "dotted", "solid"), name = "Vehicle") + 
  geom_point(size = 4) + 
  geom_line(stat = "identity", size = 1) +
  ylab("Number of tours") +
  xlab("Share of cargo bikes (%)") +
  scale_y_continuous(limits = c(0,400), expand = c(0, 0)) + 
  theme_bw() 

ggplot(summary %>% filter(vehicle != "Feeder", vehicle != "Feeder (shop)", share_of_ev == 0), aes(y=parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = c("gray25", "gray80"), name = "Vehicle") +
  geom_bar(stat = "identity", position =  "fill", color  ="black") +
  ylab("Number ofparcels")  +
  xlab("Share of parcels by cargo bike (%)") +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

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
  scale_fill_manual(values = c("gray80", "gray66", "gray25"), name = "Vehicle") + 
  geom_bar(stat = "identity", position =  "stack", color = "black") +
  ylab("Distance travelled (km)") + 
  xlab("Share of cargo bikes (%)") +
  scale_y_continuous(expand = c(0,0), limits = c(0,6000)) +
  theme_bw()

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
  theme_bw() + theme() +scale_y_continuous(limits = c(0,4e6),expand = c(0, 0))


# ggplot(summary, aes(y= NOx, x=scenario, fill = vehicle, group = vehicle)) +
#   scale_fill_manual(values = colors_4_vehicles) + 
#   geom_area(stat = "identity", position = "stack") +
#   ylab("NOx emissions (g)") + 
#   xlab("Share of cargo bikes (%)") +
#   theme(text=element_text(size=14)) +
#   facet_wrap(.~share_of_ev) +
#   theme_bw()

#####tours####

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

tours_all$vehicle_type = factor(tours_all$vehicle_type, levels=c("truck", "cargoBike", "truck_feeder","truck_shop"), 
                                labels = c("Van", "Cargo bike", "Feeder", "Feeder (shop)"))
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
  scale_fill_manual(values = colors_4_vehicles) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) +
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  xlab("Share of cargo bikes (%)") +
  ylab("Share of distance") +
  labs(alpha = "Occupation rate (%)") + 
  theme(legend.position = "bottom")




leg_vs_service = tours_all %>% group_by(type, vehicle_type, scenario) %>%
  filter(vehicle_type != "truck_feeder") %>%
  summarize(time = sum(time), distance = sum(distance))

ggplot(leg_vs_service, aes(y=time, x = scenario, fill = vehicle_type, alpha = type)) +
  scale_alpha_manual(values = c(1,0.7)) + 
  scale_fill_manual(values = colors_4_vehicles) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~vehicle_type) + 
  theme_bw() + 
  labs(fill = "Vehicle")+ 
  labs(alpha = "Action (%)") + 
  xlab("Share of cargo bikes (%)") +
  ylab("Share of time") +
  theme(legend.position = "bottom")

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
